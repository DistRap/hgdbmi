{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Gdb.Monad
  ( GDBT
  , GDBError(..)
  , runGDBT
  , MonadGDB(..)
  , runGDB
  , runGDBConfig
  , command
  , commandRaw
  , cmd
  , cmd'
  , cmdAny
  , cmdWarn
  , run
  , continue
  , interrupt
  , waitStop
  , isBreak
  , isBreakHit
  , waitBreak
  , onBreak
  , showStops
  , cli
  , echo
  , break
  , breakpoint
  , breakpoint'
  , eval
  , readMem
  , Programmer(..)
  , extRemote
  , file
  , load
  ) where

import Prelude hiding (break)
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Default.Class (Default(def))
import Gdbmi.IO (Config(..), Context)

import System.Posix.Signals (installHandler, Handler(Catch), sigINT)

import Text.Printf (printf)

import qualified Gdbmi.IO             as G
import qualified Gdbmi.Commands       as C
import qualified Gdbmi.Semantics      as S
import qualified Gdbmi.Representation as R

data GDBContext = GDBContext {
    contextGdbMI   :: G.Context
  , contextStops   :: TMVar [S.Stopped]
  , contextLog     :: TBQueue String
  }

data GDBError
  = GDBError_IOException SomeException
  | GDBError_CommandFailed String String String
  | GDBError_UnexepectedResponse String [R.Result]
  deriving (Show)

newtype GDBT m a = GDBT
  { _unGDBT
      :: ExceptT GDBError
          (ReaderT GDBContext m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader GDBContext
    , MonadError GDBError
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadIO
    )

instance MonadTrans GDBT where
  lift = GDBT . lift . lift

-- | Run GDBT transformer
runGDBT
  :: Monad m
  => GDBContext
  -> GDBT m a
  -> m (Either GDBError a)
runGDBT ctx =
    (`runReaderT` ctx)
  . runExceptT
  . _unGDBT

class ( MonadIO m
      , MonadError GDBError m
      ) => MonadGDB m where
  getContext :: m GDBContext
  getMIContext :: m Context

instance MonadIO m => MonadGDB (GDBT m) where
  getContext = ask
  getMIContext = contextGdbMI <$> ask

runGDB
  :: ( MonadIO m
     , MonadMask m
     )
  => GDBT m a
  -> m (Either GDBError a)
runGDB = runGDBConfig def

runGDBConfig
  :: ( MonadIO m
     , MonadMask m
     )
  => Config
  -> GDBT m b
  -> m (Either GDBError b)
runGDBConfig config act =
  withGDB config act

-- | Set-up GDB connection and run `GDBT` application
withGDB
  :: MonadIO m
  => Config
  -> GDBT m a
  -> m (Either GDBError a)
withGDB config act = do
  (stops, streamQ, notifQ, userQ) <- liftIO $ do
    stops <- atomically $ newEmptyTMVar
    (streamQ, notifQ, userQ) <- atomically $ (,,)
      <$> newTBQueue 1000
      <*> newTBQueue 1000
      <*> newTBQueue 1000
    pure (stops, streamQ, notifQ, userQ)
  ctx <- liftIO $ G.setup config (callbackQueues stops streamQ notifQ)

  x <- runGDBT (GDBContext ctx stops userQ) act
  liftIO $ G.shutdown ctx
  pure x

-- | `withGdb` variant that prints data from queues, installs `sigintHandler`
-- and prints all logs at the end
_withGDB'
  :: MonadIO m
  => Config
  -> GDBT IO a
  -> m (Either GDBError a)
_withGDB' config act = liftIO $ do
  stops <- atomically $ newEmptyTMVar
  (streamQ, notifQ, userQ) <- atomically $ (,,)
    <$> newTBQueue 1000
    <*> newTBQueue 1000
    <*> newTBQueue 1000

  lock <- newMVar ()
  --- XXX: make this verbose mode configurable
  void $ async $ forever $ do
    stream <- atomically $ readTBQueue streamQ
    withMVar lock $ const $ pstream stream

  void $ async $ forever $ do
    q <- atomically $ readTBQueue userQ
    withMVar lock $ const $ putStrLn q

  ctx <- G.setup config (callbackQueues stops streamQ notifQ)

  x <- try
    $ sigintHandler
    $ runGDBT (GDBContext ctx stops userQ) act

  let res = case x of
        Left e -> Left $ GDBError_IOException (e :: SomeException)
        Right (Left e) -> Left e
        Right (Right r) -> pure r

  G.shutdown ctx

  -- dump remaining data in streams
  (logs, _events) <- atomically $ (,) <$> flushTBQueue streamQ <*> flushTBQueue notifQ

  pstream $ concat logs
  -- we ignore async notifications (events) for now
  --print notif
  pure res
  where
    pstream' (R.Stream _ s) = putStr s
    pstream = mapM_ pstream'

    sigintHandler :: IO b -> IO b
    sigintHandler ofWhat = do
        tid <- myThreadId
        bracket
          (installHandler sigINT (Catch $ throwTo tid UserInterrupt) Nothing)
          (\old -> installHandler sigINT old Nothing)
          $ pure ofWhat

callbackQueues
  :: TMVar [S.Stopped]
  -> TBQueue [R.Stream]
  -> TBQueue [R.Notification]
  -> G.Callback
callbackQueues mv logs events =
  G.Callback
    (toQueue logs)
    (toQueue events)
    (Just (atomically . putTMVar mv))
  where
    toQueue q = atomically . writeTBQueue q

sendCommand
  :: MonadGDB m
  => R.Command
  -> m R.Response
sendCommand c = do
  ctx <- getMIContext
  liftIO $ G.send_command ctx c

-- |Send GDB-MI command and fail if response differs from
-- expected response `rc`
command
  :: MonadGDB m
  => R.ResultClass
  -> R.Command
  -> m [R.Result]
command rc x = do
  resp <- sendCommand x
  unless
    (R.respClass resp == rc)
    $ throwError
        $ GDBError_CommandFailed
            (R.render_command x)
            (show (R.respClass resp))
            ((show . S.response_error . R.respResults) resp)
  pure (R.respResults resp)

-- |Send GDB-MI command and return raw response
commandRaw
  :: MonadGDB m
  => R.Command
  -> m R.Response
commandRaw = sendCommand

-- |Send GDB command but don't fail if response differs from
-- expected response `rc`, print error message instead
cmdWarn
  :: MonadGDB m
  => R.ResultClass
  -> R.Command
  -> m ()
cmdWarn rc x = do
  resp <- commandRaw x
  unless
    (R.respClass resp /= rc)
    $ echo
       $ Text.Printf.printf "command '%s' got unexpected response (%s): %s"
           (R.render_command x)
           (show (R.respClass resp))
           ((show . S.response_error . R.respResults) resp)

-- | Accept any response class
cmdAny
  :: MonadGDB m
  => R.Command
  -> m ()
cmdAny = void . commandRaw

-- | Send GDB command expecting `rc` response
cmd
  :: MonadGDB m
  => R.ResultClass
  -> R.Command
  -> m [R.Result]
cmd = command

-- | Like `cmd` but discards result
cmd'
  :: MonadGDB m
  => R.ResultClass
  -> R.Command
  -> m ()
cmd' rc c = cmd rc c >> pure ()

-- | Run program loaded in GDB
run
  :: MonadGDB m
  => m [R.Result]
run = do
  cmd R.RCRunning $ C.exec_run

-- | Continue execution (for example after breakpoint)
continue
  :: MonadGDB m
  => m ()
continue = do
  cmd' R.RCRunning $ C.exec_continue

-- | Interrupt background execution of the target
interrupt
  :: MonadGDB m
  => m ()
interrupt = do
  cmd'
    R.RCDone
    $ C.exec_interrupt (Left True)

-- | Wait for stop condition (e.g. breakpoint)
waitStop
  :: MonadGDB m
  => m [S.Stopped]
waitStop = do
  tmvar <- contextStops <$> getContext

  _flushedOldStops <- liftIO $ atomically $ do
    tryTakeTMVar tmvar

  stops <- liftIO $ atomically $ do
    takeTMVar tmvar
  pure stops

-- | Did we stop due to breakpoint
isBreak
  :: S.Stopped
  -> Bool
isBreak = isBreakHit . S.stoppedReason

-- | Did we stop due to breakpoint
isBreakHit
  :: S.StopReason
  -> Bool
isBreakHit S.BreakpointHit{} = True
isBreakHit _ = False

-- | Wait till breakpoint is hit
waitBreak
  :: MonadGDB m
  => m S.Stopped
waitBreak = do
  stops <- waitStop
  if any isBreak stops
    then pure $ head $ filter isBreak stops
    else waitBreak

-- | Perform action `act` when breakpoint is hit
-- and continue afterwards
onBreak
  :: MonadGDB m
  => (S.Stopped -> m ())
  -> m ()
onBreak act = do
  brk <- waitBreak
  act brk
  continue

-- TODO: more pretty
showStops
  :: MonadGDB m
  => [S.Stopped]
  -> m ()
showStops = mapM_ showStop
  where
    showStop
      :: ( MonadIO m
         , Show a
         )
      => a
      -> m ()
    showStop s = liftIO $ print s

-- | Run raw GDB cli command
cli
  :: MonadGDB m
  => String
  -> m ()
cli x = cmdAny $ C.cli_command x

echo
  :: MonadGDB m
  => String
  -> m ()
echo msg = do
  logQ <- contextLog <$> getContext
  liftIO $ atomically $ writeTBQueue logQ msg

-- | Send Ctrl-C to GDB
break
  :: MonadGDB m
  => m ()
break =
  getMIContext
  >>= liftIO . G.interrupt

-- | Create new breakpoint
breakpoint
  :: MonadGDB m
  => C.Location
  -> m S.Breakpoint
breakpoint loc = do
  res <- cmd R.RCDone $ C.break_insert False False False False False Nothing Nothing Nothing loc
  case S.response_break_insert res of
    Just value -> pure value
    Nothing ->
      throwError
        $ GDBError_UnexepectedResponse
            "response_break_insert"
            res

-- | Create new breakpoint, discard result
breakpoint'
  :: MonadGDB m
  => C.Location
  -> m ()
breakpoint' = void . breakpoint

-- | Like `p` (var printing)
eval
  :: MonadGDB m
  => String
  -> m String
eval expr = do
  res <- cmd R.RCDone $ C.data_evaluate_expression expr
  case S.response_data_evaluate_expression res of
    Just value -> pure value
    Nothing ->
      throwError
        $ GDBError_UnexepectedResponse
            "response_data_evaluate_expression"
            res

readMem
  :: ( MonadGDB m
     , Show a
     , Num b
     )
  => a
  -> Int
  -> m (Maybe b)
readMem addr size = do
  res <- cmd R.RCDone $ C.data_read_memory_bytes Nothing (show addr) size
  pure $ S.response_read_memory_bytes res

data Programmer =
    BMP String
  | BMPHosted String Int
  deriving (Eq, Show)

toTarget :: Programmer -> C.Medium
toTarget (BMP dev) = C.SerialDevice dev
toTarget (BMPHosted host port) = C.TcpHost host port

extRemote
  :: MonadGDB m
  => Programmer
  -> m ()
extRemote prog = do
  cmd' R.RCDone $ C.gdb_set "mi-async on"
  _ <- cmd R.RCConnected $ C.target_select $ C.ExtendedRemote $ toTarget prog
  cli "monitor swdp_scan"
  cli "monitor connect_srs disable"
  cli "set mem inaccessible-by-default off"
  cmd' R.RCDone $ C.gdb_set "mem inaccessible-by-default off"
  _ <- cmd R.RCDone $ C.target_attach (Left 1)
  pure ()

-- | Load file and its symbols
file
  :: MonadGDB m
  => FilePath
  -> m ()
file fp = do
  cmd' R.RCDone $ C.file_exec_and_symbols (Just fp)

-- | Upload file to target device
load
  :: MonadGDB m
  => m [R.Result]
load = do
  cmd R.RCDone $ C.target_download
