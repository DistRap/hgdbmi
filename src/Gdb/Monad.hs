{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Gdb.Monad
  (
  -- * GDBError
    GDBError(..)
  -- * Transformer
  , GDBT
  , runGDBT
  -- * Monad
  , MonadGDB(..)
  -- * Runners
  , runGDB
  , runGDBConfig
  -- * Command
  , command
  , commandRaw
  , cmd
  , cmd'
  , cmdAny
  , cmdWarn
  , cli
  -- * Control
  , run
  , continue
  , interrupt
  , waitStop
  , showStops
  , break
  , file
  , load
  -- * Breakpoints
  , breakpoint
  , breakpoint'
  , waitBreak
  , onBreak
  , isBreak
  , isBreakHit
  -- * Evaluate expression
  , eval
  -- * MemAddress
  , MemAddress(..)
  , memAddr
  -- * Read memory
  , readMem
  -- * Programmer
  , Programmer(..)
  , extRemote
  -- * Util
  , echo
  ) where

import Prelude hiding (break)
import Control.Exception (SomeException)
import Control.Concurrent.STM (TMVar, TBQueue)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Bits (FiniteBits(..))
import Data.Default.Class (Default(def))
import Data.Word (Word32)
import Gdbmi.Commands (Medium)
import Gdbmi.IO (Config(..), Context)
import Gdbmi.Representation
  ( Command
  , Notification
  , Response(..)
  , Result
  , ResultClass(..)
  , Stream(..)
  , render_command
  )
import Gdbmi.IO (Callback(..))
import Gdbmi.Semantics (Breakpoint(..), Stopped, StopReason(..))
import System.Posix.Signals (Handler(Catch))

import qualified Control.Concurrent
import qualified Control.Concurrent.Async
import qualified Control.Concurrent.STM
import qualified Control.Exception
import qualified Control.Monad
import qualified Gdbmi.IO
import qualified Gdbmi.Commands
import qualified Gdbmi.Semantics
import qualified System.Posix.Signals
import qualified Text.Printf

data GDBContext = GDBContext {
    contextGdbMI   :: Context
  , contextStops   :: TMVar [Stopped]
  , contextLog     :: TBQueue String
  }

-- * GDBError

data GDBError
  = GDBError_IOException SomeException
  | GDBError_CommandFailed String String String
  | GDBError_UnexepectedResponse String [Result]
  deriving (Show)


-- * Transformer

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

-- * Monad

class ( MonadIO m
      , MonadError GDBError m
      ) => MonadGDB m where

  getContext :: m GDBContext
  default getContext
    :: ( MonadTrans t
       , MonadGDB m'
       , m ~ t m'
       )
    => m GDBContext
  getContext = lift getContext

  getMIContext :: m Context
  default getMIContext
    :: MonadGDB m
    => m Context
  getMIContext = contextGdbMI <$> getContext

instance MonadIO m => MonadGDB (GDBT m) where
  getContext = ask

instance MonadGDB m => MonadGDB (StateT s m)
instance MonadGDB m => MonadGDB (ReaderT r m)
instance MonadGDB m => MonadGDB (ExceptT GDBError m)

-- * Runners

-- | Run GDBT transformer with default @Config@
runGDB
  :: ( MonadIO m
     , MonadMask m
     )
  => GDBT m a
  -> m (Either GDBError a)
runGDB = runGDBConfig def

-- | Run GDBT transformer with custom @Config@
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
    stops <-
      Control.Concurrent.STM.atomically
      $ Control.Concurrent.STM.newEmptyTMVar
    (streamQ, notifQ, userQ) <-
      Control.Concurrent.STM.atomically
      $ (,,)
        <$> Control.Concurrent.STM.newTBQueue 1000
        <*> Control.Concurrent.STM.newTBQueue 1000
        <*> Control.Concurrent.STM.newTBQueue 1000
    pure (stops, streamQ, notifQ, userQ)
  ctx <-
    liftIO
      $ Gdbmi.IO.setup
          config
          (callbackQueues stops streamQ notifQ)

  x <- runGDBT (GDBContext ctx stops userQ) act
  liftIO $ Gdbmi.IO.shutdown ctx
  pure x

-- | `withGdb` variant that prints data from queues, installs `sigintHandler`
-- and prints all logs at the end
_withGDB'
  :: MonadIO m
  => Config
  -> GDBT IO a
  -> m (Either GDBError a)
_withGDB' config act = liftIO $ do
  stops <-
    Control.Concurrent.STM.atomically
    $ Control.Concurrent.STM.newEmptyTMVar
  (streamQ, notifQ, userQ) <-
    Control.Concurrent.STM.atomically $ (,,)
      <$> Control.Concurrent.STM.newTBQueue 1000
      <*> Control.Concurrent.STM.newTBQueue 1000
      <*> Control.Concurrent.STM.newTBQueue 1000

  lock <- Control.Concurrent.newMVar ()
  --- XXX: make this verbose mode configurable
  Control.Monad.void
    $ Control.Concurrent.Async.async
      $ Control.Monad.forever $ do
          stream <-
            Control.Concurrent.STM.atomically
            $ Control.Concurrent.STM.readTBQueue streamQ
          Control.Concurrent.withMVar
            lock
            $ const
            $ pstream stream

  Control.Monad.void
    $ Control.Concurrent.Async.async
      $ Control.Monad.forever $ do
          q <-
            Control.Concurrent.STM.atomically
            $ Control.Concurrent.STM.readTBQueue userQ
          Control.Concurrent.withMVar
            lock
            $ const
            $ putStrLn q

  ctx <-
    Gdbmi.IO.setup
      config
      (callbackQueues stops streamQ notifQ)

  x <-
    Control.Exception.try
    $ sigintHandler
    $ runGDBT (GDBContext ctx stops userQ) act

  let res = case x of
        Left e -> Left $ GDBError_IOException (e :: SomeException)
        Right (Left e) -> Left e
        Right (Right r) -> pure r

  Gdbmi.IO.shutdown ctx

  -- dump remaining data in streams
  (logs, _events) <-
    Control.Concurrent.STM.atomically
      $ (,)
        <$> Control.Concurrent.STM.flushTBQueue streamQ
        <*> Control.Concurrent.STM.flushTBQueue notifQ

  pstream $ concat logs
  -- we ignore async notifications (events) for now
  --print notif
  pure res
  where
    pstream' (Stream _ s) = putStr s
    pstream = mapM_ pstream'

    sigintHandler :: IO b -> IO b
    sigintHandler ofWhat = do
        tid <- Control.Concurrent.myThreadId
        Control.Exception.bracket
          (System.Posix.Signals.installHandler
             System.Posix.Signals.sigINT
             (Catch
                $ Control.Exception.throwTo
                    tid
                    Control.Exception.UserInterrupt
              )
             Nothing
          )
          (\old ->
             System.Posix.Signals.installHandler
               System.Posix.Signals.sigINT
               old
               Nothing
          )
          $ pure ofWhat

callbackQueues
  :: TMVar [Stopped]
  -> TBQueue [Stream]
  -> TBQueue [Notification]
  -> Callback
callbackQueues mv logs events =
  Callback
    (toQueue logs)
    (toQueue events)
    (Just
      (Control.Concurrent.STM.atomically
      . Control.Concurrent.STM.putTMVar mv)
    )
  where
    toQueue q =
      Control.Concurrent.STM.atomically
      . Control.Concurrent.STM.writeTBQueue q

sendCommand
  :: MonadGDB m
  => Command
  -> m Response
sendCommand c = do
  ctx <- getMIContext
  liftIO $ Gdbmi.IO.send_command ctx c

-- * Command

-- |Send GDB-MI command and fail if response differs from
-- expected response `rc`
command
  :: MonadGDB m
  => ResultClass
  -> Command
  -> m [Result]
command rc x = do
  resp <- sendCommand x
  Control.Monad.unless
    (respClass resp == rc)
    $ throwError
        $ GDBError_CommandFailed
            (render_command x)
            (show (respClass resp))
            (
              (  show
                . Gdbmi.Semantics.response_error
                . respResults
              )
              resp
            )
  pure (respResults resp)

-- |Send GDB-MI command and return raw response
commandRaw
  :: MonadGDB m
  => Command
  -> m Response
commandRaw = sendCommand

-- |Send GDB command but don't fail if response differs from
-- expected response `rc`, print error message instead
cmdWarn
  :: MonadGDB m
  => ResultClass
  -> Command
  -> m ()
cmdWarn rc x = do
  resp <- commandRaw x
  Control.Monad.unless
    (respClass resp /= rc)
    $ echo
       $ Text.Printf.printf
           "command '%s' got unexpected response (%s): %s"
           (render_command x)
           (show (respClass resp))
           (
             ( show
             . Gdbmi.Semantics.response_error
             . respResults
             )
             resp
           )

-- | Accept any response class
cmdAny
  :: MonadGDB m
  => Command
  -> m ()
cmdAny = Control.Monad.void . commandRaw

-- | Send GDB command expecting `rc` response
cmd
  :: MonadGDB m
  => ResultClass
  -> Command
  -> m [Result]
cmd = command

-- | Like `cmd` but discards result
cmd'
  :: MonadGDB m
  => ResultClass
  -> Command
  -> m ()
cmd' rc c = cmd rc c >> pure ()

-- | Run raw GDB cli command
cli
  :: MonadGDB m
  => String
  -> m ()
cli x = cmdAny $ Gdbmi.Commands.cli_command x

-- * Control

-- | Run program loaded in GDB
run
  :: MonadGDB m
  => m [Result]
run = do
  cmd
    RCRunning
    $ Gdbmi.Commands.exec_run

-- | Continue execution (for example after breakpoint)
continue
  :: MonadGDB m
  => m ()
continue = do
  cmd'
    RCRunning
    $ Gdbmi.Commands.exec_continue

-- | Interrupt background execution of the target
interrupt
  :: MonadGDB m
  => m ()
interrupt = do
  cmd'
    RCDone
    $ Gdbmi.Commands.exec_interrupt (Left True)

-- | Wait for stop condition (e.g. breakpoint)
waitStop
  :: MonadGDB m
  => m [Stopped]
waitStop = do
  tmvar <- contextStops <$> getContext

  _flushedOldStops <-
    liftIO
      $ Control.Concurrent.STM.atomically
      $ Control.Concurrent.STM.tryTakeTMVar tmvar

  stops <-
    liftIO
      $ Control.Concurrent.STM.atomically
      $ Control.Concurrent.STM.takeTMVar tmvar

  pure stops

-- TODO: more pretty
showStops
  :: MonadGDB m
  => [Stopped]
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

-- | Send Ctrl-C to GDB
break
  :: MonadGDB m
  => m ()
break =
  getMIContext
  >>= liftIO . Gdbmi.IO.interrupt

-- | Load file and its symbols
file
  :: MonadGDB m
  => FilePath
  -> m ()
file fp = do
  cmd'
    RCDone
    $ Gdbmi.Commands.file_exec_and_symbols (Just fp)

-- | Upload file to target device
load
  :: MonadGDB m
  => m [Result]
load = do
  cmd
    RCDone
    $ Gdbmi.Commands.target_download

-- * Breakpoints

-- | Create new breakpoint
breakpoint
  :: MonadGDB m
  => Gdbmi.Commands.Location
  -> m Breakpoint
breakpoint loc = do
  res <-
    cmd
      RCDone
      $ Gdbmi.Commands.break_insert
          False
          False
          False
          False
          False
          Nothing
          Nothing
          Nothing
          loc
  case Gdbmi.Semantics.response_break_insert res of
    Just value -> pure value
    Nothing ->
      throwError
        $ GDBError_UnexepectedResponse
            "response_break_insert"
            res

-- | Create new breakpoint, discard result
breakpoint'
  :: MonadGDB m
  => Gdbmi.Commands.Location
  -> m ()
breakpoint' = Control.Monad.void . breakpoint

-- | Wait till breakpoint is hit
waitBreak
  :: MonadGDB m
  => m Stopped
waitBreak = do
  stops <- waitStop
  if any isBreak stops
    then pure $ head $ filter isBreak stops
    else waitBreak

-- | Perform action `act` when breakpoint is hit
-- and continue afterwards
onBreak
  :: MonadGDB m
  => (Stopped -> m a)
  -> m a
onBreak act = do
  brk <- waitBreak
  ret <- act brk
  continue
  pure ret

-- | Did we stop due to breakpoint
isBreak
  :: Stopped
  -> Bool
isBreak = isBreakHit . Gdbmi.Semantics.stoppedReason

-- | Did we stop due to breakpoint
isBreakHit
  :: StopReason
  -> Bool
isBreakHit BreakpointHit{} = True
isBreakHit _ = False

-- * Evaluate expression

-- | Like `p` (var printing)
eval
  :: MonadGDB m
  => String
  -> m String
eval expr = do
  res <-
    cmd
      RCDone
      $ Gdbmi.Commands.data_evaluate_expression expr
  case Gdbmi.Semantics.response_data_evaluate_expression res of
    Just value -> pure value
    Nothing ->
      throwError
        $ GDBError_UnexepectedResponse
            "response_data_evaluate_expression"
            res

-- * MemAddress

newtype MemAddress = MemAddress
  { unMemAddress :: Word32 }
  deriving (Eq, Ord, Show, Num)

-- | Shorthand constructor
memAddr :: Word32 -> MemAddress
memAddr = MemAddress

-- * Read memory

-- | Read single memory segment from @MemAddress@
-- Segment size depends on Word type.
readMem
  :: forall a m
   . ( MonadGDB m
     , FiniteBits a
     , Integral a
     )
  => MemAddress -- ^ Memory address to read from
  -> m (Maybe a)
readMem addr = do
  res <-
    cmd
      RCDone
      $ Gdbmi.Commands.data_read_memory_bytes
          Nothing
          (show $ unMemAddress addr)
          (finiteBitSize (0 :: a) `div` 4)
  pure $ Gdbmi.Semantics.response_read_memory_bytes res

-- * Programmer

data Programmer =
    BMP String
  | BMPHosted String Int
  | RemoteGDB String Int
  deriving (Eq, Show)

extRemote
  :: MonadGDB m
  => Programmer
  -> m ()
extRemote prog = do
  cmd'
    RCDone
    $ Gdbmi.Commands.gdb_set "mi-async on"
  cmd'
    RCConnected
    $ Gdbmi.Commands.target_select
    $ Gdbmi.Commands.ExtendedRemote $ toTarget prog
  cli "monitor swdp_scan"
  cli "monitor connect_srs disable"
  cli "set mem inaccessible-by-default off"
  cmd'
    RCDone
    $ Gdbmi.Commands.gdb_set
        "mem inaccessible-by-default off"
  Control.Monad.unless
    (isRemoteGDB prog)
    $ cmd' RCDone $ Gdbmi.Commands.target_attach (Left 1)
  pure ()
  where
    isRemoteGDB (RemoteGDB _ _) = True
    isRemoteGDB _ = False

    toTarget
      :: Programmer
      -> Medium
    toTarget (BMP dev) =
      Gdbmi.Commands.SerialDevice dev
    toTarget (BMPHosted host port) =
      Gdbmi.Commands.TcpHost host port
    toTarget (RemoteGDB host port) =
      Gdbmi.Commands.TcpHost host port

-- * Util

-- | Write to log queue
echo
  :: MonadGDB m
  => String
  -> m ()
echo msg = do
  logQ <- contextLog <$> getContext
  liftIO
    $ Control.Concurrent.STM.atomically
    $ Control.Concurrent.STM.writeTBQueue logQ msg
