-- | Semantical data structures and conversion functions for GDB\/MI output.
--
-- While working with 'Gdbmi.Representation.Response' and 'Gdbmi.Representation.Notification' is always possible in general,
-- handling the generic 'Gdbmi.Representation.Result' lists is cumbersome.
-- This module provides convenient data types instead to facilitate pattern matching etc..
--
-- This module is incomplete, as we only implemented what we needed up to now.
module Gdbmi.Semantics
-- export {{{1
(
  -- * Conversion Functions
  -- | A conversion fails if the result list does not contain the expected values.
  response_break_insert,
  response_data_evaluate_expression,
  response_exec_return,
  response_stack_info_frame,
  response_stack_list_frames,
  response_read_memory_bytes,
  response_error,
  notification_stopped,
  -- * Types
  -- | Please consult the GDB manual for details on the returned responses.
  Breakpoint(..), BreakpointType, BreakpointDisp(..), BkptNumber,
  Stack(..), Frame(..), Arg(..),
  Stopped(..), StopReason(..),
  toBigEndian
) where

-- import {{{1
import Control.Applicative ((<|>))
import Control.Monad (guard, msum, (<=<))
import Data.List (find)

import Gdbmi.Representation

-- types {{{1
type BkptNumber = Int
data Breakpoint = Breakpoint { -- {{{2
    bkptNumber           :: BkptNumber
  , bkptType             :: BreakpointType
  , bkptDisp             :: BreakpointDisp
  , bkptEnabled          :: Bool
  , bkptAddress          :: String
  , bkptFunc             :: String
  , bkptFile             :: String
  , bkptFullname         :: String
  , bkptLine             :: Int
  , bkptTimes            :: Int
  , bkptOriginalLocation :: String
  }
  deriving Show

type BreakpointType = String -- {{{2

data BreakpointDisp -- {{{2
  = BreakpointKeep
  | BreakpointDel
  deriving Show

instance Read BreakpointDisp where
  readsPrec _ "del" = [(BreakpointDel, "")]
  readsPrec _ "keep" = [(BreakpointKeep, "")]
  readsPrec _ _ = []

newtype Stack -- {{{2
  = Stack {stackFrames :: [Frame] }
  deriving Show

data Frame = Frame { -- {{{2
    frameLevel    :: Maybe Int
  , frameAddr     :: String
  , frameFunc     :: String
  , frameArgs     :: Maybe [Arg]
  , frameFile     :: Maybe String
  , frameFullname :: Maybe String
  , frameLine     :: Maybe Int
  } deriving Show

data Stopped = Stopped { -- {{{2
      stoppedReason   :: StopReason
    , stoppedFrame    :: Frame
    , stoppedThreadId :: Int
    , stoppedThreads  :: String
    , stoppedCore     :: Int
  }
  deriving Show

data StopReason -- {{{2
  = BreakpointHit {
      bkptHitDisp   :: BreakpointDisp
    , bkptHitNumber :: BkptNumber
    }
  | EndSteppingRange
  | FunctionFinished
  | NoReason -- after attach we get stopped with missing reason

  deriving Show

data Arg = Arg { -- {{{2
    argName  :: String
  , argValue :: String
  } deriving Show

-- composition {{{1
responseBreakpoint :: Result -> Maybe Breakpoint -- {{{2
responseBreakpoint (Result variable value) = do
  guard (variable == "bkpt")
  (Tuple rs) <- asTuple value
  Breakpoint
    <$> get rs tryRead "number"
    <*> get rs Just    "type"
    <*> get rs tryRead "disp"
    <*> get rs gdbBool "enabled"
    <*> get rs Just    "addr"
    <*> get rs Just    "func"
    <*> get rs Just    "file"
    <*> get rs Just    "fullname"
    <*> get rs tryRead "line"
    <*> get rs tryRead "times"
    <*> get rs Just    "original-location"

responseStack :: Result -> Maybe Stack -- {{{2
responseStack (Result variable value) = do
  guard (variable == "stack")
  list <- asList value
  case list of
    EmptyList -> Just $ Stack []
    ResultList is ->
      Stack <$> mapM responseFrame is
    _ -> Nothing

responseFrame :: Result -> Maybe Frame -- {{{2
responseFrame (Result variable value) = do
  guard (variable == "frame")
  (Tuple rs) <- asTuple value
  Frame
    <$> Just (get rs tryRead "level")
    <*>       get rs Just    "addr"
    <*>       get rs Just    "func"
    <*> Just (msum (map responseArgs rs))
    <*> Just (get rs Just   "file")
    <*> Just (get rs Just   "fullname")
    <*> Just (get rs tryRead "line")

responseStopped :: [Result] -> Maybe Stopped -- {{{2
responseStopped rs = do
  Stopped
    <$> (responseStopReason rs <|> pure NoReason)
    <*> msum (map responseFrame rs)
    <*> get rs tryRead "thread-id"
    <*> get rs Just    "stopped-threads"
    <*> (get rs tryRead "core" <|> pure 0)

responseStopReason :: [Result] -> Maybe StopReason  -- {{{2
responseStopReason rs = do
  reason <- find (("reason"==) . resVariable) rs >>= asConst . resValue
  case reason of
    "breakpoint-hit" ->
      BreakpointHit
        <$> get rs tryRead "disp"
        <*> get rs tryRead "bkptno"
    "end-stepping-range" -> Just EndSteppingRange
    "function-finished"  -> Just FunctionFinished
    _ -> Nothing

responseArgs :: Result -> Maybe [Arg] -- {{{2
responseArgs (Result variable value) = do
  guard (variable == "args")
  list <- asList value
  case list of
    EmptyList -> Just []
    ValueList is -> do
      mapM ((responseArg . tupleResults) <=< asTuple) is
    _ -> Nothing

responseArg :: [Result] -> Maybe Arg -- {{{2
responseArg rs = do
  Arg
    <$> get rs Just "name"
    <*> get rs Just "value"

-- functions {{{1
response_stack_list_frames :: [Result] -> Maybe Stack -- {{{2
-- | Convert the result list of a 'Gdbmi.Commands.stack_list_frames' command response.
response_stack_list_frames [item] = responseStack item
response_stack_list_frames _      = Nothing

response_stack_info_frame :: [Result] -> Maybe Frame -- {{{2
-- | Convert the result list of a 'Gdbmi.Commands.stack_info_frame' command response.
response_stack_info_frame [item] = responseFrame item
response_stack_info_frame _      = Nothing

response_break_insert :: [Result] -> Maybe Breakpoint -- {{{2
-- | Convert the result list of a 'Gdbmi.Commands.break_insert' command response.
response_break_insert [item] = responseBreakpoint item
response_break_insert _      = Nothing

response_data_evaluate_expression :: [Result] -> Maybe String -- {{{2
-- | Convert the result list of a 'Gdbmi.Commands.data_evaluate_expression' command response.
response_data_evaluate_expression [(Result variable value)] = do
  guard  (variable == "value")
  asConst value
response_data_evaluate_expression _ = Nothing

response_exec_return :: [Result] -> Maybe Frame -- {{{2
-- | Convert the result list of a 'Gdbmi.Commands.exec_return' command response.
response_exec_return [item] = responseFrame item
response_exec_return _      = Nothing

-- | Convert the result of a `Gdmi.data_read_memory_bytes` to number
response_read_memory_bytes :: (Num a) => [Result] -> Maybe a
response_read_memory_bytes [(Result variable value)] = do
  guard (variable == "memory")
  l <- asList value
  t <- case l of
    ValueList is -> do
      mapM ((hexContents . tupleResults) <=< asTuple) is
    _ -> Nothing

  guard (t /= mempty)
  fromInteger <$> headMay t
response_read_memory_bytes _ = Nothing

hexContents :: [Result] -> Maybe Integer
hexContents rs = get rs (tryRead . ("0x"++) . toBigEndian) "contents"

toBigEndian :: String -> String
toBigEndian a = go $ reverse a
  where go (x:y:xs) = y:x:(go xs)
        go [_] = error "Got even number of characters in toBigEndian"
        go [] = ""

headMay :: [a] -> Maybe a
headMay (x:_xs) = Just x
headMay [] = Nothing


response_error :: [Result] -> Maybe String -- {{{2
-- | Convert the result list of a 'Gdbmi.Representation.Response' with 'Gdbmi.Representation.ResultClass' 'Gdbmi.Representation.RCError'.
response_error [(Result variable value)] = do
  guard (variable == "msg")
  asConst value
response_error _ = Nothing

notification_stopped :: [Result] -> Maybe Stopped -- {{{2
-- | Convert the result list of a 'Gdbmi.Representation.Notification' with 'Gdbmi.Representation.NotificationClass' 'Gdbmi.Representation.Exec' and 'Gdbmi.Representation.AsyncClass' 'Gdbmi.Representation.ACStop'.
notification_stopped items = responseStopped items

-- utils {{{1
get :: [Result] -> (String -> Maybe a) -> (String -> Maybe a) -- {{{2
get rs parser key = find ((key==) . resVariable) rs >>= asConst . resValue >>= parser

tryRead :: Read a => String -> Maybe a -- {{{2
tryRead str = case readsPrec 0 str of
  [(x, "")] -> Just x
  _ -> Nothing

gdbBool :: String -> Maybe Bool -- {{{2
gdbBool "y" = Just True
gdbBool "n" = Just False
gdbBool _ = Nothing
