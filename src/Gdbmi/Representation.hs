-- | Data structures, parsers and printers for GDB/MI communication.
--
-- This is a low-level module not intended to be used by clients of this library.
-- Use 'Gdbmi.Commands' to create commands and receive responses as well as notifications via 'Gdbmi.IO'.
--
-- On-the-wire types reflect the GDB/MI grammar. Please consult the cited GDB documentation for details.
-- The simplified types provide a simple abstraction over the on-the-wire types.
module Gdbmi.Representation
-- exports {{{1
(
  -- * On-the-Wire Types
  -- ** Input
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Input-Syntax.html>
  Command(..),
  Token,
  Operation,
  Option(..),
  Parameter(..),
  -- ** Output
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html>
  Output(..),
  ResultRecord(..),
  OutOfBandRecord(..),
  AsyncRecord(..),
  ExecAsyncOutput(..),
  StatusAsyncOutput(..),
  NotifyAsyncOutput(..),
  AsyncOutput(..),
  ResultClass(..),
  AsyncClass(..),
  Result(..),
  Variable,
  Value(..), Const, Tuple(..), List(..),
  StreamRecord(..),
  ConsoleStreamOutput(..),
  TargetStreamOutput(..),
  LogStreamOutput(..),
  CString,
  -- * Simplified
  Response(..),
  Notification(..), NotificationClass(..),
  Stream(..), StreamClass(..),
  -- * Functions
  render_command,
  parse_output,
  output_response,
  output_notification,
  output_stream,
  GetToken(..),
  asConst, asTuple, asList,
  parameter_valid
) where

-- imports {{{1
import Data.Char (isAscii)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.Foldable (asum)
import Text.ParserCombinators.Parsec hiding (token)

-- input {{{1
-- types {{{2
data Command -- {{{3
  = CLICommand (Maybe Token) String
  | MICommand (Maybe Token) Operation [Option] [Parameter]

type Operation = String -- {{{3

data Option = Option Parameter (Maybe Parameter) -- {{{3

data Parameter -- {{{3
  -- the documentation does not specify this, but de-facto some parameters have
  -- to be quoted and other must not
  = RawString String
  | QuotedString String

-- rendering {{{2
render_command :: Command -> String -- {{{3
-- | Generate the on-the-wire string suitable to be sent to GDB.
render_command cmd = r_command cmd ""

r_command :: Command -> ShowS -- {{{3
r_command (CLICommand tok str) = maybe id r_token tok . showString str . showString "\n"
r_command (MICommand tok operation options parameters) =
    maybe id shows tok
  . showString "-" . r_operation operation
  . foldl (\f o -> f . showString " " . r_option o) id options
  . (if null parameters
    then id
    else showString " --" . foldl (\f p -> f . showString " " . r_parameter p) id parameters)
  . showString "\n"

r_operation :: Operation -> ShowS -- {{{3
r_operation op = (op++)

r_option :: Option -> ShowS -- {{{3
 -- the documentation specifies a "-" before each option but some operations
 -- such as file-exec-and-symbols are not happy with this :-/
r_option (Option p p') =
    r_parameter p
  . maybe id (\x -> showString " " . r_parameter x) p'

r_parameter :: Parameter -> ShowS -- {{{3
r_parameter (RawString s)    = showString s
r_parameter (QuotedString s) = shows s

r_token :: Token -> ShowS -- {{{3
r_token = shows

-- output {{{1
-- types {{{2
data Output -- {{{3
  = Output [OutOfBandRecord] (Maybe ResultRecord)
  deriving Show

data ResultRecord -- {{{3
  = ResultRecord (Maybe Token) ResultClass [Result]
  deriving Show

data OutOfBandRecord -- {{{3
  = OOBAsyncRecord AsyncRecord
  | OOBStreamRecord StreamRecord
  deriving Show

data AsyncRecord -- {{{3
  = ARExecAsyncOutput ExecAsyncOutput
  | ARStatusAsyncOutput StatusAsyncOutput
  | ARNotifyAsyncOutput NotifyAsyncOutput
  deriving Show

data ExecAsyncOutput -- {{{3
  = ExecAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data StatusAsyncOutput -- {{{3
  = StatusAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data NotifyAsyncOutput -- {{{3
  = NotifyAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data AsyncOutput -- {{{3
  = AsyncOutput AsyncClass [Result]
  deriving Show

data ResultClass -- {{{3
  = RCDone
  | RCRunning
  | RCConnected
  | RCError
  | RCExit
  deriving (Show, Eq)

data AsyncClass -- {{{3
  = ACStop
  | ACThreadGroupAdded
  | ACThreadGroupStarted
  | ACThreadCreated
  | ACThreadSelected
  | ACRunning
  | ACLibraryLoaded
  | ACThreadExited
  | ACThreadGroupExited
  | ACBreakpointCreated
  | ACBreakpointDeleted
  | ACBreakpointModified
  | ACParamChanged
  | ACMemoryChanged
  | ACRecordStarted
  | ACRecordStopped
  | ACDownload
  deriving (Show, Eq)

data Result -- {{{3
  = Result {
      resVariable :: Variable
    , resValue    :: Value
  }
  deriving Show

type Variable = String -- {{{3

data Value -- {{{3
  = VConst Const
  | VTuple Tuple
  | VList  List
  deriving Show

type Const = CString -- {{{3

data Tuple -- {{{3
  = Tuple {
      tupleResults :: [Result]
  }
  deriving Show

data List -- {{{3
  = EmptyList
  | ValueList [Value]
  | ResultList [Result]
  deriving Show

data StreamRecord  -- {{{3
  = SRConsoleStreamOutput ConsoleStreamOutput
  | SRTargetStreamOutput TargetStreamOutput
  | SRLogStreamOutput LogStreamOutput
  deriving Show

data ConsoleStreamOutput -- {{{3
  = ConsoleStreamOutput CString
  deriving Show

data TargetStreamOutput -- {{{3
  = TargetStreamOutput CString
  deriving Show

data LogStreamOutput -- {{{3
  = LogStreamOutput CString
  deriving Show

type CString = String -- {{{3

-- parsing {{{2
parse_output :: String -> Output -- {{{3
-- | Turn an GDB output string to an 'Output' value.
parse_output str = case parse p_output "gdb" str of
  Left pe -> error $ "parse failed: " ++ show pe ++ " input was: " ++ str
  Right o -> o

p_output :: Parser Output -- {{{3
-- http://sourceware.org/bugzilla/show_bug.cgi?id=7708
-- p_output = Output <$> many p_outOfBandRecord <*> optionMaybe p_resultRecord <* string "(gdb) "
p_output = do
  oob  <- many        p_outOfBandRecord
  rr   <- optionMaybe p_resultRecord
  oob' <- many        p_outOfBandRecord
  optional $ string "(gdb) "
  return $ Output (oob ++ oob') rr

p_resultRecord :: Parser ResultRecord -- {{{3
p_resultRecord =
  ResultRecord <$> optionMaybe p_token <* char '^' <*> p_resultClass <*> many (char ',' >> p_result) <* optional newline

p_outOfBandRecord :: Parser OutOfBandRecord -- {{{3
p_outOfBandRecord =
       try (p_asyncRecord  >>= return . OOBAsyncRecord)
  <|>      (p_streamRecord >>= return . OOBStreamRecord)

p_asyncRecord :: Parser AsyncRecord -- {{{3
p_asyncRecord =
      try (p_execAsyncOutput   >>= return . ARExecAsyncOutput)
  <|> try (p_statusAsyncOutput >>= return . ARStatusAsyncOutput)
  <|>     (p_notifyAsyncOutput >>= return . ARNotifyAsyncOutput)

p_execAsyncOutput :: Parser ExecAsyncOutput -- {{{3
p_execAsyncOutput =
  ExecAsyncOutput <$> optionMaybe p_token <* char '*' <*> p_asyncOutput

p_statusAsyncOutput :: Parser StatusAsyncOutput -- {{{3
p_statusAsyncOutput =
  StatusAsyncOutput <$> optionMaybe p_token <* char '+' <*> p_asyncOutput

p_notifyAsyncOutput :: Parser NotifyAsyncOutput -- {{{3
p_notifyAsyncOutput =
  NotifyAsyncOutput <$> optionMaybe p_token <* char '=' <*> p_asyncOutput

p_asyncOutput :: Parser AsyncOutput -- {{{3
p_asyncOutput =
  AsyncOutput <$> p_asyncClass <*> many (char ',' >> p_result) <* optional newline

p_resultClass :: Parser ResultClass -- {{{3
p_resultClass =
      try (string "done"      >> return RCDone)
  <|> try (string "running"   >> return RCRunning)
  <|> try (string "connected" >> return RCConnected)
  <|> try (string "error"     >> return RCError)
  <|>     (string "exit"      >> return RCExit)

p_asyncClass :: Parser AsyncClass -- {{{3
p_asyncClass = asum $ map try $ [
    string "breakpoint-created"   >> return ACBreakpointCreated
  , string "breakpoint-deleted"   >> return ACBreakpointDeleted
  , string "breakpoint-modified"  >> return ACBreakpointModified
  , string "cmd-param-changed"    >> return ACParamChanged
  , string "running"              >> return ACRunning
  , string "record-started"       >> return ACRecordStarted
  , string "record-stopped"       >> return ACRecordStopped
  , string "stopped"              >> return ACStop
  , string "thread-group-added"   >> return ACThreadGroupAdded
  , string "thread-group-started" >> return ACThreadGroupStarted
  , string "thread-created"       >> return ACThreadCreated
  , string "thread-selected"      >> return ACThreadSelected
  , string "thread-exited"        >> return ACThreadExited
  , string "thread-group-exited"  >> return ACThreadGroupExited
  , string "memory-changed"       >> return ACMemoryChanged
  , string "library-loaded"       >> return ACLibraryLoaded
  , string "download"             >> return ACDownload
  ]

p_result :: Parser Result -- {{{3
p_result =
      Result <$> p_variable <* char '=' <*> p_value
  <|> Result <$> pure "tuple" <*> p_value
  -- in case of download we have
  -- "7+download,{section=\".isr_vector\",section-size=\"472\",total-size=\"311082\"}"

p_variable :: Parser Variable -- {{{3
p_variable = many1 (letter <|> digit <|> oneOf "_-")

p_value :: Parser Value -- {{{3
p_value =
      (p_const >>= return . VConst)
  <|> (p_tuple >>= return . VTuple)
  <|> (p_list  >>= return . VList)

p_const :: Parser Const -- {{{3
p_const = p_cString

p_tuple :: Parser Tuple -- {{{3
p_tuple = try p_emptyTuple <|> p_filledTuple
  where
    p_emptyTuple = string "{}" >> return (Tuple [])
    p_filledTuple = do
      _ <- char '{'
      first <- p_result
      rest <- many (char ',' >> p_result)
      _ <- char '}'
      return $ Tuple (first:rest)

p_list :: Parser List -- {{{3
p_list = try p_emptyList <|> try p_valueList <|> p_resultList
  where
    p_emptyList = string "[]" >> return EmptyList
    p_valueList = do
      _ <- char '['
      first <- p_value
      rest <- many (char ',' >> p_value)
      _ <- char ']'
      return $ ValueList (first:rest)

    p_resultList = do
      _ <- char '['
      first <- p_result
      rest <- many (char ',' >> p_result)
      _ <- char ']'
      return $ ResultList (first:rest)

p_streamRecord :: Parser StreamRecord -- {{{3
p_streamRecord = do
  sr <- anyStreamRecord
  _ <- optional newline -- the documentation does not specifiy this newline, but this is what GDB is doing
  return sr
  where
    anyStreamRecord =
          (p_consoleStreamOutput >>= return . SRConsoleStreamOutput)
      <|> (p_targetStreamOutput  >>= return . SRTargetStreamOutput)
      <|> (p_logStreamOutput     >>= return . SRLogStreamOutput)

p_consoleStreamOutput :: Parser ConsoleStreamOutput -- {{{3
p_consoleStreamOutput = char '~' >> p_cString >>= return . ConsoleStreamOutput

p_targetStreamOutput :: Parser TargetStreamOutput -- {{{3
p_targetStreamOutput = char '@' >> p_cString >>= return . TargetStreamOutput

p_logStreamOutput :: Parser LogStreamOutput -- {{{3
p_logStreamOutput = char '&' >> p_cString >>= return . LogStreamOutput

p_cString :: Parser CString -- {{{3
p_cString = between (char '"') (char '"') (many p_cchar)
  where
    p_cchar = p_cbackslash
          <|> noneOf "\""
    p_cbackslash = do
      _ <- char '\\'
      c <- anyChar
      case c of
        '\\' -> return '\\'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        '"' -> return '"'
        _ -> fail $ "unknown backslash escape: " ++ show c

p_token :: Parser Token -- {{{3
p_token = many1 digit >>= return . read

-- simplification {{{1
data Response -- {{{2
  -- | The 'ResultRecord' of an 'Output'
  = Response {
      respClass   :: ResultClass
    , respResults :: [Result]
    }
    deriving (Show)

data Notification -- {{{2
  -- | Simplification of the 'AsyncRecord' type hierarchie, a possible 'OutOfBandRecord' value of an 'Output'.
  = Notification {
      notiClass      :: NotificationClass
    , notiAsyncClass :: AsyncClass
    , notiResults    :: [Result]
    }
    deriving Show

data NotificationClass -- {{{3
  = Exec    -- ^ 'ARExecAsyncOutput'
  | Status  -- ^ 'ARStatusAsyncOutput'
  | Notify  -- ^ 'ARNotifyAsyncOutput'
  deriving (Show, Eq)

data Stream -- {{{2
  -- | Simplifcation of the 'StreamRecord' type hierarchie, a possible 'OutOfBandRecord' value of an 'Output'.
  = Stream StreamClass String
  deriving Show

data StreamClass -- {{{3
  = Console  -- ^ 'SRConsoleStreamOutput'
  | Target   -- ^ 'SRTargetStreamOutpu'
  | Log      -- ^ 'SRLogStreamOutput'
  deriving Show

output_response :: Output -> Maybe Response -- {{{2
-- | Extract the response from an output, if existent.
output_response (Output _ Nothing) = Nothing
output_response (Output _ (Just (ResultRecord _ rc rs))) = Just $ Response rc rs

output_notification :: Output -> [Notification] -- {{{2
-- | Extract the (possible empty) list of notifications of an output.
output_notification (Output oobs _) = map (notification . unp) $ filter isNotification oobs
  where
    isNotification (OOBAsyncRecord _) = True
    isNotification _ = False

    unp (OOBAsyncRecord x) = x
    unp x = error $ "unexpected parameter: " ++ show x

    notification (ARExecAsyncOutput (ExecAsyncOutput _ (AsyncOutput ac rs))) = Notification Exec ac rs
    notification (ARStatusAsyncOutput (StatusAsyncOutput _ (AsyncOutput ac rs))) = Notification Status ac rs
    notification (ARNotifyAsyncOutput (NotifyAsyncOutput _ (AsyncOutput ac rs))) = Notification Notify ac rs

output_stream :: Output -> [Stream] -- {{{2
-- | Extract the (possibly) empty list of notifications of an output.
output_stream (Output oobs _) = map (stream . unp) $ filter isStream oobs
  where
    isStream (OOBStreamRecord _) = True
    isStream _ = False

    unp (OOBStreamRecord x) = x
    unp x = error $ "unexpected parameter: " ++ show x

    stream (SRConsoleStreamOutput (ConsoleStreamOutput s)) = Stream Console s
    stream (SRTargetStreamOutput (TargetStreamOutput s)) = Stream Target s
    stream (SRLogStreamOutput (LogStreamOutput s)) = Stream Log s

-- utils {{{2
asConst :: Value -> Maybe Const -- {{{2
-- | Coerce a value to a const.
asConst (VConst x) = Just x
asConst _          = Nothing

asTuple :: Value -> Maybe Tuple -- {{{2
-- | Coerce a value to a tuple.
asTuple (VTuple x) = Just x
asTuple _          = Nothing

asList  :: Value -> Maybe List -- {{{2
-- | Coerce a value to a list.
asList (VList x) = Just x
asList _         = Nothing

-- token {{{1
type Token = Int

-- | Return the token of the given object, if existent.
class GetToken a where
  get_token :: a -> Maybe Token

instance GetToken ResultRecord where
  get_token (ResultRecord token _ _) = token

instance GetToken Command where
  get_token (CLICommand token _) = token
  get_token (MICommand token _ _ _) = token

instance GetToken Output where
  get_token (Output _ (Just r)) = get_token r
  get_token _ = Nothing

instance GetToken OutOfBandRecord where
  get_token (OOBAsyncRecord r) = get_token r
  get_token (OOBStreamRecord _) = Nothing

instance GetToken AsyncRecord where
  get_token (ARExecAsyncOutput x) = get_token x
  get_token (ARStatusAsyncOutput x) = get_token x
  get_token (ARNotifyAsyncOutput x) = get_token x

instance GetToken ExecAsyncOutput where
  get_token (ExecAsyncOutput token _) = token

instance GetToken StatusAsyncOutput where
  get_token (StatusAsyncOutput token _) = token

instance GetToken NotifyAsyncOutput where
  get_token (NotifyAsyncOutput token _) = token

-- utils {{{1
parameter_valid :: Parameter -> Bool -- {{{2
-- | Verify that the given parameter is either a c-string or a \"non-blank-sequence\".
-- See <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Input-Syntax.html>
parameter_valid (RawString s) = validParam s
parameter_valid (QuotedString s) = validParam s

validParam :: String -> Bool -- {{{2
validParam param
  | null param = False
  | isCString param = isNothing $ find (not . isAscii) param
  | otherwise = isNothing $ find isSpecial param
  where
    isCString ('"':rest) = last rest == '"'
    isCString _          = False

    isSpecial ' '  = True
    isSpecial '-'  = True
    isSpecial '\n' = True
    isSpecial '"'  = True
    isSpecial _    = False
