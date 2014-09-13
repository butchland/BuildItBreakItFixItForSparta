module UIInput where

import HighLevelTypes
import Util
import SafeNat

import Control.Monad
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.Char
import System.Environment
import Text.Read hiding (get)
import Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as BS

data CommandType =
    SummaryType
  | RoomType
  | SimultaneousType
  | RangeType
  | RangeDifferenceType

data UIReadCommand = UIReadCommand
  { readCommandToken :: ByteString
  , readCommandFilename :: FilePath
  , readCommandCommand :: ReadCommand String
  }
data ReadCommand n =
    SummaryC OutputMode
  | RoomC OutputMode (Person n)
  | TotalTimeC (Person n)
  | SimultaneousC OutputMode [Person n]
  | RangeC OutputMode (SafeNat32, SafeNat32)
  | RangeDifferenceC OutputMode (SafeNat32, SafeNat32) (SafeNat32, SafeNat32)
  deriving (Eq, Ord, Show)
readCommandMapPerson :: (Person a -> Person b) -> ReadCommand a -> ReadCommand b
readCommandMapPerson _ (SummaryC om) = SummaryC om
readCommandMapPerson f (RoomC om p) = RoomC om $ f p
readCommandMapPerson f (TotalTimeC p) = TotalTimeC $ f p
readCommandMapPerson f (SimultaneousC om ps) = SimultaneousC om $ map f ps
readCommandMapPerson _ (RangeC om r) = RangeC om r
readCommandMapPerson _ (RangeDifferenceC om r1 r2) = RangeDifferenceC om r1 r2

data UIAppendCommandTL =
    AppendC UIAppendCommand
  | BatchC FilePath
  deriving (Eq, Ord, Show)
data UIAppendCommand = UIAppendCommand
  { appendCommandToken :: ByteString
  , appendCommandFilename :: FilePath
  , appendCommandCommand :: AppendCommand String
  }
  deriving (Eq, Ord, Show)

data OutputMode = Text | HTML
  deriving (Eq, Ord, Show)

data BatchingMode = Batch | Single
  deriving (Eq, Ord, Show)

type Input a = StateT [String] Maybe a

runInput :: [String] -> Input a -> Maybe a
runInput args aI = evalStateT aI args

runInputOn :: Input a -> [String] -> Maybe a
runInputOn = flip runInput

runInputIO :: Input a -> IO (Maybe a)
runInputIO aM = do
  args <- getArgs
  return $ runInput args aM

pluck :: Input String
pluck = do
  i <- get
  case i of
    [] -> mzero
    s:ss -> do
      put ss
      return s

eoi :: Input ()
eoi = do
  i <- get
  case i of
    [] -> return ()
    _:_ -> mzero

expect :: Maybe a -> Input a
expect Nothing = mzero
expect (Just a) = return a

-- Predicates {{{

literalP :: String -> String -> Maybe ()
literalP x y
  | x == y = Just ()
  | otherwise = Nothing

alphaNumP :: String -> Maybe String
alphaNumP s = do
  guard $ all isAlphaNum s
  return s

filenameP :: String -> Maybe String
filenameP s = do
  guard $ all (\ c -> isAscii c || c == '_') s
  return s

justAlphaP :: String -> Maybe String
justAlphaP s = do
  guard $ all isAlpha s
  return s

personTypeP :: String -> Maybe PersonType
personTypeP "-E" = Just Employee
personTypeP "-G" = Just Guest
personTypeP _ = Nothing

nonNeg32IntegerP :: String -> Maybe SafeNat32
nonNeg32IntegerP s = do
  i :: Integer <- readMaybe s
  guard $ i >= 0
  guard $ i <= safeNat32Max
  return $ fromIntegral i

commandP :: String -> Maybe CommandType
commandP "-S" = Just SummaryType
commandP "-R" = Just RoomType
commandP "-I" = Just SimultaneousType
commandP "-A" = Just RangeType
commandP "-B" = Just RangeDifferenceType
commandP _ = Nothing

actionP :: String -> Maybe Action
actionP "-A" = Just Enter
actionP "-L" = Just Leave
actionP _ = Nothing

-- }}}

-- Small Parsing Actions {{{

parseLiteral :: String -> Input ()
parseLiteral s = expect . literalP s *$ pluck

parseToken :: Input ByteString
parseToken = liftM BS.pack $ expect . alphaNumP *$ pluck

parseFilename :: Input String
parseFilename = expect . filenameP *$ pluck

parseBound :: Input SafeNat32
parseBound = expect . nonNeg32IntegerP *$ pluck

parseBounds :: Input (SafeNat32, SafeNat32)
parseBounds = do
  parseLiteral "-L"
  low <- parseBound
  parseLiteral "-U"
  high <- parseBound
  return (low, high)

parseTime :: Input SafeNat32
parseTime = expect . nonNeg32IntegerP *$ pluck

parsePersonType :: Input PersonType
parsePersonType = expect . personTypeP *$ pluck

parsePersonName :: Input String
parsePersonName = expect . justAlphaP *$ pluck

parseRoom :: Input SafeNat32
parseRoom = expect . nonNeg32IntegerP *$ pluck

parsePerson :: Input (Person String)
parsePerson = do
  personType <- parsePersonType
  personName <- parsePersonName
  return $ Person personType personName

parseAction :: Input Action
parseAction = expect . actionP *$ pluck

parseLocation :: Input Location
parseLocation = do
  roomM <- optional $ parseLiteral "-R"
  case roomM of
    Nothing -> return Gallery
    Just () -> do
      room <- parseRoom
      return $ Room room

parseCommand :: Input CommandType
parseCommand = expect . commandP *$ pluck

-- }}}

parseReadCommand :: Input UIReadCommand
parseReadCommand = do
  parseLiteral "-K"
  token <- parseToken
  command <- msum
    [ do
        parseLiteral "-T"
        person <- parsePerson
        return $ TotalTimeC person
    , do
        htmlB <- liftM (maybe False $ const True) $ optional $ parseLiteral "-H"
        let outputMode = if htmlB then HTML else Text
        command <- parseCommand
        case command of
          SummaryType -> do
            return $ SummaryC outputMode
          RoomType -> do
            person <- parsePerson
            return $ RoomC outputMode person
          SimultaneousType -> do
            people <- many parsePerson
            return $ SimultaneousC outputMode people
          RangeType -> do
            bounds <- parseBounds
            return $ RangeC outputMode bounds
          RangeDifferenceType -> do
            bounds1 <- parseBounds
            bounds2 <- parseBounds
            return $ RangeDifferenceC outputMode bounds1 bounds2
    ]
  filename <- parseFilename
  eoi
  return $ UIReadCommand token filename command

parseAppendCommand :: Input UIAppendCommand
parseAppendCommand = do
  parseLiteral "-T"
  time <- parseTime
  parseLiteral "-K"
  token <- parseToken
  person <- parsePerson
  action <- parseAction
  location <- parseLocation
  filename <- parseFilename
  eoi
  return $ UIAppendCommand token filename $ AppendCommand time person action location

parseAppendCommandTL :: Input UIAppendCommandTL
parseAppendCommandTL = msum
  [ do
      parseLiteral "-B"
      filename <- parseFilename
      eoi
      return $ BatchC filename
  , do
      liftM AppendC parseAppendCommand
  ]

parseAppendCommands :: IO (Maybe (BatchingMode, [Maybe UIAppendCommand]))
parseAppendCommands = do
  cM <- runInputIO parseAppendCommandTL
  case cM of
    Nothing -> return Nothing
    Just c -> liftM Just $ do
      case c of
        BatchC filename -> do
          let mangle :: ByteString -> [[String]]
              mangle = map (map BS.unpack . BS.words) . BS.lines
          contents <- liftM mangle $ BS.readFile filename
          return $ (Batch, map (runInputOn parseAppendCommand) contents)
        AppendC uicommand ->
          return $ (Single, [Just uicommand])

coalesceBatchByFile :: [Maybe UIAppendCommand] -> [Maybe (FilePath, [UIAppendCommand])]
coalesceBatchByFile cs = 
  let (xs, xM) = coalesceBatchByFileRec cs
  in case xM of
    Nothing -> xs
    Just x -> Just x : xs

-- recursive function, returns a list of batched commands, and a list which
-- could combine with elements in front of the list to form batch commands
coalesceBatchByFileRec :: [Maybe UIAppendCommand] -> ([Maybe (FilePath, [UIAppendCommand])], Maybe (FilePath, [UIAppendCommand]))
-- the end of the commands.  result in no commands, with no possible batching commands
coalesceBatchByFileRec [] = ([], Nothing)
-- the current command is bad.  process the rest of the commands, and force
-- adding the batch commands to the output list (after this bad command), and
-- return no accumulator.
coalesceBatchByFileRec (Nothing:xs) = 
  let (output, toBatch) = coalesceBatchByFileRec xs
  in (Nothing : toBatch : output, Nothing)
-- the current command isi good.  process the rest of the commands and check
-- the accumulator.  if the accumulator is missing, return the output and use
-- this command as an accumulator.  if the accumulator is there and shares the
-- same filname, add this to the list of accumulated commands.  if the
-- accumulator is there but has a different filename, prepend the accumulator
-- to the output and use this a a single accumulator.
coalesceBatchByFileRec (Just x:xs) =
  let (output, toBatch) = coalesceBatchByFileRec xs
      xfn = appendCommandFilename x
  in case toBatch of
    Nothing -> (output, Just (xfn, [x]))
    Just (fn, batchCommands)
      | fn == xfn -> (output, Just (fn, x : batchCommands))
      | otherwise -> (Just (fn, batchCommands) : output, Just (xfn, [x]))
