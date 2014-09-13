module LowLevel where

import Types
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy (ByteString)
import System.IO
import MRand
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import SafeNat
import Data.Time.LocalTime
import FixedSizeSerializable

import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BS

newtype LogFile = LogFile { runLogFile :: Handle }
newtype LogSize = LogSize { runLogSize :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral, Show)

newtype MFile a = MFile { unMFile :: ReaderT LogFile (ErrorT Err (StateT LogSize MRand)) a}
  deriving (Monad, MonadIO, MonadReader LogFile, MonadState LogSize, MonadError Err)
instance LiftMRand MFile where
  liftMRand = MFile . lift . lift . lift
class LiftMFile m where
  liftMFile :: MFile a -> m a
instance LiftMFile MFile where
  liftMFile = id

doLogging :: Bool
doLogging = False

logfile :: FilePath
logfile = "log.log"

runMFile :: LogFile -> LogSize -> MFile a -> MRand (Either Err a, LogSize)
runMFile lf size aM = runStateT (runErrorT (runReaderT (unMFile aM) lf)) size

execMFile :: FilePath -> MFile a -> IO (Either Err a)
execMFile fp aM = do
  withFile fp ReadWriteMode $ \ h -> do
    size <- hFileSize h
    liftM fst $ runRandom $ runMFile (LogFile h) (LogSize $ fromIntegral size) aM

getLogFile :: MFile LogFile
getLogFile = ask

getLogSize :: MFile LogSize
getLogSize = get

putLogSize :: LogSize -> MFile ()
putLogSize = put

throwMFile :: Err -> MFile a
throwMFile = throwError

catchMFile :: MFile a -> (Err -> MFile a) -> MFile a
catchMFile = catchError

guardErr :: Err -> Bool -> MFile ()
guardErr _ True = return ()
guardErr err False = throwMFile err

mlog :: String -> MFile ()
mlog msg = when doLogging $ liftIO $ withFile logfile AppendMode $ \ h -> hPutStrLn h msg

mlogLines :: [String] -> MFile ()
mlogLines = mlog . concat . List.intersperse "\n"

blockPtrToOffset :: BlockPtr -> Integer
blockPtrToOffset (BlockPtr p) = sinteger masterBlockSize + sinteger p * sinteger dataBlockSize

blockPtrFromOffset :: Integer -> BlockPtr
blockPtrFromOffset i = BlockPtr $ fromInteger $ (i - sinteger masterBlockSize) `div` sinteger dataBlockSize

padFile :: SafeNat32 -> MFile ()
padFile n = do
  lfh <- liftM runLogFile getLogFile
  liftIO $ BS.hPut lfh $ BS.replicate (sint64 n) ' '

initMFile :: MFile Bool
initMFile = do
  when doLogging $ liftIO $ do
    time <- getZonedTime
    withFile logfile AppendMode $ \ h -> do
      hPutStrLn h "--"
      hPutStrLn h $ "new session: " ++ show time
      hPutStrLn h "--"
  size <- getLogSize
  if size == 0
    then do
      padFile masterBlockSize
      putLogSize $ LogSize $ sinteger masterBlockSize
      return True
    else
      return False

newBlock :: MFile BlockPtr
newBlock = do
  lfh <- liftM runLogFile getLogFile
  oldSize <- liftM runLogSize getLogSize
  let newSize = LogSize $ oldSize + sinteger dataBlockSize
  liftIO $ hSeek lfh AbsoluteSeek $ fromIntegral oldSize
  padFile dataBlockSize
  putLogSize newSize
  return $ blockPtrFromOffset oldSize

readDataBlock :: BlockPtr -> MFile (FixedSizeBytes (SizeOf EncDataBlock))
readDataBlock blk = do
  lfh <- liftM runLogFile $ getLogFile
  liftIO $ do
    hSeek lfh AbsoluteSeek $ blockPtrToOffset blk
    liftM assertFixedSizeBytes $ BS.hGet lfh $ sint dataBlockSize

writeDataBlock :: BlockPtr -> FixedSizeBytes (SizeOf EncDataBlock) -> MFile ()
writeDataBlock blk s = do
  lfh <- liftM runLogFile $ getLogFile
  liftIO $ do
    hSeek lfh AbsoluteSeek $ blockPtrToOffset blk
    BS.hPut lfh $ getByteString s

readMasterBlock :: MFile (FixedSizeBytes (SizeOf EncMasterBlock))
readMasterBlock = do
  lfh <- liftM runLogFile $ getLogFile
  liftIO $ do
    hSeek lfh AbsoluteSeek 0 
    liftM assertFixedSizeBytes $ BS.hGet lfh $ sint masterBlockSize

writeMasterBlock :: FixedSizeBytes (SizeOf EncMasterBlock) -> MFile ()
writeMasterBlock s = do
  lfh <- liftM runLogFile $ getLogFile
  liftIO $ do
    hSeek lfh AbsoluteSeek 0
    BS.hPut lfh $ getByteString s

readTailBlock :: TailBlockPtr -> SafeNat32 -> MFile ByteString
readTailBlock blk tailSize = do
  lfh <- liftM runLogFile $ getLogFile
  size <- liftM runLogSize getLogSize
  let offset = blockPtrToOffset $ unTailBlockPtr blk
  when (offset + sinteger tailSize > size) $
    throwMFile $ TailBlockOverflowRead blk tailSize
  liftIO $ do
    hSeek lfh AbsoluteSeek offset
    BS.hGet lfh $ sint tailSize

destroyTailBlock :: TailBlockPtr -> MFile ()
destroyTailBlock blk = do
  let offset = blockPtrToOffset $ unTailBlockPtr blk
  putLogSize $ LogSize offset

appendTailBlock :: ByteString -> MFile TailBlockPtr
appendTailBlock bs = do
  lfh <- liftM runLogFile getLogFile
  size <- liftM runLogSize getLogSize
  liftIO $ do
    hSeek lfh AbsoluteSeek size
    BS.hPut lfh bs
  putLogSize $ LogSize $ size + toInteger (BS.length bs)
  return $ TailBlockPtr $ blockPtrFromOffset size
