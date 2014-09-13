module MidLevel where

import CryptoUtil
import LowLevel
import MRand
import FixedSizeSerializable
import SafeNat
import Util
import Lens
import Types
import HighLevelTypes
import Data.Vector (Vector)
import HighLevel
import qualified Data.Serialize as Cereal
import Codec.Compression.GZip

import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (fail, last)
import qualified Data.Vector as Vector

import Data.Word (Word8)
import qualified Data.ByteString.Lazy.Char8 as BS

newtype MM a = MM { unMM :: StateT MasterBlockInfo MFile a}
  deriving (Monad, MonadState MasterBlockInfo, MonadIO)
instance LiftMFile MM where
  liftMFile = MM . lift
instance LiftMRand MM where
  liftMRand = MM . lift . liftMRand

compressOptions = defaultCompressParams { compressLevel = bestSpeed }

getMasterBlockInfo :: MM MasterBlockInfo
getMasterBlockInfo = MM get

putMasterBlockInfo :: MasterBlockInfo -> MM ()
putMasterBlockInfo = MM . put

getMetaAndSummary :: CryptoT MM TailBlock
getMetaAndSummary = do
  tailInfoM <- lift $ tailBlockInfo ^$ getMasterBlockInfo 
  case tailInfoM of
    Nothing -> return $ TailBlock Vector.empty initialSummary
    Just (TailBlockInfo ptr size hash nonce) -> do
      rawCipher <- liftMFile $ readTailBlock ptr size
      assertMultipleSizeBytes rawCipher $ \ cipher -> do
        mac <- hmac $ getByteString cipher
        liftMFile $ when (mac /= hash) $
          throwMFile IntegrityViolation
        markedCompressedPlainText <- getByteString ^$ decrypt nonce $ CipherText cipher
        (padLen :: Word8,paddedCompressedPlainText) <- do
          liftMFile $ whenLeft (const $ throwMFile CorruptedData) $
            Cereal.runGetLazyState Cereal.getWord8 markedCompressedPlainText
        let compressedPlainText = BS.drop (fromIntegral padLen) paddedCompressedPlainText
        let plainText = decompress $ getByteString compressedPlainText
        liftMFile $ whenLeft (const $ throwMFile CorruptedData) $ Cereal.decode $ BS.toStrict plainText

getAndDestroyMetaAndSummary :: CryptoT MM TailBlock
getAndDestroyMetaAndSummary = do
  ms <- getMetaAndSummary
  tailInfoM <- lift $ tailBlockInfo ^$ getMasterBlockInfo 
  case tailInfoM of
    Nothing -> return ()
    Just tbi -> liftMFile $ destroyTailBlock $ tailBlockInfoPtr tbi
  return ms

putMetaAndSummary :: TailBlock -> CryptoT MM ()
putMetaAndSummary tb = do
  nonce <- liftMRand generateNonce
  let rawPlainText = BS.fromStrict $ Cereal.encode tb
      rawCompressedPlainText = compressWith compressOptions rawPlainText
  let bsLen = snat $ BS.length rawCompressedPlainText + 1
      resultLen = nextMultiple bsLen 8
  let padLen :: Word8 = fromIntegral $ sinteger $ resultLen - bsLen
  let pad = BS.replicate (fromIntegral $ padLen) '\0'
  let rawMarkedCompressedPlainText = Cereal.encodeLazy padLen `BS.append` pad `BS.append` rawCompressedPlainText
  assertMultipleSizeBytes rawMarkedCompressedPlainText $ \ paddedCompressedPlainText -> do
    cipher <- encrypt nonce $ PlainText paddedCompressedPlainText
    mac <- hmac $ getByteString cipher
    tbp <- liftMFile $ appendTailBlock $ getByteString cipher
    mbi <- lift getMasterBlockInfo
    lift $ putMasterBlockInfo $ mbi { tailBlockInfo = Just $ TailBlockInfo tbp (snat $ BS.length $ getByteString cipher) mac nonce }
    
fsDecodeThrowCorrupt :: (FixedSizeSerializable a) => FixedSizeBytes (SizeOf a) -> MFile a
fsDecodeThrowCorrupt = whenLeft (const $ throwMFile CorruptedData) . fsDecodeExact

encMasterBlock :: Nonce -> MasterBlockPayload -> CryptoT MFile EncMasterBlock
encMasterBlock salt payload = do
  nonce <- liftMRand generateNonce
  plainText <- liftMRand $ plainTextWithPadding $ fsEncode payload
  cipher <- encrypt nonce plainText
  mac <- hmac $ BS.concat
    [ getByteString salt
    , getByteString nonce
    , getByteString cipher
    ]
  return $ EncMasterBlock (MasterBlockHeader mac salt nonce) cipher

putMasterBlock :: Nonce -> MasterBlockPayload -> CryptoT MFile ()
putMasterBlock salt info = liftMFile . writeMasterBlock *$ fsEncode ^$ encMasterBlock salt info

decMasterBlock :: ByteString -> EncMasterBlock -> MFile (Key, DecMasterBlock)
decMasterBlock password (EncMasterBlock mbh cipher) = do
  let key = generateKey password $ mbhSalt mbh
  runCrypto key $ do
    check <- hmac $ BS.concat
      [ getByteString $ mbhSalt mbh
      , getByteString $ mbhNonce mbh
      , getByteString cipher
      ]
    when (check /= mbhMbmac mbh) $
      liftMFile $ throwMFile $ SecurityViolation
    plainText <- decrypt (mbhNonce mbh) cipher
    mbd <- liftMFile $ fsDecodeThrowCorrupt $ truncateMultiple $ getPlainText plainText
    return (key, DecMasterBlock mbh mbd)

getMasterBlock :: ByteString -> MFile (Key, DecMasterBlock)
getMasterBlock password = decMasterBlock password *$ liftMFile $ fsDecodeThrowCorrupt *$ readMasterBlock

decDataBlock :: EncDataBlock -> CryptoT MM DecDataBlock
decDataBlock (EncDataBlock dbh cipher) = do
  plainText <- decrypt (dataBlockHeaderNonce dbh) cipher
  dbd <- liftMFile $ fsDecodeThrowCorrupt $ truncateMultiple $ getPlainText plainText
  return $ DecDataBlock dbh dbd

getDataBlock :: BlockPtr -> CryptoT MM DecDataBlock
getDataBlock bp = decDataBlock *$ liftMFile $ fsDecodeThrowCorrupt *$ readDataBlock bp

encDataBlock :: BlockPtr -> Maybe BlockPtr -> DataBlockPayload -> CryptoT MFile (EncDataBlock, HMACCtx -> HMACCtx)
encDataBlock bp nbp payload = do
  nonce <- liftMRand generateNonce
  plainText <- liftMRand $ plainTextWithPadding $ fsEncode payload
  cipher <- encrypt nonce plainText
  hmac' <- hmac $ BS.concat
    [ getByteString $ fsEncode bp
    , getByteString $ fsEncode nbp
    , getByteString nonce
    , getByteString cipher
    ]
  return (EncDataBlock (DataBlockHeader hmac' nbp nonce) cipher, flip hmac_update $ getByteString $ fsEncode hmac')

putDataBlock :: BlockPtr -> Maybe BlockPtr -> DataBlockPayload -> CryptoT MFile (HMACCtx -> HMACCtx)
putDataBlock bp nbp payload = do
  (db, f) <- encDataBlock bp nbp payload
  liftMFile $ writeDataBlock bp $ fsEncode db
  return f

verifyChain :: Chain -> CryptoT MFile ChainInfo
verifyChain (Chain first final_hash) = do
  initial <- hmac_init
  (afh, fh, last) <- verifyChain' first (Just first) initial initial
  liftMFile $ guardErr IntegrityViolation $ hmac_finalize fh == final_hash
  return $ ChainInfo first last fh afh

verifyChain' :: BlockPtr -> Maybe BlockPtr -> HMACCtx -> HMACCtx -> CryptoT MFile (HMACCtx, HMACCtx, BlockPtr)
verifyChain' lst Nothing hm hm_last = return (hm,hm_last,lst)
verifyChain' _ (Just bp) _ hm = do
  EncDataBlock dbh dbp <- liftMFile $ fsDecodeThrowCorrupt *$ readDataBlock bp
  hmac_blk <- hmac $ BS.concat
    [ getByteString $ fsEncode bp
    , getByteString $ fsEncode $ dataBlockHeaderNextPtr dbh
    , getByteString $ dataBlockHeaderNonce dbh
    , getByteString dbp
    ]
  liftMFile $ guardErr IntegrityViolation $ dataBlockHeaderHmac dbh == hmac_blk
  let next = dataBlockHeaderNextPtr dbh
  verifyChain' bp next hm $ hmac_update hm $ getByteString $ fsEncode hmac_blk

chainInfoToChain :: ChainInfo -> Chain
chainInfoToChain ci = Chain (firstBlock ci) $ hmac_finalize $ fullHmac ci

mbiToPayload :: MasterBlockInfo -> MasterBlockPayload
mbiToPayload (MasterBlockInfo ciM tailBP) = MasterBlockPayload (liftM chainInfoToChain ciM) tailBP

runMM :: forall a. ByteString -> CryptoT MM a -> MFile a
runMM password cmd = do
  mbiIsNew <- initMFile
  (salt, mbinfo, key) <- if mbiIsNew
    then do
      salt <- liftMRand generateNonce
      let key = generateKey password salt
      runCrypto key $ putMasterBlock salt $ mbiToPayload mbinfo0
      return (salt, mbinfo0, key)
    else do
      (key, DecMasterBlock mbh mbi) <- getMasterBlock password
      mm_state <- runCrypto key $ do
        dc <- case masterBlockPayloadChain mbi of
          Nothing -> return Nothing
          Just sm -> liftM Just $ verifyChain sm
        return $ MasterBlockInfo dc $ masterBlockPayloadTailPtr mbi
      return (mbhSalt mbh, mm_state, key)
  (val, mbinfo') <- runStateT (unMM $ runCrypto key cmd) mbinfo
  let mbdata = mbiToPayload mbinfo
  let mbdata' = mbiToPayload mbinfo'
  when (mbdata /= mbdata') $ runCrypto key $ putMasterBlock salt mbdata'
  return val

getDataPayloadVector :: PlainText (BoundedVectorSize DataBlockPayloadMultiple (SizeOf SerialAppendCommand)) -> MFile (Vector (AppendCommand SafeNat32))
getDataPayloadVector plainText = do
  liftM (Vector.map fromSAppendCommand . getBoundedVector) $ liftMFile $ whenLeft (const $ throwMFile CorruptedData) $
    boundedVectorDecode $ truncateMultiple $ getPlainText plainText

foldChain :: forall b. Lens MasterBlockInfo (Maybe ChainInfo) -> b -> (AppendCommand SafeNat32 -> b -> b) -> CryptoT MM b
foldChain l i f = do
  the_chain <- lift $ getL l
  let headBlock = fmap firstBlock the_chain
  traverse headBlock i
  where
    traverse :: Maybe BlockPtr -> b -> CryptoT MM b
    traverse Nothing acc = return acc
    traverse (Just bp) acc = do
      DecDataBlock dbh dbp <- getDataBlock bp
      actualData <- liftMFile $ getDataPayloadVector $ dataBlockPayloadContents dbp
      traverse (dataBlockHeaderNextPtr dbh) $ Vector.foldl' (flip f) acc actualData

writeBlockChain' :: BlockPtr -> [DataBlockPayload] -> ChainInfo -> CryptoT MFile ChainInfo
writeBlockChain' _bp [] hm = return hm
writeBlockChain' bp [bc] (ChainInfo fbp _lbp _fhm afh) = do
  hm' <- putDataBlock bp Nothing bc
  return $ ChainInfo fbp bp (hm' afh) afh
writeBlockChain' bp (bc : bcs) (ChainInfo fpb lbp _fhm alh) = do
  nextBlock <- lift $ newBlock
  hm' <- putDataBlock bp (Just nextBlock) bc
  writeBlockChain' nextBlock bcs $ ChainInfo fpb lbp (hm' alh) (hm' alh)

-- Goes to the last block, writes onto the last block, and might write more blocks if necessary.
writeBlockChain :: [DataBlockPayload] -> ChainInfo -> CryptoT MFile ChainInfo
writeBlockChain records ci = writeBlockChain' (lastBlock ci) records ci

appendRecordToChain :: Lens MasterBlockInfo (Maybe ChainInfo) -> AppendCommand SafeNat32 -> CryptoT MM ()
appendRecordToChain l val = do
  chainM <- getL l
  (chain, xs) <- case chainM of
    -- there is no chain
    Nothing -> do
      nbp <- liftMFile $ newBlock
      hmac_ctx <- hmac_init
      return (ChainInfo nbp nbp hmac_ctx hmac_ctx, Vector.empty)
    -- the chain already exists
    Just chain -> do
      xs <- liftMFile . getDataPayloadVector . dataBlockPayloadContents . decDataBlockPayload *$ getDataBlock $ lastBlock chain
      return (chain, xs)
  let toWriteBlocks = 
        if snat (Vector.length xs + 1) <= sizeParameter (P :: P DataBlockPayloadMultiple)
          -- we can add val to the block
          then [Vector.snoc xs val]
          -- we need a new block
          else [xs, Vector.singleton val]
  dataPlainTexts <- liftMRand $ forM toWriteBlocks $ \ vector -> 
    plainTextWithPadding *$ boundedVectorEncode $ assertBoundedVector (Vector.map toSAppendCommand vector)

  newChain <- mapCrypto liftMFile $ writeBlockChain (map DataBlockPayload dataPlainTexts) chain
  lift $ putL l $ Just newChain
