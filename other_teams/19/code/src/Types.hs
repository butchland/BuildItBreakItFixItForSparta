module Types where

import FixedSizeSerializable
import Util
import Control.Applicative
import qualified Crypto.MAC as CMAC
import Crypto.Hash (SHA3_256)
import SafeNat
import Lens
import Control.Monad.Error
import Data.ByteString (ByteString)
import HighLevelTypes

-- Low Level Types --

-- TODO: change this to get
newtype BlockPtr = BlockPtr { unBlockptr :: SafeNat32 }
  deriving (Eq, Ord, Num, Show, FixedSizeSerializable)
newtype TailBlockPtr = TailBlockPtr { unTailBlockPtr :: BlockPtr }
  deriving (Eq, Ord, Num, Show, FixedSizeSerializable)

-- Crypto Types --

newtype PlainText p = PlainText { getPlainText :: FixedSizeBytes (RoundUpToMultipleSize MultipleOfEight p) } 
  deriving (Eq, Show, GetByteString)
deriving instance (SizeParameter p) => FixedSizeSerializable (PlainText p)
newtype CipherText p = CipherText { getCipherText :: FixedSizeBytes (RoundUpToMultipleSize MultipleOfEight p) } 
  deriving (Eq, Show, GetByteString)
deriving instance (SizeParameter p) => FixedSizeSerializable (CipherText p)

-- HMAC SHA3_256
data HashSize
instance SizeParameter HashSize where
  sizeParameter _ = 256 `sdiv` 8
newtype Hash = Hash { hashBytes :: FixedSizeBytes HashSize } 
  deriving (Eq, Show, FixedSizeSerializable, GetByteString)

data NonceSize
nonceSize :: SafeNat32
nonceSize = sizeParameter (P :: P NonceSize)
instance SizeParameter NonceSize where
  sizeParameter _ = 128 `sdiv` 8
newtype Nonce = Nonce { nonceBytes :: FixedSizeBytes NonceSize } 
  deriving (Eq, Show, FixedSizeSerializable, GetByteString)

-- Master Block Format --

-- ------------------------- | mbmac = hmac (salt || nonce || <encrypted payload>)
--                    header | salt
-- ------------------------- | nonce
--                      data | payload
-- (padded to multiple of 8) | ...

masterBlockSize :: SafeNat32
masterBlockSize = fsSize (P :: P EncMasterBlock)
data EncMasterBlock = EncMasterBlock
  { encMasterBlockHeader :: MasterBlockHeader
  , encMasterBlockCipher :: CipherText (SizeOf MasterBlockPayload)
  }
instance FixedSizeSerializable EncMasterBlock where
  fsSize _ = fsSize (P :: P MasterBlockHeader) + fsSize (P :: P (CipherText (SizeOf MasterBlockPayload)))
  fsGet = pure EncMasterBlock <*> fsGet <*> fsGet
  fsPut (EncMasterBlock h d) = fsPut h >> fsPut d

data MasterBlockHeader = MasterBlockHeader 
  { mbhMbmac :: Hash 
  , mbhSalt :: Nonce
  , mbhNonce :: Nonce
  } deriving (Eq)
instance FixedSizeSerializable MasterBlockHeader where
  fsSize _ = 2 * fsSize (P :: P Nonce) + fsSize (P :: P Hash)
  fsGet = pure MasterBlockHeader <*> fsGet <*> fsGet <*> fsGet
  fsPut (MasterBlockHeader m s n) = fsPut m >> fsPut s >> fsPut n

-- Master Block Decoded --

data DecMasterBlock = DecMasterBlock
  { decMasterBlockHeader :: MasterBlockHeader
  , decMasterBlockData :: MasterBlockPayload
  }
data MasterBlockPayload = MasterBlockPayload 
  { masterBlockPayloadChain :: Maybe Chain 
  , masterBlockPayloadTailPtr :: Maybe TailBlockInfo
  } deriving (Eq)
instance FixedSizeSerializable MasterBlockPayload where
  fsSize _ = fsSize (P :: P (Maybe Chain)) + fsSize (P :: P (Maybe TailBlockInfo))
  fsGet = pure MasterBlockPayload <*> fsGet <*> fsGet
  fsPut (MasterBlockPayload c t) = fsPut c >> fsPut t

data Chain = Chain 
  { chainBlockPtr :: BlockPtr
  , chainHash :: Hash 
  } deriving (Eq)
instance FixedSizeSerializable Chain where
  fsSize _ = fsSize (P :: P BlockPtr) + fsSize (P :: P Hash)
  fsGet = pure Chain <*> fsGet <*> fsGet
  fsPut (Chain i h) = fsPut i >> fsPut h

data TailBlockInfo = TailBlockInfo
  { tailBlockInfoPtr :: TailBlockPtr
  , tailBlockInfoLen :: SafeNat32
  , tailBlockInfoHash :: Hash
  , tailBlockInfoNonce :: Nonce
  } deriving (Eq,Show)
instance FixedSizeSerializable TailBlockInfo where
  fsSize _ = fsSize (P :: P TailBlockPtr) + fsSize (P :: P SafeNat32) + fsSize (P :: P Hash) + fsSize (P :: P Nonce)
  fsGet = pure TailBlockInfo <*> fsGet <*> fsGet <*> fsGet <*> fsGet
  fsPut (TailBlockInfo p l h n) = fsPut p >> fsPut l >> fsPut h >> fsPut n

-- Data Block Format --

-- ------------------------- | hmac_block == hmac (block_number || next_pointer || encrypt_nonce || encrypted_data)
--                    header | next_pointer
-- ------------------------- | encrypt_nonce
--                      data | encrypted_data
-- (padded to multiple of 8) | ...

dataBlockSize :: SafeNat32
dataBlockSize = fsSize (P :: P EncDataBlock)
data EncDataBlock = EncDataBlock
  { encDataBlockHeader :: DataBlockHeader
  , encDataBlockCipher :: CipherText (SizeOf DataBlockPayload)
  }
instance FixedSizeSerializable EncDataBlock where
  fsSize _ = fsSize (P :: P DataBlockHeader) + fsSize (P :: P (CipherText (SizeOf DataBlockPayload)))
  fsGet = pure EncDataBlock <*> fsGet <*> fsGet
  fsPut (EncDataBlock h d) = fsPut h >> fsPut d

data DataBlockHeader = DataBlockHeader
  { dataBlockHeaderHmac :: Hash
  , dataBlockHeaderNextPtr :: Maybe BlockPtr
  , dataBlockHeaderNonce :: Nonce
  }
instance FixedSizeSerializable DataBlockHeader where
  fsSize _ = fsSize (P :: P Hash) + fsSize (P :: P (Maybe BlockPtr)) + fsSize (P :: P Nonce)
  fsGet = pure DataBlockHeader <*> fsGet <*> fsGet <*> fsGet
  fsPut (DataBlockHeader hm np n) = fsPut hm >> fsPut np >> fsPut n

-- Data Block Decoded --

data DecDataBlock = DecDataBlock
  { decDataBlockHeader :: DataBlockHeader
  , decDataBlockPayload :: DataBlockPayload
  }
data DataBlockPayloadMultiple
instance SizeParameter DataBlockPayloadMultiple where
  sizeParameter _ = 16
newtype DataBlockPayload = DataBlockPayload 
  { dataBlockPayloadContents :: PlainText (BoundedVectorSize DataBlockPayloadMultiple (SizeOf SerialAppendCommand))
  } deriving (FixedSizeSerializable)

-- Mid Level Types --

type HMACCtx = CMAC.HMACContext (SHA3_256)

data ChainInfo = ChainInfo
  { firstBlock     :: BlockPtr
  , lastBlock      :: BlockPtr
  , fullHmac       :: HMACCtx
  , almostFullHmac :: HMACCtx
  }

data MasterBlockInfo = MasterBlockInfo 
  { dataChainInfo :: Maybe ChainInfo 
  , tailBlockInfo :: Maybe TailBlockInfo
  }
mbinfo0 :: MasterBlockInfo
mbinfo0 = MasterBlockInfo Nothing Nothing

dataChainInfoL :: Lens MasterBlockInfo (Maybe ChainInfo)
dataChainInfoL = lens dataChainInfo $ \ s i -> s { dataChainInfo = i }

-- Global Error Enumeration Type --

data Err =
  -- thrown by App
    InvalidInput
  | InternalError String
  -- thrown by MidLevel
  | SecurityViolation
  | IntegrityViolation
  | CorruptedData
  -- thrown by LowLevel
  | MasterBlockOverflow ByteString
  | DataBlockOverflow BlockPtr ByteString
  | TailBlockOverflowRead TailBlockPtr SafeNat32
  -- these should not be used by us
  | NoMsg
  | StrMsg String
  deriving (Eq, Ord, Show)
instance Error Err where
  noMsg = NoMsg
  strMsg = StrMsg
