module FixedSizeSerializable where

import SafeNat
import Util
import Data.Serialize hiding (getByteString)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Exception
import Control.Applicative
import qualified Data.Serialize as Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import MRand

class SizeParameter a where
  sizeParameter :: P a -> SafeNat32

class FixedSizeSerializable a where
  fsSize :: P a -> SafeNat32
  fsGet :: Get a
  fsPut :: a -> PutM ()

instance FixedSizeSerializable SafeNat32 where
  fsSize _ = 4
  fsGet = liftM UnsafeNat32 getWord32be
  fsPut = putWord32be . unSafeNat32

instance FixedSizeSerializable Bool where
  fsSize _ = 1
  fsGet = get
  fsPut = put

instance FixedSizeSerializable Word8 where
  fsSize _ = 1
  fsGet = get
  fsPut = put

instance (FixedSizeSerializable a, FixedSizeSerializable b) => FixedSizeSerializable (a, b) where
  fsSize _ = fsSize (P :: P a) + fsSize (P :: P b)
  fsGet = pure (,) <*> fsGet <*> fsGet
  fsPut (a, b) = fsPut a >> fsPut b

instance (FixedSizeSerializable a) => FixedSizeSerializable (Maybe a) where
  fsSize _ = fsSize (P :: P Bool) + fsSize (P :: P a)
  fsGet = do
    b <- get
    if b then liftM Just fsGet else do
      skip $ sint $ fsSize (P :: P a)
      return Nothing
  fsPut Nothing = fsPut False >> putLazyByteString (padding $ fsSize (P :: P a))
  fsPut (Just a) = fsPut True >> fsPut a

newtype FixedSizeBytes p = UnsafeFixedSizeBytes { getFixedSizeBytes :: ByteString }
  deriving (Eq, Ord, Show)
assertFixedSizeBytes :: forall p. (SizeParameter p) => ByteString -> FixedSizeBytes p
assertFixedSizeBytes b =
  assert (snat (BS.length b) == sizeParameter (P :: P p)) $
  UnsafeFixedSizeBytes b
assertMultipleSizeBytes :: forall p c. (SizeParameter p) => ByteString -> (forall a. FixedSizeBytes (RoundUpToMultipleSize p a) -> c) -> c
assertMultipleSizeBytes bs f =
  assert (snat (BS.length bs) `smod` sizeParameter (P :: P p) == 0) $
  f $ UnsafeFixedSizeBytes bs
instance (SizeParameter p) => FixedSizeSerializable (FixedSizeBytes p) where
  fsSize _ = sizeParameter (P :: P p)
  fsGet = getFixedBytes UnsafeFixedSizeBytes
  fsPut = putFixedBytes getFixedSizeBytes
data RoundUpToMultipleSize p a
nextMultiple :: SafeNat32 -> SafeNat32 -> SafeNat32
nextMultiple n factor = 
  let diff = factor - (n `smod` factor)
  in if diff == factor then n else n + diff
instance (SizeParameter p, SizeParameter a) => SizeParameter (RoundUpToMultipleSize p a) where
  sizeParameter _ = 
    let rawSize = sizeParameter (P :: P a)
        factor = sizeParameter (P :: P p)
    in nextMultiple rawSize factor
data MultipleOfEight
instance SizeParameter MultipleOfEight where
  sizeParameter _ = 8
data SizeOf a
instance (FixedSizeSerializable a) => SizeParameter (SizeOf a) where
  sizeParameter _ = fsSize (P :: P a)

truncateMultiple :: forall p a. (SizeParameter p, SizeParameter a) => FixedSizeBytes (RoundUpToMultipleSize p a) -> FixedSizeBytes a
truncateMultiple b =
  let truncated = sizeParameter (P :: P a)
  in assertFixedSizeBytes $ BS.take (sint64 truncated) $ getByteString b

class GetByteString a where
  getByteString :: a -> ByteString
instance GetByteString ByteString where
  getByteString = id
instance GetByteString (FixedSizeBytes a) where
  getByteString = getFixedSizeBytes

padding :: SafeNat32 -> ByteString
padding n = BS.replicate (sint64 n) '\0'

getFixedBytes :: forall a. (FixedSizeSerializable a) => (ByteString -> a) -> Get a
getFixedBytes c = liftM c $ Serialize.getLazyByteString $ sint64 $ fsSize (P :: P a)

putFixedBytes :: forall a. (FixedSizeSerializable a) => (a -> ByteString) -> a -> PutM ()
putFixedBytes d a = do
  let bs = d a
  assert (fromIntegral (BS.length bs) == fsSize (P :: P a)) $
    putLazyByteString bs

fsEncode :: (FixedSizeSerializable a) => a -> FixedSizeBytes (SizeOf a)
fsEncode = assertFixedSizeBytes . BS.fromStrict . runPut . fsPut

fsDecodeExact :: forall a. (FixedSizeSerializable a) => FixedSizeBytes (SizeOf a) -> Either String a
fsDecodeExact bs = do
  (a, bs') <- runGetState fsGet (BS.toStrict $ getByteString bs) 0
  let sbs' = BS.fromStrict bs'
  if BS.null sbs'
    then return a
    else Left "tail isn't null"

newtype BoundedVector p a = UnsafeBoundedVector { getBoundedVector :: Vector a }
assertBoundedVector :: forall p a. (SizeParameter p) => Vector a -> BoundedVector p a
assertBoundedVector v =
  assert (snat (Vector.length v) <= sizeParameter (P :: P p)) $
  UnsafeBoundedVector v

data BoundedVectorSize p a
instance (SizeParameter p, SizeParameter a) => SizeParameter (BoundedVectorSize p a) where
  sizeParameter _ = fsSize (P :: P SafeNat32) + sizeParameter (P :: P p) * sizeParameter (P :: P a)

boundedVectorDecode :: forall a p. (FixedSizeSerializable a, SizeParameter p) => FixedSizeBytes (BoundedVectorSize p (SizeOf a)) -> Either String (BoundedVector p a)
boundedVectorDecode bs = flip runGet (BS.toStrict $ getByteString bs) $ do
  i <- fsGet :: Get SafeNat32
  result <- liftM Vector.fromList $ loop i
  skip $ sint $ (sizeParameter (P :: P p) - i) * fsSize (P :: P a)
  return $ assertBoundedVector result
  where
    loop 0 = return []
    loop i = do
      x <- fsGet
      xs <- loop $ i - 1
      return $ x : xs

boundedVectorEncode :: forall a p. (FixedSizeSerializable a, SizeParameter p) => BoundedVector p a -> MRand (FixedSizeBytes (BoundedVectorSize p (SizeOf a)))
boundedVectorEncode xs =
  let output = BS.fromStrict $ runPut $ do
        fsPut $ snat $ Vector.length $ getBoundedVector xs
        Vector.forM_ (getBoundedVector xs) fsPut
  in do
    let bsLen = snat $ BS.length output
        resultLen = sizeParameter (P :: P (BoundedVectorSize p (SizeOf a)))
    let pad = BS.replicate (sint64 $ resultLen - bsLen) '\0'
    let bytes :: FixedSizeBytes (BoundedVectorSize p (SizeOf a))
        bytes = assertFixedSizeBytes $ getByteString output `BS.append` pad
    return bytes
