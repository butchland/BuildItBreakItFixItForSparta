module CryptoUtil where

import MRand
import Types
import FixedSizeSerializable
import Util
import SafeNat
import LowLevel

import Control.Monad.Reader
import Control.Monad.State
import Crypto.Cipher.AES
import Crypto.Hash (HMAC, SHA3_256)
import Data.ByteString.Lazy (ByteString)
import Data.Byteable
import Control.Exception

import qualified Crypto.MAC as CMAC
import qualified Data.ByteString.Lazy.Char8 as BS

newtype CryptoT m a = CryptoT { unCryptoT :: ReaderT (AES,ByteString) m a }
    deriving (Monad, MonadIO, MonadReader (AES,ByteString), MonadTrans)
deriving instance (MonadState s m) => MonadState s (CryptoT m)
instance (LiftMRand m, Monad m) => LiftMRand (CryptoT m) where
  liftMRand = CryptoT . lift . liftMRand
instance (LiftMFile m, Monad m) => LiftMFile (CryptoT m) where
  liftMFile = CryptoT . lift . liftMFile

newtype Key = Key { keyBytes :: ByteString } 
  deriving (Eq,Show, GetByteString)

plainTextWithPadding :: forall p. (SizeParameter p) => FixedSizeBytes p -> MRand (PlainText p)
plainTextWithPadding bs = do
  let bsLen = sizeParameter (P :: P p)
      resultLen = sizeParameter (P :: P (RoundUpToMultipleSize MultipleOfEight p))
  let pad = BS.replicate (sint64 $ resultLen - bsLen) '\0'
  let plaintext :: FixedSizeBytes (RoundUpToMultipleSize MultipleOfEight p)
      plaintext = assertFixedSizeBytes $ getByteString bs `BS.append` pad
  return $ PlainText plaintext

mapCrypto :: (m a -> n a) -> CryptoT m a -> CryptoT n a
mapCrypto f = CryptoT . mapReaderT f . unCryptoT

generateNonce :: MRand Nonce
generateNonce =
  assert (nonceSize `smod` 8 == 0) $
  liftM (Nonce . assertFixedSizeBytes . BS.fromStrict) $ generateRandom nonceSize

sha3_256_hmac :: ByteString -> ByteString -> ByteString
sha3_256_hmac key message = BS.fromStrict $ toBytes (CMAC.hmac (BS.toStrict key) (BS.toStrict message) :: HMAC SHA3_256)

runCrypto :: Key -> CryptoT m a -> m a
runCrypto (Key key) aM = do
  let aes = initAES $ BS.toStrict key
  runReaderT (unCryptoT aM) (aes,key)

liftRandom :: MRand a -> CryptoT MRand a
liftRandom = CryptoT . lift

generateKey :: ByteString -> Nonce -> Key
generateKey password salt =
  Key $ sha3_256_hmac (getByteString salt) password

encrypt :: forall p m. (Monad m) => Nonce -> PlainText p -> CryptoT m (CipherText p)
encrypt nonce plaintext = do
  aes <- liftM fst ask
  let ciphertext :: FixedSizeBytes (RoundUpToMultipleSize MultipleOfEight p)
      ciphertext = UnsafeFixedSizeBytes $ BS.fromStrict $ encryptCTR aes (BS.toStrict $ getByteString nonce) $ BS.toStrict $ getByteString plaintext
  return $ CipherText ciphertext

decrypt :: forall p m. (Monad m) => Nonce -> CipherText p -> CryptoT m (PlainText p)
decrypt (Nonce nonce) ciphertext = do
  aes <- liftM fst ask
  let plaintext :: FixedSizeBytes (RoundUpToMultipleSize MultipleOfEight p)
      plaintext = UnsafeFixedSizeBytes $ BS.fromStrict $ decryptCTR aes (BS.toStrict $ getByteString nonce) $ BS.toStrict $ getByteString ciphertext
  return $ PlainText plaintext

hmac :: (Monad m) => ByteString -> CryptoT m Hash
hmac message = do
  key <- liftM snd ask
  return $ Hash $ UnsafeFixedSizeBytes $ sha3_256_hmac (getByteString key) message

hmac_init :: (Monad m) => CryptoT m HMACCtx
hmac_init = do
   key <- liftM snd ask
   return $ CMAC.hmacInit $ BS.toStrict key

hmac_update :: HMACCtx -> ByteString -> HMACCtx
hmac_update h b = CMAC.hmacUpdate h $ BS.toStrict b

hmac_finalize :: HMACCtx -> Hash
hmac_finalize ctx = Hash $ UnsafeFixedSizeBytes $ BS.fromStrict $ toBytes $ CMAC.hmacFinalize ctx
