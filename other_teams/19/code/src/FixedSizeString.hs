module FixedSizeString where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import FixedSizeSerializable
import SafeNat
import Data.Serialize

fsStringSize :: SafeNat32
fsStringSize = 32

newtype FSASCII = UnsafeFixedSizedASCII { getFSASCII :: ByteString }
  deriving (Eq, Ord, Show)

instance FixedSizeSerializable FSASCII where
  fsSize _ = fsStringSize
  fsGet = do
    s <- getBytes $ sint fsStringSize
    return $ UnsafeFixedSizedASCII $ BS.takeWhile ((/=) '\0') s
  fsPut (UnsafeFixedSizedASCII s) = do
    putByteString s
    let len = snat $ BS.length s
    let count = sint $ fsStringSize - len
    putByteString $ BS.replicate count '\0'

fsASCII :: ByteString -> Maybe FSASCII
fsASCII s
  | snat (BS.length s) > fsStringSize = Nothing
  | otherwise = Just $ UnsafeFixedSizedASCII s
