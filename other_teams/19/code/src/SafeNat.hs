module SafeNat where

import Data.Word
import Control.Exception
import Data.Serialize
import Data.Int

newtype SafeNat32 = UnsafeNat32 { unSafeNat32 :: Word32 }
  deriving (Eq, Ord, Serialize)
instance Show SafeNat32 where
  show = show . unSafeNat32

instance Num SafeNat32 where
  a + b            = snat $ sinteger a + sinteger b
  a * b            = snat $ sinteger a * sinteger b
  a - b            = snat $ sinteger a - sinteger b
  abs              = id
  signum i 
    | i == 0       = 0
    | otherwise    = 1
  fromInteger i    = snat i

safeNat32Max :: Integer
safeNat32Max = toInteger (maxBound :: Word32)

sdiv :: SafeNat32 -> SafeNat32 -> SafeNat32
sdiv (UnsafeNat32 x) (UnsafeNat32 y) = UnsafeNat32 $ x `div` y

smod :: SafeNat32 -> SafeNat32 -> SafeNat32
smod (UnsafeNat32 x) (UnsafeNat32 y) = UnsafeNat32 $ x `mod` y

sinteger :: SafeNat32 -> Integer
sinteger i = fromIntegral $ unSafeNat32 i

sint :: SafeNat32 -> Int
sint n = 
  assert (n <= snat (maxBound :: Int)) $
  fromIntegral $ unSafeNat32 n

sint64 :: SafeNat32 -> Int64
sint64 = fromIntegral . unSafeNat32

snat :: (Integral a) => a -> SafeNat32
snat = sfromInteger . fromIntegral

sfromInteger :: Integer -> SafeNat32
sfromInteger i  
  | i < 0 = error "cannot coerce negative integer to nat"
  | i >= safeNat32Max = error "cannot coerce large integer to nat"
  | otherwise = UnsafeNat32 $ fromIntegral i
