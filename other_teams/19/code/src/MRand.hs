module MRand where

import SafeNat
import Control.Exception.Base (assert)
import Control.Monad.State
import Crypto.Random
import Data.ByteString (ByteString)

newtype MRand a = MRand { unMRand :: StateT SystemRNG IO a }
    deriving (Monad, MonadState SystemRNG, MonadIO)
class LiftMRand m where
  liftMRand :: MRand a -> m a

runRandom :: MRand a -> IO a
runRandom (MRand aM) = do
  entPool <- createEntropyPool
  evalStateT aM $ cprgCreate entPool

generateRandom :: SafeNat32 -> MRand ByteString
generateRandom size = assert (size >= 0) $ do
  g <- get
  let (b,g') = cprgGenerate (sint size) g
  put g'
  return b
