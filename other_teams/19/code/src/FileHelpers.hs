module FileHelpers where

import Control.Monad
import System.Directory

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists p = do
  b <- doesFileExist p
  when b $ removeFile p
