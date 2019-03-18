module Pretty where

import qualified Data.Text as T
import qualified Data.Text.IO as T

class Pretty a where
  pshow :: a -> T.Text
  pprint :: a -> IO ()
  pprint = T.putStrLn . pshow
