
module Delter.Types (
  DiffResult(..)
  ) where


import Data.ByteString (ByteString)
import Data.Time

data DiffResult = DiffResult {
  diffBytes :: ByteString
  , diffSize :: Int
  , diffTime :: NominalDiffTime
  } deriving (Show)
