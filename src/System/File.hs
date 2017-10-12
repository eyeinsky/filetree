module System.File where

import Pr
import Data.ByteString.Lazy
import System.Path.Dir

type Content = ByteString
data File = File Name Content
  deriving (Eq, Show)
