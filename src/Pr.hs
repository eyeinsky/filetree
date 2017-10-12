module Pr
   ( module Export
   , module Pr
   ) where

import Prelude as Export hiding (readFile, putStrLn, putStr, (/), (^))
import Control.Lens as Export
import Data.Monoid as Export
import Data.String as Export (IsString(..))
import Control.Monad as Export
import Control.Monad.IO.Class as Export
import Data.Maybe as Export

class HasBasename s a | s -> a where
  basename :: Lens' s a
  {-# MINIMAL basename #-}

class HasDirname s a | s -> a where
  dirname :: Lens' s a
  {-# MINIMAL dirname #-}

class HasExtensions s a | s -> a where
  extensions :: Lens' s a
  {-# MINIMAL extensions #-}

infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)
