module System.FSTree.DSL where

import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.Writer

import Pr
import System.FSTree.Core
import qualified System.File as F
import System.Path

newtype FSMT m a = FSMT
  { unFSMT :: WriterT FileTree m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter FileTree, MonadIO)

runFSMT :: Monad m => FSMT m a -> m (a, FileTree)
runFSMT = runWriterT . unFSMT

file :: Monad m => Name -> F.Content -> FSMT m F.File
file name content = do
  tell $ FSTree $ M.singleton name $ Right file
  return file
  where file = F.File name content

dir :: Monad m => Name -> FSMT m a -> FSMT m a
dir name content = do
  (a, tr) <- lift $ runFSMT content
  tell $ FSTree $ M.singleton name $ Left tr
  return a

infixr 4 /
(/) :: Monad m => Name -> FSMT m a -> FSMT m a
(/) = dir

-- ** FSM

type FSM = FSMT Identity

runFSM :: FSM a -> (a, FileTree)
runFSM = runIdentity . runFSMT

execFSM :: FSM a -> FileTree
execFSM = snd . runFSM
