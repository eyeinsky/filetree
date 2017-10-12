module System.Path.IO where

import qualified Data.ByteString.Lazy as BL
import System.Directory

import Pr
import System.Path.Dir
import System.Path.File
import qualified System.File as F

readFile :: Dir root -> Name -> IO F.File
readFile root name = F.File name <$> readContent (root `addFile` name)

readContent :: File root -> IO F.Content
readContent fp = BL.readFile $ renderPath fp

currentPath :: IO (Dir 'Absolute)
currentPath = getCurrentDirectory <&> fromString

cd :: Dir r -> IO ()
cd p = setCurrentDirectory (renderPath p)

inPath :: Dir r -> IO a -> IO a
inPath path action = do
  current <- currentPath
  cd path
  result <- action
  cd current
  return result

mkDir :: Dir r -> IO ()
mkDir p = createDirectoryIfMissing True (renderPath p)
