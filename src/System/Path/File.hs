module System.Path.File
   ( module System.Path.File
   , module System.Path.Dir
   )
   where

import Pr

import Data.String (IsString(..))

import qualified Data.Text as T

import System.Path.Dir

newtype File root = File { unFile :: (Dir root, Name) }

instance Show (File 'Absolute) where
  show (File (d, f)) = show d <> "/" <> T.unpack f

instance Show (File 'Relative) where
  show (File (d, f)) = show d <> "/" <> T.unpack f

instance Path (File root) where
   renderPath (File (d, f)) = renderPath d <> "/" <> T.unpack f

-- * Lens

instance HasBasename (File root) Name where
  basename f (File (d, n)) = fmap (\n' -> File (d, n')) (f n)

instance HasDirname (File r) (Dir r) where
  dirname f (File (d, n)) = fmap (\d' -> File (d', n)) (f d)

instance HasExtensions Name [Name] where
  extensions :: Lens' Name [Name]
  extensions f n = fmap (\xs' -> i $ x : xs') (f xs)
    where
      (x : xs) = T.split (== '.') n :: [Name]
      i = T.intercalate "."

instance HasExtensions (File root) [Name] where
  extensions = basename.extensions

-- * IsString

instance IsString (File 'Absolute) where
   fromString str = if head str == '/'
         then case reverse $ T.split (== '/') $ T.pack str of
            fn : dir | T.null fn -> error "filename must not be empty"
                     | otherwise -> File (DA $ reverse dir, fn)
            _ -> error "empty"
         else error "absolute paht must start with '/'"
instance IsString (File 'Relative) where
   fromString str = if head str /= '/'
         then case reverse $ T.split (== '/') $ T.pack str of
            fn : dir | T.null fn -> error "filename must not be empty"
                     | otherwise -> File (DR $ reverse dir, fn)
            _ -> error "empty"
         else error "relative path must not start with '/'"

addFile :: Dir r -> Name -> File r
addFile dir fn = File (dir, fn)
