module System.Path
  ( module System.Path
  , module System.Path.Dir
  , module System.Path.File
  , module Export
  , HasExtensions(..)
  ) where

import System.IO hiding (FilePath)
import System.Directory (doesDirectoryExist)
import qualified Data.Text as T

import Pr
import System.Path.Dir
import System.Path.File hiding (nameToString)
import System.Path.IO as Export

type Some p = Either (p 'Absolute) (p 'Relative)

instance (Path (p 'Absolute), Path (p 'Relative)) => Path (Some p) where
  renderPath = either renderPath renderPath

instance IsString (Some Dir) where
  fromString = fromString'

instance IsString (Some File) where
  fromString = fromString'

fromString' :: (IsString a, IsString b) => [Char] -> Either a b
fromString' s = case s of
  '/' : _ -> Left (fromString s)
  _ -> Right (fromString s)

type SomePath a = Either (Dir a) (File a)

type AnyPath = Either (SomePath 'Absolute) (SomePath 'Relative)

checkPath :: FilePath -> IO (Either (SomePath 'Absolute) (SomePath 'Relative))
checkPath fp = do
  let (abs, ns) = case fp of
        '/' : fp' -> (True, f fp')
        _ -> (False, f fp)
  dir <- doesDirectoryExist fp
  return $ g abs dir ns
  where
    f = T.pack ^ T.split (== '/')
    -- abs dir path-parts
    g :: Bool -> Bool -> [Name] -> AnyPath
    g True True ns = Left $ Left $ DA ns
    g True False ns = Left $ Right $ File (DA $ init ns, last ns)
    g False True ns = Right $ Left $ DR ns
    g False False ns = Right $ Right $ File (DR $ init ns, last ns)

isDirPath :: FilePath -> IO (Maybe (Either (Dir 'Absolute) (Dir 'Relative)))
isDirPath p = doesDirectoryExist p <&> (\b -> if b then Just res else Nothing)
  where
    res = case p of
      '/' : _ -> p & T.pack ^ T.split (== '/') ^ tail ^ DA ^ Left
      _ -> p & T.pack ^ T.split (== '/') ^ DR ^ Right

ensureAbsolute :: Dir r -> Dir 'Absolute
ensureAbsolute p = case p of
  DR _ -> error "ensureAbsolute: not implemented"
  DA ns -> DA ns

ensureRelative :: Dir r -> Dir 'Relative
ensureRelative p = case p of
  DR ns -> DR ns
  DA _ -> error "ensureRelative: not implemented"
