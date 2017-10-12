module System.Path.Dir
  ( module System.Path.Dir
  , HasBasename(..)
  , HasDirname(..)
  ) where

import Data.List (intercalate)
import qualified Data.Text as T

import Pr


type Name = T.Text

nameToString :: Name -> String
nameToString = T.unpack

data Root = Absolute | Relative

data Dir (r :: Root) where
   DA :: [Name] -> Dir 'Absolute
   DR :: [Name] -> Dir 'Relative

makePrisms ''Dir

instance Eq (Dir r) where
  (DA ns) == (DA ns') = ns == ns'
  (DR ns) == (DR ns') = ns == ns'
  _ == _ = False

instance Show (Dir r) where
  show d = case d of
    DR ns -> intercalate "/" $ map T.unpack ns
    DA ns -> "/" <> show (DR ns)

instance IsString (Dir 'Absolute) where
   fromString str = if head str == '/'
     then DA (filter (not . T.null) $ T.split (== '/') $ T.pack str)
     else error "absolute paht must start with '/'"

instance IsString (Dir 'Relative) where
  fromString str = case str of
    "" -> DR []
    _ -> if head str /= '/'
       then DR (filter (not . T.null) $ T.split (== '/') $ T.pack str)
       else error "relative path must not start with '/'"

class Path a where
  renderPath :: a -> FilePath

instance Path (Dir r) where
  renderPath d = T.unpack $ case d of
    DA p -> "/" <> T.intercalate "/" p
    DR p -> T.intercalate "/" p

instance HasBasename (Dir r) Name where
  basename f d = case d of
    DA ns -> fmap (\n' -> DA $ ns & lens' .~ n') (ns^.lens' & f)
    DR ns -> fmap (\n' -> DR $ ns & lens' .~ n') (ns^.lens' & f)
    where
      lens' = reversed.ix 0

instance HasDirname (Dir r) (Dir r) where
  dirname = id

instance Monoid (Dir 'Relative) where
  mempty = DR []
  DR xs `mappend` DR ys = DR (xs <> ys)

append :: Dir r -> Dir 'Relative -> Dir r
append a b = case a of
  DA ns -> DA (ns <> getNames b)
  DR ns -> DR (ns <> getNames b)

appendName :: Dir r -> Name -> Dir r
appendName d name = case d of
  DA ns -> DA (ns <> [name])
  DR ns -> DR (ns <> [name])

popName :: Dir r -> (Dir r, Name)
popName d = case d of
  DA ns -> (DA $ init ns, last ns)
  DR ns -> (DR $ init ns, last ns)

getNames :: forall r. Dir r -> [Name]
getNames a = case a of
  DA ns -> ns
  DR ns -> ns
