module System.FSTree.Core where

import Data.Either
import System.Directory
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

import Pr
import System.Path.Dir as DP
import System.Path.Dir (renderPath, nameToString)
import System.Path.File as FP
import System.Path
import qualified System.File as F


newtype FSTree a = FSTree
  { unFSTree :: M.Map Name (Node a)
  } deriving (Show)

type Node a = Either (FSTree a) a

singleton :: Name -> Node a -> FSTree a
singleton name node = FSTree $ M.singleton name node

fsTip :: b -> Either a b
fsTip = Right

fsBranch :: a -> Either a b
fsBranch = Left

-- * Instances

instance Functor FSTree where
  fmap f = unFSTree ^ fmap f' ^ FSTree
    where
      f' = either (Left . fmap f) (Right . f)

instance Foldable FSTree where
  foldMap f = unFSTree ^ M.elems ^ foldMap f'
    where
      f' = either (foldMap f) f

instance Traversable FSTree where
  traverse
    :: forall f a b. Applicative f
    => (a -> f b) -> FSTree a -> f (FSTree b)
  traverse f (FSTree m) = FSTree <$> traverse f' m
    where
      f' :: Node a -> f (Node b)
      f' = either (fmap Left . traverse f) (fmap Right . f)

instance Monoid (FSTree a) where
  mempty = FSTree M.empty
  mappend (FSTree m) (FSTree m') = FSTree $ M.unionWith go m m'
    where
      go :: Node a -> Node a -> Node a
      go (Left d) (Left d') = Left (d <> d')
      go _ f = f

-- * Shorthands

-- * Read from disk

type FileTree = FSTree F.File
type NameTree = FSTree Name

class FromDisk a where
  fromDisk :: Dir r -> IO a

instance FromDisk NameTree where
  fromDisk init = FSTree . M.fromList <$> (mapM go =<< gdc (renderPath init))
    where
      go name = let
          dirname = init `addDirSuffix` name
        in do
        isDir <- doesDirectoryExist $ renderPath dirname
        if isDir
          then (name, ) . Left <$> fromDisk dirname
          else do
            isFile <- doesFileExist $ renderPath $ init `addFile` name
            if isFile
              then return $ (name, Right name)
              else error "fromDisk: not a directory, not a file"

instance FromDisk FileTree where
  fromDisk init = fromDisk init >>= nameTreeToFileTree init

instance FromDisk (FSTree (File 'Relative)) where
  fromDisk = fromDisk' (either ensureRelative id)

instance FromDisk (FSTree (File 'Absolute)) where
  fromDisk = fromDisk' (either id ensureAbsolute)

fromDisk'
  :: Path a
  => (Either (Dir 'Absolute) (Dir 'Relative) -> Dir r)
  -> a
  -> IO (FSTree (File r))
fromDisk' ensure path = do
  e <- isDirPath (renderPath path)
  go $ ensure $ fromJust e
  where
    go :: forall r. Dir r -> IO (FSTree (File r))
    go path = do
      (dirNames, fileNames) <- gdc1 path <&> partitionEithers
      subDirs :: FSTree (File r) <- mapM (sub path) dirNames <&> mconcat
      return $ subDirs <> mconcat (map mkFile fileNames)
      where
        mkFile name = singleton name $ fsTip $ path `addFile` name
        sub path name = go newPath <&> fsBranch ^ singleton name
          where
            newPath = appendName path name

nameTreeToFileTree :: Dir root -> NameTree -> IO FileTree
nameTreeToFileTree root tree = fmap (FSTree . M.fromList) $ (<>) <$> mapM rfile files <*> mapM rdir dirs
  where
    (dirs :: [(Name, NameTree)], files :: [Name]) = fmap (map fst) . partitionEithers . map pullEither . M.toList $ unFSTree tree
    rfile name = (name,) . Right <$> readFile root name
    rdir (name, fs) = (name,) . Left <$> nameTreeToFileTree (root `addDirSuffix` name) fs

-- * Write to disk

-- | Write an FSTree to path. Currently fails when parts of the FSTree already exist
walkFSTree :: forall a r. Dir r -> FSTree a -> (Name -> a -> IO ()) -> IO ()
walkFSTree dest tree action = inPath dest (go tree)
   where
     go = unFSTree ^ M.toList ^ mapM_ (uncurry create)
     create :: Name -> Either (FSTree a) a -> IO ()
     create name node = case node of
        Left tr -> let n = nameToString name
           in do
              createDirectoryIfMissing False n
              setCurrentDirectory n
              go tr
              setCurrentDirectory ".."
        Right a -> action name a

-- * FSTree to paths

paths :: FSTree a -> [SomePath 'Relative]
paths tree = go mempty tree
  where
    go :: Dir 'Relative -> FSTree a -> [SomePath 'Relative]
    go prefix tree = (Left prefix : concat dirs) <> map Right files
      where
        dirs :: [[SomePath 'Relative]]
        files :: [File 'Relative]
        (dirs, files) = partitionEithers $ map (bimap recurse (addFile prefix . fst)) . fsToEithers $ tree
        recurse :: (Name, FSTree a) -> [SomePath 'Relative]
        recurse (name, tree') = go (prefix `addDirSuffix` name) tree'

-- * Helpers

-- | Get directory contents: FilePath as input, Name as output
gdc :: FilePath -> IO [Name]
gdc p = paths <&> map T.pack
  where
    paths :: IO [FilePath]
    paths = getDirectoryContents p <&> filter (\fn -> not (fn `elem` [".", ".."]))

gdc1 :: forall r. Dir r -> IO [Either Name Name]
gdc1 path = do
  names <- gdc $ renderPath path
  forM names $ \name -> do
    isDir <- doesDirectoryExist $ renderPath $ appendName path name
    if isDir
      then return $ Left name
      else return $ Right name

-- | Get directory contents: relative folders and files as either
gdc2 :: forall r. Dir r -> IO [SomePath 'Relative]
gdc2 path = do
  names <- gdc $ renderPath path
  forM names $ \name -> do
    isDir <- doesDirectoryExist $ renderPath $ appendName path name
    if isDir
      then return $ Right $ FP.File (DR [], name)
      else return $ Left $ DR [name]

addDirSuffix :: Dir root -> Name -> Dir root
addDirSuffix base sub = case base of DA li -> DA (li <> [sub]); DR li -> DR (li <> [sub])

fsToEithers :: FSTree a -> [Either (Name, FSTree a) (Name, a)]
fsToEithers = map pullEither . M.toList . unFSTree

pullEither :: (Name, Node a) -> Either (Name, FSTree a) (Name, a)
pullEither (n, eith) = either (Left . (n,)) (Right . (n,)) eith
