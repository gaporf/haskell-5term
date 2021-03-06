{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module FS
  (
    FS (Dir, File)
  , readFS
  , fileName
  , dirName
  , contents
  , cd
  , ls
  , file
  ) where
   
   
import Data.Set as Set (fromList)
import Lens.Micro
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (splitDirectories, takeFileName)

-- | data represent simple file system consists of directories and files with only file names
data FS
  = Dir
  {
    _name     :: FilePath  -- only file name without full file path
  , _contents :: [FS]
  }
  | File
  {
    _name     :: FilePath  -- only file name without full file path
  }
  deriving (Ord, Show)
  
isEqual
  :: [FS]
  -> [FS]
  -> Bool
isEqual a b = (Set.fromList a) == (Set.fromList b)
  
instance Eq FS where
  (==)
    :: FS
    -> FS
    -> Bool
  (==) (File a) (File b)     = a == b
  (==) (Dir a f1) (Dir b f2) = (a == b) && (isEqual f1 f2)
  (==) _ _                   = False
  
takeDir
  :: FilePath
  -> FilePath
takeDir = last . splitDirectories

-- | takes file path and returns a strict representation of file system. If the file path is
-- incorrect, then returns a file with empty name
readFS
  :: FilePath
  -> IO FS
readFS filePath = do
  isFile <- doesFileExist filePath
  if isFile
    then return $ File $ takeFileName filePath
    else do
      isDirectory <- doesDirectoryExist $ filePath
      if not isDirectory
        then return $ File ""
        else do
          cont <- getDirectoryContents filePath
          fCont <- return $ filter (\p -> ((p /= ".") && (p /= ".."))) cont
          return $ Dir (takeDir filePath) 
                       (map (\p -> unsafePerformIO $ readFS $ filePath ++ "/" ++ p) fCont)

-- | name lens
nameLens :: Lens' FS FilePath
nameLens = lens _name (\fs newName -> fs { _name = newName })

-- | contents lens
contentsLens :: Lens' FS [FS]
contentsLens = lens _contents (\dir newContents -> dir { _contents = newContents })

    
-- | returns the filename if the FS is File, otherwise returns an empty string
fileName :: Traversal' FS FilePath
fileName nameF (File oldName) = File <$> nameF oldName
fileName _ d                  = pure d

-- | returns the directory name if the FS is Dir, otherwise returns an empty string
dirName :: Traversal' FS FilePath
dirName nameF (Dir oldName contents) = Dir <$> nameF oldName <*> pure contents
dirName _ f                          = pure f

-- | returns the content of directory otherwise returns an empty list
contents :: Traversal' FS [FS]
contents contentsF (Dir name oldContents) = Dir <$> pure name <*> contentsF oldContents
contents _ f                              = pure f

-- | changes the current directory to the subdirectory
cd 
  :: FilePath
  -> Traversal' FS FS
cd dir = contents.traversed.filtered(\path -> path^.dirName == dir)

-- | shows the content of the current directory
ls :: Traversal' FS FilePath
ls = contents.traversed.failing dirName fileName

-- | shows the file path if the file exists
file 
  :: FilePath
  -> Traversal' FS FilePath
file f = failing 
  (fileName.filtered(== f)) 
  (contents.traversed.filtered(\path -> path^.fileName == f).fileName)
