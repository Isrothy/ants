module Path.Bridge where

import Control.Monad
import Data.Time
import Path
import System.Directory
import qualified System.Directory as S

createDirectory :: Path b Dir -> IO ()
createDirectory = S.createDirectory . toFilePath

createDirectoryIfMissing :: Bool -> Path b Dir -> IO ()
createDirectoryIfMissing p dir = S.createDirectoryIfMissing p (toFilePath dir)

removeDirectory :: Path b Dir -> IO ()
removeDirectory = S.removeDirectory . toFilePath

removeDirectoryRecursive :: Path b Dir -> IO ()
removeDirectoryRecursive = S.removeDirectoryRecursive . toFilePath

removePathForcibly :: Path b t -> IO ()
removePathForcibly = S.removePathForcibly . toFilePath

renameDirectory :: Path a Dir -> Path b Dir -> IO ()
renameDirectory old new = S.renameDirectory (toFilePath old) (toFilePath new)

listDirectory :: Path b dir -> IO ([Path Rel Dir], [Path Rel File])
listDirectory dir = do
  entities <- S.listDirectory (toFilePath dir)
  dirs <- filterM S.doesDirectoryExist entities >>= mapM parseRelDir
  files <- filterM S.doesFileExist entities >>= mapM parseRelFile
  return (dirs, files)

getCurrentDirectory :: IO (Path Abs Dir)
getCurrentDirectory = S.getCurrentDirectory >>= parseAbsDir

setCurrentDirectory :: Path b Dir -> IO ()
setCurrentDirectory = S.setCurrentDirectory . toFilePath

withCurrentDirectory :: Path b Dir -> IO a -> IO a
withCurrentDirectory dir = S.withCurrentDirectory (toFilePath dir)

getHomeDirectory :: IO (Path Abs Dir)
getHomeDirectory = S.getHomeDirectory >>= parseAbsDir

getXdgDirectory :: XdgDirectory -> Path Rel Dir -> IO (Path Abs Dir)
getXdgDirectory xdg path = S.getXdgDirectory xdg (toFilePath path) >>= parseAbsDir

getXdgDirectoryList :: XdgDirectoryList -> IO [Path Abs Dir]
getXdgDirectoryList xdg = S.getXdgDirectoryList xdg >>= mapM parseAbsDir

getAppUserDataDirectory :: Path Rel t -> IO (Path Abs Dir)
getAppUserDataDirectory path = S.getAppUserDataDirectory (toFilePath path) >>= parseAbsDir

getUserDocumentsDirectory :: IO (Path Abs Dir)
getUserDocumentsDirectory = S.getUserDocumentsDirectory >>= parseAbsDir

getTemporaryDirectory :: IO (Path Abs Dir)
getTemporaryDirectory = S.getTemporaryDirectory >>= parseAbsDir

removeFile :: Path b File -> IO ()
removeFile = S.removeFile . toFilePath

renameFile :: Path a File -> Path b File -> IO ()
renameFile old new = S.renameFile (toFilePath old) (toFilePath new)

renamePath :: Path a t -> Path b t -> IO ()
renamePath old new = S.renamePath (toFilePath old) (toFilePath new)

copyFile :: Path a File -> Path b File -> IO ()
copyFile old new = S.copyFile (toFilePath old) (toFilePath new)

copyFileWithMetadata :: Path a File -> Path b File -> IO ()
copyFileWithMetadata old new = S.copyFileWithMetadata (toFilePath old) (toFilePath new)

getFileSize :: Path b File -> IO Integer
getFileSize = S.getFileSize . toFilePath

makeAbsoluteFile :: Path b File -> IO (Path Abs File)
makeAbsoluteFile path = S.makeAbsolute (toFilePath path) >>= parseAbsFile

makeAbsoluteDir :: Path b File -> IO (Path Abs File)
makeAbsoluteDir path = S.makeAbsolute (toFilePath path) >>= parseAbsFile

doesPathExist :: Path b t -> IO Bool
doesPathExist = S.doesPathExist . toFilePath

doesFileExist :: Path b File -> IO Bool
doesFileExist = S.doesFileExist . toFilePath

doesDirectoryExist :: Path b Dir -> IO Bool
doesDirectoryExist = S.doesDirectoryExist . toFilePath

findExecutable :: String -> IO (Maybe (Path Abs File))
findExecutable exe = S.findExecutable exe >>= traverse parseAbsFile

findExecutables :: String -> IO [Path Abs File]
findExecutables exe = S.findExecutables exe >>= mapM parseAbsFile

findExecutablesInDirectories :: [Path b Dir] -> String -> IO [Path Abs File]
findExecutablesInDirectories dirs exe =
  S.findExecutablesInDirectories (toFilePath <$> dirs) exe >>= mapM parseAbsFile

findFile :: [Path b Dir] -> String -> IO (Maybe (Path Abs File))
findFile dirs file = S.findFile (toFilePath <$> dirs) file >>= traverse parseAbsFile

findFiles :: [Path b Dir] -> String -> IO [Path Abs File]
findFiles dirs file = S.findFiles (toFilePath <$> dirs) file >>= mapM parseAbsFile

findFileWith :: (SomeBase File -> IO Bool) -> [Path b Dir] -> String -> IO (Maybe (Path Abs File))
findFileWith f dirs name = S.findFileWith f' (toFilePath <$> dirs) name >>= traverse parseAbsFile
  where
    f' file = maybe (return False) f (parseSomeFile file)

findFilesWith :: (SomeBase File -> IO Bool) -> [Path b Dir] -> String -> IO [Path Abs File]
findFilesWith f dirs name = S.findFilesWith f' (toFilePath <$> dirs) name >>= mapM parseAbsFile
  where
    f' file = maybe (return False) f (parseSomeFile file)

createFileLink :: Path a File -> Path b File -> IO ()
createFileLink old new = S.createFileLink (toFilePath old) (toFilePath new)

createDirectoryLink :: Path a Dir -> Path b Dir -> IO ()
createDirectoryLink old new = S.createDirectoryLink (toFilePath old) (toFilePath new)

removeDirectoryLink :: Path b Dir -> IO ()
removeDirectoryLink = S.removeDirectoryLink . toFilePath

pathIsSymbolicLink :: Path b t -> IO Bool
pathIsSymbolicLink = S.pathIsSymbolicLink . toFilePath

getSymbolicLinkTarget :: Path b t -> IO String
getSymbolicLinkTarget = S.getSymbolicLinkTarget . toFilePath

getPermissions :: Path b t -> IO Permissions
getPermissions = S.getPermissions . toFilePath

setPermissions :: Path b t -> Permissions -> IO ()
setPermissions = S.setPermissions . toFilePath

copyPermissions :: Path a t -> Path b s -> IO ()
copyPermissions old new = S.copyPermissions (toFilePath old) (toFilePath new)

getAccessTime :: Path b t -> IO UTCTime
getAccessTime = S.getAccessTime . toFilePath

getModificationTime :: Path b t -> IO UTCTime
getModificationTime = S.getModificationTime . toFilePath

setAccessTime :: Path b t -> UTCTime -> IO ()
setAccessTime = S.setAccessTime . toFilePath

setModificationTime :: Path b t -> UTCTime -> IO ()
setModificationTime = S.setModificationTime . toFilePath
