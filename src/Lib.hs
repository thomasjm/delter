module Lib (
  watchFileForChanges
  , DiffResult(..)
  ) where

import Control.Monad (unless, forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Time
import System.Exit (ExitCode(..))
import qualified System.FSNotify as FS
import System.FSNotify hiding (watchDir)
import System.FilePath
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Process


data DiffResult = DiffResult {
  diffBytes :: ByteString
  , diffSize :: Int
  , diffTime :: NominalDiffTime
  } deriving (Show)

watchFileForChanges :: FilePath -> (DiffResult -> IO ()) -> IO ()
watchFileForChanges filePath callback = do
  let watchDir = takeDirectory filePath
      fileName = takeFileName filePath

  absFilePath <- makeAbsolute filePath
  absDirPath <- makeAbsolute watchDir

  fileExists <- doesFileExist absFilePath
  unless fileExists $ error $ "File does not exist: " ++ absFilePath

  withSystemTempDirectory "delter" $ \tempDir -> do
    let tempFilePath = tempDir </> "previous_" ++ fileName

    copyFile absFilePath tempFilePath

    withManager $ \mgr -> do
      _ <- FS.watchDir mgr absDirPath (const True) $ \event -> do
        case event of
          Modified path _ _ | path == absFilePath -> handleChange path tempFilePath callback
          _ -> return ()

      putStrLn $ "Watching " ++ absFilePath ++ " for changes (Ctrl+C to stop)..."
      forever $ threadDelay 1000000

handleChange :: FilePath -> FilePath -> (DiffResult -> IO ()) -> IO ()
handleChange currentFile previousFile callback = do
  startTime <- getCurrentTime

  withSystemTempFile "diff.patch" $ \patchPath patchHandle -> do
    hClose patchHandle

    (exitCode, _, errorOutput) <- readProcessWithExitCode "xdelta3" [
      "-e", "-s", previousFile, currentFile, patchPath
      ] ""

    case exitCode of
      ExitSuccess -> do
        patchSize <- fromIntegral <$> getFileSize patchPath
        endTime <- getCurrentTime
        let timeTaken = diffUTCTime endTime startTime

        patchBytes <- B.readFile patchPath

        callback $ DiffResult patchBytes patchSize timeTaken

        copyFile currentFile previousFile
      ExitFailure _ -> do
        putStrLn $ "xdelta3 error: " ++ errorOutput
