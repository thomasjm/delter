module Delter.Executable (
  watchFileForChanges
  , diffByteStrings
  , patchByteStrings
  ,  DiffResult(..)
  ) where

import Control.Monad (unless, forever)
import Data.ByteString qualified as B
import Data.Time
import Delter.Types
import System.Exit (ExitCode(..))
import qualified System.FSNotify as FS
import System.FSNotify hiding (watchDir)
import System.FilePath
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Process


watchFileForChanges :: FilePath -> (DiffResult -> IO ()) -> IO ()
watchFileForChanges filePath callback = do
  let watchDir = takeDirectory filePath
      fileName = takeFileName filePath

  absFilePath <- makeAbsolute filePath
  absDirPath <- makeAbsolute watchDir

  fileExists <- doesFileExist absFilePath
  unless fileExists $
    throwIO $ userError $ "File does not exist: " ++ absFilePath

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

-- | Generate a binary diff between two ByteStrings using external xdelta3 binary
diffByteStrings :: B.ByteString -> B.ByteString -> IO DiffResult
diffByteStrings currentBytes previousBytes = do
  startTime <- getCurrentTime

  withSystemTempDirectory "delter-executable" $ \tempDir -> do
    let currentPath = tempDir </> "current"
        previousPath = tempDir </> "previous"
        patchPath = tempDir </> "patch"

    B.writeFile currentPath currentBytes
    B.writeFile previousPath previousBytes

    (exitCode, _, errorOutput) <- readProcessWithExitCode "xdelta3" [
      "-e", "-f", "-s", previousPath, currentPath, patchPath
      ] ""

    case exitCode of
      ExitSuccess -> do
        patchSize <- fromIntegral <$> getFileSize patchPath
        endTime <- getCurrentTime
        let timeTaken = diffUTCTime endTime startTime

        patchBytes <- B.readFile patchPath
        return $ DiffResult patchBytes patchSize timeTaken
      ExitFailure _ -> do
        error $ "xdelta3 error: " ++ errorOutput

-- | Apply a binary patch to a ByteString using external xdelta3 binary
patchByteStrings :: B.ByteString -> B.ByteString -> IO (Either String B.ByteString)
patchByteStrings patch source = do
  withSystemTempDirectory "delter-patch" $ \tempDir -> do
    let sourcePath = tempDir </> "source"
        patchPath = tempDir </> "patch"
        outputPath = tempDir </> "output"

    B.writeFile sourcePath source
    B.writeFile patchPath patch

    (exitCode, _, errorOutput) <- readProcessWithExitCode "xdelta3" [
      "-d", "-f", "-s", sourcePath, patchPath, outputPath
      ] ""

    case exitCode of
      ExitSuccess -> do
        outputExists <- doesFileExist outputPath
        if outputExists
          then Right <$> B.readFile outputPath
          else return $ Left "xdelta3 did not create output file"
      ExitFailure code ->
        return $ Left $ "xdelta3 decode failed with code " ++ show code ++ ": " ++ errorOutput

handleChange :: FilePath -> FilePath -> (DiffResult -> IO ()) -> IO ()
handleChange currentFile previousFile callback = do
  currentBytes <- B.readFile currentFile
  previousBytes <- B.readFile previousFile

  diffResult <- diffByteStrings currentBytes previousBytes
  callback diffResult
  copyFile currentFile previousFile
