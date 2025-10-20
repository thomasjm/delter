module Delter.FFI (
  watchFileForChanges
  , diffByteStrings
  , DiffResult(..)
  ) where

import Control.Monad (unless, forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Time
import qualified Delter.FFI.C as XD3
import Delter.Types
import qualified System.FSNotify as FS
import System.FSNotify hiding (watchDir)
import System.FilePath
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory

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

-- | Generate a binary diff between two ByteStrings using C FFI
diffByteStrings :: ByteString -> ByteString -> IO DiffResult
diffByteStrings currentBytes previousBytes = do
  startTime <- getCurrentTime

  result <- XD3.encodeMemory currentBytes previousBytes
  case result of
    Right patchBytes -> do
      endTime <- getCurrentTime
      let timeTaken = diffUTCTime endTime startTime
          patchSize = B.length patchBytes
      return $ DiffResult patchBytes patchSize timeTaken
    Left errorMsg -> do
      error $ "xdelta3 error: " ++ errorMsg

handleChange :: FilePath -> FilePath -> (DiffResult -> IO ()) -> IO ()
handleChange currentFile previousFile callback = do
  currentBytes <- B.readFile currentFile
  previousBytes <- B.readFile previousFile

  diffResult <- diffByteStrings currentBytes previousBytes
  callback diffResult
  copyFile currentFile previousFile
