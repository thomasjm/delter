module Lib (
  watchFileForChanges
  , DiffResult(..)
  ) where

import Control.Concurrent
import Control.Monad
import Data.Time
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

data DiffResult = DiffResult
    { diffSize :: Int
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
            _ <- watchDir mgr absDirPath (const True) $ \event -> do
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

        (exitCode, _, stderr) <- readProcessWithExitCode
            "xdelta3"
            ["-e", "-s", previousFile, currentFile, patchPath]
            ""

        case exitCode of
            ExitSuccess -> do
                patchSize <- fromIntegral <$> getFileSize patchPath
                endTime <- getCurrentTime
                let timeTaken = diffUTCTime endTime startTime

                callback $ DiffResult patchSize timeTaken

                copyFile currentFile previousFile
            ExitFailure _ -> do
                putStrLn $ "xdelta3 error: " ++ stderr
