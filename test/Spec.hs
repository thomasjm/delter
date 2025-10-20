{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString qualified as B
import qualified Delter
import qualified Delter.Executable as Exe
import qualified Delter.FFI as FFI
import System.Exit (ExitCode(..))
import System.FilePath
import Test.Sandwich
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Process


testImplementations :: [(String, FilePath -> (FFI.DiffResult -> IO ()) -> IO ())]
testImplementations = [
  ("Delter.FFI", FFI.watchFileForChanges)
  , ("Delter.Executable", Exe.watchFileForChanges)
  ]

delterTests :: TopSpec
delterTests = describe "Delter Tests" $ do
  mapM_ (\(name, watchFn) -> describe name $ do
    describe "File diff generation" $ do
      it "should handle non-existent file gracefully" $ do
        let nonExistentFile = "/tmp/non-existent-file.txt"
        let callback _ = return ()

        result <- liftIO $ tryAny (watchFn nonExistentFile callback)
        case result of
          Left ex -> show ex `shouldContain` "File does not exist"
          Right _ -> expectationFailure "Should have thrown an error for non-existent file"
    ) testImplementations

  describe "Binary diff with FFI" $ do
    it "should create smaller patches for small changes using diffByteStrings" $ do
      let content1 = B.pack $ replicate 1000 65  -- 1000 'A's
          content2 = B.pack $ (replicate 999 65) ++ [66]  -- 999 'A's + 'B'

      result <- liftIO $ FFI.diffByteStrings content2 content1
      let patchSize = FFI.diffSize result

      when (patchSize >= 1000) $
        fail $ "FFI patch size too large: " ++ show patchSize

      when (patchSize == 0) $
        fail "FFI patch size should not be zero for different content"

    it "should work with default module diffByteStrings" $ do
      let content1 = B.pack $ replicate 500 65   -- 500 'A's
          content2 = B.pack $ (replicate 499 65) ++ [66]  -- 499 'A's + 'B'

      result <- liftIO $ Delter.diffByteStrings content2 content1
      let patchSize = Delter.diffSize result

      when (patchSize >= 500) $
        fail $ "Default patch size too large: " ++ show patchSize

      when (patchSize == 0) $
        fail "Default patch size should not be zero for different content"

  describe "Binary diff with xdelta3 executable (compatibility)" $ do
    it "should create smaller patches for small changes" $ do
      liftIO $ withSystemTempDirectory "delter-test" $ \tempDir -> do
        let file1 = tempDir </> "file1.txt"
            file2 = tempDir </> "file2.txt"
            patchFile = tempDir </> "patch.bin"

        let content1 = replicate 1000 'a'
            content2 = replicate 999 'a' ++ "b"

        writeFile file1 content1
        writeFile file2 content2

        result <- tryAny $ readProcessWithExitCode
          "xdelta3"
          ["-e", "-s", file1, file2, patchFile]
          ""

        case result of
          Left ex ->
            -- Skip test if xdelta3 is not available
            liftIO $ putStrLn $ "Skipping test - xdelta3 not available: " ++ show ex
          Right (exitCode, _, _) -> do
            when (exitCode /= ExitSuccess) $
              fail "xdelta3 command failed"

            patchExists <- doesFileExist patchFile
            when (not patchExists) $
              fail "Patch file was not created"

            patchSize <- getFileSize patchFile
            when (patchSize >= 1000) $
              fail $ "Patch size too large: " ++ show patchSize

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions delterTests
