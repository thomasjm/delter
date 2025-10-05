{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Time
import Lib
import System.Exit (ExitCode(..))
import System.FilePath
import Test.Sandwich
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Process

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions delterTests

delterTests :: TopSpec
delterTests = describe "Delter Tests" $ do

  describe "DiffResult" $ do
    it "should show size and time correctly" $ do
      let result = DiffResult 100 (secondsToNominalDiffTime 0.5)
      show result `shouldBe` "DiffResult {diffSize = 100, diffTime = 0.5s}"

    it "should handle zero size diffs" $ do
      let result = DiffResult 0 (secondsToNominalDiffTime 0.1)
      diffSize result `shouldBe` 0
      diffTime result `shouldBe` secondsToNominalDiffTime 0.1

  describe "File diff generation" $ do
    it "should handle non-existent file gracefully" $ do
      let nonExistentFile = "/tmp/non-existent-file.txt"
      let callback _ = return ()

      result <- liftIO $ tryAny (watchFileForChanges nonExistentFile callback)
      case result of
        Left ex -> show ex `shouldContain` "File does not exist"
        Right _ -> expectationFailure "Should have thrown an error for non-existent file"

  describe "Binary diff with xdelta3" $ do
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
