
import Control.Monad
import Data.ByteString qualified as B
import qualified Delter.Executable as Exe
import qualified Data.Text as T
import qualified Delter.FFI as FFI
import Test.Sandwich
import UnliftIO


testImplementations :: [
  (String
  , FilePath -> (FFI.DiffResult -> IO ()) -> IO ()
  , B.ByteString -> B.ByteString -> IO FFI.DiffResult
  )
  ]
testImplementations = [
  ("Delter.FFI", FFI.watchFileForChanges, FFI.diffByteStrings)
  , ("Delter.Executable", Exe.watchFileForChanges, Exe.diffByteStrings)
  ]

delterTests :: TopSpec
delterTests = describe "Delter Tests" $ do
  forM_ testImplementations $ \(name, watchFn, diffFn) -> describe name $ do
    describe "File diff generation" $ do
      it "should handle non-existent file gracefully" $ do
        liftIO (tryAny (watchFn "/tmp/non-existent-file.txt" (const $ return ()))) >>= \case
          Left ex -> show ex `shouldContain` "File does not exist"
          Right _ -> expectationFailure "Should have thrown an error for non-existent file"

    describe "diffByteStrings" $ do
      it "should create smaller patches for small changes" $ do
        let content1 = B.pack $ replicate 1000 65  -- 1000 'A's
        let content2 = B.pack $ (replicate 999 65) ++ [66]  -- 999 'A's + 'B'

        result <- liftIO $ diffFn content2 content1
        let patchSize = FFI.diffSize result

        info ("patch result: " <> T.show result)

        when (patchSize >= 1000) $
          fail $ name ++ " patch size too large: " ++ show patchSize

        when (patchSize == 0) $
          fail $ name ++ " patch size should not be zero for different content"


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions delterTests
