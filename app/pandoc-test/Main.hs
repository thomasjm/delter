module Main (main) where

import Control.Monad (forM_, unless)
import Data.Time
import Delter.Executable
import System.Environment
import System.Exit
import Text.Printf
import UnliftIO.Directory
import UnliftIO.Process


runPandocTest :: FilePath -> IO ()
runPandocTest outputFile = do
  let samples = ["sample1.md", "sample2.md", "sample3.md"]

  putStrLn "=== Pandoc Markdown to PDF Test ==="
  putStrLn ""

  forM_ samples $ \sample -> do
    printf "Converting %s to %s...\n" sample outputFile
    compilePandoc sample outputFile
    putStrLn ""

  putStrLn "=== Starting file watcher on sample3.md ==="
  putStrLn "Edit sample3.md to see diff results (Ctrl+C to stop)"
  putStrLn ""

  watchFileForChanges "sample3.md" $ \diffResult -> do
    printf "File changed! Recompiling...\n"
    compilePandoc "sample3.md" outputFile
    printDiffResult diffResult

compilePandoc :: FilePath -> FilePath -> IO ()
compilePandoc inputFile outputFile = do
  inputExists <- doesFileExist inputFile
  if not inputExists
    then putStrLn $ "Warning: " ++ inputFile ++ " does not exist, skipping"
    else do
      startTime <- getCurrentTime
      (exitCode, stdoutOutput, stderrOutput) <- readProcessWithExitCode "pandoc"
        [ inputFile
        , "-o", outputFile
        , "--pdf-engine=xelatex"
        , "--variable", "geometry:margin=1in"
        , "--variable", "fontsize=12pt"
        ] ""
      endTime <- getCurrentTime
      let compilationTime = diffUTCTime endTime startTime
      case exitCode of
        ExitSuccess -> do
          printf "✓ Successfully converted %s in %.3f seconds\n" inputFile (realToFrac compilationTime :: Double)
          unless (null stdoutOutput) $ putStrLn $ "  Output: " ++ stdoutOutput
        ExitFailure code -> do
          printf "✗ Failed to convert %s (exit code %d) after %.3f seconds\n" inputFile code (realToFrac compilationTime :: Double)
          unless (null stderrOutput) $ putStrLn $ "  Error: " ++ stderrOutput

printDiffResult :: DiffResult -> IO ()
printDiffResult (DiffResult _bytes size time) = do
  printf "  → Diff: %d bytes in %.3f seconds\n" size (realToFrac time :: Double)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputFile] -> runPandocTest outputFile
    _ -> do
      putStrLn "Usage: pandoc-test <output-pdf-file>"
      putStrLn "  This will compile sample1.md, sample2.md, sample3.md to the output file"
      putStrLn "  and then watch sample3.md for changes, showing diffs"
      exitFailure
