module Main (main) where

import Lib
import Control.Monad (forM_, unless)
import Data.Time
import System.Environment
import System.Exit
import Text.Printf
import UnliftIO.Directory
import UnliftIO.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputFile] -> runTypstTest outputFile
    _ -> do
      putStrLn "Usage: typst-test <output-pdf-file>"
      putStrLn "  This will compile sample1.typ, sample2.typ, sample3.typ to the output file"
      putStrLn "  and then watch sample3.typ for changes, showing diffs"
      exitFailure

runTypstTest :: FilePath -> IO ()
runTypstTest outputFile = do
  let samples = ["sample1.typ", "sample2.typ", "sample3.typ"]
  
  putStrLn "=== Typst Rendering Test ==="
  putStrLn ""
  
  forM_ samples $ \sample -> do
    printf "Compiling %s to %s...\n" sample outputFile
    compileTypst sample outputFile
    putStrLn ""
  
  putStrLn "=== Starting file watcher on sample3.typ ==="
  putStrLn "Edit sample3.typ to see diff results (Ctrl+C to stop)"
  putStrLn ""
  
  watchFileForChanges "sample3.typ" $ \diffResult -> do
    printf "File changed! Recompiling...\n"
    compileTypst "sample3.typ" outputFile
    printDiffResult diffResult

compileTypst :: FilePath -> FilePath -> IO ()
compileTypst inputFile outputFile = do
  inputExists <- doesFileExist inputFile
  if not inputExists
    then putStrLn $ "Warning: " ++ inputFile ++ " does not exist, skipping"
    else do
      startTime <- getCurrentTime
      (exitCode, stdoutOutput, stderrOutput) <- readProcessWithExitCode "typst" ["compile", inputFile, outputFile] ""
      endTime <- getCurrentTime
      let compilationTime = diffUTCTime endTime startTime
      case exitCode of
        ExitSuccess -> do
          printf "✓ Successfully compiled %s in %.3f seconds\n" inputFile (realToFrac compilationTime :: Double)
          unless (null stdoutOutput) $ putStrLn $ "  Output: " ++ stdoutOutput
        ExitFailure code -> do
          printf "✗ Failed to compile %s (exit code %d) after %.3f seconds\n" inputFile code (realToFrac compilationTime :: Double)
          unless (null stderrOutput) $ putStrLn $ "  Error: " ++ stderrOutput

printDiffResult :: DiffResult -> IO ()
printDiffResult (DiffResult _bytes size time) = do
  printf "  → Diff: %d bytes in %.3f seconds\n" size (realToFrac time :: Double)