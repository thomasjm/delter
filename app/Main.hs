module Main (main) where

import Lib
import System.Environment
import System.Exit
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      putStrLn $ "Starting to watch file: " ++ filePath
      watchFileForChanges filePath printDiffResult
    _ -> do
      putStrLn "Usage: delter-exe <file-path>"
      exitFailure

printDiffResult :: DiffResult -> IO ()
printDiffResult (DiffResult size time) = do
  printf "Patch generated: %d bytes in %.3f seconds\n" size (realToFrac time :: Double)
