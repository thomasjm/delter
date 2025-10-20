{-|
Module      : Delter
Description : Binary diffing library with both C FFI and executable backends
License     : BSD-3-Clause
Maintainer  : example@example.com

This module provides two implementations for binary diffing using xdelta3:

* "Delter.FFI" - Direct C FFI bindings to xdelta3 library (faster, in-memory)
* "Delter.Executable" - Uses external xdelta3 binary via process calls (more portable)

Both implementations provide the same 'DiffResult' type and 'watchFileForChanges' function.

Example usage:

@
import qualified Delter.FFI as FFI
import qualified Delter.Executable as Exe

main = do
  -- Use C FFI version (recommended for performance)
  FFI.watchFileForChanges "myfile.txt" $ \result -> do
    putStrLn $ "FFI: Patch size: " ++ show (FFI.diffSize result) ++ " bytes"
  
  -- Or use executable version (better for portability)  
  Exe.watchFileForChanges "myfile.txt" $ \result -> do
    putStrLn $ "Exe: Patch size: " ++ show (Exe.diffSize result) ++ " bytes"
@

For backwards compatibility, this module also exports the FFI version as the default:
-}

module Delter (
  -- * Default implementation (FFI-based)
  watchFileForChanges,
  diffByteStrings,
  DiffResult(..),
  
  -- * Submodules for explicit choice
  -- | Import these qualified to choose implementation:
  --
  -- @
  -- import qualified Delter.FFI as FFI
  -- import qualified Delter.Executable as Exe  
  -- @
  
) where

import Delter.FFI (watchFileForChanges, diffByteStrings, DiffResult(..))