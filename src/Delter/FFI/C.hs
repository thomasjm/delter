{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Delter.FFI.C (
  encodeMemory,
  decodeMemory
) where

import Foreign
import Foreign.C.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

-- Type aliases to match xdelta3 C types
type USize = CSize
type XDFlags = CInt

-- Foreign function declarations for xdelta3 C API
foreign import ccall "xdelta3.h xd3_encode_memory"
  c_xd3_encode_memory :: Ptr Word8      -- input buffer
                      -> USize          -- input size  
                      -> Ptr Word8      -- source buffer
                      -> USize          -- source size
                      -> Ptr Word8      -- output buffer
                      -> Ptr USize      -- output size (in/out)
                      -> USize          -- available output space
                      -> XDFlags        -- flags
                      -> IO CInt

foreign import ccall "xdelta3.h xd3_decode_memory"  
  c_xd3_decode_memory :: Ptr Word8      -- input buffer
                      -> USize          -- input size
                      -> Ptr Word8      -- source buffer  
                      -> USize          -- source size
                      -> Ptr Word8      -- output buffer
                      -> Ptr USize      -- output size (in/out)
                      -> USize          -- available output space
                      -> XDFlags        -- flags
                      -> IO CInt

-- Error codes from xdelta3 (common ones)
xd3_SUCCESS :: CInt
xd3_SUCCESS = 0

-- High-level Haskell wrappers
encodeMemory :: ByteString -> ByteString -> IO (Either String ByteString)
encodeMemory input source = do
  let inputSize = fromIntegral $ B.length input
      sourceSize = fromIntegral $ B.length source
      maxOutputSize = inputSize + sourceSize + 1024  -- Conservative estimate
  
  BU.unsafeUseAsCStringLen input $ \(inputPtr, _) ->
    BU.unsafeUseAsCStringLen source $ \(sourcePtr, _) ->
      allocaBytes (fromIntegral maxOutputSize) $ \outputPtr ->
        alloca $ \outputSizePtr -> do
          poke outputSizePtr maxOutputSize
          
          result <- c_xd3_encode_memory
            (castPtr inputPtr)
            inputSize
            (castPtr sourcePtr) 
            sourceSize
            outputPtr
            outputSizePtr
            maxOutputSize
            0  -- no special flags
            
          if result == xd3_SUCCESS
            then do
              actualSize <- peek outputSizePtr
              output <- B.packCStringLen (castPtr outputPtr, fromIntegral actualSize)
              return $ Right output
            else return $ Left $ "xdelta3 encode error: " ++ show result

decodeMemory :: ByteString -> ByteString -> IO (Either String ByteString)
decodeMemory patch source = do
  let patchSize = fromIntegral $ B.length patch
      sourceSize = fromIntegral $ B.length source
      maxOutputSize = sourceSize + patchSize + 1024  -- Conservative estimate
      
  BU.unsafeUseAsCStringLen patch $ \(patchPtr, _) ->
    BU.unsafeUseAsCStringLen source $ \(sourcePtr, _) ->
      allocaBytes (fromIntegral maxOutputSize) $ \outputPtr ->
        alloca $ \outputSizePtr -> do
          poke outputSizePtr maxOutputSize
          
          result <- c_xd3_decode_memory
            (castPtr patchPtr)
            patchSize
            (castPtr sourcePtr)
            sourceSize
            outputPtr
            outputSizePtr
            maxOutputSize
            0  -- no special flags
            
          if result == xd3_SUCCESS
            then do
              actualSize <- peek outputSizePtr
              output <- B.packCStringLen (castPtr outputPtr, fromIntegral actualSize)
              return $ Right output
            else return $ Left $ "xdelta3 decode error: " ++ show result