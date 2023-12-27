{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module MyLib.Fibonacci where

import Foreign (Ptr, Storable (poke))
import Foreign.C.Types (CDouble, CInt (..), CULLong)

fib :: (Integral a) => [a]
fib = 0 : 1 : zipWith (+) fib (tail fib)

foreign export capi fibonacci :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt

fibonacci :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt
fibonacci n intPtr dblPtr
  | badInt && badDouble = return 2
  | badInt = do
      poke dblPtr dbl_result
      return 1
  | otherwise = do
      poke intPtr (fromIntegral result)
      poke dblPtr dbl_result
      return 0
  where
    result = fib !! fromIntegral n
    dbl_result = realToFrac result
    badInt = result > toInteger (maxBound :: CULLong)
    badDouble = isInfinite dbl_result
