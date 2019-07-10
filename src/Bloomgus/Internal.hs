{-# LANGUAGE
    BangPatterns
  #-}
module Bloomgus.Internal where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

type Hasher a = a -> [Word32]

data Bloom a = B { blmHash  :: (a -> [Word32])
                 , blmArray :: UArray Word32 Bool
                 }

data MutBloom s a = MB { mutHash :: (a -> [Word32])
                       , mutArray :: STUArray s Word32 Bool
                       }
