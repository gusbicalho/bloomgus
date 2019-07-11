module Bloomgus.Immutable
  ( Bloom
  , Hasher
  , length
  , elem
  , notElem
  , fromList
  ) where

import Bloomgus.Internal
import Bloomgus.Mutable (insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

fromList :: Hasher a -> Word32 -> [a] -> Bloom a
fromList hasher numBits es = B hasher $ runSTUArray $ do
  mb <- new hasher numBits
  mapM_ (insert mb) es
  return (mutArray mb)

bitIndices :: Bloom a -> a -> [Word32]
bitIndices filt e = map (`mod` modulus) (blmHash filt e)
  where modulus = length' filt

elem :: a -> Bloom a -> Bool
elem e filt = all (blmArray filt !) (bitIndices filt e)

notElem :: a -> Bloom a -> Bool
notElem e filt = not $ e `elem` filt

length :: Bloom a -> Int
length = fromIntegral . length'

length' :: Bloom a -> Word32
length' = succ . snd . bounds . blmArray
