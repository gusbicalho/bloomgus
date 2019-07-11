module Bloomgus.Easy
  ( Bloom
  , Hasher
  , length
  , elem
  , notElem
  , fromList
  , simpleFromList
  , suggestSizing
  , doubleHash
  ) where

import Bloomgus.Internal
import Bloomgus.Hash
import Data.Word (Word32)
import Data.List (genericLength)
import Data.Maybe (mapMaybe)
import Bloomgus.Immutable
import Prelude hiding (length, elem, notElem)

simpleFromList :: Hashable a => Double -> [a] -> Either String (Bloom a)
simpleFromList errorRate es = do 
  (numBits, numHashes) <- suggestSizing errorRate (genericLength es)
  return $ fromList (doubleHash numHashes) numBits es

suggestSizing :: Double -> Integer -> Either String (Word32, Int)
suggestSizing errRate capacity
    | capacity <= 0                = Left "capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes               = Left "capacity too large"
    | otherwise                    = Right (minimum saneSizes)
  where saneSizes = mapMaybe sanitize $ sizings capacity errRate
        sanitize (bits,hashes)
          | bits > maxWord32 - 1 = Nothing
          | otherwise            = Just (ceiling bits, truncate hashes)
          where maxWord32 = fromIntegral (maxBound :: Word32)

sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
    [((-k) * cap / log (1 - (errRate ** (1 / k))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
