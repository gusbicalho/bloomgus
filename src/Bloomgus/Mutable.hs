{-# LANGUAGE
    BlockArguments
  #-}
module Bloomgus.Mutable
  ( MutBloom
  , elem
  , notElem
  , insert
  , length
  , new
  ) where

import Bloomgus.Internal

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

new :: Hasher a -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash <$> newArray (0,numBits-1) False

length :: MutBloom s a -> ST s Word32
length filt = (succ . snd) <$> getBounds (mutArray filt)

bitIndices :: MutBloom s a -> a -> ST s [Word32]
bitIndices filt e = hashModulus <$> length filt
  where hashModulus modulus = map (`mod` modulus) (mutHash filt e)

insert :: MutBloom s a -> a -> ST s ()
insert filt e = do
  bits <- bitIndices filt e
  forM_ bits \bit ->
    writeArray (mutArray filt) bit True

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _     []     = return True
allM mPred (a:as) = do
  ok <- mPred a
  if ok then return True else allM mPred as

elem :: MutBloom s a -> a -> ST s Bool
elem filt e = do
  bits <- bitIndices filt e
  allM (readArray (mutArray filt)) bits

notElem :: MutBloom s a -> a -> ST s Bool
notElem filt e = not <$> elem filt e
