{-# LANGUAGE
    ForeignFunctionInterface
  #-}
module Bloomgus.Hash
  ( Hashable (..)
  , hash
  , doubleHashSalt
  , doubleHash
  ) where

import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "lookup3.h hashword2" hashWord2
  :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
  :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

hashIO :: Ptr a    -- value to hash
       -> CSize    -- number of bytes
       -> Word64   -- salt
       -> IO Word64
hashIO ptr bytes salt =
    with (fromIntegral salt) $ \sp -> do
      let p1 = castPtr sp
          p2 = castPtr sp `plusPtr` 4
      go p1 p2
      peek sp
  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) numWords p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2
        numWords = bytes `div` 4

class Hashable a where
    hashSalt :: Word64        -- ^ salt
             -> a             -- ^ value to hash
             -> Word64

defaultHashingSalt :: Word64
defaultHashingSalt = 0x106fc397cf62f64d

defaultDoublingSalt :: Word64
defaultDoublingSalt = 0x9150a946c4a8966e

hash :: Hashable a => a -> Word64
hash = hashSalt defaultHashingSalt

doubleHashSalt :: Hashable a => Word64 -> Int -> a -> [Word32]
doubleHashSalt salt numHashes value = [h1 + h2 * i | i <- [0..num]]
    where h   = hashSalt salt value
          h1  = fromIntegral (h `shiftR` 32) .&. maxBound
          h2  = fromIntegral h
          num = fromIntegral numHashes

doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash = doubleHashSalt defaultDoublingSalt

-- Implementations for hashSalt

instance Hashable Char   where hashSalt = hashStorable
instance Hashable Int    where hashSalt = hashStorable
instance Hashable Double where hashSalt = hashStorable

hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr ->
                        hashIO ptr (fromIntegral (sizeOf k)) salt

-- Storable Lists
instance Storable a => Hashable [a] where
  hashSalt salt xs = unsafePerformIO $ hashList salt xs

hashList :: (Storable a) => Word64 -> [a] -> IO Word64
hashList salt xs = withArrayLen xs $ \len ptr ->
                     hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs

instance (Hashable a, Hashable b) => Hashable (a,b) where
  hashSalt salt (a,b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
  hashSalt salt (a,b,c) = hash2 c . hash2 b . hash2 a $ salt

hash2 :: (Hashable a) => a -> Word64 -> Word64
hash2 = flip hashSalt

-- ByteStrings

instance Hashable Strict.ByteString where
    hashSalt salt bs = unsafePerformIO $ hashByteString salt bs

instance Hashable Lazy.ByteString where
    hashSalt salt bs = unsafePerformIO $ foldM hashByteString salt (rechunk bs)

hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs = Strict.useAsCStringLen bs $ \(ptr, len) ->
                         hashIO ptr (fromIntegral len) salt

rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
    | Lazy.null s = []
    | otherwise   = let (pre,suf) = Lazy.splitAt chunkSize s
                    in  repack pre : rechunk suf
    where repack    = Strict.concat . Lazy.toChunks
          chunkSize = 64 * 1024
