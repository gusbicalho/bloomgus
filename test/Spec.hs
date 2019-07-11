{-# LANGUAGE
    TypeApplications
  #-}
{- HLINT ignore "Redundant do" -}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Word (Word8, Word32)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

import Bloomgus.Hash (Hashable)
import qualified Bloomgus.Easy as B

import Helpers

prop_all_present :: Hashable a => [a] -> Property
prop_all_present xs =
  forAll errorRate $ \errRate ->
    B.simpleFromList errRate xs =~> \filt ->
      all (`B.elem` filt) xs

prop_suggest_sane :: Property
prop_suggest_sane =
    forAll errorRate $ \errRate ->
      forAll (choose (1,fromIntegral maxWord32 `div` 8)) $ \cap ->
        let bestSize = fst . minimum $ B.sizings cap errRate
        in bestSize < fromIntegral maxWord32 ==>
           either (const False) sane $ B.suggestSizing errRate cap
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32 = maxBound :: Word32

main :: IO ()
main = hspec $ do
  describe "All elements in the input for a bloomfilter are present in the bloomfilter" $
    modifyMaxSuccess (const 1000) $ do
      prop "Int" $ prop_all_present @Int
      prop "Char" $ prop_all_present @Char
      prop "Word8" $ prop_all_present @Word8
      prop "Double" $ prop_all_present @Double
      prop "Strict ByteString" $ prop_all_present @Strict.ByteString
      prop "Lazy ByteString" $ prop_all_present @Lazy.ByteString
  modifyMaxSuccess (const 1000) $ prop "Size suggestions are always sane" prop_suggest_sane
