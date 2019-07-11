{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helpers where

import Test.QuickCheck

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

checkTimes :: Testable a => Int -> a -> IO ()
checkTimes limit = quickCheckWith stdArgs { maxSuccess = limit }

errorRate :: Gen Double
errorRate = choose (epsilon, 1 - epsilon)
  where epsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

instance Arbitrary Lazy.ByteString where
    arbitrary = Lazy.pack `fmap` arbitrary

instance Arbitrary Strict.ByteString where
    arbitrary = Strict.pack `fmap` arbitrary
