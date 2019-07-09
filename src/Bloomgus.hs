{-# LANGUAGE
    BangPatterns
  #-}
module Bloomgus
    ( Bloomfilter
    , empty
    , add
    , contains
    ) where

newtype Bloomfilter a = B [a]

empty :: Bloomfilter a
empty = B []

add :: a -> Bloomfilter a -> Bloomfilter a
add !a (B as) = B (a:as)

contains :: Eq a => Bloomfilter a -> a -> Bool
contains (B as) a = a `elem` as
