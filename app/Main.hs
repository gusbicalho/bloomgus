{-# LANGUAGE
    BangPatterns
  #-}
module Main where

import System.Environment
import System.Posix
import System.IO (stdin)
import Data.List (foldl')
import Data.Either (fromRight)
import Data.Word (Word32)
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Bloomgus.Easy as Bl

getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path

getLines :: FilePath -> IO [L8.ByteString]
getLines path = L8.lines <$> contents path
  where contents "-"   = L8.hGetContents stdin
        contents path' = L8.readFile path'

sumAndCount :: (Foldable t, Num a, Num b) => t a -> (a, b)
sumAndCount = foldl' acc (0, 0)
  where acc (!total, !count) x = (total + x, count + 1)

mean :: [Double] -> Double
mean [] = 0
mean xss = uncurry (/) . sumAndCount $ xss

estimateCapacity :: FilePath -> [L8.ByteString] -> IO Integer
estimateCapacity path ls = (`div` estimatedLineSize) . fromIntegral <$> getFileSize path
  where
    firstFew = take 10
    estimatedLineSize :: Integer
    estimatedLineSize = round . mean . map (fromIntegral . L8.length) . firstFew $ ls

fallbackSizing :: (Word32, Int)
fallbackSizing = (0x10000, 10)

loadBloom :: FilePath -> IO (Bl.Bloom L8.ByteString)
loadBloom path = do
    ls <- getLines path
    capacity <- estimateCapacity path ls
    let (numBits, numHashes) = sizing 0.005 capacity
    return $ Bl.fromList (Bl.doubleHash numHashes) numBits ls
  where sizing errRate capacity = fromRight fallbackSizing $ Bl.suggestSizing errRate capacity

testAll :: Bl.Bloom L8.ByteString -> [L8.ByteString] -> [(L8.ByteString, Bool)]
testAll bloom = map testResult
  where testResult test = (test, test `Bl.elem` bloom)

run :: FilePath -> FilePath -> IO ()
run filterPath testPath = do
    bloom <- loadBloom filterPath
    ls <- getLines testPath
    mapM_ printResult $ testAll bloom ls
  where printResult (test, result) = putStrLn $ L8.unpack test ++ "\t" ++ show result

main :: IO ()
main = do
  (filterPath : testPath : _) <- getArgs
  run filterPath testPath
