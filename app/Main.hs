module Main where

import System.Environment
import Data.List (foldl')
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Bloomgus as Bl

loadBloom :: FilePath -> IO (Bl.Bloomfilter L8.ByteString)
loadBloom path = fold <$> lines
  where fold = foldl' (flip Bl.add) Bl.empty
        lines = L8.lines <$> L8.readFile path

main :: IO ()
main = do
    (pathArg : testArgs) <- getArgs
    bloom <- loadBloom pathArg
    mapM_ putStrLn . map (testResult bloom) $ testArgs
  where
    testResult bloom test = test ++ "\t" ++ show (Bl.contains bloom (L8.pack test))
