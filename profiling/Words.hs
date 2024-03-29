module Main where

import Control.Parallel.Strategies (rdeepseq)
import Control.DeepSeq (NFData(..))
import Control.Monad (forM_, mapM_)
import qualified Bloomgus.Easy as B
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)

timed :: (NFData a) => String -> IO a -> IO a
timed desc act = do
    start <- getCurrentTime
    ret <- act
    end <- rdeepseq ret `seq` getCurrentTime
    putStrLn $ show (diffUTCTime end start) ++ " to " ++ desc
    return ret

instance NFData (B.Bloom a) where
    rnf filt = B.length filt `seq` ()

main :: IO ()
main = do
  args <- getArgs
  let files | null args = ["/usr/share/dict/words"]
            | otherwise = args
  forM_ files $ \file -> do

    words <- timed "read words" $
      BS.lines `fmap` BS.readFile file

    let len = length words
        errRate = 0.01

    putStrLn $ show len ++ " words"
    putStrLn $ "suggested sizings: " ++
               show (B.suggestSizing errRate (fromIntegral len))

    filt <- timed "construct filter" $
      case B.simpleFromList errRate words of
        Left errmsg -> do
          putStrLn $ "Error: " ++ errmsg
          exitFailure
        Right filt -> return filt

    timed "query every element" $
      mapM_ print $ filter (not . (`B.elem` filt)) words
