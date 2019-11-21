module Main where

import Control.Concurrent.Async (async, waitAny)
import Control.Monad (replicateM)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

import Crossed (printGrid, run)

main :: IO ()
main = do
  args <- getArgs
  let batchSize = read $ head args
      gridSize = read $ args !! 1
      minStart = read $ args !! 2
      maxWords = read $ args !! 3
  lines <- BS.lines <$> BS.readFile "clues-desc.tsv"
  runs <- replicateM 4 $ async (run lines batchSize gridSize minStart maxWords)
  (_, grid) <- waitAny runs
  printGrid gridSize grid
