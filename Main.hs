module Main where

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
      gas = read $ args !! 4
      visualize = read $ args !! 5
  lines <- BS.lines <$> BS.readFile "clues-desc.tsv"
  grid <- run visualize lines batchSize gridSize minStart maxWords gas
  printGrid False gridSize grid
