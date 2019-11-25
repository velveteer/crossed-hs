{-# LANGUAGE RecordWildCards            #-}

module Main where

import Options.Applicative
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

import Crossed (printClues, printGrid, run)

data Options =
  Options
    { visualize :: Bool
    , batchSize :: Int
    , gridSize  :: Int
    , minStart  :: Int
    , words     :: Int
    , gas       :: Int
    }

options :: Parser Options
options = Options
  <$> switch
      (  long "visualize"
      <> short 'v'
      <> help "Print intermediate grids during generation."
      )
  <*> option auto
      (  long "batchSize"
      <> short 'b'
      <> help "Control the available word pool. Taken from a random shuffle of ~283,000 words. Increases generation time."
      <> showDefault
      <> value 1000
      <> metavar "INT"
      )
  <*> option auto
      (  long "gridSize"
      <> short 'g'
      <> help "Length and width of the grid."
      <> showDefault
      <> value 15
      <> metavar "INT"
      )
  <*> option auto
      (  long "minStart"
      <> short 's'
      <> help "Minimum length for starting word."
      <> showDefault
      <> value 5
      <> metavar "INT"
      )
  <*> option auto
      (  long "words"
      <> short 'w'
      <> help "Number of words needed for a solution. Increases generation time."
      <> showDefault
      <> value 40
      <> metavar "INT"
      )
  <*> option auto
      (  long "checkLimit"
      <> short 'l'
      <> help "Number of word placement attempts made before returning a solution."
      <> showDefault
      <> value 200
      <> metavar "INT"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info (options <**> helper)
      (  fullDesc
      <> progDesc "Generate a sparse crossword puzzle, i.e. given a list of dictionary words and their clues, find a populated grid that solves for the given constraints."
      )

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo
  lines <- BS.lines <$> BS.readFile "clues-desc.tsv"
  grid <- run visualize lines batchSize gridSize minStart words gas
  printGrid gridSize grid
  printClues grid
