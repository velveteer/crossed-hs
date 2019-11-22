{-# LANGUAGE RecordWildCards            #-}

module Main where

import Options.Applicative
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

import Crossed (printGrid, run)

data Options =
  Options
    { visualize :: Bool
    , batchSize :: Int
    , gridSize  :: Int
    , minStart  :: Int
    , minWords  :: Int
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
      <> help "Control the available word pool. Taken from a random shuffle of ~283,000 words."
      <> showDefault
      <> value 300
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
      (  long "minWords"
      <> short 'w'
      <> help "Number of words to fill before returning a solution. Increases generation time."
      <> showDefault
      <> value 30
      <> metavar "INT"
      )
  <*> option auto
      (  long "checkLimit"
      <> short 'l'
      <> help "Number of word placement attempts made before returning a solution."
      <> showDefault
      <> value 100
      <> metavar "INT"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info (options <**> helper)
      (  fullDesc
      <> progDesc "Generate a crossword puzzle"
      )

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo
  lines <- BS.lines <$> BS.readFile "clues-desc.tsv"
  grid <- run visualize lines batchSize gridSize minStart minWords gas
  printGrid False gridSize grid
