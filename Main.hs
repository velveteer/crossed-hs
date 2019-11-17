{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)

import Control.Applicative (ZipList(..))
import Control.Monad (guard, replicateM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (RandT, evalRandIO, uniform)
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Traversable (for)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random as Random
import qualified System.Random.Shuffle as Random

type Line = BS.ByteString
type Word = BS.ByteString
type Clue = BS.ByteString

word :: Line -> Word
word = (!!1) . BS.split '\t'

clue :: Line -> Clue
clue = (!!0) . BS.split '\t'

type Template = (Int, Char)
type TemplateMap = Map Template [(Word, Clue)]

linesToMap :: [Line] -> TemplateMap
linesToMap bss = Map.unionsWith (++) $ toMap <$> bss
  where mapping bs str = zip (zip [0..] str) (repeat [(word bs, clue bs)])
        toMap bs = Map.fromList . mapping bs . BS.unpack $ word bs

data Direction = Across | Down deriving (Eq, Show)

data GWord =
  GWord { x :: !Int
        , y :: !Int
        , l :: !Int
        , d :: !Direction
        , w :: !BS.ByteString
        , c :: !BS.ByteString
        } deriving (Eq, Show)

data Cell =
  Cell { cc :: !(Maybe Char)
       , cx :: !Int
       , cy :: !Int
       } deriving (Eq, Show)

type Grid = [GWord]

-- TODO Validation and backtracking
placeWord :: Grid -> GWord -> Grid
placeWord grid gword = gword : grid

getCells :: GWord -> [Cell]
getCells gw =
  case d gw of
    Across ->
      getZipList $
        Cell <$> ZipList (Just <$> BS.unpack (w gw))
             <*> ZipList [x gw..]
             <*> ZipList (repeat $ y gw)
    Down ->
      getZipList $
        Cell <$> ZipList (Just <$> BS.unpack (w gw))
             <*> ZipList (repeat $ x gw)
             <*> ZipList [y gw..]

getGrid :: Int -> Grid -> [Cell]
getGrid size grid =
    List.unionBy (\a b -> cx a == cx b && cy a == cy b)
    (concatMap getCells grid)
    (Cell <$> [Nothing] <*> [0..size] <*> [0..size])

getTemplates :: Int -> Grid -> GWord -> [[Template]]
getTemplates size grid gw =
  let gcs = getGrid size grid
      gwcs = getCells gw
  in
  case d gw of
    Across -> removeInvalid $ flip fmap gwcs $ \cell ->
      fmap ((,) <$> cy <*> cc)  (filter (\c -> cx c == cx cell) gcs)
    Down -> removeInvalid $ flip fmap gwcs $ \cell ->
      fmap ((,) <$> cx <*> cc)  (filter (\c -> cy c == cy cell) gcs)

removeInvalid :: [[(Int, Maybe Char)]] -> [[Template]]
removeInvalid tmps = fmap concat $ fmap clean <$> tmps
  where
    clean tmp =
      case snd tmp of
        Just n  -> pure (fst tmp, n)
        Nothing -> mempty

matchTemplate :: TemplateMap -> Template -> [(Word, Clue)]
matchTemplate tmap tmp = fromMaybe mempty $ Map.lookup tmp tmap

matchAllTemplates :: TemplateMap -> [[Template]] -> IO [(Word, Clue)]
matchAllTemplates tmap tmps = do
  let candidates = fmap (matchTemplate tmap) <$> tmps
  let cds = filter (not . null) $ fmap (List.foldl1 List.intersect) candidates
  evalRandIO $ traverse uniform cds

getNextWord :: Int -> Grid -> TemplateMap -> GWord -> IO (Word, Clue)
getNextWord size g tmap gw = do
  let tmps = getTemplates size g gw
  words <- matchAllTemplates tmap tmps
  pure $ List.maximumBy (comparing (BS.length . fst)) words

printGrid :: Int -> Grid -> IO ()
printGrid size grid =
  for_ [0..size] $ \i -> do
    for_ [0..size] $ \j -> do
      let mChar = cc =<< List.find
                  (\c -> cx c == j && cy c == i)
                  (concatMap getCells grid)
      case mChar of
        Nothing ->
          putStr $ "  " ++ "#" ++ "  "
        Just c ->
          putStr $ "  " ++ [c] ++ "  "
    putStrLn "\n"

printClues :: Grid -> IO ()
printClues grid =
  for_ grid $ \gword ->
    putStrLn $ show (w gword) ++ ": " ++ show (c gword)

main :: IO ()
main = do
  lines <- BS.lines <$> BS.readFile "clues-desc.tsv"
  batch <- take 10000 <$> Random.shuffleM lines
  gen <- Random.newStdGen
  let total       = length batch
      (i, gen')   = Random.randomR (0, total) gen
      size        = 10
      (w, c)      = (,) <$> word <*> clue $ (batch !! i)
      firstGWord  = GWord 0 0 (BS.length w) Across w c
      tmap        = linesToMap batch
      grid        = placeWord mempty firstGWord
  printGrid size grid
  putStrLn "Current words: "
  printClues grid
  putStrLn "Candidate for next word: "
  print =<< getNextWord size grid tmap (last grid)
