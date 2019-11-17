{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Word)

import Control.Applicative (ZipList(..))
import Control.Monad (MonadPlus(..), guard, replicateM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logic
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

buildGWord :: (Int, Int) -> Direction -> Line -> GWord
buildGWord (x,y) d line =
  let (w, c) = ((,) <$> word <*> clue) line
  in GWord x y (BS.length w) d w c

placeInitialWord :: MonadPlus m => [Line] -> TemplateMap -> Int -> m (GWord, Grid)
placeInitialWord batch tmap size = do
  gword <- msum [pure $ buildGWord (0,0) Across x | x <- take 100 batch]
  guard $ canPlaceWord size mempty gword
  pure (gword, gword:mempty)

canPlaceWord :: Int -> Grid -> GWord -> Bool
canPlaceWord size g gw =
  let gwcs = getCells gw
      gcs  = getGrid size g
      maxX = cx $ List.maximumBy (comparing cx) gcs
      maxY = cy $ List.maximumBy (comparing cy) gcs
  in
  all (\gwc -> cx gwc <= maxX && cy gwc <= maxY) gwcs

getNextWords :: Int -> Grid -> TemplateMap -> GWord -> IO [(Word, Clue)]
getNextWords size g tmap gw = do
  let tmps = getTemplates size g gw
  words <- matchAllTemplates tmap tmps
  pure $ List.sortOn (BS.length . fst) words

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
  let size           = 4
      tmap           = linesToMap batch
      (active, grid) = observe $ placeInitialWord batch tmap size
  printGrid size grid
  putStrLn "Current words: "
  printClues grid
  putStrLn "Candidates for next word: "
  print =<< getNextWords size grid tmap active
