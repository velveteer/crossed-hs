{-# LANGUAGE OverloadedStrings #-}

module Crossed
  ( printGrid
  , run
  ) where

import Prelude hiding (Word)

import Control.Applicative (ZipList(..))
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logic
import Control.Monad.Random (evalRandIO, uniform)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (for)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  Cell { cc :: !Char
       , cx :: !Int
       , cy :: !Int
       } deriving (Eq, Show)

type Grid = [GWord]

buildGWord :: (Int, Int) -> Direction -> (Word, Clue) -> GWord
buildGWord (x,y) d (w, c) = GWord x y (BS.length w) d w c

generateGrid :: (MonadIO m, MonadPlus m) => [Line] -> TemplateMap -> Int -> Int -> Int -> m Grid
generateGrid batch tmap size minStart maxWords = do
  (gword, grid) <- placeInitialWord batch minStart
  let loop gw g k = do
        (gword', grid') <- placeNextWord size g tmap gw
        if k == 0
        then pure grid'
        else loop gword' grid' (k - 1)
  loop gword grid (maxWords - 2)

placeInitialWord :: (MonadIO m, MonadPlus m) => [Line] -> Int -> m (GWord, Grid)
placeInitialWord batch minStart = do
  gword <- msum [pure $ buildGWord (0,0) Across (word x, clue x) | x <- batch]
  guard $ l gword >= minStart
  pure (gword, gword:mempty)

placeNextWord :: (MonadIO m, MonadPlus m) => Int -> Grid -> TemplateMap -> GWord -> m (GWord, Grid)
placeNextWord size grid tmap active = do
  possibleWords <- liftIO $ getNextWords size grid tmap active
  nextWord <- msum [pure x | x <- possibleWords]
  guard $ canPlaceWord size grid nextWord
  pure (nextWord, nextWord:grid)

canPlaceWord :: Int -> Grid -> GWord -> Bool
canPlaceWord size g gw =
  let gwcs = getCells gw
      gcs  = getGrid size g
      xys = fmap ((,) <$> cx <*> cy )
      acrossWords = filter (\gw -> d gw == Across) g
      downWords = filter (\gw -> d gw == Down) g
      acrossCells = xys $ getCells `concatMap` acrossWords
      downCells = xys $ getCells `concatMap` downWords
      gridCells = getCells `concatMap` g
      starts = head . xys . getCells <$> g
      lasts = last . xys . getCells <$> g
      maxX = cx $ List.maximumBy (comparing cx) gcs
      maxY = cy $ List.maximumBy (comparing cy) gcs
      overlapsX = any (`elem` acrossCells) (xys gwcs)
      overlapsY = any (`elem` downCells) (xys gwcs)
      allTheSame xs = null xs || all (== head xs) (tail xs)
      intersectsMatch =
        allTheSame $ fmap cc
          <$> List.group (List.intersectBy
              (\a b -> cx a == cx b && cy a == cy b)
              gridCells gwcs)
  in
     w gw `notElem` (w <$> g)
     && all (\gwc -> cx gwc <= maxX && cy gwc <= maxY) gwcs
     && (if d gw == Across then not overlapsX else not overlapsY)
     && intersectsMatch
     && if d gw == Down
        then all (\c -> (cx c + 1, cy c - 1) `notElem` downCells) gwcs
             && all (\c -> (cx c - 1, cy c + 1) `notElem` downCells) gwcs
             && all (\c -> (cx c, cy c - 1) `notElem` downCells) gwcs
             && all (\c -> (cx c, cy c + 1) `notElem` downCells) gwcs
             && all (\c -> (cx c + 1, cy c) `notElem` starts) gwcs
             && all (\c -> (cx c - 1, cy c) `notElem` lasts) gwcs
             && all (\c -> (cx c, cy c + 1) `notElem` lasts) gwcs
             && (x gw, y gw - 1) `notElem` acrossCells
             && (x gw, y gw + 1) `notElem` acrossCells
             && (x gw, y gw + l gw) `notElem` acrossCells
        else all (\c -> (cx c - 1, cy c + 1) `notElem` acrossCells) gwcs
             && all (\c -> (cx c + 1, cy c - 1) `notElem` acrossCells) gwcs
             && all (\c -> (cx c - 1, cy c) `notElem` acrossCells) gwcs
             && all (\c -> (cx c + 1, cy c) `notElem` acrossCells) gwcs
             && all (\c -> (cx c, cy c + 1) `notElem` starts) gwcs
             && all (\c -> (cx c, cy c - 1) `notElem` lasts) gwcs
             && all (\c -> (cx c, cy c + 1) `notElem` lasts) gwcs
             && (x gw - 1, y gw) `notElem` downCells
             && (x gw + 1, y gw) `notElem` downCells
             && (x gw + l gw, y gw) `notElem` downCells

getNextWords :: (MonadPlus m, MonadIO m) => Int -> Grid -> TemplateMap -> GWord -> m [GWord]
getNextWords size g tmap gw = do
  let end = if d gw == Across then y gw else x gw
      plcs = concat $ getPlacements <$> [0..end] <*> [size] <*> [g] <*> [gw]
  matchAllTemplates tmap gw plcs

matchTemplate :: TemplateMap -> Template -> [(Word, Clue)]
matchTemplate tmap tmp = fromMaybe mempty $ Map.lookup tmp tmap

matchAllTemplates :: (MonadPlus m, MonadIO m) => TemplateMap -> GWord -> [Placement] -> m [GWord]
matchAllTemplates tmap gword plcs = fmap catMaybes $ for plcs $ \plc -> do
  let candidates = concatMap (matchTemplate tmap) (templates plc)
      cds =   Set.toList
            . Set.fromList
            $ concat
            $ filter (not . null)
            $ List.groupBy (\a b -> fst a == fst b) candidates
  if null cds
  then pure Nothing
  else do
    word <- liftIO . evalRandIO $ uniform cds
    pure $
      Just $ buildGWord
      (startX plc, startY plc)
      (d $ switchDirection gword)
      word

switchDirection :: GWord -> GWord
switchDirection gword = gword { d = if d gword == Across then Down else Across }

getCells :: GWord -> [Cell]
getCells gw =
  case d gw of
    Across ->
      getZipList $
        Cell <$> ZipList (BS.unpack (w gw))
             <*> ZipList [x gw..]
             <*> ZipList (repeat $ y gw)
    Down ->
      getZipList $
        Cell <$> ZipList (BS.unpack (w gw))
             <*> ZipList (repeat $ x gw)
             <*> ZipList [y gw..]

getGrid :: Int -> Grid -> [Cell]
getGrid size grid =
    List.unionBy (\a b -> cx a == cx b && cy a == cy b)
    (concatMap getCells grid)
    (Cell <$> [' '] <*> [0..size] <*> [0..size])

data Placement =
  Placement
    { startX    :: !Int
    , startY    :: !Int
    , direction :: !Direction
    , templates :: [Template]
    } deriving (Eq, Show)

getPlacements :: Int -> Int -> Grid -> GWord -> [Placement]
getPlacements start size grid gw =
  let gcs = getGrid size grid
      acrossWords = filter (\c -> d c == Across) grid
      downWords = filter (\c -> d c == Down) grid
      acrossCells = getCells `concatMap` acrossWords
      downCells = getCells `concatMap` downWords
  in
  case d gw of
    Across -> concat $ flip fmap acrossCells $ \cell ->
      getZipList $ Placement
        <$> ZipList [cx cell]
        <*> ZipList [start..]
        <*> ZipList [Down]
        <*> ZipList [(,) <$> (flip (-) start . cy) <*> cc <$> filter (\c -> cx c == cx cell && cy c >= start) gcs]
    Down -> concat $ flip fmap downCells $ \cell ->
      getZipList $ Placement
        <$> ZipList [start..]
        <*> ZipList [cy cell]
        <*> ZipList [Across]
        <*> ZipList [(,) <$> (flip (-) start . cx) <*> cc <$> filter (\c -> cy c == cy cell && cx c >= start) gcs]

printGrid :: Int -> Grid -> IO ()
printGrid size grid = do
  for_ [0..size] $ \i -> do
    for_ [0..size] $ \j -> do
      let mChar = cc <$> List.find
                  (\c -> cx c == j && cy c == i)
                  (concatMap getCells grid)
      case mChar of
        Nothing ->
          putStr $ "  " ++ " " ++ "  "
        Just c ->
          putStr $ "  " ++ [c] ++ "  "
    putStrLn "\n"
  putStrLn "\n"
  printClues grid

printClues :: Grid -> IO ()
printClues grid = do
  let across = zip [1..] (filter ((==) Across . d) grid)
      down = zip [1..] (filter ((==) Down . d) grid)
  for_ across $ \(num, gword) -> do
    putStrLn "Across"
    putStrLn $ show (num :: Int) ++ ". " ++ show (w gword) ++ ": " ++ show (c gword)
  for_ down $ \(num, gword) -> do
    putStrLn "Down"
    putStrLn $ show (num :: Int) ++ ". " ++ show (w gword) ++ ": " ++ show (c gword)

run :: [Line] -> Int -> Int -> Int -> Int -> IO Grid
run lines batchSize gridSize minStart maxWords = do
  batch <- List.sortBy (flip compare) . take batchSize <$> Random.shuffleM lines
  let tmap = linesToMap batch
  observeT $ generateGrid batch tmap gridSize minStart maxWords
