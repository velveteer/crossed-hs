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
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
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
  Cell { cc :: !Char
       , cx :: !Int
       , cy :: !Int
       } deriving (Eq, Show)

type Grid = [GWord]

buildGWord :: (Int, Int) -> Direction -> (Word, Clue) -> GWord
buildGWord (x,y) d (w, c) = GWord x y (BS.length w) d w c

placeInitialWord :: MonadPlus m => [Line] -> Int -> m (GWord, Grid)
placeInitialWord batch size = do
  gword <- msum [pure $ buildGWord (0,0) Across (word x, clue x) | x <- take 100 batch]
  guard $ l gword >= 5
  guard $ canPlaceWord size mempty gword
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
      maxX = cx $ List.maximumBy (comparing cx) gcs
      maxY = cy $ List.maximumBy (comparing cy) gcs
  in
  all (\gwc -> cx gwc <= maxX && cy gwc <= maxY) gwcs

getNextWords :: Int -> Grid -> TemplateMap -> GWord -> IO [GWord]
getNextWords size g tmap gw = do
  let plcs = getPlacements 0 size g gw
  gwords <- matchAllTemplates tmap gw plcs
  pure $ List.sortOn l gwords

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
    { startX :: !Int
    , startY :: !Int
    , templates :: [Template]
    } deriving (Eq, Show)

-- TODO Reject starts greater than the word position
getPlacements :: Int -> Int -> Grid -> GWord -> [Placement]
getPlacements start size grid gw =
  let gcs = getGrid size grid
      gwcs = getCells gw
  in
  case d gw of
    Across -> concat $ flip fmap gwcs $ \cell ->
      getZipList $ Placement
        <$> ZipList [cx cell]
        <*> ZipList [start..]
        <*> ZipList [(,) <$> (flip (-) start . cy) <*> cc <$> filter (\c -> cx c == cx cell && cy c >= start) gcs]
    Down -> concat $ flip fmap gwcs $ \cell ->
      getZipList $ Placement
        <$> ZipList [start..]
        <*> ZipList [cy cell]
        <*> ZipList [(,) <$> (flip (-) start .cx) <*> cc <$> filter (\c -> cy c == cy cell && cx c >= start) gcs]

matchTemplate :: TemplateMap -> Template -> [(Word, Clue)]
matchTemplate tmap tmp = fromMaybe mempty $ Map.lookup tmp tmap

matchAllTemplates :: TemplateMap -> GWord -> [Placement] -> IO [GWord]
matchAllTemplates tmap gword plcs = for plcs $ \plc -> do
  let candidates = concatMap (matchTemplate tmap) (templates plc)
  let cds = concat $ filter (not . null) $ List.groupBy (\a b -> fst a == fst b) candidates
  word <- evalRandIO $ uniform cds
  pure $ buildGWord (startX plc, startY plc) (d $ switchDirection gword) word

switchDirection :: GWord -> GWord
switchDirection gword = gword { d = if d gword == Across then Down else Across }

printGrid :: Int -> Grid -> IO ()
printGrid size grid =
  for_ [0..size] $ \i -> do
    for_ [0..size] $ \j -> do
      let mChar = cc <$> List.find
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
  let size           = 6
      tmap           = linesToMap batch
  (active, grid) <- observeT $ placeInitialWord batch size
  (active', grid') <- observeT $ placeNextWord size grid tmap active
  printGrid size grid'
  putStrLn "Current words: "
  printClues grid'
  putStrLn "Candidates for next word: "
  print =<< getNextWords size grid' tmap active'
