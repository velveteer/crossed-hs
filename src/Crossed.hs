{-# LANGUAGE OverloadedStrings          #-}

module Crossed
  ( printClues
  , printGrid
  , run
  ) where

import Prelude hiding (Word)

import Control.Applicative (ZipList(..))
import Control.Monad.Logic
import Control.Monad.Random (getRandomR, evalRandIO, uniform)
import Data.Foldable (for_)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (for)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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

makeTemplateMap :: [Line] -> TemplateMap
makeTemplateMap bss = Map.unionsWith (++) $ toMap <$> bss
  where mapping bs str = zip (zip [0..] str) (repeat [(word bs, clue bs)])
        toMap bs = Map.fromList . mapping bs . BS.unpack $ word bs

data Direction = Across | Down deriving (Eq, Show)

data GWord =
  GWord { x   :: !Int
        , y   :: !Int
        , l   :: !Int
        , d   :: !Direction
        , w   :: !BS.ByteString
        , c   :: !BS.ByteString
        , bs  :: ![(Int, Int)]
        , num :: !Int
        } deriving (Eq, Show)

data Cell =
  Cell { cc :: !Char
       , cx :: !Int
       , cy :: !Int
       } deriving (Eq, Show)

type Grid = [GWord]

xys :: [Cell] -> [(Int, Int)]
xys = fmap ((,) <$> cx <*> cy )

buildGWord :: (Int, Int) -> Direction -> (Word, Clue) -> GWord
buildGWord (x,y) d (w, c) =
  let l = BS.length w
      bs = if d == Down
           then [(x, y - 1), (x, y + l)]
           else [(x - 1, y), (x + l, y)]
  in
  GWord x y l d w c bs 0

generateGrid :: (MonadIO m, MonadPlus m) => Bool -> [Line] -> TemplateMap -> Int -> Int -> Int -> Int -> m Grid
generateGrid v batch tmap size minStart minWords gas = do
  (gword, grid) <- placeInitialWord size batch minStart
  asRef <- liftIO $ newIORef (0 :: Int)
  let loop gw g = do
        as <- liftIO $ readIORef asRef
        if as > gas || (length g == minWords)
        then pure g
        else do
          (gword', grid') <- placeNextWord v size g tmap gw asRef
          loop gword' grid'
  annotateGrid <$> loop gword grid

placeInitialWord :: (MonadIO m, MonadPlus m) => Int -> [Line] -> Int -> m (GWord, Grid)
placeInitialWord size batch minStart = do
  ix <- liftIO . evalRandIO $ getRandomR (0, size `div` 2)
  iy <- liftIO . evalRandIO $ getRandomR (0, size `div` 2)
  gword <- msum [pure $ buildGWord (ix,iy) Across (word x, clue x) | x <- batch]
  guard $ l gword >= minStart
  pure (gword, gword:mempty)

placeNextWord :: (MonadIO m, MonadPlus m)
              => Bool
              -> Int
              -> Grid
              -> TemplateMap
              -> GWord
              -> IORef Int
              -> m (GWord, Grid)
placeNextWord viz size grid tmap active asRef = do
  possibleWords <- liftIO $ getNextWords size grid tmap active
  nextWord <- msum [pure x | x <- possibleWords]
  liftIO $ modifyIORef' asRef (+1)
  guard $ canPlaceWord size grid nextWord
  when viz $ liftIO $ printGrid size (nextWord:grid)
  when viz $ liftIO $ putStrLn "\n"
  when viz $ liftIO $ putStrLn (concat $ replicate (size * 2) " = ")
  when viz $ liftIO $ putStrLn "\n"
  pure (nextWord, nextWord:grid)

canPlaceWord :: Int -> Grid -> GWord -> Bool
canPlaceWord size g gw =
  let gwcs = getCells gw
      gwxys = xys gwcs
      gcs  = getGrid size g
      blocks = bs `concatMap` g
      acrossWords = filter (\gw -> d gw == Across) g
      downWords = filter (\gw -> d gw == Down) g
      acrossXys = xys $ getCells `concatMap` acrossWords
      downXys = xys $ getCells `concatMap` downWords
      gridCells = getCells `concatMap` g
      maxX = cx $ List.maximumBy (comparing cx) gcs
      maxY = cy $ List.maximumBy (comparing cy) gcs
      intersectsMatch =
        List.intersectBy (\a b -> cx a == cx b && cy a == cy b) gridCells gwcs
        ==
        List.intersectBy (\a b -> cx a == cx b && cy a == cy b && cc a == cc b) gridCells gwcs
      isWithinBounds = all (\gwc -> cx gwc <= maxX && cy gwc <= maxY) gwcs
      isUnique = w gw `notElem` (w <$> g)
      isNotBlocked = all (`notElem` blocks) gwxys && all (`notElem` xys gridCells) (bs gw)
      isNotAdjacent =
        if d gw == Down
        then
          all (\(gwx, gwy) -> (gwx + 1, gwy) `notElem` downXys) gwxys &&
          all (\(gwx, gwy) -> (gwx - 1, gwy) `notElem` downXys) gwxys
        else
          all (\(gwx, gwy) -> (gwx, gwy + 1) `notElem` acrossXys) gwxys &&
          all (\(gwx, gwy) -> (gwx, gwy - 1) `notElem` acrossXys) gwxys
  in
        isWithinBounds
     && isUnique
     && intersectsMatch
     && isNotBlocked
     && isNotAdjacent
{-# INLINE canPlaceWord #-}

getNextWords :: (MonadPlus m, MonadIO m) => Int -> Grid -> TemplateMap -> GWord -> m [GWord]
getNextWords size g tmap gw = do
  let end = if d gw == Across then y gw else x gw
      plcs = prunePlacement <$> concat (getPlacements <$> [0..end] <*> [size] <*> [g] <*> [gw])
  matchAllTemplates size tmap gw plcs
{-# INLINE getNextWords #-}

matchTemplate :: TemplateMap -> Template -> [(Word, Clue)]
matchTemplate tmap tmp = fromMaybe mempty $ Map.lookup tmp tmap
{-# INLINE matchTemplate #-}

matchAllTemplates :: (MonadPlus m, MonadIO m) => Int -> TemplateMap -> GWord -> [Placement] -> m [GWord]
matchAllTemplates size tmap gword plcs = fmap catMaybes $ for plcs $ \plc -> do
  let candidates = concatMap (matchTemplate tmap) (templates plc)
      words = filter (\(w, _) -> BS.length w < size `div` 2) candidates
  if null words
  then pure Nothing
  else do
    word <- liftIO . evalRandIO $ uniform words
    pure $
      Just $ buildGWord
      (startX plc, startY plc)
      (d $ switchDirection gword)
      word
{-# INLINE matchAllTemplates #-}

data Placement =
  Placement
    { startX    :: !Int
    , startY    :: !Int
    , direction :: !Direction
    , templates :: ![Template]
    } deriving (Eq, Show)

prunePlacement :: Placement -> Placement
prunePlacement plc =
  plc { templates = filter (\(_, c) -> c /= ' ') (templates plc) }

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
        <*> ZipList [start..size]
        <*> ZipList [Down]
        <*> ZipList [(,) <$> (flip (-) start . cy) <*> cc <$> filter (\c -> cx c == cx cell && cy c >= start) gcs]
    Down -> concat $ flip fmap downCells $ \cell ->
      getZipList $ Placement
        <$> ZipList [start..size]
        <*> ZipList [cy cell]
        <*> ZipList [Across]
        <*> ZipList [(,) <$> (flip (-) start . cx) <*> cc <$> filter (\c -> cy c == cy cell && cx c >= start) gcs]
{-# INLINE getPlacements #-}

switchDirection :: GWord -> GWord
switchDirection gword = gword { d = if d gword == Across then Down else Across }
{-# INLINE switchDirection #-}

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
{-# INLINE getCells #-}

getGrid :: Int -> Grid -> [Cell]
getGrid size grid =
    List.unionBy (\a b -> cx a == cx b && cy a == cy b)
    (concatMap getCells grid)
    (Cell <$> [' '] <*> [0..size] <*> [0..size])
{-# INLINE getGrid #-}

printGrid :: Int -> Grid -> IO ()
printGrid size grid =
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

printClues :: Grid -> IO ()
printClues grid = do
  let across = filter ((==) Across . d) grid
      down = filter ((==) Down . d) grid
  for_ across $ \gword -> do
    putStrLn "Across"
    putStrLn $ show (num gword) ++ ". " ++ show (w gword) ++ ": " ++ show (c gword)
  for_ down $ \gword -> do
    putStrLn "Down"
    putStrLn $ show (num gword) ++ ". " ++ show (w gword) ++ ": " ++ show (c gword)

annotateGrid :: Grid -> Grid
annotateGrid g =
  let sorted = List.sortOn x g
      across = zip [1..] $ List.sortOn y $ filter ((==) Across . d) sorted
      down = zip [1..] $ List.sortOn y $ filter ((==) Down . d) sorted
      setNum (i, gw) = gw { num = i }
  in setNum <$> across <> down

run :: Bool -> [Line] -> Int -> Int -> Int -> Int -> Int -> IO Grid
run viz lines batchSize gridSize minStart minWords gas = do
  batch <- List.sortBy (flip compare) . take batchSize <$> Random.shuffleM lines
  let tmap = makeTemplateMap batch
  observeT $ generateGrid viz batch tmap gridSize minStart minWords gas
