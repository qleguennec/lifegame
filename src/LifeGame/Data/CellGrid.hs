{-# LANGUAGE TypeFamilies #-}

module LifeGame.Data.CellGrid (
    CellGrid(..)
    , cellGrid
    , randCellGrid

    , searchCell
    , alives
    , deads
) where

import LifeGame.Data.Cell (Cell(..), State(..), setState, isAlive, isDead)
import Data.Maybe (catMaybes)

import Math.Geometry.Grid
import Math.Geometry.Grid.SquareInternal (RectSquareGrid(..), SquareDirection)

import qualified System.Random as R (newStdGen, random)
import qualified Control.Monad as M (mapM)

data CellGrid = CellGrid (Int, Int) [Cell]
  deriving (Eq)

cellGrid :: Int -> Int -> State -> CellGrid
cellGrid r c s = CellGrid (r, c) $ [Cell s (x, y) | x <- [0..c-1], y <- [0..r-1]]

-- A CellGrid with random State Cells
randCellGrid :: Int -> Int -> IO (CellGrid)
randCellGrid r c = (M.mapM (\cell -> R.newStdGen >>= return . setState cell . fst . R.random)
                        $ indices . cellGrid r c $ Alive)
                   >>= return . CellGrid (r, c)

searchCell :: CellGrid -> (Int, Int) -> Maybe Cell
searchCell (CellGrid _ cs) i = case filter (\(Cell _ ind) -> ind == i) cs of
  [] -> Nothing
  xs -> Just . head $ xs

instance Show CellGrid where
  show (CellGrid (r, c) _) = "CellGrid " ++ (show r) ++ " " ++ (show c)
 
instance Grid CellGrid where
  type Index CellGrid = Cell
  type Direction CellGrid = SquareDirection

  indices (CellGrid _ xs) = xs
  neighbours cg (Cell _ (x, y)) = catMaybes [ 
     searchCell cg (x, y+1)
   , searchCell cg (x, y-1)
   , searchCell cg (x+1, y)
   , searchCell cg (x-1, y)]
  distance _ (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2-x1) + abs (y2-y1)
 -- We transform our Cells into Indexes (so our CellGrid into RectSquareGrid) so we can get directions
  directionTo (CellGrid rc cs) (Cell _ xy1) (Cell _ xy2) = (\rc' -> directionTo rc' xy1 xy2) . RectSquareGrid rc . map (\(Cell _ xy) -> xy) $  cs
 
alives :: CellGrid -> [Cell]
alives = filter (isAlive) . indices

deads :: CellGrid -> [Cell]
deads = filter (isDead) . indices
