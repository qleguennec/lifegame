module LifeGame.Iter.Rules (
    ruler
) where

import LifeGame.Data.Cell (Cell, birth, death)
import LifeGame.Data.CellGrid (CellGrid, alives)

import Math.Geometry.Grid (neighbours)

import Data.List (intersect)

ruler :: CellGrid -> Cell -> Cell
ruler cg c
    | nAlives == 3 = birth c
    | nAlives < 2 || nAlives > 3 = death c
    | otherwise = c
 where nAlives = length $ (alives cg) `intersect` (neighbours cg c)
