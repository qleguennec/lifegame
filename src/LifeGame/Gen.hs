module LifeGame.Gen (
    nextGen
    , Gen(..)
    , gens
) where

import LifeGame.Data.Cell (Cell, birth, death)
import LifeGame.Data.CellGrid (CellGrid(..), alives, neighbours, population)

import Data.List (intersect)

rule :: CellGrid -> Cell -> Cell
rule cg c
  | nAlives == 3 = birth c
  | nAlives < 2 || nAlives > 3 = death c
  | otherwise = c
    where nAlives = length $ (alives cg) `intersect` (neighbours cg c)

nextGen :: CellGrid -> CellGrid
nextGen cg@(CellGrid rc xs) = CellGrid rc $ map (rule cg) xs

-- a generation is defined by a time and a cellgrid
data Gen = Gen Integer CellGrid

instance Show Gen where
  show (Gen t cg) = "Time: " ++ (show t) ++ ", Population: " ++ (show . population $ cg)

-- gens produces the infinite list of generations of a cellgrid input
gens :: CellGrid -> [Gen]
gens cg = gens' $ Gen 0 cg
  where gens' (Gen t cg') = (Gen t cg') : gens' (Gen (t+1) $ nextGen cg')
