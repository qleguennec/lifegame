module LifeGame.Gen.Iter (
    nextGen
) where

import LifeGame.Gen.Rules (ruler)
import LifeGame.Data.CellGrid (CellGrid(..))

nextGen :: CellGrid -> CellGrid
nextGen cg@(CellGrid rc xs) = CellGrid rc $ map (ruler cg) xs
