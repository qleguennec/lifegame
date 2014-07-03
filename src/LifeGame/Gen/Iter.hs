module LifeGame.Gen.Iter (
    nextGen
    , Gen
    , gens
) where

import LifeGame.Gen.Rules (ruler)
import LifeGame.Data.CellGrid (CellGrid(..))

type Gen = (Int, CellGrid)

nextGen :: CellGrid -> CellGrid
nextGen cg@(CellGrid rc xs) = CellGrid rc $ map (ruler cg) xs

gens :: CellGrid -> [Gen]
gens cg = gens' (0, cg)
  where gens' (t, cg') = (t, cg') : gens' (t+1, nextGen cg')
