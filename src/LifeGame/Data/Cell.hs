module LifeGame.Data.Cell (
    State(..)
    , Cell (..)

    , isAlive
    , isDead
    , revState
    , setState
    , birth
    , death
) where

import System.Random as R (Random, randomR, random)

data State = Alive | Dead
  deriving (Eq, Bounded, Show)

instance R.Random State where
  randomR (a, b) g = case R.randomR (stToBool a, stToBool b) g of
    (True, g')  -> (Alive, g')
    (False, g') -> (Dead, g')

  random g = R.randomR (minBound, maxBound) g

data Cell = Cell State (Int, Int)
  deriving (Eq)

instance Show Cell where
  show (Cell s ind) = (show s) ++ " " ++ (show ind)

isAlive :: Cell -> Bool
isAlive (Cell s _) = s == Alive

isDead :: Cell -> Bool
isDead = not . isAlive

revState :: Cell -> Cell
revState (Cell s xy) = case s of
  Alive -> Cell Dead xy
  Dead  -> Cell Alive xy

stToBool :: State -> Bool
stToBool s = case s of
  Alive -> True
  Dead  -> False

setState :: Cell -> State -> Cell
setState (Cell _ xy) s' = Cell s' xy

birth :: Cell -> Cell
birth = flip setState $ Alive

death :: Cell -> Cell
death = flip setState $ Dead
