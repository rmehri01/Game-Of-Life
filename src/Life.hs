{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Life where

import           Data.Bool                      ( bool )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Distributive              ( Distributive(..) )
import qualified Data.Vector                   as V
import           Data.Functor.Rep               ( Representable(..)
                                                , distributeRep
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Control.Arrow                  ( (***) )
import           Control.Comonad.Representable.Store
                                                ( Store(..)
                                                , StoreT(..)
                                                , store
                                                , experiment
                                                )
import           Control.Comonad                ( Comonad(..) )

-- | Fixed sized Vector of length gridSize.
-- 
-- A Representable instance can only be defined on fixed sized vectors since
-- normal vectors can vary in size and thus, may be empty and not indexable.
-- 
-- Indexing higher than the length of the vector will wrap around to the beginning.
newtype VBounded a = VBounded (V.Vector a)
    deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
    distribute = distributeRep

instance Representable VBounded where
    type Rep VBounded = Int
    index (VBounded v) i = v V.! (i `mod` gridSize)
    tabulate desc = VBounded (V.generate gridSize desc)

-- | The size of one side length in a gridSize x gridSize grid.
gridSize :: Int
gridSize = 20

-- | Memoized grid of values indexed by Coord.
type Grid a = Store (Compose VBounded VBounded) a

-- | An (x, y) coordinate on the grid
type Coord = (Int, Int)

-- | The status of a cell in the grid.
-- Avoids boolean blindness and is arguably easier to read.
data Cell = Alive | Dead
    deriving Eq

-- | Given a list of the coords that are alive, creates the grid.
mkGrid :: [Coord] -> Grid Cell
mkGrid xs = store lookup (0, 0)
    where lookup crd = if crd `elem` xs then Alive else Dead

-- | Generic rules for the game that determine how a cell changes based on the current grid.
type Rule = Grid Cell -> Cell

-- | The 8 neighbouring tiles relative to (0, 0), the cell itself
neighbourCoords :: [Coord]
neighbourCoords =
    [ (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0) ]

-- | The typical rules for the Game of Life.
-- https://en.wikipedia.org/wiki/Conway's_Game_of_Life
-- From wikipedia, at each step, the following transitions occur:
-- * Any live cell with fewer than two live neighbours dies, as if by underpopulation.
-- * Any live cell with two or three live neighbours lives on to the next generation.
-- * Any live cell with more than three live neighbours dies, as if by overpopulation.
-- * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
basicRule :: Rule
basicRule g = case cellStatus of
    Alive -> if numNeighboursAlive `elem` [2, 3] then Alive else Dead
    Dead  -> if numNeighboursAlive == 3 then Alive else Dead
  where
    cellStatus = extract g
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbours         = experiment (\s -> addCoords s <$> neighbourCoords) g
    numNeighboursAlive = length (filter (== Alive) neighbours)

-- | Advances the grid by one step according to the given rule set.
step :: Rule -> Grid Cell -> Grid Cell
step = extend
