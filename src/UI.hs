module UI where

import           Life
import           Data.Vector                    ( toList )
import           Control.Comonad.Representable.Store
                                                ( Store(..)
                                                , StoreT(..)
                                                , runStore
                                                )
import           Graphics.Gloss

-- | Render a picture of the grid at a given instance.
renderGrid :: Grid Cell -> Picture
renderGrid g = pictures
  [ translate (fromIntegral x * squareLen)
              (-(fromIntegral y * squareLen))
              (color c (rectangleSolid squareLen squareLen))
  | x <- [0 .. gridSize - 1]
  , y <- [0 .. gridSize - 1]
  , let c = chooseColor ((fst $ runStore g) (y, x))
  ]
 where
  chooseColor Alive = makeColorI 254 95 85 255
  chooseColor Dead  = makeColorI 238 245 219 255

-- | Shift the picture of the grid to make the top left corner (0, 0).
centerGrid :: Picture -> Picture
centerGrid = translate (-distToMiddle) distToMiddle
 where
  distToMiddle = fromIntegral (gridSize `div` 2) * squareLen - (squareLen / 2)

-- | Side length of a single square in the picture.
squareLen :: Float
squareLen = 20

-- | Window as big as the picture of the grid is.
window :: Display
window = InWindow "Game of Life" (sideLen, sideLen) (0, 0)
  where sideLen = gridSize * round squareLen

-- | Uses a rule and grid to continuously simulate the game by stepping at each iteration.
runSimulation :: Rule -> Grid Cell -> IO ()
runSimulation r g =
  simulate window white 2 g (centerGrid . renderGrid) (\_ _ -> step r)

-- TODO: add grid to distinguish squares better
-- TODO: add cycle counter
-- TODO: mouse interaction to set up initial grid and start key to run
-- TODO: add variable speed or maybe manually control stepping
