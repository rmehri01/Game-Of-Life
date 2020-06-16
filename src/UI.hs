module UI where

import           Life
import           Data.Vector                    ( toList )
import           Control.Comonad.Representable.Store
                                                ( Store(..)
                                                , StoreT(..)
                                                , runStore
                                                )
import           Graphics.Gloss

gameOfLifePicture :: Grid Cell -> Picture
gameOfLifePicture g = translate (-distToMiddle) distToMiddle $ pictures
  [ translate (fromIntegral x * squareLen)
              (-(fromIntegral y * squareLen))
              (color c (rectangleSolid squareLen squareLen))
  | x <- [0 .. gridSize - 1]
  , y <- [0 .. gridSize - 1]
  , let c = chooseColor ((fst $ runStore g) (y, x))
  ]
 where
  distToMiddle = fromIntegral (gridSize `div` 2) * squareLen - (squareLen / 2)
  chooseColor Alive = red
  chooseColor Dead  = blue

squareLen :: Float
squareLen = 20

window :: Display
window = InWindow "Game of Life" (sideLen, sideLen) (0, 0)
  where sideLen = gridSize * round squareLen

runSimulation :: Rule -> Grid Cell -> IO ()
runSimulation r g = simulate window white 2 g gameOfLifePicture (\_ _ -> step r)