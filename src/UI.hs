module UI where

import           Life
import           Data.Vector                    ( toList )
import           Control.Comonad.Representable.Store
                                                ( Store(..)
                                                , StoreT(..)
                                                , runStore
                                                , experiment
                                                )
import           Control.Comonad                ( extract )
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

-- | Uses a rule and grid to continuously simulate the game.
-- Steps at each iteration as long as the game is not paused.
runSimulation :: Rule -> Grid Cell -> IO ()
runSimulation r g = play window
                         white
                         2
                         (g, False)
                         (centerGrid . renderGrid . fst)
                         handleEvent
                         (\_ (g', b) -> (if b then step r g' else g', b))

-- | Window as big as the picture of the grid is.
window :: Display
window = InWindow "Game of Life" (sideLen, sideLen) (0, 0)
  where sideLen = gridSize * round squareLen

-- | Side length of a single square in the picture.
squareLen :: Float
squareLen = 20

-- | Render a picture of the grid at a given instance.
renderGrid :: Grid Cell -> Picture
renderGrid g = pictures
  [ translate (fromIntegral x * squareLen)
              (-(fromIntegral y * squareLen))
              singleSquare
  | x <- [0 .. gridSize - 1]
  , y <- [0 .. gridSize - 1]
  , let c = chooseColor ((fst $ runStore g) (y, x))
  , let singleSquare = rectangleWire squareLen squareLen
          <> color c (rectangleSolid squareLen squareLen)
  ]
 where
  chooseColor Alive = makeColorI 254 95 85 255
  chooseColor Dead  = makeColorI 238 245 219 255

-- | Shift the picture of the grid to make the top left corner (0, 0).
centerGrid :: Picture -> Picture
centerGrid = translate (-distToMiddle) distToMiddle
 where
  distToMiddle = fromIntegral (gridSize `div` 2) * squareLen - (squareLen / 2)

-- | Handles adding and removing cells as well as pausing the game.
-- Click on a cell to change it from Dead to Alive and vice versa
-- Press the space key to start and stop the simulation.
handleEvent :: Event -> (Grid Cell, Bool) -> (Grid Cell, Bool)
handleEvent e w@(g, b) = case e of
  (EventKey (SpecialKey KeySpace) Down _ _) -> (g, not b)
  (EventKey (MouseButton LeftButton) Down _ (x, y)) ->
    let clickedCoord = (- (round (y / squareLen + 0.5)) + 20, round (x / squareLen - 0.5) + 20)
    in  (step (flipRule clickedCoord) g, b)
  _ -> w

flipRule clickedCoord g = case cellFound of
  Just _ -> case cellStatus of
    Alive -> Dead
    Dead  -> Alive
  Nothing -> cellStatus
 where
  cellStatus = extract g
  cellFound =
    experiment (\c -> if c == clickedCoord then Just c else Nothing) g


-- TODO: add cycle counter
-- TODO: add variable speed or maybe manually control stepping
