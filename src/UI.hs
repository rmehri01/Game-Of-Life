module UI
  ( runSimulation
  )
where

import           Prelude
import           Life
import           Control.Comonad.Representable.Store
import           Control.Comonad                ( extract )
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

-- | Uses a rule and grid to continuously simulate the game.
-- Steps at each iteration as long as the game is not paused.
runSimulation :: Rule -> Grid Cell -> IO ()
runSimulation r g = play window
                         white
                         8
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
  , let c = chooseColor (peek (y, x) g)
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
    let halfGrid = gridSize `div` 2
        clickedCoord =
            ( -(ceiling (y / squareLen)) + halfGrid
            , floor (x / squareLen) + halfGrid
            )
    in  (step (flipCoord clickedCoord) g, b)
  _ -> w

-- | Creates a rule that only flips the cell state of a single coord.
flipCoord :: Coord -> Rule
flipCoord coord g = if coord == currentPos then flippedCell else cellStatus
 where
  currentPos  = pos g
  cellStatus  = extract g
  flippedCell = case cellStatus of
    Alive -> Dead
    Dead  -> Alive
