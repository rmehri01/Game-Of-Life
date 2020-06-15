module Main where

import           Life
import           Control.Concurrent

tickTime :: Int
tickTime = 200000

main :: IO ()
main = loop (step basicRule) start

loop :: (Grid Cell -> Grid Cell) -> Grid Cell -> IO ()
loop stepper g = do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)
