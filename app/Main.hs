module Main where

import           Life
import           UI

main :: IO ()
main = runSimulation basicRule start
