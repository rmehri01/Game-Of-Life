module Main
    ( main
    )
where

import           Prelude
import           Life
import           UI

main :: IO ()
main = runSimulation basicRule startEmpty
