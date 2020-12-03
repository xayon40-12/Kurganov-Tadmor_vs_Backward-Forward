module Main where
    
import Graphics.Gloss
import Data.Array.Accelerate (constant)

import Evolve
import BF
import KT

v = (1,1)
f u = fst v*u
g u = snd v*u
f' _ = fst v
g' _ = snd v
dx = constant 0.1
dt = dx / uncurry (+) v / constant 10
start = [(Sys f f' g g' dx dt kt arr),(Sys f f' g g' dx dt bf arr)]

main :: IO ()
main = play (InWindow "KT" (2*width+10, 2*hight+10) (0, 0)) white 60 start makePicture handleEvent stepWorld

