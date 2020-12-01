module Main where
    
import Graphics.Gloss
import Data.Array.Accelerate (constant)

import Evolve
import BF

v = (10,10)
f u = fst v*u
g u = snd v*u
f' _ = fst v
g' _ = snd v
dx = constant 0.1
dt = dx / uncurry (+) v / constant 10

main :: IO ()
main = do
    let start = replicate 4 (Sys f f' g g' dx dt bf arr)
    play (InWindow "KT" (2*width+10, 2*hight+10) (0, 0)) white 60 start makePicture handleEvent stepWorld

