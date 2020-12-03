{-# LANGUAGE BangPatterns #-}

module Main where
            
import Graphics.Gloss
import Data.Array.Accelerate (constant)

import Evolve
import BF
import KT


main :: IO ()
main = simulate (InWindow "KT" (2*width+10, 2*hight+10) (0, 0)) white 30 start makePicture stepWorld
    where
        v = (1,1)
        f u = fst v*u
        g u = snd v*u
        f' _ = fst v
        g' _ = snd v
        dx = constant 0.1
        dt = dx / uncurry (+) v / constant 10
        !start = [(Conf f f' g g' dx dt kt,arr),(Conf f f' g g' dx dt bf, arr)]
