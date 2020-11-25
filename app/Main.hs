module Main where

import BF
import Data.Array.Unboxed
import Evolve
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import KT

data Sym = Sym Evolve (Vec T)

type State = (Sym, Sym)

n = div (width -10) 2

h = fromIntegral hight

list = zip [0 ..] $ take n $ replicate (div n 3) h ++ repeat 0

a = Sym kt $ array (0, n -1) list

b = Sym bf $ array (0, n -1) list

v = 1

dx = 1 / fromIntegral width

dt = dx / v / 10

skip = 10

f :: T -> T
f u = v * u

f' :: T -> T
f' _ = v

width = 1200 :: Int

hight = 600 :: Int

main :: IO ()
main = play (InWindow "KT" (width, hight) (0, 0)) white 100 (a, b) makePicture handleEvent stepWorld

makePicture :: State -> Picture
makePicture (Sym _ u, Sym _ v) =
  pictures $
    draw u (- fromIntegral width / 2) ++ draw v 10 ++ [polygon [(10, - h2), (10, h2), (-10, h2), (-10, - h2)]]
  where
    h2 = h / 2
    h = fromIntegral hight
    draw u start = [color blue $ line [(x, - h2), (x, h - h2)] | (x, h) <- zip [start ..] $ map realToFrac $ elems u]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ (Sym cu u, Sym cv v) = (Sym cu $ it cu u, Sym cv $ it cv v)
  where
    it fd = (!! skip) . iterate (euler dx dt fd f f')
