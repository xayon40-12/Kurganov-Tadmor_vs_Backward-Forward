module Main where

import Data.Array.Unboxed
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import KT

type State = (Vec T, Vec T)

n = div (width -10) 2

list = zip [0 ..] $ take n $ take (div n 3) (repeat 1) ++ (repeat 0)

a = array (0, n -1) list

b = array (0, n -1) list

v = 1

dx = 1 / fromIntegral width

dt = dx / v / 10

skip = 10

fd = c

f :: T -> T
f u = v * u

f' :: T -> T
f' _ = v

width = 1200 :: Int

hight = 600 :: Int

main :: IO ()
main = play (InWindow "KT" (width, hight) (0, 0)) white 100 (a, b) makePicture handleEvent stepWorld

makePicture :: State -> Picture
makePicture (u, v) =
  pictures $
    draw u (- fromIntegral width / 2) ++ draw v 0 ++ [polygon [(0, - h2), (0, h2), (-10, h2), (-10, - h2)]]
  where
    h2 = h / 2
    h = fromIntegral hight
    draw u start = [color blue $ line [(x, - h2), (x, h - h2)] | (x, h) <- zip [start ..] $ map ((* h) . realToFrac) $ elems u]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ (u, v) = (it c u, it c2 v)
  where
    it fd = (!! skip) . iterate (next dx dt fd f f')
