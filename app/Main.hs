module Main where

import BF (bf)
import Data.Array.Unboxed
import Evolve
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import KT (kt)

data Sym = Sym Evolve System

type State = (Sym, Sym)

n = div (width -10) 2

h2 = fromIntegral $ div (hight -10) 2
w2 = fromIntegral $ div (width -10) 2

list = zip [0 ..] $ take n $ replicate (div n 3) h2 ++ repeat 0

a = Sym kt $ Sys f f' n (array (0, n -1) list)

b = Sym bf $ Sys f f' n (array (0, n -1) list)

v = 1

dx = 1 / w2

dt = dx / v / 10

skip = 10

f :: T -> T
f u = v * u

f' :: T -> T
f' _ = v

width = 1200 :: Int

hight = 1200 :: Int

main :: IO ()
main = play (InWindow "KT" (width, hight) (0, 0)) white 100 (a, b) makePicture handleEvent stepWorld

makePicture :: State -> Picture
makePicture (Sym _ (Sys _ _ _ u), Sym _ (Sys _ _ _ v)) =
  pictures $
       draw u (-w2)
    ++ draw v 6
    ++ [polygon [(5, - h2), (5, h2), (-6, h2), (-6, - h2)]
       ,polygon [(-w2,-5),(w2,-5),(w2,5),(-w2,5)]]
  where
    h2 = fromIntegral hight /2
    w2 = fromIntegral width /2
    draw u start = [color blue $ line [(x, 5), (x, 5+h)] | (x, h) <- zip [start ..] $ map realToFrac $ elems u]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ (Sym cu u, Sym cv v) = (Sym cu $ it cu u, Sym cv $ it cv v)
  where
    it fd = (!! skip) . iterate (euler dx dt fd)
