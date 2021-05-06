module Main where

import D1.BF (bf)
import D1.Evolve
import D1.KT (kt)
import Data.Array.Unboxed
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Sym = Sym Evolve System

type State = [Sym]

n = div (width -10) 2

h2 = fromIntegral $ div (hight -10) 2

w2 = fromIntegral $ div (width -10) 2

gate frac = zip [0 ..] $ take n $ replicate (div n frac) h2 ++ repeat 0

sinus f = [(i, h2 / 2 * (1 + sin (fromIntegral i * 2 * pi * f / fromIntegral n))) | i <- [0 .. n -1]]

sys f f' init val = Sys f f' n (array (0, n -1) (init val))

a =
  [ Sym kt (sys cont cont' gate 3),
    Sym bf (sys cont cont' gate 3),
    Sym kt (sys burger burger' gate 2),
    Sym bf (sys burger burger' sinus 1)
  ]

v = 1

dx = 1 / w2

dt = dx / v / 5

bur = 0.003

burger u = bur * u * u

burger' u = 2 * bur * u

cont u = v * u

cont' _ = v

width = 400 :: Int

hight = 400 :: Int

main :: IO ()
main = play (InWindow "KT" (width, hight) (0, 0)) white 200 a makePicture handleEvent stepWorld

makePicture :: State -> Picture
makePicture s
  | [u, v, w, x] <- [u | (Sym _ (Sys _ _ _ u)) <- s] =
    pictures $
      draw u (- w2') 5
        ++ draw v 6 5
        ++ draw w (- w2') (- h2')
        ++ draw x 6 (- h2')
        ++ [ polygon [(5, - h2'), (5, h2'), (-6, h2'), (-6, - h2')],
             polygon [(- w2' -1, -5), (w2', -5), (w2', 5), (- w2' -1, 5)]
           ]
  where
    h2' = fromIntegral hight / 2
    w2' = fromIntegral width / 2
    draw u sx sy = [color blue $ line [(x, sy), (x, sy + h)] | (x, h) <- zip [sx ..] $ map realToFrac $ elems u]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ s = [Sym cu $ euler dx dt cu u | (Sym cu u) <- s]
