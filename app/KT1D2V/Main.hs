{-# LANGUAGE FlexibleContexts #-}

module Main where

import D1V2.Evolve
import D1V2.KT (kt)
import Data.Array.Unboxed
import Data.Vec hiding (init, map, take)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (init)

data Sym = Sym Evolve System

type State = [Sym]

n = div (width -10) 2

h2 = fromIntegral $ div (hight -10) 2

w2 = fromIntegral $ div (width -10) 2

init = zipWith3 (\a b c -> (a, b :. c :. ())) [0 ..]

gate frac = take n $ replicate (div n frac) h2 ++ repeat 0

sinus f = [h2 / 2 * (1 + sin (fromIntegral i * 2 * pi * f / fromIntegral n)) | i <- [0 .. n -1]]

sys f f' init1 init2 val = Sys f f' n (array (0, n -1) (init (init1 val) (init2 val)))

a =
  [ Sym (kt True) (sys cont cont' gate gate 3),
    Sym (kt False) (sys cont cont' gate gate 3),
    Sym (kt True) (sys cont cont' sinus sinus 1),
    Sym (kt False) (sys cont cont' sinus sinus 1)
  ]

sp = 1

dx = 1 / w2

dt = dx / sp / 5

cont (u :. v :. ()) = (sp * u) :. (- sp * u) :. ()

cont' _ = sp :. (0) :. ()

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
    h4' = (h2' - 6) / 2
    w2' = fromIntegral width / 2
    draw u sx sy = [Pictures [color blue $ line [(x, sy + h4'), (x, sy + h4' + h1 / 2)], color red $ line [(x, sy + h4' -1), (x, sy + h4' -1 - h2 / 2)]] | (x, (h1, h2)) <- zip [sx ..] $ (map (\(a :. b :. ()) -> (realToFrac a, realToFrac b)) $ elems u)]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ s = [Sym cu $ euler dx dt cu u | (Sym cu u) <- s]
