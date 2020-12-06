{-# LANGUAGE BangPatterns #-}

module Evolve where

import Data.Array.Accelerate (Acc, Array, DIM2, Exp, Word32, Boundary, Z (..), function, constant, generate, unlift, use, (:.) (..), (?))
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort (ViewPort)

type T = Float

type Arr = Array DIM2 T

type Arr2 = Array DIM2 (T, T)
type Func = Exp T -> Exp T

data Conf = Conf {_f :: Func
                 ,_f' :: Func
                 ,_g :: Func
                 ,_g' :: Func
                 ,_dx :: Exp T
                 ,_dt :: Exp T
                 ,_ev :: Evolve
                 }

type System = (Conf,Arr)

type Evolve = Conf -> Acc Arr -> Acc Arr

type State = [System]

width = 400 :: Int

hight = 400 :: Int

arr :: Arr
arr = res
  where
    -- do not use GPU here (in main thread)
    !res =
      CPU.run $
        generate
          (constant (Z :. width :. hight))
          ( \xy ->
              let (Z :. x :. y) = unlift xy
                  wl = constant (div width 3)
                  wr = constant (div (2 * width) 3)
                  hl = constant (div hight 3)
                  hr = constant (div (2 * hight) 3)
               in (x A.> wl A.&& x A.< wr A.&& y A.> hl A.&& y A.< hr) ? (constant 1, constant 0)
          )

makePicture :: State -> Picture
makePicture s =
  pictures $!
    pics
      ++ [ polygon [(5, - h2 -1), (5, h2), (-6, h2), (-6, - h2 -1)],
           polygon [(- w2 -1, -6), (w2, -6), (w2, 5), (- w2 -1, 5)]
         ]
  where
    h2 = fromIntegral hight + 5
    w2 = fromIntegral width + 5
    wp = w2 / 2 + 3
    hp = h2 / 2 + 2
    pics =
      [ translate x y $
          bitmapOfArray (CPU.run1 (A.map toWord32) a) True
        | ((x, y), (_,a)) <- zip [(- wp, hp), (wp, hp), (- wp, - hp-2), (wp, - hp-2)] s
      ]

toWord32 :: Exp T -> Exp Word32
toWord32 f = rgba v v 1 1
  where
    v = 1 - f

rgba :: Exp T -> Exp T -> Exp T -> Exp T -> Exp Word32
rgba i j k l = r + 256 * (g + 256 * (b + 256 * a))
  where
    [r, g, b, a] = [{-max 0 $ min 255 $ -}A.floor (c * 255) | c <- [i, j, k, l]]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: ViewPort -> Float -> State -> State
stepWorld _ _ s = next <$> s

next :: System -> System
next (conf@(Conf _ _ _ _ _ dt c),u) = (conf,un)
  where
    un = GPU.run1 (\u -> u2 u $ u1 u) u
    c' = c conf
    u1 u = A.map (\v -> let (a, b) = unlift v in a + dt * b) $ A.zip u (c' u)
    n1 = 0.5
    u2 u u1 =
      A.map
        ( \v ->
            let (a, b, c) = unlift v
             in n1 * a + (1 - n1) * (b + dt * c)
        ) $ A.zip3 u u1 (c' u1)


boundary :: Acc Arr -> Boundary Arr
boundary _ = function $ const 0
