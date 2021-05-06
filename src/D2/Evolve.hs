{-# LANGUAGE BangPatterns #-}

module D2.Evolve where

import Data.Array.Accelerate (Acc, Array, Boundary, DIM2, Exp, Word32, Z (..), constant, function, generate, unlift, (:.) (..), (?))
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game

type T = Float

type Arr = Array DIM2 T

type Arr2 = Array DIM2 (T, T)

type Func = Exp T -> Exp T

data Conf = Conf
  { _f :: Func,
    _f' :: Func,
    _g :: Func,
    _g' :: Func,
    _dx :: Exp T,
    _dt :: Exp T,
    _ev :: Evolve
  }

type System = (Conf, Arr)

type Evolve = Conf -> Acc Arr -> Acc Arr

type State = [System]

width :: Int
width = 400

hight :: Int
hight = 400

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
  pictures
    $! pics
    ++ [ polygon [(5, - h2 -1), (5, h2), (-6, h2), (-6, - h2 -1)],
         polygon [(- w2 -1, -6), (w2, -6), (w2, 5), (- w2 -1, 5)]
       ]
  where
    h2 = fromIntegral hight + 5
    w2 = fromIntegral width + 5
    wp = w2 / 2 + 3
    hp = h2 / 2 + 3
    pics =
      [ translate x y $
          bitmapOfArray (CPU.run1 (A.map toWord32) a) True
        | ((x, y), (_, a)) <- zip [(- wp, hp), (wp, hp), (- wp, - hp -2), (wp, - hp -2)] s
      ]

toWord32 :: Exp T -> Exp Word32
toWord32 f = (f A.> 1) A.? (rgba 1 0 0 1, (f A.< 0) ? (rgba 0 1 0 1, rgba v v 1 1))
  where
    v = 1 - f

rgba :: Exp T -> Exp T -> Exp T -> Exp T -> Exp Word32
rgba i j k l = r + 256 * (g + 256 * (b + 256 * a))
  where
    [r, g, b, a] = [A.floor (c * 255) | c <- [i, j, k, l]]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: ViewPort -> Float -> State -> State
stepWorld _ _ s = next <$> s

next :: System -> System
next (conf@(Conf _ _ _ _ _ dt e), u') = (conf, un)
  where
    un = GPU.run1 (\u -> u2 u $ u1' u) u'
    e' = e conf
    u1' u = A.map (\v -> let (a, b) = unlift v in a + dt * b) $ A.zip u (e' u)
    n1 = 0.5
    u2 u u1 =
      A.map
        ( \v ->
            let (a, b, c) = unlift v
             in n1 * a + (1 - n1) * (b + dt * c)
        )
        $ A.zip3 u u1 (e' u1)

boundary :: Acc Arr -> Boundary Arr
boundary _ = function $ const 0
