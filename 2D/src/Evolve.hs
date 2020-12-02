{-# language BangPatterns #-}

module Evolve where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array.Accelerate (Acc,Array,DIM2,Z(..),(:.)(..),Exp,Word32,use,constant,generate,unlift,(?))
import qualified Data.Array.Accelerate as A
import Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import qualified Data.Array.Accelerate.LLVM.Native as CPU

type T = Float
type Arr = Array DIM2 T
type Arr2 = Array DIM2 (T,T)
type Func = Exp T -> Exp T
data System = Sys Func Func Func Func (Exp T) (Exp T) Evolve Arr
type Evolve = System -> Acc Arr

type State = [System]

width = 400 :: Int

hight = 400 :: Int

arr :: Arr
arr = res
    where !res = CPU.runN $ generate (constant (Z :. width :. hight))
               (\xy -> let (Z :. x :. y) = unlift xy
                           wl = constant (div width 3)
                           wr = constant (div (2*width) 3)
                           hl = constant (div hight 3)
                           hr = constant (div (2*hight) 3)
                       in (x A.> wl A.&& x A.< wr A.&& y A.> hl A.&& y A.< hr) ? (constant 1,constant 0))


makePicture :: State -> Picture
makePicture s =
  pictures $ pics ++
    [ polygon [(5, - h2-1), (5, h2), (-6, h2), (-6, - h2-1)],
      polygon [(- w2 -1, -6), (w2, -6), (w2, 5), (- w2 -1, 5)]
    ]
  where
    h2 = fromIntegral hight + 5
    w2 = fromIntegral width + 5
    wp = w2/2+3
    hp = h2/2+3
    !pics = [translate x y $
                bitmapOfArray (CPU.runN $ A.map toWord32 (use a)) False
                | ((x,y),Sys _ _ _ _ _ _ _ a) <- zip [(-wp,hp),(wp,hp),(-wp,-hp),(wp,-hp)] s]

toWord32 :: Exp T -> Exp Word32
toWord32 f = rgba v v 1 1
    where v = 1-f

rgba :: Exp T -> Exp T -> Exp T -> Exp T -> Exp Word32
rgba i j k l = r + 256*(g + 256*(b + 256*a))
    where [r,g,b,a] = [A.floor (c*255) | c <- [i,j,k,l]]

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: T -> State -> State
stepWorld _ s = [next i | i <- s]

next :: System -> System
next sys@(Sys f f' g g' dx dt c u) = Sys f f' g g' dx dt c u2
    where !u2 = GPU.runN $ A.map (\v -> let (a,b) = unlift v in a+dt*b) $ A.zip (use u) (c sys)
