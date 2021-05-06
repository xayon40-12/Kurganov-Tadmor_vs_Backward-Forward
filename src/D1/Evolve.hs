module D1.Evolve where

--import Control.Concurrent
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed

type T = Float

type Arr = UArray Int T

type Id = Int

type Size = Int

type Func = T -> T

data System = Sys Func Func Size Arr

type Evolve = T -> System -> Id -> T

u__ :: System -> Id -> T
u__ (Sys _ _ s u) i = u ! mod i s

euler :: T -> T -> Evolve -> System -> System
euler dx dt e sys@(Sys f f' size u) = Sys f f' size u2
    where u1 = runSTUArray $ do
                  let s = (\(a, b) -> b - a) . bounds $ u
                  let c = e dx sys
                  u1 <- newArray (0, s) 0
                  forM_ [0 .. s] $ \i -> do
                    let un = u ! i
                    let cun = c i
                    writeArray u1 i $ un + dt * cun
                  return u1
          u2 = runSTUArray $ do
                  let s = (\(a, b) -> b - a) . bounds $ u
                  let c1 = e dx (Sys f f' size u1)
                  let n1 = 1/2
                  u2 <- newArray (0, s) 0
                  forM_ [0 .. s] $ \i -> do
                    let un = u ! i
                    let ul = u1 ! i 
                    let cul = c1 i
                    writeArray u2 i $ n1*un + (1-n1)*(ul + dt * cul)
                  return u2
             
