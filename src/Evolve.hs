module Evolve where

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
euler dx dt e sys@(Sys f f' size u) = Sys f f' size $ runSTUArray $ do
  let s = (\(a, b) -> b - a) . bounds $ u
  let c = e dx sys
  tu <- newArray (0, s) 0
  forM_ [0 .. s] $ \i -> do
    let x = u__ sys i
    let y = c i
    writeArray tu i $ x + dt * y
  return tu
