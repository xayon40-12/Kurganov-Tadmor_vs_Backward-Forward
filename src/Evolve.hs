module Evolve where

--import Control.Concurrent
import Control.Monad
import Data.Array.ST hiding (index)
import Data.Array.Unboxed hiding (index)

type T = Double

type Vec a = UArray Int a

type Id = Int

type Func = T -> T

type Evolve = T -> Func -> Func -> Vec T -> Id -> T

euler :: T -> T -> Evolve -> Func -> Func -> Vec T -> Vec T
euler dx dt e f f' u = runSTUArray $ do
  let s = (\(a, b) -> b - a) . bounds $ u
  let c = e dx f f'
  tu <- newArray (0, s) 0
  forM_ [0 .. s] $ \i -> do
    let x = u ! i
    let y = c u i
    writeArray tu i $ x + dt * y
  return tu
