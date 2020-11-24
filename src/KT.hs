{-# LANGUAGE BangPatterns #-}

module KT where

import Control.Concurrent
import Control.Monad
import Data.Array.ST hiding (index)
import Data.Array.Unboxed hiding (index)
import System.IO

type T = Double

type Vec a = UArray Int a

type Id = Int

minmod :: [T] -> T
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

rho :: Func -> T -> T
rho f' u = abs $ f' u

type Func = T -> T

type Evolve = T -> Func -> Func -> Vec T -> Id -> T

c :: Evolve
c dx f f' u i = - (hp - hm) / dx
  where
    s = (\(a, b) -> b - a) . bounds $ u
    theta = 1.7
    l = s + 1
    um2 = u ! mod (i -2) l
    um = u ! mod (i -1) l
    uc = u ! mod i l
    up = u ! mod (i + 1) l
    up2 = u ! mod (i + 2) l
    uxm = minmod [theta * (um - um2), (uc - um2) / 2, theta * (uc - um)] / dx
    ux = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)] / dx
    uxp = minmod [theta * (up - uc), (up2 - uc) / 2, theta * (up2 - up)] / dx
    upp = up - dx / 2 * uxp
    ump = uc + dx / 2 * ux
    upm = uc - dx / 2 * ux
    umm = um + dx / 2 * uxm
    rhof = rho f'
    ap = max (rhof upp) (rhof ump)
    am = max (rhof upm) (rhof umm)
    hp = (f upp + f ump - ap * (upp - ump)) / 2
    hm = (f upm + f umm - am * (upm - umm)) / 2

c2 :: Evolve
c2 dx f fp u i = - ux / dx
  where
    s = (\(a, b) -> b - a) . bounds $ u
    l = s + 1
    um = f $ u ! mod (i -1) l
    uc = f $ u ! mod i l
    up = f $ u ! mod (i + 1) l
    ux =
      if fp (u ! mod i l) < 0
        then up - uc
        else uc - um

next :: T -> T -> Evolve -> Func -> Func -> Vec T -> Vec T
next dx dt e f f' u = runSTUArray $ do
  let s = (\(a, b) -> b - a) . bounds $ u
  let c = e dx f f'
  tu <- newArray (0, s) 0
  forM_ [0 .. s] $ \i -> do
    let x = u ! i
    let y = c u i
    writeArray tu i $ x + dt * y
  return tu
