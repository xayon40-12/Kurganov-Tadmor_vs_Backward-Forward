module KT where

import Evolve
import Data.Array.Unboxed

minmod :: [T] -> T
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

rho :: Func -> T -> T
rho f' u = abs $ f' u

kt :: Evolve
kt dx f f' u i = - (hp - hm) / dx
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

