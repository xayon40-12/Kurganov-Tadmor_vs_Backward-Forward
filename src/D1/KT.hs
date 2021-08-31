module D1.KT where

import D1.Evolve
import Debug.Trace (trace)
import Prelude hiding (id)

data Dir = P | M deriving (Eq) -- "+" or "-"

data IDir = Ip | Im deriving (Eq) -- "+1/2" or "-1/2"

infix 9 .+

(.+) :: IDir -> Dir -> Int
(.+) Ip P = 1
(.+) Im M = -1
(.+) _ _ = 0

theta = 2

minmod :: [T] -> T
minmod [] = error "minmod takes at least one value"
minmod [a] = a
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

minmodt :: (T, T, T) -> T
minmodt (um, uc, up) = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)]

minmodtf :: Func -> (T, T, T) -> T
minmodtf f (um, uc, up) = minmod [theta * d uc um, d up um / 2, theta * d up uc]
  where
    d a b = if a == b then 0 else (f a - f b) / (a - b)

kt :: Evolve
kt dx s i = - (h dx s i Ip - h dx s i Im) / dx

h :: Float -> System -> Id -> IDir -> T
h dx s@(Sys f f' _ _) i id = (fup + fum - a * (up - um)) / 2
  where
    fup = f up
    fum = f um
    upm = u_ s (i -1) P id
    up = u_ s i P id
    upp = u_ s (i + 1) P id
    umm = u_ s (i -1) M id
    um = u_ s i M id
    ump = u_ s (i + 1) M id
    af = a_ f' up um
    fup_ = abs $ minmodtf f (upm, up, upp)
    fum_ = abs $ minmodtf f (umm, um, ump)
    am = max fup_ fum_
    a = am -- if am /= 0 && af /= 0 then trace (show am <> "  " <> show af <> "  " <> show ((af - am) / af)) am else am

u_ :: System -> Id -> Dir -> IDir -> T
u_ u i d id = ui + sgn * uxi / 2
  where
    sgn = if d == P then -1 else 1
    ui = u__ u (i + id .+ d)
    uxi = ux_ u (i + id .+ d)

ux_ :: System -> Id -> T
ux_ u i = minmodt (um, uc, up)
  where
    um = u__ u (i -1)
    uc = u__ u i
    up = u__ u (i + 1)

a_ :: Func -> T -> T -> T
a_ f' up um = max (rhof up) (rhof um)
  where
    rhof = abs . f'
