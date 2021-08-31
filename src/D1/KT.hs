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

theta :: T
theta = 1.9

minmod :: [T] -> T
minmod [] = error "minmod takes at least one value"
minmod [a] = a
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

minmodt :: (T, T, T) -> T
minmodt (um, uc, up) = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)]

minmodtf :: Func -> (T, T, T) -> T
minmodtf f (um, uc, up) = minmod [theta * d uc um, d up um, theta * d up uc]
  where
    d a b = if a == b then 0 else (f a - f b) / (a - b)

kt :: Bool -> Evolve
kt mano dx s i = - (h mano s i Ip - h mano s i Im) / dx

h :: Bool -> System -> Id -> IDir -> T
h mano s@(Sys f f' _ _) i id = (f up + f um - a * (up - um)) / 2
  where
    up = u_ s i P id
    um = u_ s i M id
    am = am_ s i id
    af = a_ f' up um
    a = if mano then am else af

am_ :: System -> Int -> IDir -> T
am_ s@(Sys f _ _ _) i id = am
  where
    upp = u__ s (i + id .+ P + 1)
    up = u__ s (i + id .+ P)
    um = u__ s (i + id .+ M)
    umm = u__ s (i + id .+ M -1)
    fup = abs $ minmodtf f (upp, up, um)
    fum = abs $ minmodtf f (up, um, umm)
    am = max fup fum

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
