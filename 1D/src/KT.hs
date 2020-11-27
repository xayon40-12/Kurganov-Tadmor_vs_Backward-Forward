module KT where

import Evolve

data Dir = P | M deriving (Eq) -- "+" or "-"

data IDir = Ip | Im deriving (Eq) -- "+1/2" or "-1/2"

infix .+
(.+) Ip P = 1
(.+) Im M = -1
(.+) _ _ = 0

minmod :: [T] -> T
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

kt :: Evolve
kt dx s i = - (h s i Ip - h s i Im) / dx

h :: System -> Id -> IDir -> T
h s@(Sys f f' _ u) i id = (f up + f um - a * (up - um)) / 2
    where
        up = u_ s i P id
        um = u_ s i M id
        a = a_ f' up um

u_ :: System -> Id -> Dir ->  IDir -> T
u_ u i d id | d == P = ui - uxi / 2
            | d == M = ui + uxi / 2
    where
        ui = u__ u (i+id.+d)
        uxi = ux_ u (i+id.+d)

a_ :: Func -> T -> T -> T
a_ f' up um = max (rhof up) (rhof um)
  where
    rhof = abs . f'

ux_ :: System -> Id -> T
ux_ u i = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)]
    where
        um = u__ u (i -1)
        uc = u__ u i
        up = u__ u (i + 1)
        theta = 2
