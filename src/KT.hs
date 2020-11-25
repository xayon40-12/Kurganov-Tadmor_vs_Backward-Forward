module KT where

import Data.Array.Unboxed
import Evolve

data Dir = P | M deriving (Eq)

data IDir = Ip | Im deriving (Eq)

minmod :: [T] -> T
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

kt :: Evolve
kt dx f f' u i = - (h f f' u i Ip - h f f' u i Im) / dx

h_ :: Func -> Func -> T -> T -> T
h_ f f' up um = (f up + f um - a_ f' up um * (up - um)) / 2

h :: Func -> Func -> Vec T -> Id -> IDir -> T
h f f' u i id = h_ f f' (u_ u i id P) (u_ u i id M)

u_ :: Vec T -> Id -> IDir -> Dir -> T
u_ u i id d | (Ip,P) == (id,d) = upp
            | (Im,P) == (id,d) = upm
            | (Ip,M) == (id,d) = ump
            | (Im,M) == (id,d) = umm
    where
        upp = up - uxp / 2
        ump = uc + ux / 2
        upm = uc - ux / 2
        umm = um + uxm / 2
        s = snd . bounds $ u
        theta = 1.7
        l = s + 1
        um2 = u ! mod (i -2) l
        um = u ! mod (i -1) l
        uc = u ! mod i l
        up = u ! mod (i + 1) l
        up2 = u ! mod (i + 2) l
        uxm = ux_ theta um2 um uc
        ux = ux_ theta um uc up
        uxp = ux_ theta uc up up2
          

a_ :: Func -> T -> T -> T
a_ f' up um = max (rhof up) (rhof um)
  where
    rhof = abs . f'

ux_ :: T -> T -> T -> T -> T
ux_ theta um uc up = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)]
