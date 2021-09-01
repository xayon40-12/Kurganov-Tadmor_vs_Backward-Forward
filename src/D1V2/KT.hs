module D1V2.KT where

import D1V2.Evolve
import Data.Vec
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

vmin :: Vec2 T -> Vec2 T -> Vec2 T
vmin (u1 :. u2 :. ()) (v1 :. v2 :. ()) = min u1 v1 :. min u2 v2 :. ()

vmax :: Vec2 T -> Vec2 T -> Vec2 T
vmax (u1 :. u2 :. ()) (v1 :. v2 :. ()) = max u1 v1 :. max u2 v2 :. ()

vdiv :: Vec2 T -> Vec2 T -> Vec2 T
vdiv (u1 :. u2 :. ()) (v1 :. v2 :. ()) = (if v1 /= 0 then u1 / v1 else 0) :. (if v2 /= 0 then u2 / v2 else 0) :. ()

minmod :: [Vec2 T] -> Vec2 T
minmod [] = error "minmod takes at least one value"
minmod [a] = a
minmod [a, b] = (signum a + signum b) / 2 * vmin (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs

minmodt :: (Vec2 T, Vec2 T, Vec2 T) -> Vec2 T
minmodt (um, uc, up) = minmod [t * (uc - um), (up - um) / 2, t * (up - uc)]
  where
    t = vec theta

minmodtf :: Func -> (Vec2 T, Vec2 T, Vec2 T) -> Vec2 T
minmodtf f (um, uc, up) = minmod [t * d uc um, d up um, t * d up uc]
  where
    d a b = (vdiv (f a - f (a + v1)) d1 + vdiv (f a - f (a + v2)) d2 + vdiv (f (a + v2) - f b) d1 + vdiv (f (a + v1) - f b) d2) / 2 -- FIXME should be per vector coord
      where
        (v1, d1, v2, d2) = (\(a1 :. a2 :. ()) (b1 :. b2 :. ()) -> let x = b1 - a1; y = b2 - a2 in (x :. 0 :. (), vec x, 0 :. y :. (), vec y)) a b
    t = vec theta

kt :: Bool -> Evolve
kt mano dx s i = - (h mano s i Ip - h mano s i Im) * vec (1 / dx)

h :: Bool -> System -> Id -> IDir -> Vec2 T
h mano s@(Sys f f' _ _) i id = (f up + f um - a * (up - um)) / 2
  where
    up = u_ s i P id
    um = u_ s i M id
    am = am_ s i id
    af = a_ f' up um
    a = if mano then if af /= am && False then trace (show af <> "   " <> show am) am else am else af

am_ :: System -> Int -> IDir -> Vec2 T
am_ s@(Sys f _ _ _) i id = am
  where
    upp = u__ s (i + id .+ P + 1)
    up = u__ s (i + id .+ P)
    um = u__ s (i + id .+ M)
    umm = u__ s (i + id .+ M -1)
    fup = abs $ minmodtf f (upp, up, um)
    fum = abs $ minmodtf f (up, um, umm)
    am = vmax fup fum

u_ :: System -> Id -> Dir -> IDir -> Vec2 T
u_ u i d id = ui + vec (sgn / 2) * uxi
  where
    sgn = if d == P then -1 else 1
    ui = u__ u (i + id .+ d)
    uxi = ux_ u (i + id .+ d)

ux_ :: System -> Id -> Vec2 T
ux_ u i = minmodt (um, uc, up)
  where
    um = u__ u (i -1)
    uc = u__ u i
    up = u__ u (i + 1)

a_ :: Func -> Vec2 T -> Vec2 T -> Vec2 T
a_ f' up um = vec $ max (rhof up) (rhof um)
  where
    rhof = (\(a :. b :. ()) -> max (abs a) (abs b)) . f'
