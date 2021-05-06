module D2.KT where

import D2.Evolve
import Data.Array.Accelerate
import Prelude ()

kt :: Evolve
kt (Conf f f' g g' dx _ _) u = stencil skt (boundary u) u
  where
    skt :: Stencil5x5 T -> Exp T
    skt
      ( (_, _, t2, _, _),
        (_, _, t, _, _),
        (l2, l, c, r, r2),
        (_, _, b, _, _),
        (_, _, b2, _, _)
        ) =
        - ( h f f' (l, c, r, r2) - h f f' (l2, l, c, r)
              + h g g' (b, c, t, t2) - h g g' (b2, b, c, t)
          )
          / dx

h :: Func -> Func -> (Exp T, Exp T, Exp T, Exp T) -> Exp T
h f f' (a, b, c, d) = (f up + f um - v * (up - um)) / 2
  where
    up = c - (ux_ (b, c, d) / 2)
    um = b + (ux_ (a, b, c) / 2)
    v = a_ f' up um

a_ :: Func -> Exp T -> Exp T -> Exp T
a_ f' up um = max (rhof up) (rhof um)
  where
    rhof = abs . f'

ux_ :: (Exp T, Exp T, Exp T) -> Exp T
ux_ (um, uc, up) = minmod [theta * (uc - um), (up - um) / 2, theta * (up - uc)]
  where
    theta = 1.1

minmod :: [Exp T] -> Exp T
minmod [] = error "minmod takes at least one value"
minmod [a] = a
minmod [a, b] = (signum a + signum b) / 2 * min (abs a) (abs b)
minmod (a : b : xs) = minmod $ minmod [a, b] : xs
