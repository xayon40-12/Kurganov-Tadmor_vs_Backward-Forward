module D1.BF where

import D1.Evolve

bf :: Evolve
bf dx s@(Sys f f' _ _) i = - ux / dx
  where
    um = f $ u__ s (i -1)
    uc = f $ u__ s i
    up = f $ u__ s (i + 1)
    ux =
      if f' (u__ s i) < 0
        then up - uc
        else uc - um
