module BF where

import Evolve
import Data.Array.Unboxed
    
bf :: Evolve
bf dx f fp u i = - ux / dx
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
