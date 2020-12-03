module BF where

import Data.Array.Accelerate
import Prelude ()

import Evolve

bf :: Evolve
bf (Sys f f' g g' dx _ _ _) u = stencil sbf (boundary u) u
    where sbf :: Stencil3x3 T -> Exp T
          sbf ((_,a,_),(b,c,d),(_,e,_)) = -((f' c < 0 ? (cd,bc)) + (g' c > 0 ? (ec,ca)))/dx
              where 
                    bc = f c-f b
                    cd = f d-f c
                    ca = g a-g c
                    ec = g c-g e

