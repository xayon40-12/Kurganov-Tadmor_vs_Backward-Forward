{-# language BangPatterns #-}
module KT where
    
import System.IO
import Control.Concurrent
--import Data.Array.ST hiding (index)
--import Data.Array.Unboxed hiding (index)
--import Control.Monad
--import Control.Monad.ST

type T = Double
type Vec a = [a]
type Id = Int

disp :: Vec T -> IO ()
disp xs = do
    putStr $! "\ESC[2J\ESC[H" ++ disp' xs
    hFlush stdout
    threadDelay 100000
    where disp' (x:xs) = take (floor (x*mul)) (cycle "#") ++ "\n" ++ tail
              where !tail = disp' $ drop 0 xs
          disp' _ = ""
          mul = 100

minmod :: [T] -> T
minmod [a,b] = (signum a + signum b)/2*min (abs a) (abs b)
minmod (a:b:xs) = minmod $ minmod [a,b] : xs

index :: Vec T -> Id -> T
index u i = u !! mod i l
    where l = length u

type Func = T -> T
type Evolve = T -> Func -> Func -> Vec T -> Vec T

c :: Evolve
c dx f f' u = c' <$> [0..length u -1]
    where c' i = -(hp - hm)/dx
            where upp = up - dx/2*uxp
                  ump = uc + dx/2*ux
                  upm = uc - dx/2*ux
                  umm = um + dx/2*uxm
                  um2 = index u (i-2)
                  um = index u (i-1)
                  uc = index u i
                  up = index u (i+1)
                  up2 = index u (i+2)
                  uxm = minmod [theta*(um-um2),(uc-um2)/2,theta*(uc-um)] /dx
                  ux = minmod [theta*(uc-um),(up-um)/2,theta*(up-uc)] /dx
                  uxp = minmod [theta*(up-uc),(up2-uc)/2,theta*(up2-up)] /dx
                  ap = max (rhof upp) (rhof ump)
                  am = max (rhof upm) (rhof umm)
                  hp = (f upp + f ump - ap*(upp - ump))/2
                  hm = (f upm + f umm - am*(upm - umm))/2
                  rhof = rho f'
                  theta = 1.7 -- WARNING modifiable parameter


c2 :: Evolve
c2 dx f fp u = c2' <$> [0..l-1]
    where l = length u
          c2' i = -ux/dx
            where um = f $ index u (i-1)
                  uc = f $ index u i
                  up = f $ index u (i+1)
                  ux = if fp (index u i) < 0
                       then up-uc
                       else uc-um

next :: T -> T -> Evolve -> Func -> Func -> Vec T -> Vec T
next dx dt e f f' u = u3
    where u1 = zipWith (\x y -> x+dt*y) u (c u)
          u2 = zipWith3 (\u u1 cu1 -> n1*u + (1-n1)*(u1+dt*cu1)) u u1 (c u1)
          u3 = zipWith3 (\u u2 cu2 -> n2*u + (1-n2)*(u2+dt*cu2)) u u2 (c u2)
          n1 = 3/4
          n2 = 1/3
          c = e dx f f'

rho :: Func -> T -> T
rho f' u = abs $ f' u
