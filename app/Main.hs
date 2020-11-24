module Main where

import KT

every = 10

v = 0.3
z = repeat 0
o = repeat 1
n = 50
a = take n $ take 10 o ++ z :: Vec T
l=1
dx = l/ fromIntegral n
dt = dx/v/10

f :: T -> T
f u = v*u

f' :: T -> T
f' _ = v

main :: IO ()
main = do
    mapM_ disp $ [iterate (next dx dt c f f') a !! i | i <- [0,every..]]
