module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array.Accelerate hiding (fromIntegral)
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Array.Accelerate.LLVM.PTX (run)

type Arr = Acc (Array DIM2 Float)
newtype System = Sys Arr
type State = [System]

width = 400 :: Int

hight = 400 :: Int

arr :: Arr
arr = generate (constant (Z :. width :. hight)) (\xi -> constant 0)
start = []

main :: IO ()
main = play (InWindow "KT" (width+10, hight+10) (0, 0)) white 60 start makePicture handleEvent stepWorld

makePicture :: State -> Picture
makePicture _ =
  pictures
    [ polygon [(5, - h2), (5, h2), (-6, h2), (-6, - h2)],
      polygon [(- w2 -1, -5), (w2, -5), (w2, 5), (- w2 -1, 5)]
    ]
  where
    h2 = fromIntegral hight / 2
    w2 = fromIntegral width / 2
    --topic = bitmapOfArray False

handleEvent :: Event -> State -> State
handleEvent _ = id

stepWorld :: Float -> State -> State
stepWorld _ = id
