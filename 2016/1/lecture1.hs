{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

frame :: Picture
frame = rectangle 2.5 5.5

botCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

lights :: Integer -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n-1))

spread :: Picture -> Double -> Integer -> Picture
spread pic dx 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))

ourPicture :: Picture
ourPicture = spread (trafficLight True) 3 4

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False
  
tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

{-main = drawingOf ourPicture-}
main :: IO ()
main = drawingOf (tree 8)