{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise1

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle, midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))

frame :: Picture
frame = rectangle 3 10

trafficLight :: Integer -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame -- long green
trafficLight 2  = botCircle black & midCircle yellow & topCircle black  & frame -- short amber
trafficLight 3  = botCircle black & midCircle black & topCircle red & frame -- long red
trafficLight 4  = botCircle black & midCircle yellow & topCircle red & frame -- short red and amber

trafficController :: Double -> Picture
trafficController t
  | round(t) `mod` 6 == 0 = trafficLight 1
  | round(t) `mod` 6 == 1 = trafficLight 1
  | round(t) `mod` 6 == 2 = trafficLight 2
  | round(t) `mod` 6 == 3 = trafficLight 3
  | round(t) `mod` 6 == 4 = trafficLight 3
  | round(t) `mod` 6 == 5 = trafficLight 4

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))
  
exercise2 :: IO ()
exercise2 = undefined

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    undefined
ground =  undefined
storage = undefined
box =     undefined

drawTile :: Integer -> Picture
drawTile = undefined

         
pictureOfMaze :: (Integer -> Integer -> Integer) -> Picture
pictureOfMaze = undefined

exercise3 :: IO ()
exercise3 = undefined
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 