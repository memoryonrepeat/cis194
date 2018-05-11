{-# OPTIONS_GHC -Wall #-}
module HW02 where

{-import Data.List-}

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length( filter (\(x,y) -> x==y) (zip a b))

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\color -> length( filter (\peg -> peg==color) (code))) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess = sum( map (\(x,y) -> min x y) (zip (countColors secret) (countColors guess) ))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exactMatches secret guess) ((matches secret guess) - (exactMatches secret guess))

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code
  | (Move guess exact nonExact) == getMove code guess  = True
  | otherwise 										   = False

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move code = filter (isConsistent move) code 

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes length = 

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined