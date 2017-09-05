{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0          = []
  | otherwise       = (lastDigit n) : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
-- NOTE:
-- Without base cases, the notation (x:xs:xss) will throw non-exhaustive pattern error
-- Because (x:xs:xss) only match lists with at least 2 elements
-- Read more about list notation in "The Haskell road to logic math and programming"
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs:xss) = x : 2*xs : doubleEveryOther(xss)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = case x<10 of
  True -> x + sumDigits(xs)
  False -> sumDigits(toRevDigits x) + sumDigits(xs)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = sumDigits(doubleEveryOther(toRevDigits n)) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- Why ++ works instead of : ?
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a

-- hanoi4 (Reve's puzzle) To-do: 
-- Use 3-peg case with brute force (on number of discs to move each time and the intermediary disc to use) to find a possible minimum solution for 4-peg case
-- Or use Frame–Stewart algorithm which was proven to be optimal
-- To fix the type converting issue

{-hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n 
  | n <= n-round(sqrt(2*n+1))+1		= [(a,b)]
  | otherwise 						= hanoi4 (n-round(sqrt(2*n+1))+1) a c b d ++ hanoi4 (round(sqrt(2*n+1))+1) a b c d ++ hanoi4 (n-round(sqrt(2*n+1))+1) c b a d
-}