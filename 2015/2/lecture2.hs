-- use let to define a local var scoped within an expression
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

-- use where to define a local var scoped over multiple guarded branches
frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- calculate the sum until it's greater than 20
sum20 :: [Int] -> Int
sum20 n = go 0 n
  where go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (x:xs)
          | acc >= 20 = acc
          | otherwise = go (acc+x) xs