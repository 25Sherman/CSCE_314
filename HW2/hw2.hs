{-
Name    : Milan Grewal
UIN     : 532003542
email   : msgrewal@gmail.com
-} 


-- Q1 -- GCD              -- Replace placeholder with your code.  
gcdd :: Integer -> Integer -> Integer
gcdd x y
    | x < 0 || y < 0 = error "One of the inputs is negative."
    | x > y = gcdd (x - y) y
    | x < y = gcdd x (y - x)
    | otherwise = x

                            

-- Q2 -- unwrapList . 
unwrapList :: [Maybe Integer] -> [Integer]
unwrapList  lst_maybe = [x | Just x <- lst_maybe]    -- Replace identity placeholder with your code.


-- Q3 -- drop nth 
dropNth :: Integer -> [Integer] -> [Integer]
dropNth f xs = helper f xs 1
    where helper _ [] _ = []
          helper f (y:ys) i
              | i `mod` f == 0 = helper f ys (i + 1)
              | otherwise = y : helper f ys (i + 1)


-- Q4 -- asInt. Initial code copy given
asInt :: String -> Int
asInt xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs)
              | x == '-' = negate $ loop acc xs
              | toDig x = loop (acc * 10 + ctoInt x) xs

toDig :: Char -> Bool
toDig c = c >= '0' && c <= '9'

ctoInt :: Char -> Int
ctoInt c = fromEnum c - fromEnum '0'



--loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  --in loop acc' xs


-- Q5 -- mergesort
halve :: [a] -> ([a], [a])
halve xs = splitAt((length xs + 1) `div` 2) xs         

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)   
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys  

mergesort :: Ord a => [a] -> [a]
mergesort [x] = [x]
mergesort xs = merge (mergesort l) (mergesort r)
    where (l, r) = halve xs  




-- Q6 -- capitalize
capitalize :: [String] -> [String]
capitalize = map (map upper')

upper' :: Char -> Char
upper' var
    | var >= 'a' && var <= 'z' = toEnum (fromEnum var - fromEnum 'a' + fromEnum 'A')
    | otherwise = var

