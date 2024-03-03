{-
Name    : Milan Grewal
UIN     : 532003542
email   : msgrewal@tamu.edu
-} 


-- Q1 -- Fixing quicksort
                        -- Add type anotation
qsort  :: [Integer] -> [Integer]
qsort  []     = []
qsort   (x:xs) = qsort   zs ++ [x] ++ qsort  ys
           where
             ys = [a | a <- xs, a <= x]
             zs = [b | b <- xs, b > x]

quicksort  :: [Integer] -> [Integer]
quicksort  []     = []
quicksort   (x:xs) = quicksort   zs ++ [x] ++ quicksort  ys
           where
             ys = [a | a <- xs, a <= x]
             zs = [b | b <- xs, b > x]



-- Q2 -- randomCalculation. Revise number systems if needed
randomCalc :: Integer -> Integer
randomCalc x =   -- Replace identity placeholder with your code.
    if 1 <= x && x <= 9
       then 
            let n = x 
                nn = read (replicate 2 (head (show x))) :: Integer
                nnn = read (replicate 3 (head (show x))) :: Integer
            in n + nn + nnn
        else error "Invalid input. Input must be between 1 and 9"
      
      

-- Q3 -- nth Lucus number
lucas :: Integer -> Integer
lucas 0 = 2
lucas 1 = 1
lucas x = lucas (x - 1) + lucas (x - 2)   -- Replace identity placeholder with your code.



-- Q4 -- average of a List
average :: [Double] -> Double    -- Replace identity placeholder with your code.
average x = sum x / fromIntegral(length x) 



-- Q5 -- Grade calculation type

data Scores = Scores {
  classAverage :: Double,
  hwAverage :: Double,
  midterm :: Double,
  final :: Double
}

gradeGroups :: Double -> Double -> Double -> Double -> Scores
gradeGroups ca hw mid fin = Scores {
  classAverage = ca,
  hwAverage = hw,
  midterm = mid,
  final = fin
}



-- Q6 -- total score calculation for grade
totalPoints :: Scores -> Double  -- Replace identity placeholder with your code.
totalPoints scores = classAverage scores * 0.05 +
                     hwAverage scores * 0.45 +
                     midterm scores * 0.25 +
                     final scores * 0.25      
        


-- Q7 -- Polynomial calculator
type Polynomial = [Int]

polyCalcFactory :: Polynomial -> (Int -> Int)
polyCalcFactory coeffs x = calculatePoly x coeffs 0

calculatePoly :: Int -> Polynomial -> Int -> Int
calculatePoly _ [] result = result
calculatePoly x (c:cs) result = calculatePoly x cs (result * x + c)




trace :: String -> a -> a
trace message expr = 
    let result = trace message expr
    in  result

-- Q4 -- average of a List
average :: [Double] -> Double
average x = 
    let len = length x
        avg = sum x / fromIntegral len
    in  trace ("Length of the list: " ++ show len ++ ", Sum of the elements: " ++ show (sum x)) avg