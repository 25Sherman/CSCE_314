module Main where
{-
Name    : Milan Grewal
UIN     : 532003542
email   : msgrewal@tamu.edu
-} 


-- Q1 -- CSV file. Grade claculations.
--main :: IO ()
--main = undefined

import System.Environment
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Data.List (transpose)

type Student = (String, [Int])

parseFile :: String -> [Student]
parseFile content = map parseLine (tail $ lines content)  -- Skipping header line
    where
        parseLine :: String -> Student
        parseLine line = (name, map read grades)
            where
                (name:grades) = splitOn "," line


calcTotal :: Student -> Int
calcTotal (_, grades) = sum grades


calcAvg :: [Student] -> [Double]
calcAvg students = map (\x -> fromIntegral x / fromIntegral numStudents) sums
    where
        sums = foldl1 (zipWith (+)) $ map snd students
        numStudents = length students


combine :: [Student] -> [Int] -> [(String, [Int], Int)]
combine students totals = zipWith (\(name, grades) total -> (name, grades, total)) students totals


format :: [(String, [Int], Int)] -> String
format dataRows = unlines $ map formatRow dataRows
    where
        formatRow (name, grades, total) = name ++ "," ++ concatMap (\x -> printf "%d," x) grades ++ show total

hwAvg :: [Student] -> [Double]
hwAvg students = map (\i -> fromIntegral (sum (map (!! i) grades)) / fromIntegral numStudents) [0..2]
    where
        grades = map snd students
        numStudents = length students

formatHwAvg :: [Double] -> String
formatHwAvg averages = "Average," ++ concatMap (\x -> printf "%.2f," x) averages        

-- Main function
main :: IO ()
main = do
    args <- getArgs
    let inputFile = args !! 0
        outputFile = args !! 1
    inputContent <- readFile inputFile
    let students = parseFile inputContent
        totals = map calcTotal students
        averages = calcAvg students
        combinedData = combine students totals
        homeworkAverages = hwAvg students
        outputContent = "Name,HW1,HW2,HW3,Total\n" ++ format combinedData ++ formatHwAvg homeworkAverages
    writeFile outputFile outputContent

-- Q2 : matrix Multiplication

data Matrix a = Matrix [[a]]

-- define instance of (*) for matrices. 

instance Num a => Num (Matrix a) where
    (Matrix a) * (Matrix b) = Matrix [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]
    fromInteger x = Matrix [[fromInteger x]]
    (Matrix a) + (Matrix b) = undefined -- implmented to fix error
    (Matrix a) - (Matrix b) = undefined 
    abs (Matrix a) = undefined 
    signum (Matrix a) = undefined 

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

instance (Num a, Eq a) => Eq (Matrix a) where
    (Matrix a) == (Matrix b) = a == b




-- Q3 tree traversals
data Tree a b = Leaf a | Branch b (Tree a b) (Tree a b)

-- This is a generalized version of expression tree from last week.

preorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder leafFn branchFn tree = preorder' tree
  where
    preorder' (Leaf x) = [leafFn x]
    preorder' (Branch z left right) = branchFn z : preorder' left ++ preorder' right

inorder :: (a -> c) -> (b -> c) -> Tree a b -> [c] 
inorder leafFn branchFn tree = inorder' tree
  where
    inorder' (Leaf x) = [leafFn x]
    inorder' (Branch z left right) = inorder' left ++ [branchFn z] ++ inorder' right

postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder leafFn branchFn tree = postorder' tree
  where
    postorder' (Leaf x) = [leafFn x]
    postorder' (Branch z left right) = postorder' left ++ postorder' right ++ [branchFn z]

