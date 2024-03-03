{-
Name    : Milan Grewal
UIN     : 532003542
email   : msgrewal@tamu.edu
-} 


-- Q1 -- Histogram
data BarChart = BarChart {
    labels  :: [String],
    counts  :: [Integer]
}

bar1 = BarChart ["Cat", "Dog", "Others"] [5, 4, 3]

-- Q1 answer goes here

instance Show BarChart where
    show (BarChart ls cs) =
        let maxLabelLength = maximum (map length ls)
            maxCount = maximum cs
        in unlines (printBars ls cs maxLabelLength maxCount)

printBars :: [String] -> [Integer] -> Int -> Integer -> [String]
printBars [] [] _ _ = []
printBars (l:ls) (c:cs) maxLabelLength maxCount =
    let starsCount = fromIntegral (5 * c `div` maxCount)
        stars = replicate (fromIntegral starsCount) '*'
        padding = replicate (maxLabelLength - length l + 4) ' '
    in (l ++ padding ++ stars) : printBars ls cs maxLabelLength maxCount



-- Define an instance of show for this histogram


-- Q2 : prefix_calc with exception handling

-- Define your exceptions here

prefix_calc :: String -> Integer -> Integer -> Either String Integer

prefix_calc op x y 
    | op == "+" = Right (x + y)
    | op == "-" = Right (x - y)
    | op == "*" = Right (x * y)
    | op == "/" && y == 0 = Left "Exception"
    | otherwise = Right (x `div` y)


evalq2 :: String -> Integer                            

evalq2 input =
    case words input of
        (op:x:y:_) -> 
            case op of
                "+" -> read x + read y
                "-" -> read x - read y
                "*" -> read x * read y
                "/" -> if y == "0" then 0 else read x `div` read y
                _ -> error "Invalid"
        _ -> error "Invalid"


-- Q3 simple type classes

class Animal a where
    speak :: a -> String
    name :: a -> String

data Cat = Cat String

data Dog = Dog String

data Mouse  = Mouse String

instance Animal Cat where
    speak _ = "Meow"
    name (Cat x) = x

instance Animal Dog where
    speak _ = "Woof"
    name (Dog x) = x

instance Animal Mouse where 
    speak _ = "Squeak"
    name (Mouse x) = x

whatDoAnimalsSay :: Animal ani => ani -> String

whatDoAnimalsSay ani = "I am " ++ name ani ++ " and I " ++ speak ani



-- Q4 

-- Instances go here. Do not use deriving for this question.


data MyInt = MyInt Int

instance Eq MyInt where
    (MyInt x) == (MyInt y) = x == y

instance Real MyInt where
    toRational (MyInt x) = toRational x


instance Ord MyInt where
    compare (MyInt x) (MyInt y) = compare x y


instance Num MyInt where
    (MyInt x) + (MyInt y) = MyInt (x + y)
    (MyInt x) - (MyInt y) = MyInt (x - y)
    (MyInt x) * (MyInt y) = MyInt (x * y)
    negate (MyInt x) = MyInt (negate x)
    abs (MyInt x) = MyInt (abs x)
    signum (MyInt x) = MyInt (signum x)
    fromInteger = MyInt . fromInteger


instance Enum MyInt where
    succ (MyInt x) = MyInt (succ x)
    pred (MyInt x) = MyInt (pred x)
    toEnum = MyInt
    fromEnum (MyInt x) = x
    enumFrom (MyInt x) = map MyInt [x..]
    enumFromThen (MyInt x) (MyInt y) = map MyInt [x,y..]
    enumFromTo (MyInt x) (MyInt y) = map MyInt [x..y]
    enumFromThenTo (MyInt x) (MyInt y) (MyInt z) = map MyInt [x,y..z]


instance Integral MyInt where
    quot (MyInt x) (MyInt y) = MyInt (quot x y)
    rem (MyInt x) (MyInt y) = MyInt (rem x y)
    div (MyInt x) (MyInt y) = MyInt (div x y)
    mod (MyInt x) (MyInt y) = MyInt (mod x y)
    quotRem (MyInt x) (MyInt y) = let (q, r) = quotRem x y in (MyInt q, MyInt r)
    divMod (MyInt x) (MyInt y) = let (q, r) = divMod x y in (MyInt q, MyInt r)
    toInteger (MyInt x) = toInteger x

instance Show MyInt where
    show (MyInt x) = show x





-- Q5
data Expr  =  Expr (Expr -> Expr -> Expr) Expr Expr
           |  Val Int
           
instance Show Expr where
    show (Val x) = show x

plus (Val x) (Val y) = Val (x + y)
minus :: Expr -> Expr -> Expr
minus (Val x) (Val y) = Val (x - y)
mul (Val x) (Val y) = Val (x * y)
divide (Val x) (Val y) = Val (div x y)

eval e1@(Val x) = e1
eval (Expr op e1 e2) = op (eval e1) (eval e2)

{-  (200 - (10 + 6)) / (4 * 3) 

List function calls for expression 1

1.) eval Expr divide (Expr minus (Val 200) (Expr plus (Val 6) (Val 10))) (Expr mul (Val 3) (Val 4))
2.) eval (Expr minus (Val 200) (Expr plus (Val 6) (Val 10)))
3.) eval (Val 200)
4.) eval (Expr plus (Val 6) (Val 10))
5.) eval (Val 6)
6.) eval (Val 10)
7.) plus (Val 6) (Val 10)
8.) minus (Val 200) (Val(6 + 10))
9.) eval (Expr mul (Val 3) (Val 4))
10.) eval (Val 3)
11.) eval (Val 4)
12.) mul (Val 3) (Val 4)
13.) divide (Val (200 - (6 + 10))) (Val (3 * 4))

-}


{-   2 + (6 *(3 - (4/2))) = 

List function calls for expression 2

1.) eval (Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))))
2.) eval (Val 2)
3.) eval (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))))
4.) eval (Val 6)
5.) eval (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))))
6.) eval (Val 3)
7.) eval (Expr divide (Val 4) (Val 2)))
8.) eval (Val 4)
9.) eval (Val 2)
10.) divide (Val 4) (Val 2)
11.) minus (Val 3) (Val (4 div 2))
12.) mul (Val 6) (Val (3 - (4 div 2)))
13.) plus (Val 2) (Val (6 * (3 - (4 div 2))))

-}


a = Expr divide (Expr minus (Val 200) (Expr plus (Val 6) (Val 10))) (Expr mul (Val 3) (Val 4))
b = Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))


