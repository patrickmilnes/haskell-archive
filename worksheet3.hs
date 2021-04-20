-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

-- MY CODE FROM HERE

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

exOr :: Bool -> Bool -> Bool
exOr False True = True
exOr False False = False
exOr True True = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y = x
ifThenElse False x y = y

sumNumbers :: Int -> Int
sumNumbers n
    | n == 1 = 1
    | n > 1 = n + sumNumbers (n - 1)
    | otherwise = error "Problomatic."

sumSquares :: Int -> Int
sumSquares n
    | n == 1 = 1
    | n > 1 = n^2 + sumSquares(n - 1)
    | otherwise = error "Problomatic."

power :: Int -> Int -> Int
power n p
    | p == 1 = n
    | p > 1 = n * power n (p - 1)
    | otherwise = error "Problem."

sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | x > y = 0
    | x <= y = y + sumFromTo x (y - 1)
    | otherwise = error "Error."

gcd :: Int -> Int -> Int
gcd n i
    | i == 0 = n
    | otherwise = gcd i (n `mod` i)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n g
    | n == g * g = g
    | g > 0 = findRoot n (g - 1)
    | otherwise = 0