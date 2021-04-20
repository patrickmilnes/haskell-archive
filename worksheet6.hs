 {- Week6.hs
 This module illustrates the use of functions as values
-}
import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- MY CODE HERE

mult10 :: [Int] -> [Int]
mult10 xs = map (*10) xs

onlyLowerCase :: String -> String
onlyLowerCase s = filter isLower s

orAll :: [Bool] -> Bool
orAll x = foldr (||) False x

sumSquares :: [Int] -> Int
sumSquares x = foldr (+) 0 (map (^2) x)

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (<=10) . filter (>=0)

squareRoots :: [Float] -> [Float]
squareRoots x = map (sqrt) (filter (>=0) x)

countBetween :: Float -> Float -> [Float] -> Int
countBetween x y z = length (filter (>=x) (filter (<=y) z))

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f x = length (filter (>=0) (map f x)) == length x

productSquareRoots :: [Float] -> Float
productSquareRoots x = foldr (*) 1 (squareRoots x)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (x:xs)
    | f x = xs
    | otherwise = x : removeFirst f xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f list = reverse (removeFirst f (reverse list))