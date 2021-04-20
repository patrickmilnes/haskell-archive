{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

-- MY CODE HERE

headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:(x:xs)

rotate :: [a] -> [a]
rotate (y:(x:xs)) = x:y:xs
rotate xs = xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

countElems :: Int -> [Int] -> Int
countElems n [] = 0
countElems n (x:xs)
    | n == x = 1 + countElems n xs
    | otherwise = countElems n xs

removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x : removeAll n xs

type StudentMark = (String, Int)

listMark :: String -> [StudentMark] -> [Int]
listMark s [] = []
listMark s ((name, mark): xs)
    | s == name = mark : listMark s xs
    | otherwise = listMark s xs

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

    