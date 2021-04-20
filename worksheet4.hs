import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

-- MY CODE FROM HERE

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (name, mark)
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (name, mark) = (name, 40)

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [i^2 | i <- nums]
    where
        nums = [1 .. n]

capitalise :: String -> String
capitalise s = [toUpper c | c <- s]

onlyDigits :: String -> String
onlyDigits s = [c | c <- s, isDigit c == True]

capMarks :: [StudentMark] -> [StudentMark]
capMarks list = [capMark student | student <- list]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents list = [(name, grade (name, mark)) | (name, mark) <- list]

duplicate :: String -> Int -> String
duplicate string n
    | n == 1 = string
    | otherwise = string ++ duplicate string (n - 1)

divisors :: Int -> [Int]
divisors n = [i | i <- list, n `mod` i == 0]
    where
        list = [1 .. n]

isPrime :: Int -> Bool
isPrime n
    | length (divisors n) == 2 = True
    | otherwise = False

split :: [(a, b)] -> ([a], [b])
split list = (listA, listB)
    where
        listA = [a | (a, b) <- list]
        listB = [b | (a, b) <- list]