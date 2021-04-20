absolute :: Int -> Int
absolute x
    | x >= 0    = x
    | otherwise = -x

sign :: Int -> Int
sign x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z              = 3
    | x == y || y == z || z == x    = 2
    | otherwise                     = 1

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths l1 l2 l3 = p1 + p2 + p3
    where
        p1 = sqrt((l1^2) * 2)
        p2 = sqrt((l2^2) * 2)
        p3 = sqrt((l3^2) * 2)

taxiFare :: Int -> Float
taxiFare distance
    | distance <= 10 = 2.20 + fromIntegral distance * 0.5
    | distance > 10 = 2.20 + 10 * 0.5 + (fromIntegral distance - 10) * 0.30

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage num1 num2 num3
    | num1 > average && num2 > average && num3 > average    = 3
    | num1 > average && num2 > average                      = 2
    | num2 > average && num3 > average                      = 2
    | num1 > average && num3 > average                      = 2
    | num1 < average && num2 < average && num3 < average    = 1
    | otherwise                                             = 0
    where
        average = (num1 + num2 + num3) `div` 3

validDate :: Int -> Int -> Bool
validDate day month
    | day < 0                       = False
    | month == 2 && day <= 29       = True
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 && day <= 31     = True
    | month == 4 || month == 6 || month == 9 || month == 11 && day <= 30                                  = True
    | otherwise                     = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 && leap == 0 = 29
    | month == 2 && leap /= 0 = 28
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | otherwise = 31 
    where
        leap = fromIntegral year `mod` 4