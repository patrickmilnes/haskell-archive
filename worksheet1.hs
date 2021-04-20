timesTen :: Int -> Int
timesTen x = x * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle radius = pi * radius^2

volumeOfCynlinder :: Float -> Float -> Float
volumeOfCynlinder radius height = pi * radius^2 * height

distance :: Float -> Float -> Float -> Float -> Float
distance x1 x2 y1 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2 == True

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute x = if x >= 0 then x else -x