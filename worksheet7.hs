
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show, Eq)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- MY CORE HERE

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sept | Oct | Nov | Dec
     deriving (Eq, Ord, Show)
data Season = Winter | Spring | Summer | Autumn
     deriving (Eq, Ord, Show)

season :: Month -> Season
season m
     | m >= Dec && m <= Feb = Winter
     | m >= Mar && m <= May = Spring
     | m >= Jun && m <= Aug = Summer
     | m >= Sept && m <= Nov = Autumn

numberOfDays :: Month -> Int -> Int
numberOfDays month year
     | month == Feb && year `mod` 2 == 0 = 29
     | month == Feb && year `mod` 2 /= 0 = 28
     | month == Apr || month == Jun || month == Sept || month == Nov = 30
     | otherwise = 31


data Point = Dot Float Float
     deriving (Show, Eq)

data PositionedShape = End | Vertex Point PositionedShape
     deriving (Show, Eq)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (Vertex (Dot x y) np) cx cy
     | np == End = (Vertex (Dot newx newy) End)
     | otherwise = (Vertex (Dot newx newy) (move np cx cy))
     where
          newx = x + cx
          newy = y + cy

numberOfNodes :: Tree -> Int
numberOfNodes (Node n t1 t2)
     | t1 /= Null && t2 /= Null = 1 + (numberOfNodes t1) + (numberOfNodes t2)
     | t1 == Null && t2 == Null = 1
     | t1 /= Null && t2 == Null = 1 + (numberOfNodes t1)
     | otherwise = 1 + (numberOfNodes t2)

isMember :: Int -> Tree -> Bool
isMember t (Node n t1 t2)
     | t == n = True
     | t1 /= Null && t2 /= Null = (isMember t t1) || (isMember t t2)
     | t1 == Null && t2 /= Null = (isMember t t2)
     | t1 /= Null && t2 == Null = (isMember t t1)
     | otherwise = False

leaves :: Tree -> [Int]
leaves (Node n t1 t2)
     | t1 == Null && t2 == Null = n : []
     | t1 == Null && t2 /= Null = (leaves t2)
     | t1 /= Null && t2 == Null = (leaves t1)
     | otherwise = (leaves t1) ++ (leaves t2) ++ []

inOrder :: Tree -> [Int]
inOrder (Node n t1 t2)

-- INCOMPLETE