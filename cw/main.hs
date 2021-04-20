--
-- MATHFUN
-- UP899929
--



--
-- Types (define Place type here)
--
data Point = Dot Float Float
    deriving (Show, Eq)

data Place = Loc String Point [Float]
    deriving (Show, Eq)

testData :: [Place]
testData = [(Loc "London" (Dot 51.5 (-0.1)) [0, 0, 5, 8, 8, 0, 0]),
            (Loc "Cardiff" (Dot 51.5 (-3.2)) [12, 8, 15, 0, 0, 0, 2]),
            (Loc "Norwich" (Dot 52.6 1.3) [0, 6, 5, 0, 0, 0, 3]),
            (Loc "Birmingham" (Dot 52.5 (-1.9)) [0, 2, 10, 7, 8, 2, 2]),
            (Loc "Liverpool" (Dot 53.4 (-3.0)) [8, 16, 20, 3, 4, 9, 2]),
            (Loc "Hull" (Dot 53.8  (-0.3)) [0, 6, 5, 0, 0, 0, 4]),
            (Loc "Newcastle" (Dot 55.0  (-1.6)) [0, 0, 8, 3, 6, 7, 5]),
            (Loc "Belfast" (Dot 54.6  (-5.9)) [10, 18, 14, 0, 6, 5, 2]),
            (Loc "Glasgow" (Dot 55.9  (-4.3)) [7, 5, 3, 0, 6, 5, 0]),
            (Loc "Plymouth" (Dot 50.4  (-4.1)) [4, 9, 0, 0, 0, 6, 5]),
            (Loc "Aberdeen" (Dot 57.1  (-2.1)) [0, 0, 6, 5, 8, 2, 0]),
            (Loc "Stornoway" (Dot 58.2  (-6.4)) [15, 6, 15, 0, 0, 4, 2]),
            (Loc "Lerwick" (Dot 60.2  (-1.1)) [8, 10, 5, 5, 0, 0, 3]),
            (Loc "St Helier" (Dot 49.2  (-2.1)) [0, 0, 0, 0, 6, 10, 0])] 

--
--  Your functional code goes here
--

-- i
getNames :: [Place] -> [String]
getNames list = [name | (Loc name (Dot n e) values) <- list]

-- ii
averageRainfall :: String -> [Place] -> Float
averageRainfall target places = (foldr (+) 0 (values)) / 7
    where
        values = head ([values | (Loc name (Dot n e) values) <- places, name == target])

-- iii
getAllRainfallStr :: [Place] -> String
getAllRainfallStr places = foldr (++) "" nameValues
    where
        nameValues = [placeObjToStr place | place <- places]

-- Returns a formatted string with the name and the rainfall figures.
placeObjToStr :: Place -> String
placeObjToStr (Loc name (Dot x y) values)
    | (length name) < 8 = name ++ "\t\t" ++ (valuesStr)
    | (length name) >= 8 = name ++ "\t" ++ (valuesStr)
    where
        valuesStr = floatListToStr values

-- Returns a formatted string of the rainfall figures.
floatListToStr :: [Float] -> String
floatListToStr [] = "\n"
floatListToStr (x:xs) = (show x) ++ "\t" ++ (floatListToStr xs)

-- iv
getDryPlaces :: [Place] -> Int -> [String]
getDryPlaces places daysAgo
    | daysAgo > 0 && daysAgo < 9 = [name | (Loc name (Dot x y) values) <- places, values !! (daysAgo - 1) == 0]
    | otherwise = []

-- v
updateData :: [Place] -> [Float] -> [Place]
updateData _ [] = []
updateData [] _ = []
updateData (x:xs) (y:ys) = updatePlaceData x y : (updateData xs ys)

-- Returns place object with updated figure.
updatePlaceData :: Place -> Float -> Place
updatePlaceData (Loc name (Dot x y) values) newFigure
    | newFigure > 0 = (Loc name (Dot x y) (newFigure : (removeLastFigure values)))
    | otherwise = (Loc name (Dot x y) (0 : (removeLastFigure values)))

-- Returns a list with the last figure removed.
removeLastFigure :: [Float] -> [Float]
removeLastFigure (x:xs)
    | null xs = []
    | otherwise = x : (removeLastFigure xs)

-- vi
replace :: [Place] -> String -> Place -> [Place]
replace [] _ _ = []
replace (x:xs) placeToReplace newPlace
    | isPlaceEqual x placeToReplace = newPlace : xs
    | otherwise = x : (replace xs placeToReplace newPlace)

-- Returns bool baseed on if the name of the place is equal to the target.
isPlaceEqual :: Place -> String -> Bool
isPlaceEqual (Loc name (Dot x y) values) target = name == target

-- vii
getClosestDryPlace :: Point -> [Place] -> String
getClosestDryPlace location places = head [name | (Loc name (Dot x y) values) <- places, (getDistance (Loc name (Dot x y) values) location) == minDis]
    where
        minDis = getMinDistance places location

-- Returns the minumum distance between a location and all the places.
getMinDistance :: [Place] -> Point -> Float
getMinDistance places location = minimum [getDistance place location | place <- places]

-- Returns the distance between two points.
getDistance :: Place -> Point -> Float
getDistance (Loc name (Dot x2 y2) valuse) (Dot x1 y1) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2))

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print (getNames testData) -- display the names of all the places
demo 2 = putStrLn (show (averageRainfall "Cardiff" testData)) -- display, to two decimal places, the average rainfall in Cardiff
demo 3 = putStrLn (getAllRainfallStr testData)
demo 4 = print (getDryPlaces testData 2) -- display the names of all places that were dry two days ago
demo 5 = putStrLn (getAllRainfallStr (updateData testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0])) -- update the data with most recent rainfall 
         --[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 6 = putStrLn (getAllRainfallStr (replace testData "Plymouth" (Loc "Portsmouth" (Dot 50.8 (-1.1)) [0,0,3,2,5,2,1]))) 
        -- replace "Plymouth" with "Portsmouth" which has 
        -- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 7 = putStrLn (getClosestDryPlace (Dot 50.8 (-1.1)) testData) -- display the name of the place closest to 50.9 (N), -1.3 (E) 
         -- that was dry yesterday
demo 8 = do
    clearScreen
    printRainfallMap testData
    goTo (3, 80) -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--

printRainfallMap :: [Place] -> IO ()
printRainfallMap (x:xs) = do
    if (length xs) == 0 then
        return ()
    else do
        writeAt pos str
        printRainfallMap xs
    where
        str = ("+ " ++ name ++ " " ++ (show (averageRainfall name (x:xs))))
        pos = getScreenPos x
        name = extractName x

convertEToScreenPos :: Float -> Int
convertEToScreenPos i = round ((i + 7.4) * 10)

convertNToScreenPos :: Float -> Int
convertNToScreenPos i = round (50 - ((i - 48.2) * (50/12)))

getScreenPos :: Place -> ScreenPosition
getScreenPos (Loc name (Dot n e) values) = (convertEToScreenPos e, convertNToScreenPos n)

extractName :: Place -> String
extractName (Loc name (Dot n e) values) = name


--
-- Your user interface (and loading/saving) code goes here
--
 
