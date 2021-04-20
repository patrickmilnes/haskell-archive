helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9
 
celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

-- MY CODE HERE

greeting :: IO ()
greeting = do
    name <- getLine
    putStrLn ("Hello," ++ name)

addTwoNumbers :: IO ()
addTwoNumbers = do
    n1 <- getInt
    n2 <- getInt
    putStrLn (show (n1 + n2))

copyFile :: IO ()
copyFile = do
    putStr "Enter file name: "
    f <- getLine
    putStr "Enter new file name: "
    nf <- getLine
    contents <- readFile f
    writeFile nf contents

buildList :: [String] -> IO ()
buildList list = do
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else do
        print (str : list)
        buildList (str : list)
        
listBuilder :: IO ()
listBuilder = buildList []

addNum :: Int ->  Int -> IO Int
addNum 0 acc = return acc
addNum n acc = do putStr "Enter next number: "
                  i <- getInt
                  addNum (n - 1) (acc + i)

addNumbers :: IO ()
addNumbers = do putStr "How many numbers? "
                n <- getInt
                s <- addNum n 0
                putStrLn ("The sum is " ++ (show s))

