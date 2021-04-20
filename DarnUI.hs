copyFile :: IO ()
copyFile = do
    putStr "Enter file name: "
    f <- getLine
    putStr "Enter new file name: "
    nf <- getLine
    contents <- readFile f
    writeFile nf contents

getPaths :: String -> IO String
getPaths = do
    putStr "Enter location of ini to install: "
    f <- getLine
    putStr "Enter location of Fallout.ini: "
    f1 <- getLine
    putStr "Enter location of Fallout_default.ini: "
    f2 <- getLine

copyContents :: String -> String -> IO ()
copyContents source location = do
    contents <- readFile 
    