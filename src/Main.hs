module Main where

-- Need `hFlush stdout` to handle inputs post compilation
import System.IO

import Coursework
import Data

{-
    Main function will load the data from the text file
-}
main :: IO ()
main = do 
    -- Start with reading & parsing file 
    contents <- readFile "Data.txt"
    let places = map read $ lines contents :: [Place] 
    loop places

loop :: [Place] -> IO ()
loop places = do

    -- Display possible options

    putStr "Option: " -- Ask for an option
    hFlush stdout
    option <- getLine

    -- Make any potential modifications to the data
    let newData = process places option

    -- Loop round with modified data
    loop newData

{-
    Loop function will serve to continuously ask the user what they want
    to do until they're done, at which point they're able to quit.
-}
loop :: [Place] -> IO ()
loop places = do

    putStrLn "" -- Padding
    putStrLn "Please select one of the following options to proceed:"

    -- Options that the user can perform
    putStrLn " q : Quit the application"
    putStrLn " 1 : List All Places"
    putStrLn "" -- Padding

    -- Ask the user for their option
    putStr "Option: "
    hFlush stdout
    input <- getLine
    putStrLn "" -- Padding

    -- Allow the user to quit the application
    if input == "q"
        then return ()
        else do -- Otherwise loop
            handle input places
            loop places


{-
    Function to handle each of the various options
-}
handle :: String -> [Place] -> IO ()

-- List all places
handle "1" places = do
    putStrLn "Places:"
    mapM_ putStrLn $ getNames places

-- If an invalid option is given
handle _ _ = putStrLn "Invalid Option!"



