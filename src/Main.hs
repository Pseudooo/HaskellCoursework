module Main where

-- Need `hFlush stdout` to handle inputs post compilation
import System.IO

import Text.Printf

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
    putStrLn " 2 : View average rainfall"
    putStrLn " 3 : Rainfall Table"
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
            newPlaces <- handle input places
            loop newPlaces


{-
    Function to handle each of the various options
-}
handle :: String -> [Place] -> IO [Place]

-- List all places
handle "1" places = do
    putStrLn "Places:"
    mapM_ putStrLn $ getNames places
    pure places

handle "2" places = do
    putStr "Place Name: "
    hFlush stdout
    input <- getLine
    let result = averageRainfall places input
    case result of
        Nothing -> putStrLn "Invalid Place!"
        Just x -> printf "%s's Average Rainfall: %4.2f\n" input x
    pure places


handle "3" places = do
    putStrLn $ rainfallTbl places
    pure places

-- If an invalid option is given
handle _ places = do
    putStrLn "Invalid Option!"
    pure places



