module Main where

-- Need `hFlush stdout` to handle inputs post compilation
import System.IO

import Text.Printf
import Text.Read

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
    putStrLn " 4 : Dry Places (Given day)"
    putStrLn " 5 : Update Rainfall Data"
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

-- Get a place's avg rain
handle "2" places = do
    putStr "Place Name: "
    hFlush stdout
    input <- getLine
    let result = averageRainfall places input
    case result of
        Nothing -> putStrLn "Invalid Place!"
        Just x -> printf "%s's Average Rainfall: %4.2f\n" input x
    pure places

-- Table of rainfall data
handle "3" places = do
    putStrLn $ rainfallTbl places
    pure places

-- Dry places x days ago
handle "4" places = do
    -- Ask for days
    putStr "Days ago: "
    hFlush stdout
    input <- getLine

    -- Use of readMaybe to verify parse
    let result = readMaybe input :: Maybe Int
    case result of -- Handle invalids
        Nothing -> putStrLn "Invalid Input!"
        Just x -> do 
            if x > 0 && x <= 7 -- Check valid value is given
                then do
                    printf "The following were dry %d day(s) ago:\n" x
                    mapM_ putStr $ dryPlaces places (x - 1) -- Shift x for indexing
                else putStrLn "Invalid Value\n0 < x <= 7"
    pure places

handle "5" places = do
    newData <- askData places
    let newPlaces = updateRecords places newData
    pure newPlaces


-- If an invalid option is given
handle _ places = do
    putStrLn "Invalid Option!"
    pure places

-- * * * * * * * * * * * * * * * * * * * * HELPER FUNCTIONS

{-
    Ask data asks the user for a new rainfall value for each record
-}
askData :: [Place] -> IO [Int]
askData [] = return []
askData (p:ps) = do
    let (name, _, _) = p 
        in printf "Rainfall for %s: " name
    hFlush stdout
    input <- getLine
    let val = readMaybe input :: Maybe Int
    case val of
        Nothing -> do
            putStrLn "Invalid Value"
            askData (p:ps)

        Just x -> do
            remain <- askData ps
            pure $ x : remain