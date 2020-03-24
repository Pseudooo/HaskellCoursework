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
    putStr $ rainfallTbl $ places

    newPlaces <- loop places

    -- Convert the new data into a string to be written to the file
    let newData = foldr1 (++) $ map ((++ "\n") . show) newPlaces
        in writeFile "Data.txt" newData
 
{-
    Loop function will serve to continuously ask the user what they want
    to do until they're done, at which point they're able to quit.
-}
loop :: [Place] -> IO [Place]
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
    putStrLn " 6 : Replace place"
    putStrLn " 7 : Closest Dry Place"
    putStrLn "" -- Padding

    -- Ask the user for their option
    putStr "Option: "
    hFlush stdout
    input <- getLine
    putStrLn "" -- Padding

    -- Allow the user to quit the application
    if input == "q"
        then return places
        else do
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
    return places

-- Get a place's avg rain
handle "2" places = do
    putStr "Place Name: "
    hFlush stdout
    input <- getLine
    let result = averageRainfall places input
    case result of
        Nothing -> putStrLn "Invalid Place!"
        Just x -> printf "%s's Average Rainfall: %4.2f\n" input x
    return places

-- Table of rainfall data
handle "3" places = do
    putStrLn $ rainfallTbl places
    return places

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
    return places

-- Update the rainfall data for each place(
handle "5" places = do
    newData <- askData places
    let newPlaces = updateRecords places newData
    return newPlaces

handle "6" places = do

    putStr "Place to replace: "
    hFlush stdout
    toReplace <- getLine

    if elem toReplace $ getNames places
        then do
            newPlace <- askNewPlace
            return $ replace places toReplace newPlace
        
        else do
            putStrLn "Invalid Place!"
            return places

handle "7" places = do

    putStr "Please enter a location: "
    hFlush stdout
    input <- getLine

    case readMaybe input :: Maybe (Float, Float) of
        Nothing -> putStrLn "Invalid Location!"
        Just loc -> let (name, (long, lat), _) = closestDry places loc
            in printf "The closest dry place is %s, located at (%.2f, %.2f)\n" name long lat 

    return places

-- If an invalid option is given
handle _ places = do
    putStrLn "Invalid Option!"
    return places 

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

askNewPlace :: IO Place
askNewPlace = do

    putStr "Place Name: "
    hFlush stdout
    name <- getLine

    location <- askLocation

    rain <- askRain 7

    let newPlace = (name, location, rain) :: Place
    pure newPlace

askLocation :: IO (Float, Float)
askLocation = do

    putStr "Location: "
    hFlush stdout
    loc <- getLine -- Collect input

    case readMaybe loc :: Maybe (Float, Float) of

        Nothing -> do
            putStrLn "Invalid Location!\n"
            askLocation

        Just x -> pure x

askRain :: Int -> IO [Int]
askRain 0 = pure []
askRain n = do

    printf "Rain-data for %d day(s) ago: " n
    hFlush stdout
    input <- getLine

    case readMaybe input :: Maybe Int of

        Nothing -> do
            putStrLn "Invalid Value!"
            askRain n

        Just x -> do
            rest <- askRain $ n - 1
            pure $ x : rest
