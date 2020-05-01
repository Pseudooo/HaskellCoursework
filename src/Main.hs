module Main where

-- Need `hFlush stdout` to handle inputs post compilation
import System.IO

import Text.Printf
import Text.Read

import Coursework
import Data
import Demo
import Rainfallmap

{-
    Main function will load the data from the text file
-}
main :: IO ()
main = do 

    -- Start with reading & parsing file 
    -- (File contains 1 place per line so need to deal with that)
    contents <- readFile "places.txt"
    let places = map read $ lines contents :: [Place]
    
    putStr $ rainfallTbl $ places

    newPlaces <- loop places

    -- Convert the new data into a string to be written to the file
    let newData = foldr1 (++) $ map ((++ "\n") . show) newPlaces
        in writeFile "places.txt" newData

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
    putStrLn " r : Reset the data to the original data set"
    putStrLn " d : Run an interactive demo"
    putStrLn " m : Run the rainfall map"
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
        else
            handle input places >>= loop

{-
    Function to handle each of the various options
-}
handle :: String -> [Place] -> IO [Place]

-- List all places
handle "1" places = do
    putStrLn "Places:"

    -- Display places as a numbered list
    let format = \(name, val) -> (show val ++ ". " ++ name)
        in mapM_ (putStrLn . format) $ zip (getNames places) [1..]

    return places

-- Get a place's avg rain
handle "2" places = do

    -- Get user's input
    putStr "Place Name: "
    hFlush stdout
    input <- getLine

    -- `averageRainfall` will return -1 to indicate invalid name
    let result = averageRainfall places input
    case result of
        (-1) -> putStrLn "Invalid Place!"
        x -> printf "%s's Average Rainfall: %4.2f\n" input x

    return places

-- Table of rainfall data
handle "3" places = do
    putStr $ rainfallTbl places
    return places

-- Dry places x days ago
handle "4" places = do
    -- Ask for days
    putStr "Days ago: "
    hFlush stdout
    input <- getLine

    -- Validate input
    case readMaybe input :: Maybe Int of

        -- Invalid
        Nothing -> putStrLn "Invalid Input!"

        -- Successful parse
        Just x -> if x < 0 || x > 7
            then putStrLn "Invalid Value!" >> putStrLn "0 < x <= 7"
            else printf "The Following were dry %d day(s) ago:\n" x >>
                (mapM_ putStrLn $ dryPlaces places x)

    return places

-- Update the rainfall data for each place
handle "5" places = do
    newData <- askData places
    return $ updateRecords places newData

-- Replace a given place with a new place
handle "6" places = do

    -- Ask for the place to replace
    putStr "Place to replace: "
    hFlush stdout
    toReplace <- getLine

    if elem toReplace $ getNames places
        -- Return new version with desired changes
        then askNewPlace >>= return . replace places toReplace
        -- No changes
        else putStrLn "Invalid Place!" >> return places

handle "7" places = do

    -- Ask for a user input
    putStr "Please enter a location (x, y): "
    hFlush stdout
    input <- getLine

    -- Attempt to parse the input
    case readMaybe input :: Maybe (Float, Float) of

        Nothing -> putStrLn "Invalid Location!"
        
        -- Closest dry returns Nothing for no-dry places
        Just loc -> case closestDry places loc of
            Nothing -> putStrLn "There were no dry places yesterday!"
            Just (name, (long, lat), _) -> 
                printf "The closest dry place is %s, located at (%.2f, %.2f)\n"
                    name long lat
                    
    return places

-- Reset data
handle "r" _ = do
    putStrLn "Resetting place data!"
    return testData

-- Perform demo utility
handle "d" places = do
    clearScreen
    putStrLn "The following is a interactive utility for the demo"
    putStrLn "Each \"page\" will be dedicated to each test that's required from the `demo` function"
    putStrLn "The values of `testData` that will be used for all tests are:"
    mapM_ (putStrLn . show) testData
    putStrLn "\nPress Enter to continue..."
    clearScreen
    performDemo 1
    return places

handle "m" places = putStrLn "Rainfall map:" >> displayLocations places >> return places

-- If an invalid option is given
handle _ places = do
    putStrLn "Invalid Option!"
    return places 

-- * * * * * * * * * * * * * * * * * * * * HELPER FUNCTIONS

{-
    Ask data asks the user for a new rainfall value for each record
-}
askData :: [Place] -> IO [Int]
askData [] = return [] -- Terminator
askData (p:ps) = do

    -- Ask for a value given a specific place
    let (name, _, _) = p 
        in printf "Rainfall for %s: " name
    hFlush stdout
    input <- getLine

    -- Attempt to parse
    case readMaybe input :: Maybe Int of

        -- Failed
        Nothing -> do
            putStrLn "Invalid Value"
            askData (p:ps) -- Ask again

        -- Success
        Just x -> askData ps >>= (return . (:) x)

-- Helper function to retrieve a validated place
askNewPlace :: IO Place
askNewPlace = do

    putStr "Place Name: "
    hFlush stdout
    name <- getLine

    -- Helper functions for validating location and rain data
    location <- askLocation
    rain <- askRain 7

    return (name, location, rain)

-- Helper function for asking and validating user input for Location
askLocation :: IO (Float, Float)
askLocation = do

    -- Ask for a location
    putStr "Location: "
    hFlush stdout
    loc <- getLine -- Collect input

    -- Attempt to parse
    case readMaybe loc :: Maybe (Float, Float) of
        Nothing -> putStrLn "Invalid Location!" >> askLocation
        Just x -> return x

-- Ask user for n rainfall values that are validated
askRain :: Int -> IO [Int]
askRain 0 = pure []
askRain n = do

    -- Ask
    printf "Rain-data for %d day(s) ago: " n
    hFlush stdout
    input <- getLine

    -- Attempt parse
    case readMaybe input :: Maybe Int of
        Nothing -> putStrLn "Invalid Value!" >> askRain n
        Just x -> askRain (n - 1) >>= (return . (:) x)

-- Helper function to provide a nice helpful demo utility
performDemo :: Int -> IO ()
performDemo 8 = return ()
performDemo n =
    (putStrLn $ "Demo: " ++ show n) >>
    demo n >>
    putStrLn "Press enter to continue..." >>
    getLine >>
    clearScreen >>
    (performDemo $ n + 1)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"
