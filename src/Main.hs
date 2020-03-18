module Main where

import System.IO -- Need hFlush to handle output/input order
import Coursework

import Data

{--}

main :: IO ()
main = do 
    -- Start with reading & parsing file 
    contents <- readFile "Data.txt"
    let places = map read $ lines contents :: [Place] 
    loop places


loop :: [Place] -> IO ()
loop [] = putStrLn "Goodbye!" -- Edge case to end loop
loop places = do

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
        else do
            handle input places
            loop places


{-
    Function to handle different inputs
-}
handle :: String -> [Place] -> IO ()

-- List all places
handle "1" places = do
    putStrLn "Places:"
    mapM_ putStrLn $ getNames places 



