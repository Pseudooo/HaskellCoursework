module Demo where

{-
    Demo function will server for marking
-}

import Coursework
import Data

import Text.Printf

demo :: Int -> IO ()

demo 1 = 
    putStrLn "A list of all Places:" >>
    putStrLn (join . getNames $ testData)

demo 2 = 
    printf "Average Rainfall in Cardiff: %.2f\n" (averageRainfall testData "Cardiff")

demo 3 = 
    putStrLn "Rainfall Table:" >>
    (putStrLn $ rainfallTbl testData)

demo 4 = 
    putStrLn "Places that were dry two days ago:" >>
    case dryPlaces testData 2 of
        [] -> putStrLn "None!"
        ps -> putStrLn $ join ps

demo 5 = 
    putStrLn "Original Data:" >>
    (putStrLn $ rainfallTbl testData ++ "\n") >>
    putStrLn "Updated Data:" >>
    (putStrLn $ rainfallTbl (updateRecords testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))

demo 6 =
    putStrLn "Original Data:" >>
    mapM_ (putStrLn . show) testData >>
    putStrLn "\nUpdated Data:" >>
    mapM_ (putStrLn . show) (replace testData "Plymouth" ("Portsmout", (50.8, -1.1), [0, 0, 3, 2, 5, 2, 1]))

demo 7 =
    putStrLn "The Closest dry place to (50.9, -1.3) is:" >>
    case closestDry testData (50.9, -1.3) of
        Nothing -> putStrLn "There are no dry places!"
        Just (name, _, _) -> putStrLn name

-- demo 8 = -- display the rainfall map





join :: [String] -> String
join vals = foldr1 (\x y -> (x ++ ", " ++ y)) vals