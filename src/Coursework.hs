module Coursework where

type Place = (String, (Float, Float), [Int])

{-
    Question 1:
    Get a list of all the respective place's name
-}
getNames :: [Place] -> [String]
getNames places = [name | (name, _, _) <- places]

{-
    Question 2:
    Given the name of a place calculate its average rainfall
-}
averageRainfall :: [Place] -> String -> Maybe Float
averageRainfall [] _ = Nothing -- Invalid name provided
averageRainfall (place:places) toFind
    -- Found place
    | toFind == name = Just $ avg rain
    -- Keep looking
    | otherwise = averageRainfall places toFind
    where
        (name, _, rain) = place

{-
    Question 3:
    Make a neat string containing each place's rainfall data
-}
rainfallTbl :: [Place] -> String
rainfallTbl [] = []
rainfallTbl (place:places) = padRight 16 name ++ prepRain rain ++ "\n" ++ rainfallTbl places
    where 
        (name, _, rain) = place
        prepRain :: [Int] -> String
        prepRain [] = "|"
        prepRain (x:xs) = "|" ++ (padLeft 5 $ show x) ++ prepRain xs

-- * * * * * * * * * * * * * * * HELPER FUNCTIONS

-- Get the average of a list of numbers
avg :: (Real a, Fractional b) => [a] -> b
avg xs = (realToFrac . sum $ xs) / (fromIntegral . length $ xs)

-- Construct a whitespace string of length n
prepStr :: Int -> String
prepStr n = [' ' | _ <- [1..n]]

-- Pad a string to a given length and weight it
padLeft :: Int -> String -> String
padLeft n str
    | n > length str = (prepStr $ n - length str) ++ str 
    | otherwise = str
    
padRight :: Int -> String -> String
padRight n str
    | n > length str = str ++ (prepStr $ n - length str)
    | otherwise = str