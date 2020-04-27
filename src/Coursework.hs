module Coursework where

type Place = (String, (Float, Float), [Int])

{-
    Question 1:
    Get a list of all the respective place's name
-}
getNames :: [Place] -> [String]
getNames = map $ \(name, _, _) -> name

{-
    Question 2:
    Given the name of a place calculate its average rainfal
-}
averageRainfall :: [Place] -> String -> Float
averageRainfall [] _ = -1 -- Invalid name provided
averageRainfall (place:places) toFind
    -- Found place
    | toFind == name = avg rain
    -- Keep looking
    | otherwise = averageRainfall places toFind
    where
        (name, _, rain) = place

{-
    Question 3:
    Make a neat string containing each place 's rainfall data
-}
rainfallTbl :: [Place] -> String
rainfallTbl [] = []
rainfallTbl (place:places) = padRight 16 name ++ prepRain rain ++ "\n" ++ rainfallTbl places
    where 
        (name, _, rain) = place
        -- Smol helper function to make a nicer table appearance
        prepRain :: [Int] -> String
        prepRain [] = "|"
        prepRain (x:xs) = "|" ++ (padLeft 5 $ show x) ++ prepRain xs

{-
    Question 4:
    Return a list of places that were dry a given number of days ago
-}
dryPlaces :: [Place] -> Int -> [String]
dryPlaces places n = getNames $ filter (\(_, _, rain) -> rain !! n == 0) places

{-
    Question 5:
    Add a new field for rainfall data to each place, removing the oldest
-}
updateRecords :: [Place] -> [Int] -> [Place]
updateRecords [] _ = []
updateRecords (p:ps) (x:xs) = let (name, loc, rain) = p
    in (name, loc, x : init rain) : updateRecords ps xs

{-
    Question 6:
    Given an existing place, replace it with a new place  
-}
replace :: [Place] -> String -> Place -> [Place]
replace (p:ps) toReplace newPlace
    | toReplace == name = newPlace : ps
    | otherwise = p : replace ps toReplace newPlace
    where
        (name, _, _) = p

{-
    Question 7:
    Given a location return the closest place that was dry yesterday
-}
closestDry :: [Place] -> (Float, Float) -> Maybe Place
closestDry places fromLoc
    -- Possible there's no dry places yesterday
    | dryYesterday == [] = Nothing
    -- Find the closest by folding closerPlace over the list
    | otherwise = Just $ foldr1 (closerPlace fromLoc) dryYesterday
    where
        dryYesterday = [p | p@(_,_,rain) <- places, rain !! 0 == 0]

-- Given two places and a location, return the closer of the two to the given location
closerPlace :: (Float, Float) -> Place -> Place -> Place
closerPlace loc p1@(_,loc1,_) p2@(_,loc2,_)
    | let dist = sqrDist loc in dist loc1 > dist loc2 = p2
    | otherwise = p1

-- * * * * * * * * * * * * * * * HELPER FUNCTIONS

-- Get the average of a list of numbers
avg :: (Real a, Fractional b) => [a] -> b
avg xs = (realToFrac . sum $ xs) / (fromIntegral . length $ xs)

-- Pad a string to a given length and weight it
padLeft :: Int -> String -> String
padLeft n str
    | n > length str = (replicate (n - length str) ' ') ++ str 
    | otherwise = str

-- You get it
padRight :: Int -> String -> String
padRight n str
    | n > length str = str ++ (replicate (n - length str) ' ')
    | otherwise = str

-- Find the square of the distance between two points
sqrDist :: (Float, Float) -> (Float, Float) -> Float
sqrDist (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2