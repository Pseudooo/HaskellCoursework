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




-- * * * * * * * * * * * * * * * HELPER FUNCTIONS

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / (fromIntegral . length $ xs)