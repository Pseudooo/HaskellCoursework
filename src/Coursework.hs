module Coursework where

type Place = (String, (Float, Float), [Int])

{-
    Question 1:
    Get a list of all places and their names
-}
getNames :: [Place] -> [String]
getNames places = [name | (name, _, _) <- places]

