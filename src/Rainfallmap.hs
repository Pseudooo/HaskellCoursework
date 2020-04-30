module Rainfallmap where

import Coursework
import Data
import Text.Printf

type Screen = [String]
type ScreenPosition = (Int, Int)
type ScreenObject = (ScreenPosition, String)

type Vec = (Float, Float)
type Scale = (Vec, Vec)

blankScreen :: Screen
blankScreen = replicate 51 $ replicate 80 ' '

printScreen :: Screen -> IO ()
printScreen [] = return ()
printScreen (x:xs) = putStrLn x >> printScreen xs

placeAll :: Screen -> [ScreenObject] -> Screen
placeAll s [] = s
placeAll s (x:xs) = placeObject (placeAll s xs) x

placeObject :: Screen -> ScreenObject -> Screen
placeObject [] _ = []
placeObject (r:rs) so@((x, y), lbl)
    | y == 50 = (take (x - 1) r ++ "X" ++ drop x r) : rs
    | otherwise = r : placeObject rs ((x, y + 1), lbl)

applyScale :: Scale -> Location -> ScreenPosition
applyScale ((a, b), (c, d)) (x, y) = (round $ a*x+b, round $ c*y+d)

computeScale :: [Location] -> Scale
computeScale locs = ((a, b), (c, d))
    where
        longs = [x | (x, _) <- locs]
        lats = [x | (_, x) <- locs]

        maxLong = maximum longs
        minLong = minimum longs

        maxLat = maximum lats
        minLat = minimum lats

        a = 80 / (maxLong - minLong)
        c = 50 / (maxLat - minLat)

        b = -1 * a * minLong
        d = -1 * c * minLat

prepLabel :: String -> Float -> String
prepLabel = printf "%s: %.2f"

test :: IO ()
test = do

    let locs = [x | (_, x, _) <- testData]
    let s = computeScale locs

    let toPrint = [(applyScale s loc, prepLabel name $ avg rain) | (name, loc, rain) <- testData]
    printScreen $ placeAll blankScreen toPrint

    -- printScreen $ markAll (map (applyScale s) locs) blankScreen
    return ()

