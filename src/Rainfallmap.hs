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
blankScreen = replicate 51 $ replicate 81 ' '

printScreen :: Screen -> IO ()
printScreen [] = return ()
printScreen (x:xs) = putStrLn x >> printScreen xs

placeAll :: Screen -> [ScreenObject] -> Screen
placeAll s [] = s
placeAll s ((loc, lbl):xs) = placeLabel (placeObject (placeAll s xs) loc) lbl loc

placeObject :: Screen -> ScreenPosition -> Screen
placeObject [] _ = []
placeObject (r:rs) (x, y)
    | y == 50 = (take (x - 1) r ++ "+" ++ drop x r) : rs
    | otherwise = r : placeObject rs (x, y + 1)

placeLabel :: Screen -> String -> ScreenPosition -> Screen
placeLabel [] _ _ = []
placeLabel (r:rs) lbl (x, y)
    | y == 50 = markLabel r lbl (x, y) : rs
    | otherwise = r : placeLabel rs lbl (x, y + 1)

markLabel :: String -> String -> ScreenPosition -> String
markLabel row lbl (x, y)
    | x > 40 = take (x - 1 - length lbl) row ++ lbl ++ drop (x - 2) row
    | otherwise = take (x + 1) row ++ lbl ++ drop (x + 1 + length lbl) row


applyScale :: Scale -> Location -> ScreenPosition
applyScale ((a, b), (c, d)) (y, x) = (round $ a*x+b, round $ c*y+d)

computeScale :: [Location] -> Scale
computeScale locs = ((a, b), (c, d))
    where
        longs = [x | (_, x) <- locs]
        lats = [x | (x, _) <- locs]

        maxLong = maximum longs
        minLong = minimum longs

        maxLat = maximum lats
        minLat = minimum lats

        {-
            Solving simultanous equations
            a*maxLong+b=80 & a*minLong+b=0
            c*maxLat+d=50 & c*minLat+d=0
        -}
        a = 80 / (maxLong - minLong)
        c = 50 / (maxLat - minLat)
        b = -1 * a * minLong
        d = -1 * c * minLat

prepLabel :: Place -> String
prepLabel (name, _, rain) = printf "[%s: %.2fmm]" name (avg rain :: Float)

displayLocations :: [Place] -> IO ()
displayLocations places = printScreen $ placeAll blankScreen toDraw
    where 
        scl = applyScale $ computeScale [loc | (_, loc, _) <- places]
        toDraw = [(scl loc, prepLabel p) | p@(_, loc, _) <- places]
