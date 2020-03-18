module Data where

import Coursework

{-
    In the case where we're not loading from file this testData field can be
    used
-}
testData :: [Place]
testData = 
    [
        ("London",(51.0,-0.1),[0,0,5,8,8,0,0]),
        ("Cardiff",(51.5,-3.2),[12,8,15,0,0,0,2]),
        ("Norwich",(52.6,1.3),[0,6,5,0,0,0,3]),
        ("Birmingham",(52.5,-1.9),[0,2,10,7,8,2,2]),
        ("Liverpool",(53.4,-3.0),[8,16,20,3,4,9,2]),
        ("Hull",(53.8,-0.3),[0,6,5,0,0,0,4]),
        ("Newcastle",(55.0,-1.6),[0,0,8,3,6,7,5]),
        ("Belfast",(54.6,-5.9),[10,18,14,0,6,5,2]),
        ("Glasgow",(55.9,-4.3),[7,5,3,0,6,5,0]),
        ("Plymouth",(50.4,-4.1),[4,9,0,0,0,6,5]),
        ("Aberdeen",(57.1,-2.1),[0,0,6,5,8,2,0]),
        ("Stornoway",(58.2,-6.4),[15,6,15,0,0,4,2]),
        ("Lerwick",(60.2,-1.1),[8,10,5,5,0,0,3]),
        ("St Helier",(49.2,-2.1),[0,0,0,0,6,1,0])
    ]