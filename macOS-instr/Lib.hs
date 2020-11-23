module Lib where

import Data.Char

--data for commands
data Command = Look | Exit | DontUnderstand | SeeMap | Die | Scan
    | Pick | CheckInventory | Eat | Go | SeeHealth | Make deriving (Show)

--misc functions

--get number
getInt :: IO Int
getInt = do {
    x <- getLine ;
    return (read x) ;
    }

--turns a array of strings to lowercase    
stringToLower :: [[Char]] -> [[Char]]
stringToLower [] = []
stringToLower (x:xs) = [(map (\ i -> toLower i) x)] ++ (stringToLower xs)

--smoothie description
smoothieDesc :: IO ()
smoothieDesc = do {
    putStrLn "Ready to make the smoothie ? " ;
    putStrLn "Combine any 1 to 3 items to create a smoothie. " ;
    putStrLn "Then use any integer from 0 to 10 to blend the smoothie. " ;
    putStrLn "The best smoothie will be exactly 30 points . ";
    }
               
