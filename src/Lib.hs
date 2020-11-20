module Lib where

import Data.Char

--data for commands
data Command = Look | Exit | DontUnderstand | SeeMap | Die | Scan
    | Pick | CheckInventory | Eat deriving (Show)

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
