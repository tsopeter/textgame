module Lib where

getInt :: IO Int
getInt = do {
    x <- getLine ;
    return (read x) ;
    }
