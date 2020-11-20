module Lib where

data H = H



--get number
getInt :: IO Int
getInt = do {
    x <- getLine ;
    return (read x) ;
    }
