module Main where

--import
import Maps
import Lib

data State = Alive | Dead | UnusedState

checkState :: State -> Bool
checkState _ = True
checkState Dead = False

main :: IO ()
main = do {
    gameLoop (Plains, Alive)
    }
    
gameLoop :: (Location, State) -> IO ()
gameLoop (curLocation, curState) = do {
    putStrLn ("You are currently at " ++ show curLocation ++ ".") ;
    putStrLn "Please enter a direction: " ;
    strDir <- getLine ;
    if strDir == "Exit" then putStrLn "Exiting game..." ;
    else gameLoop (updateLocation curLocation (directionHandler strDir), Alive) ;
    }
    
    
