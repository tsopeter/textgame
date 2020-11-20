module Main where

--import
import Maps
import Lib
import ItemsAttribute

--data structures
data State = Alive | Dead | UnusedState


--functions for strutures
checkState :: State -> Bool
checkState _ = True
checkState Dead = False


--main function
main :: IO ()
main = do {
    --initailizes with beginning states
    gameLoop (Just Plains, Alive)
    }
    
--game loop
gameLoop :: (Maybe Location, State) -> IO ()
gameLoop (curLocation, curState) = do {
    displayMap ;
    putStrLn ("You are currently at " ++ show (showLocation curLocation) ++ ".") ;
    
    
    
    putStrLn "Please enter a direction: " ;
    strDir <- getLine ;
    if strDir == "Exit" 
    then putStrLn "Exiting game..." ;
    else 
        if (updateLocation curLocation (directionHandler strDir)) == Nothing
        then do putStrLn ("You cannot go there... ") ;
                gameLoop (curLocation, curState) ;
        else do gameLoop (updateLocation curLocation (directionHandler strDir), curState) ;
    }
    
    
