module Main where

--library imports
import Data.Char

--other module imports
import Maps
import Lib
import ItemsAttribute

--data structures
data State = Alive | Dead | UnusedState

data GameState = GameState (Maybe Location) State [[Items]] [Items]

--print avaliable commands
printCommands :: IO ()
printCommands = do {
    putStrLn "Commands: | look | see map | die | scan | exit | inventory | " ;
    }

--game controls
gameMenu :: GameState -> IO ()
gameMenu (GameState curLocation Dead witem pitem) = do {
    putStrLn "You are dead... " ;
    }
gameMenu (GameState curLocation Alive witem pitem) = do {
    printCommands ;
    userCommand <- command ;
    case userCommand of
        DontUnderstand -> do {
                notUnderstand (GameState curLocation Alive witem pitem) ;
            }
        Look -> do {
            lookAround (GameState curLocation Alive witem pitem) ;
            gameMenu (GameState curLocation Alive witem pitem) ;
            }
        SeeMap -> do {
            displayMap ;
            gameMenu (GameState curLocation Alive witem pitem) ;
            }
        Die -> do {
            gameMenu (GameState curLocation Dead witem pitem) ;
            }
        Scan -> do {
            putStrLn "You look around and find...! " ;
            printArray 0 (getMapItems curLocation witem) ;
            if (getMapItems curLocation witem) == [] then do {
                putStrLn "You found nothing." ;
                gameMenu (GameState curLocation Alive witem pitem) ;
                }
            else do
                putStrLn "What do you want to do with the found item(s) ?" ;
                putStrLn "Commands : | Pick up | Else |" ;
                userCommand <- command ;
                case userCommand of
                    Pick -> do {
                        itemIndex <- useItem ;
                        putStrLn "You picked up... " ;
                        putStrLn "You added to your storage... ";
                        gameMenu (GameState curLocation Alive (remove2 curLocation witem itemIndex) (pitem ++ [((getMapItems curLocation witem) !! itemIndex)])) ;
                        }
                    otherwise -> do {
                        gameMenu (GameState curLocation Alive witem pitem) ;
                    }   
            }
        CheckInventory -> do {
            putStrLn "Your Inventory: ";
            putStrLn (show pitem) ;
            if pitem == []
            then do {
                putStrLn "You have no items. ";
                gameMenu (GameState curLocation Alive witem pitem) ;
                }
            else do {
                putStrLn "What do you want to do with your item(s): " ;
                putStrLn "Commands: | Eat | Else | ";
                userCommand <- command ;
                case userCommand of
                    Eat -> do {
                        putStrLn "What item do you want to eat? " ;
                        printArray 0 pitem ;
                        itemIndex <- useItem ;
                        putStrLn ("You ate " ++ (show (pitem !! itemIndex)) ++ "!") ;
                        gameMenu (GameState curLocation Alive witem (remove itemIndex pitem)) ; 
                        }
                    otherwise -> do {
                        gameMenu (GameState curLocation Alive witem pitem) ;
                        }
                }
            }
        Exit -> do {
            putStrLn "Exiting game..." ;
            pure () ;
            }
        otherwise -> do {
               notUnderstand (GameState curLocation Alive witem pitem) ;
            }
    }
    
--define itemList
                  --arrangement
                  --Plains
                  --Desert
                  --Ocean
                  --Jungle
defaultItemList :: [[Items]]
defaultItemList = [ [Apple, Apple, Orange],
                    [],
                    [Yuzu, Pineapple],
                    [Apple, Orange, Pineapple, Yuzu, Yuzu]
                    ]   

--get item list
getMapItems :: (Maybe Location) -> [[Items]] -> [Items]
getMapItems (Just Plains) gameItems = gameItems !! 0           --first index is Plains
getMapItems (Just Desert) gameItems = gameItems !! 1           --second index is Desert
getMapItems (Just Ocean)  gameItems = gameItems !! 2           --thrid index is Ocean
getMapItems (Just Jungle) gameItems = gameItems !! 3           --fourth index is Jungle

--set index for combine
setIndex :: (Maybe Location) -> Int
setIndex (Just Plains) = 0
setIndex (Just Desert) = 1
setIndex (Just Ocean)  = 2
setIndex (Just Jungle) = 3

--drop and link items
remove2 :: (Maybe Location) -> [[Items]] -> Int -> [[Items]]
remove2 curLocation witem idx = combineMapItems curLocation witem (remove idx (getMapItems curLocation witem))

--combine
combineMapItems :: (Maybe Location) -> [[Items]] -> [Items] -> [[Items]]
combineMapItems justLoc gItems iItems = (take (setIndex justLoc) gItems) ++ [iItems] ++ (drop ((setIndex justLoc) + 1) gItems)

--useitem
useItem :: IO Int
useItem = do {
    putStrLn "What item do you want to get? ";
    putStrLn "(Hint): Use the index of item... ";
    itemIndex <- getInt ;
    return itemIndex ;
    }

--not understand
notUnderstand :: GameState -> IO ()
notUnderstand g = do {
    putStrLn "I don't understand..." ;
    gameMenu g ;
    }

--functions for strutures
checkState :: State -> Bool
checkState _ = True
checkState Dead = False


--main function
main :: IO ()
main = do {
    --initailizes with beginning states
    gameMenu (GameState (Just Plains) Alive defaultItemList [])
    }
        
command :: IO Command
command = do {
    putStrLn "Give me a command: " ;
    cinput <- getLine ;
    case stringToLower (words cinput) of
        ["die"] -> return Die ;
        ["look"] -> return Look ;
        ["exit"] -> return Exit ;
        ["see", "map"] -> return SeeMap ;
        ["scan"] -> return Scan ;
        ["pick", "up"] -> return Pick ;
        ["inventory"] -> return CheckInventory ;
        ["eat"] -> return Eat ;
        otherwise -> return DontUnderstand ;
    }
        

--prints the description of the game
lookAround :: GameState -> IO ()
lookAround (GameState curLocation curState witem pitem) = do
    putStrLn ("You are at " ++ show (showLocation curLocation) ++ ".") ;
    
    
