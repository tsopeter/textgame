module Main where

--library imports
import Data.Char

--other module imports
import Maps
import Lib
import ItemsAttribute

--data structures
data State = Alive | Dead | UnusedState

data Health a = Health a deriving (Eq, Show, Ord)

data GameState = GameState (Maybe Location) State [[Items Int]] [Items Int] (Health Int)

--print avaliable commands
printCommands :: IO ()
printCommands = do {
    putStrLn "Commands: | look | see map | die | scan | exit | inventory | Go | see health | make " ;
    }

--game controls
gameMenu :: GameState -> IO ()
gameMenu (GameState curLocation Dead witem pitem health) = do {
    putStrLn "You are dead... " ;
    }
gameMenu (GameState curLocation Alive witem pitem health) = do {
    --check fo health first
    if checkHealth health
        then do gameMenu (GameState curLocation Dead witem pitem health) ;
        else do {
            printCommands ;
            userCommand <- command ;
            case userCommand of
                DontUnderstand -> do {
                        notUnderstand (GameState curLocation Alive witem pitem health) ;
                }
                Look -> do {
                    lookAround (GameState curLocation Alive witem pitem health) ;
                    gameMenu (GameState curLocation Alive witem pitem health) ;
                    }
                SeeMap -> do {
                    displayMap ;
                    gameMenu (GameState curLocation Alive witem pitem health) ;
                    }
                Die -> do {
                    gameMenu (GameState curLocation Dead witem pitem health) ;
                    }
                Scan -> do {
                    putStrLn "You look around and find...! " ;
                    printArray 0 (getMapItems curLocation witem) ;
                    if (getMapItems curLocation witem) == [] then do {
                        putStrLn "You found nothing." ;
                        gameMenu (GameState curLocation Alive witem pitem health) ;
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
                                gameMenu (GameState curLocation Alive (remove2 curLocation witem itemIndex) (pitem ++ [((getMapItems curLocation witem) !! itemIndex)]) health) ;
                                }
                            otherwise -> do {
                                gameMenu (GameState curLocation Alive witem pitem health) ;
                            }   
                    }
                CheckInventory -> do {
                    putStrLn "Your Inventory: ";
                    putStrLn (show pitem) ;
                    if pitem == []
                    then do {
                        putStrLn "You have no items. ";
                        gameMenu (GameState curLocation Alive witem pitem health) ;
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
                                let usedItem = pitem !! itemIndex in
                                    putStrLn ("You gained... " ++ show (div (healthBar health) 2)) ;
                                let nextHealth = (-1) * (div (healthBar health) 2) in
                                    gameMenu (GameState curLocation Alive witem (remove itemIndex pitem) (decreaseHealth health nextHealth)) ; 
                                }
                            otherwise -> do {
                                gameMenu (GameState curLocation Alive witem pitem health) ;
                                }
                        }
                    }
                Go -> do {
                    putStrLn "Where do you want to go ? " ;
                    dInput <- directionCommand ;
                    checkThenUpdate (GameState curLocation Alive witem pitem health) dInput ;
                    }
                SeeHealth -> do {
                    putStrLn ("Here is your health : " ++ show (healthBar health)) ;
                    gameMenu (GameState curLocation Alive witem pitem health) ; 
                    }
                Make -> do {
                    smoothieDesc ;
                    restOfItems <- makeTheSmoothie pitem ;
                    if(restOfItems == [ExitFruit (-1)])
                        then do {
                            putStrLn "You won the game! " ;
                            pure () ;
                            }
                        else do{
                            putStrLn "Aww... try again... ";
                        let newHealth = decreaseHealth health 1 in
                            gameMenu (GameState curLocation Alive witem restOfItems newHealth) ; 
                        }
                    }
                Exit -> do {
                    putStrLn "Exiting game..." ;
                    pure () ;
                    }
                otherwise -> do {
                       notUnderstand (GameState curLocation Alive witem pitem health) ;
                    }
                }
    }    
    
--show the health
healthBar :: (Health a) -> a
healthBar (Health a) = a   

--decrease health
decreaseHealth :: (Ord a, Num a)=>(Health a) -> a -> (Health a)
decreaseHealth (Health a) b = (Health (a-b)) 

--check for health
checkHealth :: (Ord a, Num a)=>(Health a) -> Bool
checkHealth (Health a) = (a <= 0)

--check and see to move
checkThenUpdate :: GameState -> Direction -> IO ()
checkThenUpdate (GameState curLocation Alive witem pitem health) dir =
    let moved = updateLocation curLocation dir in
    if (moved == Nothing)
    then do {
            putStrLn "Cannot go here. " ;
            gameMenu (GameState curLocation Alive witem pitem health) ;
            }
    else do {
            putStrLn "Moving... " ;
            gameMenu (GameState moved Alive witem pitem health) ;
            }  

--define itemList
                  --arrangement
                  --Plains
                  --Desert
                  --Ocean
                  --Jungle
defaultItemList :: [[Items Int]]
defaultItemList = [ [Apple 2, Apple 2, Orange 3],
                    [],
                    [Yuzu 3, Pineapple 7],
                    [Apple 4, Orange 3, Pineapple 5, Yuzu 4, Yuzu 3]
                    ]   

--get item list
getMapItems :: (Maybe Location) -> [[Items a]] -> [Items a]
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
remove2 :: (Maybe Location) -> [[Items a]] -> Int -> [[Items a]]
remove2 curLocation witem idx = combineMapItems curLocation witem (remove idx (getMapItems curLocation witem))

--combine
combineMapItems :: (Maybe Location) -> [[Items a]] -> [Items a] -> [[Items a]]
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
checkState Dead = False
checkState _ = True 


--main function
main :: IO ()
main = do {
    putStrLn "Peter's haskell textgame" ;
    putStrLn "Your task is to make the" ;
    putStrLn "best smoothie. ";

    --initailizes with beginning states
    gameMenu (GameState (Just Plains) Alive defaultItemList [] (Health 3))
    }

-- direction IO
directionCommand :: IO Direction
directionCommand = do {
    putStrLn "Give me a direction: " ;
    cinput <- getLine ;
    case stringToLower (words cinput) of
        ["n"] -> return N ;
        ["north"] -> return N ;
        ["s"] -> return S ;
        ["south"] -> return S ;
        ["e"] -> return E ;
        ["east"] -> return E ;
        ["w"] -> return W ;
        ["west"] -> return W ;
        otherwise -> return InvalidDirection ;
    }

--command IO
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
        ["go" ] -> return Go ;
        ["see", "health"] -> return SeeHealth ;
        ["make"] -> return Make ;
        otherwise -> return DontUnderstand ;
    }
        

--prints the description of the game
lookAround :: GameState -> IO ()
lookAround (GameState curLocation curState witem pitem health) = do
    putStrLn ("You are at " ++ show (showLocation curLocation) ++ ".") ;
    
    
