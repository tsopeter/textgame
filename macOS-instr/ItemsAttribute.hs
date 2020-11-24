module ItemsAttribute where

--import other modules
import Lib
import Constants

--data strutcure
data Tree a = TreeEmpty | Node (Tree a) a (Tree a) deriving (Show)

data Items a= Apple a | Orange a | Pineapple a | Yuzu a | ExitFruit a deriving (Eq, Show)

data Selection = Yes | No deriving (Eq)

--get item
getItem :: (Items a) -> a
getItem (Apple a) = a
getItem (Orange a) = a
getItem (Pineapple a) = a
getItem (Yuzu a) = a


--print array
printArray :: (Show a) => Int -> [a] -> IO ()
printArray _ [] = pure ()
printArray i (x:xs) = do {
    putStrLn ((show i) ++ " | " ++ (show x)) ;
    printArray (i + 1) xs ;
    }
    
--remove
remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 [a] = []
remove _ [a] = [a]
remove i xs = (take i xs) ++ (drop (i + 1) xs)


--search Tree
search :: (Eq a) => (Tree a) -> a -> Bool
search TreeEmpty _ = False
search (Node lhs a rhs) b | a == b = True
    | otherwise = search lhs b || search rhs b
    
-- flatten Tree
flatten :: (Tree a) -> [a]
flatten TreeEmpty = []
flatten (Node lhs a rhs) = flatten lhs ++ [a] ++ flatten rhs

-- print items of tree
printTree :: (Show a) => Tree a -> IO ()
printTree TreeEmpty = putStrLn "Nothing to see here. " ;
printTree a = putStrLn (show (flatten a)) ;

--for yes or no selections
simpleYN :: IO Selection
simpleYN = do {
    x <- getLine ;
    case stringToLower (words x) of
        ["y"] -> return Yes ;
        ["yes"] -> return Yes ;
        otherwise -> return No ;
    }

--range of numbers
getRange :: IO Int
getRange = do {
    cinput <- getInt ;
    if(cinput < 0 || cinput > 10)
    then do {
        putStrLn "Out of range... Try again. " ;
        cinput2 <- getRange ;
        return cinput2 ;
        }
    else return cinput ;
    }

--item difference
diffRemove :: [Items Int] -> [Items Int] -> [Items Int]
diffRemove _ [] = []
diffRemove [] x = x
diffRemove (u:uitem) pitem = diffRemove uitem (removeByItem u pitem)

--calculate score of smoothie
calculateSmoothie :: [Items Int] -> Int -> Int
calculateSmoothie [] _ = 0
calculateSmoothie _  0 = 0
calculateSmoothie uitem rng = (getItemScore uitem) + rng

--add
getItemScore :: [Items Int] -> Int
getItemScore [] = 0
getItemScore (x:xs) = getItem x + getItemScore xs

--remove from Items int
removeByItem :: (Eq a) => a -> [a] -> [a]
removeByItem _ [] = []
removeByItem a (x:xs) | a == x = xs
    | otherwise = [x] ++ removeByItem a xs

--check against list
checkAgainstList :: (Eq a) => a -> [a] -> Bool
checkAgainstList _ [] = False
checkAgainstList i (x:xs) | i == x = True
    | otherwise = checkAgainstList i xs 

--function for combining items to make a smoothie
makeTheSmoothie :: [Items Int] -> IO [Items Int]
makeTheSmoothie [] = do {
    putStrLn "You have no items... ";
    return []
    }
makeTheSmoothie pitems = do {
    printArray 0 pitems ;
    putStrLn "Enter the indices of the items to mix " ;
    selectedIDX <- indexCombiner (length pitems) [] ;
    putStrLn "Give a range between 0 and 10 to blend the smoothie. " ;
    range <- getRange ;
    putStrLn "You selected... " ;
    let gotItems = getItemsFromIDXS selectedIDX pitems in
        do {
        printArray 0 gotItems ;
        let score = calculateSmoothie gotItems range in
           do {
               putStrLn ("Your score is... " ++ (show score)) ;
               if(score  == smoothieTotalScore)
               then do {
                   putStrLn "You Win! ";
                   return [ExitFruit (-1)] ;
                   }
               else return (diffRemove gotItems pitems) ;
           }
        }
    }

--get items from idxs
getItemsFromIDXS :: [Int] -> [Items Int] -> [Items Int]
getItemsFromIDXS _ [] = []
getItemsFromIDXS [] y = []
getItemsFromIDXS (i:is) items = [items !! i] ++ (getItemsFromIDXS is items) ;
    
--index combiner for makeTheSmoothie function    
indexCombiner :: Int -> [Int] -> IO [Int]
indexCombiner indexOfItems selectedItems = do {
    putStrLn "Enter a index : " ;
    index <- getInt ;
    if (index < 0 || index >= indexOfItems)
    then do
        putStrLn "index out of range" ;
        putStrLn "Do you want to continue? (y/yes to continue): ";
        cinput <- simpleYN ;
        if (cinput == Yes)
            then indexCombiner indexOfItems selectedItems ;
            else return selectedItems ;
    else do
        if (checkAgainstList index selectedItems)
        then do
            putStrLn "Item already selected. ";
            putStrLn "Do you want to continue? (y/yes to continue): ";
            cinput <- simpleYN ;
            if (cinput == Yes)
                then indexCombiner indexOfItems selectedItems ;
                else return selectedItems ;
        else do {
            putStrLn "Item selected... " ;
            putStrLn "Do you want to continue? (y/yes to continue): ";
            cinput <- simpleYN ;
            let newItems = (selectedItems ++ [index]) in
            if (cinput == Yes)
            then indexCombiner indexOfItems newItems ;
            else return newItems
             }
    }
            
