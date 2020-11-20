module ItemsAttribute where

--data strutcure
data Tree a = TreeEmpty | Node (Tree a) a (Tree a) deriving (Show)

data Items = Apple | Orange | Pineapple | Yuzu deriving (Eq, Show)

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


