module ItemsAttribute where

--data strutcure
data Tree a = TreeEmpty | Node (Tree a) a (Tree a) deriving (Show)

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


