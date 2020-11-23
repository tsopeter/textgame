module Maps where

data Location = Plains | Desert | Ocean | Jungle deriving (Show, Eq)
data Direction = N | W | S | E

--displaying the map of the field
displayMap :: IO ()
displayMap = do
             putStrLn ("+----------+----------+----------+") 
             putStrLn ("|..........|..........|.<Desert>.|") 
             putStrLn ("+----------+----------+----------+") 
             putStrLn ("|.<Jungle>.|.<Oceans>.|.<Plains>.|") 
             putStrLn ("+----------+----------+----------+") 
             putStrLn ("|..........|..........|..........+") 
             putStrLn ("+----------+----------+----------+") 

directionHandler :: String -> Direction
directionHandler x | x == "N" = N
    | x == "W" = W
    | x == "S" = S
    | otherwise = E

showLocation :: Maybe Location -> String
showLocation loc = drop 5 (show loc)

--updates the location
updateLocation :: Maybe Location -> Direction -> Maybe Location
--linking for the plains
updateLocation (Just Plains) N = Just Desert
updateLocation (Just Plains) W = Just Ocean
--linking for the desert
updateLocation (Just Desert) S = Just Plains
--linking for the ocean
updateLocation (Just Ocean) E = Just Plains
updateLocation (Just Ocean) W = Just Jungle
--linking for the jungle
updateLocation (Just Jungle) E = Just Ocean
--linking for NoLocation
updateLocation _ _ = Nothing

