module Maps where

data Location = Plains | Desert | Ocean | Jungle | NoLocation | Exit deriving (Show)
data Direction = N | W | S | E

directionHandler :: String -> Direction
directionHandler x | x == "N" = N
    | x == "W" = W
    | x == "S" = S
    | otherwise = E

updateLocation :: Location -> Direction -> Location
--linking for the plains
updateLocation Plains N = Desert
updateLocation Plains W = Ocean
updateLocation Plains _ = Plains
--linking for the desert
updateLocation Desert S = Plains
updateLocation Desert _ = Desert
--linking for the ocean
updateLocation Ocean E = Plains
updateLocation Ocean W = Jungle
updateLocation Ocean _ = Ocean
--linking for the jungle
updateLocation Jungle E = Ocean
updateLocation Jungle _ = Jungle
--linking for NoLocation
updateLocation NoLocation _ = NoLocation

