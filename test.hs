import Data.Maybe

deleteMaybes [] = []
deleteMaybes [x] 
    | isJust x = [fromJust x] 
    | otherwise = []
deleteMaybes (x:xs) 
    | isJust x = fromJust x : deleteMaybes xs 
    | otherwise = deleteMaybes xs