-- representation of the map
player_map = [[1, 0, 0, 0, 0],[1, 0, 0, 0, 0],[0, 0, 0, 1, 1],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]

-- print of the map
printPlayerMap = do
    putStr("\n")
    print(player_map !! 0)
    print(player_map !! 1)
    print(player_map !! 2)
    print(player_map !! 3)
    print(player_map !! 4)
    putStr("\n")

-- checking if the all row of the map has one
checkIfListHasOne = (1 `elem` player_map!!0) || 
                    (1 `elem` player_map!!1) ||
                    (1 `elem` player_map!!2) ||
                    (1 `elem` player_map!!3) ||
                    (1 `elem` player_map!!4)

-- Appending integer list to list of lists
append :: [Int] -> [[Int]] -> [[Int]]
append a [] = [a]
append a (x:xs) = x : append a xs