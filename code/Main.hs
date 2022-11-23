-- representation of the map
import Data.List
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

-- funcao que modifica um elemento da Matriz
--     [Matriz] -> novovalor (x, y)
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m player_map (r,c) =
  take r m ++
  [take c (m !! r) ++ [player_map] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

-- Appending integer list to list of lists
append :: [Int] -> [[Int]] -> [[Int]]
append a [] = [a]
append a (x:xs) = x : append a xs

attacked_positions = [[1]]

main :: IO ()
main = do

    putStrLn "Batalha Naval"
    
    print(attacked_positions)
    
    let resultado = append [0,0] attacked_positions

    print( append [1,2] (append [0,0] attacked_positions))

    print(resultado)