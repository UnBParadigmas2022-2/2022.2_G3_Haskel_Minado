-- representation of the map
import Data.List
player_one_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]
player_two_map = [[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 1]]


-- checking if the all row of the map has one
checkIfListHasOne = (1 `elem` player_one_map!!0) || 
                    (1 `elem` player_one_map!!1) ||
                    (1 `elem` player_one_map!!2) ||
                    (1 `elem` player_one_map!!3) ||
                    (1 `elem` player_one_map!!4)


-- funcao que modifica um elemento da Matriz
-- [Matriz] -> novovalor (x, y)
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m player_map (r,c) =
  take r m ++
  [take c (m !! r) ++ [player_map] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m


-- Appending integer list to list of lists
append :: [Int] -> [[Int]] -> [[Int]]
append a [] = [a]
append a (x:xs) = x : append a xs


-- print of the map
printMap :: [[Int]] -> IO ()
printMap mapToPrint = do
    putStr("\n")
    print(mapToPrint !! 0)
    print(mapToPrint !! 1)
    print(mapToPrint !! 2)
    print(mapToPrint !! 3)
    print(mapToPrint !! 4)
    putStr("\n")


runGame :: [[Int]] -> [[Int]] -> Int -> IO ()
runGame player_one_map player_two_map points = do

    --if checkIfListHasOne == True
    if points == 2
    then putStrLn "Fim de Jogo"
    else do
      putStrLn "Batalha Naval"
      --print(attacked_positions)
      --let resultado = append [0,0] attacked_positions
      --print( append [1,2] (append [0,0] attacked_positions))
      printMap player_one_map
      printMap player_two_map
      runGame player_one_map player_two_map (points + 1)


main :: IO ()
main = do
  runGame player_one_map player_two_map 1
