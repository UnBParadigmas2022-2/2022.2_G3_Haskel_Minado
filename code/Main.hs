-- representation of the map
import Data.List
import Data.Char (ord)

type Coordinate = (Int, Int)

player_one_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]
player_two_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]

-- checking if the all row of the map has one
checkShipsAlive :: [[Int]] -> Bool
checkShipsAlive mapToCheck = (1 `elem` mapToCheck!!0) || (1 `elem` mapToCheck!!1) || (1 `elem` mapToCheck!!2) || (1 `elem` mapToCheck!!3) || (1 `elem` mapToCheck!!4)

-- funcao que modifica um elemento da Matriz
-- [Matriz] -> novovalor (x, y)
updateMatrix :: [[a]] -> a -> Coordinate -> [[a]]
updateMatrix m player_map (r,c) =
  take r m ++
  [take c (m !! r) ++ [player_map] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

-- Extract the coordinate from the string
-- Also immediately convert the coordinate from range [0,10[ to [1,10]
-- An invalid coordinate is returned when the string isn't of the correct style.
convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0'), (ord y) - (ord '0'))
convertStringToCoordinates _ = (-1, -1)

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

-- Game recursion function
runGame :: [[Int]] -> [[Int]] -> IO ()
runGame player_one_map player_two_map = do

    if (checkShipsAlive player_two_map) == False || (checkShipsAlive player_one_map) == False
    then do 
      putStrLn "Fim de Jogo"

      if (checkShipsAlive player_one_map == False)
      then do
        putStrLn "Jogador 2 venceu"
      else
        putStrLn "Jogador 1 venceu"

    else do
      
      printMap player_one_map
      printMap player_two_map

      putStrLn ("Jogador 1 - Qual coordenadas para atacar (x,y)?")
      string <- getLine
      let coord = convertStringToCoordinates string
      let novoMapaJogador2 = updateMatrix player_two_map 2 coord

      putStrLn "Jogador 2 - Qual coordenadas para atacar (x,y)?"
      string2 <- getLine
      let coord2 = convertStringToCoordinates string2
      let novoMapaJogador1 = updateMatrix player_two_map 2 coord2

      runGame novoMapaJogador1 novoMapaJogador2

main :: IO ()
main = do
  putStrLn "- Batalha Naval -"
  runGame player_one_map player_two_map
