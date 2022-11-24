-- representation of the map
import Data.List
import Data.Char (ord)
import Control.Exception
import System.Process
import System.IO
import System.IO.Error
import Data.Function

-- definition types

type Vez = Int
type Coordinate = (Int, Int)
type Players = [Player]
type Name = String
type Point = Int
data Player = Player Name Point
  deriving(Show,Read)


-- maps
player_one_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]
player_two_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]


-- function that takes a string and returns an IO String
transformString :: String -> IO String
transformString string = do 
  putStr string
  ioString <- getLine
  return ioString


-- checking if the all row of the map has one
checkShipsAlive :: [[Int]] -> Bool
checkShipsAlive mapToCheck = (1 `elem` mapToCheck!!0) || (1 `elem` mapToCheck!!1) || (1 `elem` mapToCheck!!2) || (1 `elem` mapToCheck!!3) || (1 `elem` mapToCheck!!4)


-- Extract the coordinate from the string
-- Also immediately convert the coordinate from range [0,10[ to [1,10]
-- An invalid coordinate is returned when the string isn't of the correct style.
convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0'), (ord y) - (ord '0'))
convertStringToCoordinates _ = (-1, -1)


-- function that modifies an element of the matrix
-- [Matriz] -> newvalor (x, y)
updateMatrix :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
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

                                                  
                                                                                                      
-- function menu
menu :: Players -> IO Players
menu playerData = do
        system "cls"
        putStrLn"\n   ░█░█░█▀█░█▀▀░█░█░█▀▀░█░░░█░░░░░█▄█░▀█▀░█▀█░█▀█░█▀▄░█▀█"
        putStrLn"   ░█▀█░█▀█░▀▀█░█▀▄░█▀▀░█░░░█░░░░░█░█░░█░░█░█░█▀█░█░█░█░█"
        putStrLn"   ░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░░░▀░▀░▀▀▀░▀░▀░▀░▀░▀▀░░▀▀▀"
        putStrLn "  \n********************************************************\n"
        putStrLn "  Digite o numero da opção desejada:"
        putStrLn "    1 - CADASTRAR JOGADOR.                     " 
        putStrLn "    2 - JOGAR.                                  "
        putStrLn "    3 - VISUALIZAR RANKING.                  "
        putStrLn "    0 - SAIR.                                   "
        putStrLn "********************************************************\n"
        op <- getChar
        getChar
        optionMenu playerData op


-- function to manipulate chosen option
optionMenu :: Players -> Char -> IO Players
optionMenu playerData '1' = registerPlayer playerData
optionMenu playerData '2' = prepareGame playerData
optionMenu playerData '3' = do 
                                  putStrLn "\nRanking dos Players:\n" 
                                  if (null playerData) then do
                                      putStrLn("Não há Players cadastrados.") 
                                  else
                                    showRanking(reverse (ordenar playerData))
                                  putStrLn"\nPressione <Enter> para voltar ao menu. "
                                  getChar   
                                  menu playerData
optionMenu playerData '0' = do 
        putStrLn("Esperamos ver você novamente! :) \n")
        return playerData
optionMenu playerData _ = do
        putStrLn ("\nOpção inválida! Tente novamente!")
        putStr ("Pressione <Enter> para voltar ao menu. :)")
        getChar
        menu playerData


-- function responsible for the registration of players
registerPlayer :: Players -> IO Players
registerPlayer playerData = do
        name <- transformString "Digite o nome do jogador:"
        if (existsPlayer playerData name) then do
          putStr "Esse nome já foi cadastrado. :( Escolha outro!\n"
          putStr "Pressione <Enter> para voltar ao menu. :)"
          getChar
          menu playerData

        else do
          -- open file to write
          file <- openFile "playerData.txt" WriteMode 
          hPutStrLn file (show ((Player name 0):playerData)) 
          -- close file
          hClose file 
          putStrLn ("\nUsuário " ++ name ++ " cadastrado com sucesso!! :D\n")
          putStr "Pressione <Enter> para voltar ao menu. :)"
          getChar 
          -- new list to menu
          menu ((Player name 0):playerData) 



-- function that checks if a jogador already exists
existsPlayer :: Players -> Name -> Bool
existsPlayer [] _ = False 
existsPlayer ((Player n p):xs) name
        | (n == name) = True
        | otherwise = existsPlayer xs name


-- function to prepare game
-- receive the players who will participate
prepareGame :: Players -> IO Players
prepareGame playerData = do
            player1 <- transformString "\nDigite o nome do primeiro jogador: "
            -- test if jogador 1 exists
            if not (existsPlayer playerData player1) then do
              putStr "Esse jogador não existe! :("
              putStr "\nPressione <Enter> para voltar ao menu. :)"
              getChar 
              menu playerData
            else do
              player2 <- transformString "\nDigite o nome do segundo jogador:"
              -- test if jogador 2 exists
              if not (existsPlayer playerData player2) then do
                putStr "Esse jogador não existe! :("
                putStr "\nPressione <Enter> para voltar ao menu. :)"
                getChar 
                menu playerData
              else do
                -- if players exists
                newGame playerData player1 player2


-- function that starts new game
newGame :: Players -> Name -> Name -> IO Players
newGame playerData player1 player2 = do 
    putStrLn ("\n-----------------------------------------------------------------\n")
    putStrLn ("START COMBATE: \"" ++ player1 ++ " VS " ++ player2 ++ "\"!!!")
    runGame playerData player_one_map player_two_map player1 player2 0
                    

-- game recursion function
runGame :: Players -> [[Int]] -> [[Int]] -> Name -> Name -> Vez -> IO Players
runGame playerData player_one_map player_two_map player1 player2 vez = do

    if (checkShipsAlive player_two_map) == False || (checkShipsAlive player_one_map) == False
    then do 
      putStrLn "Fim de jogo!"
      if (checkShipsAlive player_one_map == False)
      then do
        putStrLn ("Parabéns, " ++ player2 ++"!! Você venceu!!")

        file_write <- openFile "playerData.txt" WriteMode
        hPutStrLn file_write (show (updatePoint playerData player2))
        hClose file_write

        file_read <- openFile "playerData.txt" ReadMode
        data_update <- hGetLine file_read
        hClose file_write

        putStr "\nPressione <Enter> para voltar ao menu. :)"
        getChar
        menu (read data_update)
        
      else do
        putStrLn ("Parabéns, " ++ player1 ++"!! Você venceu!!")

        file_write <- openFile "playerData.txt" WriteMode
        hPutStrLn file_write (show (updatePoint playerData player1))
        hClose file_write

        file_read <- openFile "playerData.txt" ReadMode
        data_update <- hGetLine file_read
        hClose file_write

        putStr "\nPressione <Enter> para voltar ao menu. :)"
        getChar
        menu (read data_update)
        
    else do
      if (vez == 0)
        then do 
          printMap player_two_map                
          putStrLn (player1 ++ " , é a sua vez! Qual coordenadas deseja atacar (x,y)?")
          string <- getLine
          let coord = convertStringToCoordinates string
          let newMapaPlayer2 = updateMatrix player_two_map 2 coord
          system "cls"
          runGame playerData player_one_map newMapaPlayer2 player1 player2 1
        else do
          printMap player_one_map 
          putStrLn (player2 ++ ", é a sua vez! Qual coordenada deseja para atacar (x,y)?")
          string2 <- getLine
          let coord2 = convertStringToCoordinates string2
          let newMapaPlayer1 = updateMatrix player_two_map 2 coord2
          system "cls"
          runGame playerData newMapaPlayer1 player_two_map player1 player2 0


-- function to update the points
updatePoint :: Players -> String -> Players
updatePoint ((Player name point) :xs) winner
                    | (name == winner) = [(Player name (point +1))] ++ xs
                    | otherwise = (Player name point):(updatePoint xs winner)


-- show player ranking
showRanking :: Players -> IO ()
showRanking [] = putStrLn"--"
showRanking (x:xs) = do 
                        putStrLn((getName x) ++ " possui " ++ (show(getPoints x)) ++ " pontos")
                        showRanking xs

-- function that takes a player and returns the score
getName :: Player -> Name
getName (Player name _) = name


-- function that takes a player and returns the score
getPoints :: Player -> Point
getPoints (Player _ point) = point


-- function that defines the ordering criteria are the player's points
ordenar :: Players -> Players
ordenar playerData = sortBy (compare `on` getPoints ) playerData

main :: IO ()
main = do
      {catch (read_file) treat_erro;}
      where
        -- treat to read  file
        read_file = do
        {
          -- open file to read
          file <- openFile "playerData.txt" ReadMode; 
          playerData <- hGetLine file;
          -- close file
          hClose file;
          menu (read playerData);
          return ()
        }
        -- if dont exist, create file
        treat_erro erro = if isDoesNotExistError erro then do
        {
          -- open file to write
          file <- openFile "playerData.txt" WriteMode; 
          hPutStrLn file "[]";
          -- close file
          hClose file;
          menu [];
          return ()
        }
        else
          ioError erro