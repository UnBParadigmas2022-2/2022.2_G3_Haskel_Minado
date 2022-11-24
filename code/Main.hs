-- representation of the map
import Data.List
import Data.Char (ord)
import Control.Exception
import System.Process
import System.IO
import System.IO.Error

-- definition types
type Coordinate = (Int, Int)
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
data Jogador = Jogador Nome Pontuacao
  deriving(Show,Read)


-- maps
player_one_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]
player_two_map = [[1, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 0, 0, 0, 0]]


-- function that takes a string and returns an IO String
getString :: String -> IO String
getString string = do 
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
-- [Matriz] -> novovalor (x, y)
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
menu :: Jogadores -> IO Jogadores
menu dadosJogador = do
        system "cls"
        putStrLn "******************** HASKELL MINADO ********************\n"
        putStrLn " Digite 1 para cadastrar jogador.                     " 
        putStrLn " Digite 2 para jogar.                                  "
        putStrLn " Digite 3 para visuzalizar o ranking.                  "
        putStrLn " Digite 0 para sair.                                   "
        putStrLn "********************************************************\n"
        putStrLn "Digite a opcão desejada:"
        op <- getChar
        getChar
        executarOpcao dadosJogador op


-- function to manipulate chosen option
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dadosJogador '1' = cadastrarJogador dadosJogador
executarOpcao dadosJogador '2' = prepararJogo dadosJogador
executarOpcao dadosJogador '0' = do 
        putStrLn("Esperamos ver você novamente! :) \n")
        return dadosJogador
executarOpcao dadosJogador _ = do
        putStrLn ("\nOpção inválida! Tente novamente!")
        putStr ("Pressione <Enter> para voltar ao menu. :)")
        getChar
        menu dadosJogador


-- function responsible for the registration of players
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dadosJogador = do
        nome <- getString "Digite o nome do jogador:"
        if (existeJogador dadosJogador nome) then do
          putStr "Esse nome já foi cadastrado. :( Escolha outro!\n"
          putStr "Pressione <Enter> para voltar ao menu. :)"
          getChar
          menu dadosJogador

        else do
          -- open file to write
          file <- openFile "dadosJogador.txt" WriteMode 
          hPutStrLn file (show ((Jogador nome 0):dadosJogador)) 
          -- close file
          hClose file 
          putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso!! :D\n")
          putStr "Pressione <Enter> para voltar ao menu. :)"
          getChar 
          -- new list to menu
          menu ((Jogador nome 0):dadosJogador) 


-- function that checks if a player already exists
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False 
existeJogador ((Jogador n p):xs) nome
        | (n == nome) = True
        | otherwise = existeJogador xs nome

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dadosJogador = do
            jogador1 <- getString "\nDigite o nome do primeiro jogador: "
            -- test if player 1 exists
            if not (existeJogador dadosJogador jogador1) then do
              putStr "Esse jogador não existe! :("
              putStr "\nPressione <Enter> para voltar ao menu. :)"
              getChar 
              menu dadosJogador
            else do
              jogador2 <- getString "\nDigite o nome do segundo jogador:"
              -- test if player 2 exists
              if not (existeJogador dadosJogador jogador2) then do
                putStr "Esse jogador não existe! :("
                putStr "\nPressione <Enter> para voltar ao menu. :)"
                getChar 
                menu dadosJogador
              else do
                -- if players exists
                novoJogo dadosJogador jogador1 jogador2


-- function that starts new game
novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dadosJogador jogador1 jogador2 = do 
    putStrLn ("\n-----------------------------------------------------------------\n")
    putStrLn ("START COMBATE: \"" ++ jogador1 ++ " VS " ++ jogador2 ++ "\"!!!")
    runGame dadosJogador player_one_map player_two_map jogador1 jogador2 1
                    

-- game recursion function
runGame :: Jogadores -> [[Int]] -> [[Int]] -> Nome -> Nome -> Int -> IO Jogadores
runGame dadosJogador player_one_map player_two_map jogador1 jogador2 vez = do

    if (checkShipsAlive player_two_map) == False || (checkShipsAlive player_one_map) == False
    then do 
      putStrLn "Fim de Jogo!"
      if (checkShipsAlive player_one_map == False)
      then do
        putStrLn "Jogador 2 venceu!!"
        putStr "\nPressione <Enter> para voltar ao menu. :)"
        getChar
        menu dadosJogador
        
      else do
        putStrLn "Jogador 1 venceu!!"
        putStr "\nPressione <Enter> para voltar ao menu. :)"
        getChar
        menu dadosJogador
        
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

      runGame dadosJogador novoMapaJogador1 novoMapaJogador2 jogador1 jogador2 vez

main :: IO ()
main = do
      {catch (ler_arquivo) tratar_erro;}
      where
        -- tenta ler o arquivo
        ler_arquivo = do
        {
          -- open file to read
          file <- openFile "dadosJogador.txt" ReadMode; 
          dadosJogador <- hGetLine file;
          -- close file
          hClose file;
          menu (read dadosJogador);
          return ()
        }
        tratar_erro erro = if isDoesNotExistError erro then do
        {
          -- open file to write
          file <- openFile "dadosPlayer.txt" WriteMode; 
          hPutStrLn file "[]";
          -- close file
          hClose file;
          menu [];
          return ()
        }
        else
          ioError erro