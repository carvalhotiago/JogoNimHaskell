module Main where

import Data.Char ()
import Lib ()
import Data.Array.MArray.Safe (mapIndices)

data Jogador = Usuario | Computador deriving (Show, Eq)
data Dificuldade = Facil | Dificil deriving (Show, Eq)

tabuleiro :: [Int]
tabuleiro = [1, 3, 5, 7]

jogo :: [Int] -> Jogador -> IO ()
jogo tabuleiro jogador =
    if tabuleiro == [0,0,0,0] then do
        putStrLn (show jogador ++ " venceu!")
    else do
        putStrLn ("Vez do " ++ show jogador)
        if jogador == Usuario then do
            mostraTabuleiro tabuleiro
            putStr "Selecione uma fileira: "
            fileira <- getLine
            putStrLn "\nSelecione o numero de palitos a serem removidos: "
            numPalitosRemovidos <- getLine
            --usuarioRemovePalitos()
            mostraTabuleiro tabuleiro
        else do
            --jogadaComputador
            mostraTabuleiro tabuleiro        
        jogo tabuleiro (alterna jogador)

-- removePalitos :: [Int] -> Int
-- removePalitos tabuleiro fileira qtdePalitos =
--   if (jogadaValida tabuleiro fileira qtdePalitos) then do
                                                  
-- jogadaValida :: [Int] -> Int -> Int -> Bool 
-- jogadaValida tabuleiro fileira qtdePalitos = if (tabuleiro !! (fileira-1)) >= qtdePalitos
--                                                  then True 
--                                              else False

alterna :: Jogador -> Jogador
alterna Usuario = Computador
alterna Computador = Usuario

mostraTabuleiro :: [Int] -> IO ()
mostraTabuleiro tabuleiro = do putStrLn ("1: " ++ replicate (tabuleiro !! 0) 'i')
                               putStrLn ("2: " ++ replicate (tabuleiro !! 1) 'i')
                               putStrLn ("3: " ++ replicate (tabuleiro !! 2) 'i')
                               putStrLn ("4: " ++ replicate (tabuleiro !! 3) 'i')

main :: IO ()
main = do
    putStrLn "Niveis de dificuldade:"
    putStrLn "0: Facil:\n1: Dificil"
    putStr "Selecione um nivel: "
    dificuldade <- readLn
    if dificuldade == Facil then do
        putStrLn "Iniciando jogo facil!"
        jogo tabuleiro Usuario
    else if dificuldade == Dificil then do
        putStrLn "Iniciando jogo dificil!"
        jogo tabuleiro Computador
    else do
        putStrLn "Dificuldade invalida"
        main