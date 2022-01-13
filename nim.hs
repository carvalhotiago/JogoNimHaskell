-- Arthur Mazzi
-- Tiago Carvalho 201665118C

{-# LANGUAGE BlockArguments #-}
module Main where
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import Data.Char ()
import Lib ()
import Data.Array.MArray.Safe (mapIndices)
import Data.Maybe
import System.Random

data Jogador = Usuario | Computador deriving (Show, Eq)

data Dificuldade = Facil | Dificil deriving (Show, Eq)

tabuleiro :: Seq.Seq Int
tabuleiro = Seq.fromList [1, 3, 5, 7]

jogo :: Seq.Seq Int -> Jogador -> Dificuldade -> IO ()
jogo tabuleiro jogador dificuldade =
    if tabuleiro == Seq.fromList [0,0,0,0] then do
        putStrLn (show (alterna jogador) ++ " venceu!")
    else do
        putStrLn ("\nVez do " ++ show jogador ++ ": \n")        
        if jogador == Usuario then do
            mostraTabuleiro tabuleiro
            putStr "\nSelecione uma fileira para remover palitos: "
            fileira <- getLine
            putStr "Selecione o numero de palitos a serem removidos: "
            numPalitosRemovidos <- getLine
            putStr "\n"
            let x = (read fileira :: Int)
            let y = (read numPalitosRemovidos :: Int)
            let tabuleiroAtualizado = jogadaUsuario tabuleiro (x-1) y
            jogo tabuleiroAtualizado (alterna jogador) Facil
        else do
            if dificuldade == Facil then do
                fileiraAleatoria <- pegaFileiraNaoVaziaAleatoria tabuleiro
                numeroPalitosAleatorio <- randomRIO (1, Seq.index tabuleiro fileiraAleatoria)
                putStrLn ("\nO computador removeu " ++ show numeroPalitosAleatorio ++ " palitos da fileira " ++ (show (fileiraAleatoria+1)))
                let tabuleiroAtualizado = removePalitos tabuleiro fileiraAleatoria numeroPalitosAleatorio
                jogo tabuleiroAtualizado (alterna jogador) Dificil
            else do
                putStrLn "Jogada dificil!"
                jogo tabuleiro (alterna jogador) Dificil


pegaFileiraNaoVaziaAleatoria :: Seq.Seq Int -> IO Int
pegaFileiraNaoVaziaAleatoria tabuleiro = do
    fileiraAleatoria <- randomRIO (0, 3)
    if Seq.index tabuleiro fileiraAleatoria == 0 then
        pegaFileiraNaoVaziaAleatoria tabuleiro
    else
        return fileiraAleatoria

jogadaUsuario :: Seq.Seq Int -> Int -> Int -> Seq.Seq Int
jogadaUsuario tabuleiro fileira palitos =
    if jogadaValida tabuleiro fileira palitos then
        removePalitos tabuleiro fileira palitos
    else tabuleiro


removePalitos :: Seq.Seq Int -> Int -> Int -> Seq.Seq Int
removePalitos tabuleiro fileira qtdePalitos =
    Seq.adjust (\x -> x - qtdePalitos) fileira tabuleiro


jogadaValida :: Seq.Seq Int -> Int -> Int -> Bool
jogadaValida tabuleiro fileira qtdePalitos =
       Seq.index tabuleiro fileira >= qtdePalitos
    && Seq.index tabuleiro fileira >= 1
    && fileira < 4
    && fileira >= 0


alterna :: Jogador -> Jogador
alterna Usuario = Computador
alterna Computador = Usuario


mostraTabuleiro :: Seq.Seq Int -> IO ()
mostraTabuleiro tabuleiro = do putStrLn ("1: " ++ replicate (head (Fol.toList tabuleiro)) 'i')
                               putStrLn ("2: " ++ replicate (Fol.toList tabuleiro !! 1) 'i')
                               putStrLn ("3: " ++ replicate (Fol.toList tabuleiro !! 2) 'i')
                               putStrLn ("4: " ++ replicate (Fol.toList tabuleiro !! 3) 'i')

main :: IO ()
main = do
    putStrLn "\n --- BEM VINDO AO JOGO NIM! ---\n"
    putStrLn "Selecione um nivel de dificuldade abaixo:"
    putStrLn "0: Facil\n1: Dificil\n"
    putStr "Nivel: "
    nivel <- getLine
    if nivel == "0" then do
        putStrLn "\nIniciando jogo no nivel facil!\n"
        jogo tabuleiro Usuario Facil
    else if nivel == "1" then do
        putStrLn "\nIniciando jogo no nivel dificil!\n"
        jogo tabuleiro Computador Dificil
    else do
        putStrLn "Dificuldade invalida!"
        main