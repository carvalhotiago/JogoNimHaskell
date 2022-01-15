-- Arthur Mazzi 201765012C
-- Tiago Carvalho 201665118C

{-# LANGUAGE BlockArguments #-}
module Main where
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import Data.Char ()
import Data.List (length)
import Data.Bool(bool)
import Lib ()
import System.Random

data Jogador = Usuario | Computador deriving (Show, Eq)

data Dificuldade = Facil | Dificil deriving (Show, Eq)

tabuleiro :: Seq.Seq Int
tabuleiro = Seq.fromList [1, 3, 5, 7]

jogo :: Seq.Seq Int -> Jogador -> Dificuldade -> IO ()
jogo tabuleiro jogador dificuldade =
    if tabuleiro == Seq.fromList [0,0,0,0] then
        putStrLn (show (alterna jogador) ++ " venceu!")
    else do
        if jogador == Usuario then do
            putStrLn "\nVez do usuario: \n"
            mostraTabuleiro tabuleiro
            putStr "\nSelecione uma fileira para remover palitos: "
            fileira <- getLine
            putStr "Selecione o numero de palitos a serem removidos: "
            numPalitosRemovidos <- getLine
            putStr "\n"
            let fileiraInt = (read fileira :: Int)
            let palitosInt = (read numPalitosRemovidos :: Int)
            let tabuleiroAtualizado = jogadaUsuario tabuleiro (fileiraInt-1) palitosInt
            jogo tabuleiroAtualizado (alterna jogador) dificuldade
        else do
            if dificuldade == Facil then do
                putStrLn "\nVez do computador: \n"
                fileiraAleatoria <- pegaFileiraNaoVaziaAleatoria tabuleiro
                numeroPalitosAleatorio <- randomRIO (1, Seq.index tabuleiro fileiraAleatoria)
                putStrLn ("\nO computador aleatoriamente " ++ show numeroPalitosAleatorio ++ " palitos da fileira " ++ show (fileiraAleatoria+1))
                let tabuleiroAtualizado = removePalitos tabuleiro fileiraAleatoria numeroPalitosAleatorio
                jogo tabuleiroAtualizado (alterna jogador) Facil
            else do
                if tabuleiro == Seq.fromList [1, 3, 5, 7] then do
                    putStrLn "\nVez do computador: \n"
                    putStrLn ("\nO computador removeu " ++ show 1 ++ " palitos da fileira " ++ show (0+1))
                    let tabuleiroAtualizado = removePalitos tabuleiro 0 1
                    jogo tabuleiroAtualizado (alterna jogador) Dificil
                else do
                    fileiraAleatoria <- pegaFileiraNaoVaziaAleatoria tabuleiro
                    numeroPalitosAleatorio <- randomRIO (1, Seq.index tabuleiro fileiraAleatoria)
                    let tabuleiroSomaZero = removePalitos tabuleiro fileiraAleatoria numeroPalitosAleatorio

                    -- converte palitos de cada fileira para binario
                    let fileira1Bin = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiroSomaZero 0))
                    let fileira2Bin = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiroSomaZero 1))
                    let fileira3Bin = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiroSomaZero 2))
                    let fileira4Bin = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiroSomaZero 3))

                    -- Soma as fileiras
                    let somaColuna1 = fileira1Bin !! 0 + fileira2Bin !! 0 + fileira3Bin !! 0 + fileira4Bin !! 0
                    let somaColuna2 = fileira1Bin !! 1 + fileira2Bin !! 1 + fileira3Bin !! 1 + fileira4Bin !! 1
                    let somaColuna3 = fileira1Bin !! 2 + fileira2Bin !! 2 + fileira3Bin !! 2 + fileira4Bin !! 2

                    let listaNim = [fromEnum (odd somaColuna1), fromEnum (odd somaColuna2), fromEnum (odd somaColuna3)]

                    if listaNim == [0,0,0] then do
                        putStrLn "\nVez do computador: "
                        putStrLn ("\nO computador removeu " ++ show numeroPalitosAleatorio ++ " palitos da fileira " ++ show (fileiraAleatoria+1))
                        jogo tabuleiroSomaZero (alterna jogador) Dificil
                    else
                        jogo tabuleiro jogador dificuldade

pegaFileiraNaoVaziaAleatoria :: Seq.Seq Int -> IO Int
pegaFileiraNaoVaziaAleatoria tabuleiro = do
    fileiraAleatoria <- randomRIO (0, 3)
    if Seq.index tabuleiro fileiraAleatoria == 0 then
        pegaFileiraNaoVaziaAleatoria tabuleiro
    else
        return fileiraAleatoria

preencheComZerosAEsquerda :: [Int] -> [Int]
preencheComZerosAEsquerda lista
  | length lista == 2 =
       [0] ++ lista
  | length lista == 1 =
       [0,0] ++ lista
  | length lista == 0 =
       [0,0,0]
  | otherwise =
       lista

bin2dec :: (Foldable f, Integral i) => f Bool -> i
bin2dec = foldl (\a -> (+) (2*a) . bool 0 1) 0

jogadaUsuario :: Seq.Seq Int -> Int -> Int -> Seq.Seq Int
jogadaUsuario tabuleiro fileira palitos =
    if jogadaValida tabuleiro fileira palitos then
        removePalitos tabuleiro fileira palitos
    else tabuleiro

dec2bin :: (Integral a) => a -> [a]
dec2bin = reverse . dec2bin'
  where dec2bin' 0 = []
        dec2bin' x = (x `mod` 2) : dec2bin' (x `quot` 2)

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