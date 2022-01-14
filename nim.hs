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
            jogo tabuleiroAtualizado (alterna jogador) dificuldade
        else do
            if dificuldade == Facil then do
                fileiraAleatoria <- pegaFileiraNaoVaziaAleatoria tabuleiro
                numeroPalitosAleatorio <- randomRIO (1, Seq.index tabuleiro fileiraAleatoria)
                putStrLn ("\nO computador aleatoriamente " ++ show numeroPalitosAleatorio ++ " palitos da fileira " ++ (show (fileiraAleatoria+1)))
                let tabuleiroAtualizado = removePalitos tabuleiro fileiraAleatoria numeroPalitosAleatorio
                jogo tabuleiroAtualizado (alterna jogador) Facil
            else do
                mostraTabuleiro tabuleiro
                -- converte palitos de cada fileira para binario
                let a = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiro 0))
                let b = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiro 1))
                let c = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiro 2))
                let d = preencheComZerosAEsquerda (dec2bin (Seq.index tabuleiro 3))
                
                print(a)
                print(b)
                print(c)
                print(d)

                -- Soma as fileiras
                let fileira1 = a !! 0 + b !! 0 + c !! 0 + d !! 0
                let fileira2 = a !! 1 + b !! 1 + c !! 1 + d !! 1
                let fileira3 = a !! 2 + b !! 2 + c !! 2 + d !! 2
                -- Cria a lista final com as somas decimais
                let fileiras = [fileira1, fileira2, fileira3]
                print(fileiras)
                -- Cria a fileira esperada a ser removida
                let listaRemover = [fromEnum (odd fileira1), fromEnum (odd fileira2), fromEnum (odd fileira3)]
                print("Fileira para remover:")
                print(listaRemover)
                -- Cria a fileira que será usada para contar o numero de palitos
                let listaRemoverBool = [odd fileira1, odd fileira2, odd fileira3]

                print(listaRemoverBool)
                let numeroPalitos = bin2dec listaRemoverBool

                print(numeroPalitos)
                print(Seq.index tabuleiro 0)
                print(Seq.index tabuleiro 1)
                print(Seq.index tabuleiro 2)
                print(Seq.index tabuleiro 3)

                let first = Seq.index tabuleiro 0
                let second = Seq.index tabuleiro 1
                let third = Seq.index tabuleiro 2
                let fourth = Seq.index tabuleiro 3                

                let ePrimeiraJogada = (tabuleiro == Seq.fromList [1,3,5,7])

                if (first >= second && first >= third && first >= fourth || ePrimeiraJogada) then do                    
                    if(first == 2 || ePrimeiraJogada) then do
                        putStrLn ("\nO computador removeu " ++ show 1 ++ " palitos da fileira " ++ (show (0+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 0 1
                        jogo tabuleiroAtualizado (alterna jogador) Dificil    
                    else do 
                        putStrLn ("\nO computador removeu " ++ show numeroPalitos ++ " palitos da fileira " ++ (show (0+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 0 numeroPalitos
                        jogo tabuleiroAtualizado (alterna jogador) Dificil                    
                else if (second >= first && second >= third && second >= fourth) then do 
                    if(second == 2) then do
                        putStrLn ("\nO computador removeu " ++ show 1 ++ " palitos da fileira " ++ (show (0+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 1 1
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                    else do
                        putStrLn ("\nO computador removeu " ++ show numeroPalitos ++ " palitos da fileira " ++ (show (1+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 1 numeroPalitos
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                else if (third >= first && third >= second && third >= fourth) then do
                    if(third == 2) then do
                        putStrLn ("\nO computador removeu " ++ show 1 ++ " palitos da fileira " ++ (show (0+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 2 1
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                    else do 
                        putStrLn ("\nO computador removeu " ++ show numeroPalitos ++ " palitos da fileira " ++ (show (2+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 2 numeroPalitos
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                else if (fourth >= first && fourth >= second && fourth >= third) then do
                    if(fourth == 2) then do
                        putStrLn ("\nO computador removeu " ++ show 1 ++ " palitos da fileira " ++ (show (0+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 3 1
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                    else do 
                        putStrLn ("\nO computador removeu " ++ show numeroPalitos ++ " palitos da fileira " ++ (show (3+1)))
                        let tabuleiroAtualizado = removePalitos tabuleiro 3 numeroPalitos
                        jogo tabuleiroAtualizado (alterna jogador) Dificil
                else do
                    putStr ("Jogada invalida")

pegaFileiraNaoVaziaAleatoria :: Seq.Seq Int -> IO Int
pegaFileiraNaoVaziaAleatoria tabuleiro = do
    fileiraAleatoria <- randomRIO (0, 3)
    if Seq.index tabuleiro fileiraAleatoria == 0 then
        pegaFileiraNaoVaziaAleatoria tabuleiro
    else
        return fileiraAleatoria

replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

preencheComZerosAEsquerda :: [Int] -> [Int]
preencheComZerosAEsquerda lista =
    if length lista == 2 then
        concat [[0], lista]
    else if length lista == 1 then
        concat [[0,0], lista]
    else if length lista == 0 then
        [0,0,0]
    else
        lista

bin2dec :: (Foldable f, Integral i) => f Bool -> i
bin2dec = foldl (\a -> (+) (2*a) . bool 0 1) 0

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

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