import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe ( fromJust, isNothing )

-- Jogador type is used to take control of the turns and who wins
data Jogador = Usuario | Computador deriving (Show, Eq)

-- Change jogador returns the jogador that is NOT the one passed as argument
alterna :: Jogador -> Jogador
alterna Usuario = Computador
alterna Computador = Usuario

-- Jogo is an alias for a Sequence of Ints, as they allow us to update single
-- elements in easily.
type Jogo = Seq.Seq Int

-- This is the initial jogo structure
initialJogo :: Jogo
initialJogo = Seq.fromList [4, 3, 2, 1]

-- The validaJogada method checks if the a movement can be executed and returns the
-- updated jogo in case it is possible
validaJogada :: Jogo -> (Int, Int) -> Maybe Jogo
validaJogada jogo (fileira, palitos)
  | (Seq.index jogo fileira >= palitos) && (
          fileira < 4) = Just (Seq.adjust (\x -> x - palitos) fileira jogo)
  | otherwise = Nothing

-- The exibejogo methods transforms a Jogo into a nice, enumerated String of pipes
exibejogo :: Jogo -> String
exibejogo jogo = List.intercalate "\n" (zipWith (++) numbers (palitos jogo))
                where numbers = ["1. ", "2. ", "3. ", "4. "]
                      palitos jogo = [concat (replicate (2*n - 1) "| ")
                                    | n <- Fol.toList jogo]

-- The next methods are the ones that control IO
main :: IO ()
main = jogoNim 

-- Main method welcomes the jogador, displays the initial jogo and calls the
-- first turn
jogoNim :: IO ()
jogoNim = do putStrLn "Inicio do jogo Nim!"
             putStrLn (exibejogo initialJogo)
             jogada initialJogo Usuario

-- The turn method displays the jogador and asks for a movement, then checks if
-- there was a problem performing that movement and continues the game. This is
-- the main game loop
jogada :: Jogo -> Jogador -> IO ()
jogada jogo jogadorAtual = do putStrLn ("\nVez do " ++ show jogadorAtual ++ "!")
                              putStrLn "Escolha uma fileira:"
                              fileira <- getLine
                              putStrLn "Escolha a quantidade de palitos para remover:"
                              palitos <- getLine
                              let turnoAtual = validaJogada jogo (read fileira - 1, read palitos)
                              if isNothing turnoAtual
                                  then do putStrLn "Invalido!"
                                          jogada jogo jogadorAtual
                              else do isOver (fromJust turnoAtual) (alterna jogadorAtual)

-- isOver checks if the Jogo is empty, and checks whether the game is over or
-- the next turn must be called
isOver :: Jogo -> Jogador -> IO()
isOver jogo jogador = do if jogo == Seq.fromList [0, 0, 0, 0]
                           then putStrLn ("O " ++ show (alterna jogador) ++ " ganhou!")
                           else do putStrLn ""
                                   putStrLn (exibejogo jogo)
                                   jogada jogo jogador
