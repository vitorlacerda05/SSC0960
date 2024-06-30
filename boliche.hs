{-
Autores:
- Vitor Antonio de Almeida Lacerda - 12544761
- Felipe Oliveira Carvalho - 14613879
- Bruno Garcia de Oliveira Breda - 11212702
-}

import Data.List

-- Representa um único arremesso (pino derrubado)
type Pino = Int

-- Representa um frame do jogo (um ou dois arremessos)
type Frame = [Pino]

-- Lê a entrada do usuário como uma lista de pinos derrubados
lerEntrada :: IO [Pino]
lerEntrada = map read . words <$> getLine

-- Formata um pino derrubado para exibição
formatarPino :: Pino -> String
formatarPino 10 = "X"
formatarPino x = show x

-- Formata um frame para exibição
formatarFrame :: Frame -> String
formatarFrame [10] = "X _"
formatarFrame [x, y]
  | x + y == 10 = show x ++ " /"
  | otherwise = show x ++ " " ++ show y
formatarFrame [10, x, y]
  | x == 10 && y == 10 = "X X X"
  | x == 10 = "X X " ++ formatarPino y
  | x + y == 10 = "X " ++ show x ++ " /"
  | otherwise = "X " ++ show x ++ " " ++ show y
formatarFrame [x, y, z]
  | x + y == 10 && z == 10 = show x ++ " / X"
  | x + y == 10 = show x ++ " / " ++ show z
  | otherwise = show x ++ " " ++ show y ++ " " ++ show z
formatarFrame pinos = unwords (map show pinos)

-- Calcula o bônus de um spare (próximo arremesso)
bonusSpare :: [Frame] -> Int
bonusSpare (x:_) = head x
bonusSpare _ = 0

-- Calcula o bônus de um strike (próximos dois arremessos)
bonusStrike :: [Frame] -> Int
bonusStrike (x:y:_) = sum (take 2 (x ++ y))
bonusStrike (x:_) = sum (take 2 x)
bonusStrike _ = 0

-- Calcula a pontuação de um frame
pontuacaoFrame :: [Frame] -> Int
pontuacaoFrame [] = 0
pontuacaoFrame ([x]:xs) = x + bonusStrike xs + pontuacaoFrame xs
pontuacaoFrame ([x,y]:xs)
  | x + y == 10 = x + y + bonusSpare xs + pontuacaoFrame xs
  | otherwise = x + y + pontuacaoFrame xs
pontuacaoFrame (frame:xs) = sum frame + pontuacaoFrame xs

-- Divide a lista de pinos derrubados em frames do jogo
agruparFrames :: [Pino] -> [Frame]
agruparFrames [] = []
agruparFrames pinos = agruparFrames' pinos 1
  where
    agruparFrames' [] _ = []
    agruparFrames' (10:xs) n
      | n < 10 = [10] : agruparFrames' xs (n + 1)
      | otherwise = [10, head xs, xs !! 1] : agruparFrames' (drop 2 xs) (n + 2)
    agruparFrames' (x:y:xs) n
      | n == 10 = [[x,y] ++ take 1 xs]
      | x + y == 10 = [x,y] : agruparFrames' xs (n + 1)
      | otherwise = [x,y] : agruparFrames' xs (n + 1)
    agruparFrames' pinos _ = [pinos]

-- Formata e exibe os frames e a pontuação final
exibirFramesEPontuacao :: [Frame] -> Int -> String
exibirFramesEPontuacao frames pontuacao =
  unwords (map (\frame -> formatarFrame frame ++ " |") (init frames)) ++ " " ++ formatarFrame (last frames) ++ " | " ++ show pontuacao

-- Função principal para executar o programa
main :: IO ()
main = do
  pinos <- lerEntrada
  let frames = agruparFrames pinos
  putStrLn $ exibirFramesEPontuacao frames (pontuacaoFrame frames)
