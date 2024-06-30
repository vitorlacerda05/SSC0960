-- Vitor Antonio de Almeida lacerda NUSP: 12544761
-- Bruno Garcia de Oliveira Breda NUSP: 11212702
-- Felipe Oliveira Carvalho NUSP: 14613879

-- Verifica se um numero eh primo
isPrime :: Int -> Int -> Bool
isPrime num i
  | num <= 1 = False
  | i * i > num = True
  | mod num i == 0 = False
  | otherwise = isPrime num (i + 1)

-- Gera lista de primos entre x e y
primeList :: Int -> Int -> [Int]
primeList x y
  | x > y = []
  | isPrime x 2 = x : primeList (x + 1) y
  | otherwise = primeList (x + 1) y

-- Calcular a diferenca maxima
maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff [_] = 0
maxDiff (p1:p2:ps) = max (p2 - p1) (maxDiff (p2:ps))

-- Funcao main para interagir com o usuário
main :: IO ()
main = do
  x <- readLn
  y <- readLn
  let primes = primeList x y
  print $ maxDiff primes
