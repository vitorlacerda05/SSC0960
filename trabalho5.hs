import System.IO
import Data.List (sort, sortBy)
import Data.Ord (comparing, Down(..))

-- Bruno Garcia de Oliveira Breda - 11212702
-- Felipe Oliveira Carvalho - 14613879
-- Vitor Antônio de Almeida Lacerda - 12544761

data CountryData = CountryData
  { countryName :: String
  , confirmed   :: Int
  , deaths      :: Int
  , recovered   :: Int
  , active      :: Int
  } deriving (Show)

readCSV :: FilePath -> IO [CountryData]
readCSV filePath = do
  content <- readFile filePath
  let rows = lines content
  return $ map parseCSVRow rows

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delimiter str = foldr f [""] str
  where
    f c l@(x:xs) | c == delimiter = "":l
                 | otherwise      = (c:x):xs

parseCSVRow :: String -> CountryData
parseCSVRow row =
  let fields = splitOn ',' row
  in CountryData
       (head fields)            -- country
       (readInt $ fields !! 1)  -- confirmed
       (readInt $ fields !! 2)  -- deaths
       (readInt $ fields !! 3)  -- recovered
       (readInt $ fields !! 4)  -- active

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ',')

readInt :: String -> Int
readInt s = case reads s of
  [(val, "")] -> val
  _ -> 0

main :: IO ()
main = do
  line <- getLine
  let [n1, n2, n3, n4] = map read (words line) :: [Int]

  countries <- readCSV "dados.csv"

  let activeSumWhereConfirmedGE = sum [active c | c <- countries, confirmed c >= n1]
  print activeSumWhereConfirmedGE

  let deathsSum = sumDeathsOfN3Countries n2 n3 countries
  print deathsSum

  let sortedNames = topN4CountriesByConfirmedSorted n4 countries
  mapM_ putStrLn sortedNames

sumDeathsOfN3Countries :: Int -> Int -> [CountryData] -> Int
sumDeathsOfN3Countries n2 n3 countries =
  let topN2Active = take n2 $ sortBy (comparing (Down . active)) countries
      smallestN3ConfirmedInTopN2Active = take n3 $ sortBy (comparing confirmed) topN2Active
  in sum $ map deaths smallestN3ConfirmedInTopN2Active

topN4CountriesByConfirmedSorted :: Int -> [CountryData] -> [String]
topN4CountriesByConfirmedSorted n4 countries =
  let topN4Confirmed = take n4 $ sortBy (comparing (Down . confirmed)) countries
  in sort $ map countryName topN4Confirmed
