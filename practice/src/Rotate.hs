-- | Round 1A 2010 Problem A. Rotate
-- https://code.google.com/codejam/contest/544101/dashboard#s=p0
module Rotate where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (sort, sortBy, foldl', inits)

-- variable Data

data TestCase = TestCase
                  Int -- ^ number of rows/cols
                  Int -- ^ number of ks in a row, col or diag
                  [String] -- ^ rows (matrix of 'K', 'B' or '.')
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase c@(TestCase numRows numKs rows) = show c

shiftRight' :: String -> (Int, String)
shiftRight' = foldr fn (0, [])
  where
    fn '.' (n, xs) = (n + 1, xs)
    fn ch (n, xs) = (n, ch:xs)

shiftRight :: String -> String
shiftRight s = pad '.' n xs
  where
    (n, xs) = shiftRight' s

pad :: a -> Int -> [a] -> [a]
pad _ 0 s = s
pad ch n s = pad ch (n-1) $ ch:s

starters :: Int -> [(Int, Int)]
starters n = [(x, 0) | x <- [0..n-1]]

indices :: ((Int, Int) -> (Int, Int)) -> Int -> (Int, Int) -> [(Int, Int)]
indices fn n start = takeWhile check $ iterate fn start
  where
    check (r, c) = c >= 0 && c < n && r >= 0 && r < n

indices1 = indices fn
  where
    fn (r, c) = (r - 1, c + 1)

indices2 = indices fn
  where
    fn (r, c) = (r + 1, c + 1)

diagIndices :: Int -> [[(Int, Int)]]
diagIndices n = d1 ++ d2
  where
    d1 = map (indices1 n) (starters n)
    d2 = map (indices2 n) (starters n)

getNm :: [[a]] -> (Int, Int) -> a
getNm xss (r, c) = (xss !! r) !! c

diagonals :: Int -> [[a]] -> [[a]]
diagonals n xss = map (map (getNm xss)) $ diagIndices n

test50 :: [[Int]]
test50 = replicate 50 [1..50]

test5 :: [[Int]]
test5 = replicate 5 [1..5]

-- Parser (variable part)

parseRow n = do
  row <- count n $ oneOf "BR."
  eol <|> eof
  return row

parseSingleCase = do
  numRows <- parseInt
  char ' '
  numKs <- parseInt
  eol
  rows <- count numRows $ parseRow numRows
  return $ TestCase numRows numKs rows

eol :: GenParser Char st ()
eol = char '\n' >> return ()

parseIntegral :: Integral a => (String -> a) -> GenParser Char st a
parseIntegral rd = rd <$> (plus <|> minus <|> number)
    where
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

parseInteger :: GenParser Char st Integer
parseInteger = parseIntegral (read :: String -> Integer)

parseIntegers :: GenParser Char st [Integer]
parseIntegers = parseInteger `sepBy` (char ' ')

parseInt :: GenParser Char st Int
parseInt = parseIntegral (read :: String -> Int)

parseInts :: GenParser Char st [Int]
parseInts = parseInt `sepBy` (char ' ')

--
-- constant part
--

-- Parsing (constant part)

-- | First number is number of test cases
data TestInput = TestInput
                  Int -- ^ number of 'TestCase's
                  [TestCase]
  deriving (Show, Ord, Eq)

parseTestCases = do
  numCases <- parseInt
  eol
  cases <- count numCases parseSingleCase
  return $ TestInput numCases cases

parseCases :: String -> Either ParseError TestInput
parseCases contents = parse parseTestCases "(stdin)" contents

-- main

runOnContent :: String -> IO ()
runOnContent content = do
  let parsed = parseCases content
  case parsed of
    Right (TestInput _ cases) -> mapM_ putStrLn (output (solveCases cases))
    Left err -> hPutStrLn stderr $ show err
    where
      solveCases xs = map solveCase xs
      consCase n s = "Case #" ++ (show n) ++ ": " ++ s
      output xs = zipWith consCase [1..] xs

-- | command line implementation
run = do
  cs <- getContents
  runOnContent cs

main = run
