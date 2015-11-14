-- | Round 1A 2008 A
-- https://code.google.com/codejam/contest/32016/dashboard#s=p0
module MinimumScalarProduct where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import Data.List (sort, sortBy)

-- variable Data

data TestCase = TestCase
                  Int -- ^ number of elements
                  [Integer] -- ^ vector 1
                  [Integer] -- ^ vector 2
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase :: TestCase -> String
solveCase c@(TestCase n xs ys) = show $ solve xs ys

solve :: [Integer] -> [Integer] -> Integer
solve xs ys = min s1 s2
  where
    s1 = prod xs1 ys1
    s2 = prod xs2 ys2
    xs1 = sortBy asc xs
    ys1 = sortBy desc ys
    xs2 = sortBy desc xs
    ys2 = sortBy asc ys

prod :: [Integer] -> [Integer] -> Integer
prod xs = sum . zipWith (*) xs

asc x y
  | x < y = LT
  | x == y = EQ
  | otherwise = GT

desc x y
  | x < y = GT
  | x == y = EQ
  | otherwise = LT

-- Parser (variable part)

parseSingleCase = do
  num <- parseInt
  eol
  xs <- parseIntegers
  eol
  ys <- parseIntegers
  eol
  return $ TestCase num xs ys

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

eol :: GenParser Char st Char
eol = char '\n'

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

parseWords :: GenParser Char st [String]
parseWords = parseWord `sepBy` (char ' ')

parseWord :: GenParser Char st String
parseWord = many1 (oneOf ['a'..'z'])

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
