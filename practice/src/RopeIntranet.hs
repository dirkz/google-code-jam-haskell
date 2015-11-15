-- | Round 1C 2010
-- https://code.google.com/codejam/contest/619102/dashboard#s=p0
module RopeIntranet where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (sort, sortBy, foldl')

-- variable Data

data TestCase = TestCase
                  [(Int,Int)]
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase c@(TestCase tuples) = show $ solve $ sort tuples

solve :: [(Int,Int)] -> Int
solve = sum . map length . filter (not . null) . collect

collect :: [(Int,Int)] -> [[(Int,Int)]]
collect [] = [[]]
collect ((x,y):xs)
  | x /= y = filter match xs : collect xs
  | otherwise = collect xs
  where
    match (x',y')
      | y' < y = True
      | otherwise = False

-- Parser (variable part)

parseSingleCase = do
  numWires <- parseInt
  eol
  tuples <- count numWires parseWire
  return $ TestCase tuples

parseWire = do
  x <- parseInt
  char ' '
  y <- parseInt
  eol <|> eof
  return (x,y)

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
