-- | Round 1A 2015 Problem A. Mushroom Monster
-- https://code.google.com/codejam/contest/4224486/dashboard
module MushroomMonster where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (group, sort, sortBy, foldl', foldl1', inits)
import Data.Char (ord)
import qualified Data.Map as M

-- variable Data

data TestCase = TestCase
                  Int -- ^ Number of intervals
                  [Int] -- ^ Intervals
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase tc@(TestCase n xs) = show c1 ++ " " ++ (show c2)
  where
    c1 = case1 xs
    c2 = case2 xs

deltas xs = zipWith (-) xs (tail xs)

case1 = sum . filter (>0) . deltas

case2 :: [Int] -> Int
case2 xs = if rate <= 0
              then 0
              else sum $ map (min rate) $ init xs
  where
    rate = maximum $ deltas xs

-- Parser (variable part)

parseSingleCase = do
  n <- parseInt
  eol
  xs <- parseInts
  eol <|> eof
  return $ TestCase n xs

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
