-- | Qualification Round Africa 2010 A
-- https://code.google.com/codejam/contest/351101/dashboard#s=p0
module StoreCredit where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import Control.Exception (bracket)
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import Data.List (sort)

-- variable Data

data TestCase = TestCase
                  Int -- ^ credit
                  Int -- ^ number of items/prices
                  [Price] -- ^ prices
  deriving (Show, Eq, Ord)

data Price = Price
              Int -- ^ index
              Int -- ^ price
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase :: TestCase -> String
solveCase c@(TestCase cr _ prs) = unwords $ map show $ solve cr prs

solve :: Int -> [Price] -> [Int]
solve cr prs = sort $ map getIndex $ head $ filter sumP good
  where
    good = cp $ suitable cr prs
    sumP ((Price _ a1):(Price _ a2):[]) = a1 + a2 == cr
    getIndex :: Price -> Int
    getIndex (Price i _) = i

suitable :: Int -> [Price] -> [Price]
suitable cr = filter (\(Price _ pr) -> pr <= cr)

cp :: [a] -> [[a]]
cp [] = []
cp (x:xs) = [[x,y] | y <- xs] ++ cp xs

-- Parser (variable part)

parseSingleCase = do
  credit <- parseInt
  eol
  numItems <- parseInt
  eol
  amounts <- parseInts
  let prices1 = zipWith (,) [1..] amounts
  let prices = map (\(i,a) -> Price i a) prices1
  eol
  return $ TestCase credit numItems prices

--
-- constant part
--

-- Parsing (constant part)

-- | First number is number of test cases
data TestInput = TestInput
                  Int -- ^ number of 'TestCase's
                  [TestCase]
  deriving (Show, Ord, Eq)

eol :: GenParser Char st Char
eol = char '\n'

parseInt :: GenParser Char st Int
parseInt = (read :: String -> Int) <$> many1 digit

parseInts :: GenParser Char st [Int]
parseInts = parseInt `sepBy` (char ' ')

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
