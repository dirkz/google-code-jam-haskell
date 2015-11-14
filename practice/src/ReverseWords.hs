-- | Qualification Round Africa 2010 B
-- https://code.google.com/codejam/contest/351101/dashboard#s=p1
module ReverseWords where

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
                  [String] -- ^ list of words
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase :: TestCase -> String
solveCase c@(TestCase ws) = unwords $ reverse ws

-- Parser (variable part)

parseSingleCase = do
  words <- parseWords
  eol
  return $ TestCase words

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

parseWords :: GenParser Char st [String]
parseWords = parseWord `sepBy` (char ' ')

parseWord :: GenParser Char st String
parseWord = many1 (oneOf ['a'..'z'])

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
