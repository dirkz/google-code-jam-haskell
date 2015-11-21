-- | Qualification Round 2015 Problem D. Ominous Omino
-- https://code.google.com/codejam/contest/6224486/dashboard#s=p3
module OminousOmino where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (group, sort, sortBy, foldl', inits)
import Data.Char (ord)
import qualified Data.Map as M

-- variable Data

data TestCase = TestCase
                  Int -- ^ X
                  Int -- ^ R
                  Int -- ^ C
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase tc@(TestCase x r c) = solve tc

solve tc@(TestCase x r c) =
  if x >= 7
      || x == 3 && s == 1
      || x == 4 && s <= 2
      || x == 5 && (s <= 2 || (s, l) == (3, 5))
      || x == 6 && s <= 3
      || rest /= 0
     then rich
     else gabe
  where
    (_, rest) = (r * c) `divMod` x
    (s, l) = (min r c, max r c)
    rich = "RICHARD"
    gabe = "GABRIEL"

divise n
  | rest == 0 = (d, d)
  | rest == 1 = (d + 1, d)
    where
      (d, rest) = n `divMod` 2

-- Parser (variable part)

parseSingleCase = do
  x <- parseInt
  char ' '
  r <- parseInt
  char ' '
  c <- parseInt
  eol <|> eof
  return $ TestCase x r c

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
