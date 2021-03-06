-- | Qualification Round 2015 Problem A. Standing Ovation
-- https://code.google.com/codejam/contest/6224486/dashboard
module StandingOvation where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (group, sort, sortBy, foldl')
import Data.Char (ord)
import qualified Data.Map as M

-- variable Data

data TestCase = TestCase
                  Int -- ^ Max shyness level
                  String -- ^ Digits as String
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase c@(TestCase n digits) = show $ solve c

solve c@(TestCase n digits) = snd $ foldl' fn (0, 0) $ zipWith (,) [0..] $ map toNum digits

fn (st, added) (lvl, num) =
  if st < lvl
     then (st + diff + num, added + diff)
     else (st + num, added)
  where
    diff = lvl - st

toNum ch = ord ch - ord '0'

-- Parser (variable part)

parseSingleCase = do
  num <- parseInt
  char ' '
  s <- many1 (oneOf ['0'..'9'])
  eol <|> eof
  return $ TestCase num s

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
