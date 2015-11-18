-- | Round 1C 2009 Problem A. All Your Base
-- https://code.google.com/codejam/contest/189252/dashboard#s=p0
module AllYourBase where

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
                  String -- ^ The number as string
  deriving (Show, Eq, Ord)

type Mults = M.Map Char Integer

-- variable implementation

solveCase c@(TestCase digits) = show $ solve c

solve c@(TestCase digits) = fst $ interpret base mults digits
  where
    mults = allDigits digits
    base1 = fromIntegral $ M.size mults
    base = if base1 == 1 then 2 else base1

interpret :: Integer -> Mults -> String -> (Integer, Integer)
interpret base mults = foldr fn (0, 1)
  where
    fn ch (total, b) =
      let m = M.lookup ch mults
      in case m of
           Nothing -> error "Could not lookup"
           Just n -> (total + n * b, b * base)

digits :: [Integer]
digits = 1 : 0 : [2..]

emptyDigits = M.empty :: Mults

allDigits :: String -> Mults
allDigits = fst . foldl' trav (emptyDigits, digits)

trav :: (Mults, [Integer]) -> Char -> (Mults, [Integer])
trav (ms, exps) ch =
  case M.lookup ch ms of
    Nothing -> (M.insert ch next ms, tail exps)
    Just _ -> (ms, exps)
  where
    next = head exps

-- Parser (variable part)

parseSingleCase = do
  s <- many1 (oneOf (concat [['a'..'z'], ['0'..'9']]))
  eol <|> eof
  return $ TestCase s

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
