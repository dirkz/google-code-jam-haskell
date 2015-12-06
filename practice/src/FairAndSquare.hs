-- | Qualification Round 2013
-- https://code.google.com/codejam/contest/2270488/dashboard#s=p2
module FairAndSquare where

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
                  Integer -- ^ A
                  Integer -- ^ B
  deriving (Show, Eq, Ord)

-- variable implementation

solveCase tc@(TestCase x y) = show $ solve tc

solve tc@(TestCase x y) = length $ takeWhile (<=y) $ dropWhile (<x) all14

-- fairAndSquare 0 (10^14)
all14 = [0,1,4,9,121,484,10201,12321,14641,40804,44944,1002001
        ,1234321,4008004,100020001,102030201,104060401,121242121,123454321
        ,125686521,400080004,404090404,10000200001,10221412201,12102420121
        ,12345654321,40000800004,1000002000001,1002003002001,1004006004001
        ,1020304030201,1022325232201,1024348434201,1210024200121,1212225222121
        ,1214428244121,1232346432321,1234567654321,4000008000004,4004009004004]

fairAndSquare x y = takeWhile (<= y) $ dropWhile (< x) $ generate $ isqrt x

generate s = filter palindrome $ map square $ filter palindrome [s..]
  where
    square n = n * n

isqrt :: Integer -> Integer
isqrt n = floor $ sqrt $ fromInteger n

palindrome :: Integer -> Bool
palindrome n = ds == (reverse ds)
  where
    ds = digits n

digits :: Integer -> [Integer]
digits i = map fst $ takeWhile notNull $ drop 1 $ iterate fn (0, i)
  where
    fn (acc, n) =
      let (d, rest) = n `divMod` 10
      in (rest, d)
    notNull (x, y) = x > 0 || y > 0

-- Parser (variable part)

parseSingleCase = do
  x <- parseInteger
  char ' '
  y <- parseInteger
  eol <|> eof
  return $ TestCase x y

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
