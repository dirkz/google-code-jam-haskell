-- | Round 1B 2010
-- https://code.google.com/codejam/contest/635101/dashboard#s=p0
module FileFixIt where

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
                  [String] -- ^ existing dirs
                  [String] -- ^ to be created
  deriving (Show, Eq, Ord)

type Fs = S.Set Dir -- ^ Filesystem

type Dir = [String] -- ^ Directory

emptyFs = S.empty :: Fs

-- variable implementation

solveCase c@(TestCase dirs new) = show $ solve dirs new

solve :: [String] -> [String] -> Int
solve dirs new = S.size fs2 - S.size fs1
  where
    fs1 :: Fs
    fs1 = mkdirAll emptyFs dirs
    fs2 :: Fs
    fs2 = mkdirAll fs1 new

strToDir :: String -> Dir
strToDir s = filter (not . null) $ nth : ls
  where
    (nth, ls) = foldr fn ("", []) s
    fn ch (cur, ls)
      | ch == '/' = ("", cur : ls)
      | otherwise = (ch : cur, ls)

mkdir :: Fs -> String -> Fs
mkdir fs [] = fs
mkdir fs path = foldl' inserter fs initials
  where
    parts = strToDir path
    initials = filter (not . null) (inits parts)
    inserter fs path = S.insert path fs

mkdirAll :: Fs -> [String] -> Fs
mkdirAll fs = foldl' mkdir fs

-- Parser (variable part)

parseDir = do
  dir <- many1 (oneOf ('/':['a'..'z'] ++ ['0'..'9']))
  eol <|> eof
  return dir

parseSingleCase = do
  numExisting <- parseInt
  char ' '
  numCreate <- parseInt
  eol
  dirs <- count numExisting parseDir
  create <- count numCreate parseDir
  return $ TestCase dirs create

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
