-- | Qualification Round 2015 Problem B. Infinite House of Pancakes
-- https://code.google.com/codejam/contest/6224486/dashboard#s=p1
module InfiniteHouseOfPancakesOrig where

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
                  Int -- ^ Number of diners
                  [Int] -- ^ List of number of pancakes on each plate
  deriving (Show, Eq, Ord)

-- |Maps number of pancakes to count (histogram of pancake number)
type Plates = M.Map Int Int

-- variable implementation

empty :: Plates
empty = M.empty

solveCase c@(TestCase n plates) = show $ solve c

solve c@(TestCase n plates) = loop 0 $ histogram plates

loop :: Int -> Plates -> Int
loop rounds hist =
  case maxK of
    0 -> rounds
    otherwise ->
      if maxK <= newMax + maxNum
         then loop (rounds + 1) (eat hist)
         else loop (rounds + maxNum) $ halfKey maxK hist
  where
    -- maxK is the current maximum number of minutes needed (by just eating),
    -- maxNum the number of minutes needed for transferring pancakes in order
    -- to have newMax minutes needed left for consumption
    (maxK, maxNum) = getMax hist
    (sndK, sndNum) = getSndMax hist
    newMax = max (half maxK) sndK

halfKey :: Int -> Plates -> Plates
halfKey k m = M.insertWith (+) kNew2 n $ M.insertWith (+) kNew n $ M.delete k m
  where
    kNew = half k
    kNew2 = k - kNew
    n = M.findWithDefault 0 k m

half :: Int -> Int
half n
  | r == 0 = d
  | otherwise = d + 1
  where
    (d, r) = n `divMod` 2

getK k = M.findWithDefault 0 k

histogram :: [Int] -> Plates
histogram = foldl' fn empty
  where
    fn m n = M.insertWith (+) n 1 m

eat :: Plates -> Plates
eat = M.foldWithKey fn empty
  where
    fn k a m = M.insert (k - 1) a m

getMax :: Plates -> (Int, Int)
getMax m
  | M.size m > 0 = M.findMax m
  | otherwise = (0, 0)

getSndMax :: Plates -> (Int, Int)
getSndMax m
  | M.size m > 1 = res keys
  | otherwise = (0, 0)
  where
    keys = sortBy keyDesc $ M.toList m
    res (x:y:xs) = y

keyDesc (k1, _) (k2, _) = invertOrdering $ compare k1 k2

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering GT = LT
invertOrdering EQ = EQ

-- Parser (variable part)

parseSingleCase = do
  num <- parseInt
  eol
  xs <- parseInts
  eol <|> eof
  return $ TestCase num xs

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
