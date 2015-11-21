-- | Qualification Round 2015 Problem C. Dijkstra
-- https://code.google.com/codejam/contest/6224486/dashboard#s=p2
module Dijkstra where

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
                  Int -- ^ L
                  Int -- ^ X
                  String -- ^ base string
  deriving (Show, Eq, Ord)

data Cont = One | I | J | K
  deriving (Show, Eq, Ord)

data Quat = Plus Cont | Minus Cont
  deriving (Show, Eq, Ord)

dict :: M.Map Quat (M.Map Quat Quat)
dict = M.fromList [
            (Plus One, M.fromList [
               (Plus One, Plus One)
              , (Plus I, Plus I)
              , (Plus J, Plus J)
              , (Plus K, Plus K)])
            , (Plus I, M.fromList [
               (Plus One, Plus I)
              , (Plus I, Minus One)
              , (Plus J, Plus K)
              , (Plus K, Minus J)])
            , (Plus J, M.fromList [
               (Plus One, Plus J)
              , (Plus I, Minus K)
              , (Plus J, Minus One)
              , (Plus K, Plus J)])
            , (Plus K, M.fromList [
               (Plus One, Plus K)
              , (Plus I, Plus J)
              , (Plus J, Minus I)
              , (Plus K, Minus One)])]

-- variable implementation

solveCase c@(TestCase l x base) = show $ solve c

solve c@(TestCase l x base) = (c, fstLevel len input)
  where
    input = concat $ take x $ repeat base
    quats = map charToQuat input
    len = l * x

fstLevel len input = filter (correct (Plus I)) $ parsed1 $ ninits 1 (len - 2) input

sndLevel = map refine
  where
    refine (q, s)

parsed1 = map (\(s, rest) -> (evalString s, rest))

correct n (m, o) = m == n

ninits :: Int -> Int -> String -> [(String, String)]
ninits lower upper xs = map (flip splitAt xs) [lower..upper]

evalString :: String -> Quat
evalString "" = error "Can't eval empty string"
evalString xs = foldl' combine (Plus One) $ map charToQuat xs

qnegate :: Quat -> Quat
qnegate (Plus m) = Minus m
qnegate (Minus m) = Plus m

mcombine :: Quat -> Quat -> Maybe Quat
mcombine (Minus n) (Minus m) = mcombine (Plus n) (Plus m)
mcombine (Minus n) (Plus m) = mcombine (Plus n) (Plus m) >>= (\r -> Just $ qnegate r)
mcombine (Plus n) (Minus m) = mcombine (Plus n) (Plus m) >>= (\r -> Just $ qnegate r)
mcombine n m = do
  m1 <- M.lookup n dict
  r <- M.lookup m m1
  return r

combine n m = case mcombine n m of
                Just o -> o
                Nothing -> error $ "Error combining " ++ show n ++ " " ++ show m

charToQuat :: Char -> Quat
charToQuat '1' = Plus One
charToQuat 'i' = Plus I
charToQuat 'j' = Plus J
charToQuat 'k' = Plus K

-- Parser (variable part)

parseSingleCase = do
  l <- parseInt
  char ' '
  x <- parseInt
  eol
  xs <- parseString
  eol <|> eof
  return $ TestCase l x xs

parseString = many1 (oneOf "1ijk")

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
