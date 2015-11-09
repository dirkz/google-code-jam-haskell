-- | Round 1A 2008 B
module Milkshakes where

import Text.ParserCombinators.Parsec
import Text.Parsec
import Control.Exception
import System.IO
import qualified Data.Map as M

type ShakeId = Int
type CustomerId = Int
type Count = Int

-- | First number is number of test cases
data TestInput = TestInput Count [TestCase]
  deriving (Show, Ord, Eq)

-- | Number of flavors, number of customers
data TestCase = TestCase Count Count [Likes]
  deriving (Show, Ord, Eq)

-- | Single preference tuple (shake number and flavor)
data Preference = Preference ShakeId Flavor
  deriving (Show, Ord, Eq)

data Flavor = Malted | Unmalted
  deriving (Show, Ord, Eq)

data Likes = Likes Count [Preference]
  deriving (Show, Ord, Eq)

-- | Map from customer to shake to list of liked flavors
type Prefs = M.Map CustomerId (M.Map ShakeId [Flavor])

solveCase :: TestCase -> String
solveCase (TestCase numFlavors numCustomers likes) =
  show prefs
    where
      prefs = zipWith zipper [1..] likes
      zipper customer prefs = (customer,prefs)

solve :: String -> IO ()
solve content = do
  let parsed = parseCases content
  case parsed of
    Right (TestInput _ cases) -> mapM_ putStrLn (output (solveCases cases))
    Left err -> hPutStrLn stderr $ show err
    where
      solveCases xs = map solveCase xs
      consCase n s = "Case #" ++ (show n) ++ ": " ++ s
      output xs = zipWith consCase [1..] xs

-- Parser

numToFlavor :: Int -> Flavor
numToFlavor 0 = Unmalted
numToFlavor 1 = Malted
numToFlavor _ = undefined

eol :: GenParser Char st Char
eol = char '\n'

parseInt :: GenParser Char st Int
parseInt = (read :: String -> Int) <$> many1 digit

parsePref :: GenParser Char st Preference
parsePref = do
  char ' '
  shake <- parseInt
  char ' '
  malted <- parseInt
  return $ Preference shake (numToFlavor malted)

parsePrefLine :: GenParser Char st Likes
parsePrefLine = do
  numPrefs <- parseInt
  prefs <- count numPrefs parsePref
  eol
  return $ Likes numPrefs prefs

parseSingleCase = do
  numFlavors <- parseInt
  eol
  numCustomers <- parseInt
  eol
  prefs <- count numCustomers parsePrefLine
  return $ TestCase numFlavors numCustomers prefs

parseTestCases = do
  numCases <- parseInt
  eol
  cases <- count numCases parseSingleCase
  return $ TestInput numCases cases

parseCases :: String -> Either ParseError TestInput
parseCases contents = parse parseTestCases "(stdin)" contents

testParse = bracket (openFile "test/sample.txt" ReadMode) hClose process
  where
    process h = do
      contents <- hGetContents h
      let cases = parseCases contents
      print cases

testSolve = bracket (openFile "test/sample.txt" ReadMode) hClose process
  where
    process h = do
      contents <- hGetContents h
      solve contents

main = do
  cs <- getContents
  solve cs
