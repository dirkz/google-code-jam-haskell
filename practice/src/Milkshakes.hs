-- | Round 1A 2008 B
-- https://code.google.com/codejam/contest/32016/dashboard#s=p1
module Milkshakes where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import Control.Exception (bracket)
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')

-- variable Data

type ShakeId = Int
type CustomerId = Int

-- | Number of shakes, number of customers
data TestCase = TestCase Count Count [Likes]
  deriving (Show, Ord, Eq)

-- | Single preference tuple (shake number and flavor)
data Preference = Preference ShakeId Flavor
  deriving (Show, Ord, Eq)

data Flavor = Malted | Unmalted
  deriving (Show, Ord, Eq)

data Likes = Likes Count [Preference]
  deriving (Show, Ord, Eq)

-- variable implementation

solveCase :: TestCase -> String
solveCase c@(TestCase n _ _) = string $ check (customers c) batch
  where
    batch = zipWith (,) [1..n] (repeat Unmalted)
    string [] = "IMPOSSIBLE"
    string xs = foldl' spacer "" $ map (\(_, fl) -> toS fl) xs
    spacer a e = if a == "" then e else a ++ " " ++ e
    toS Malted = "1"
    toS Unmalted = "0"

-- |Converts a 'TestCase' to a list of tuples of customers and their 'Preference's
customers :: TestCase -> [(CustomerId, [Preference])]
customers (TestCase _ _ likes) = map build iLikes
  where
    iLikes = zipWith (,) [1..] likes
    build (i,(Likes _ prefs)) = (i,prefs)

-- |Is the given customer ok with the given batch of flavors?
customerOk :: (CustomerId, [Preference]) -> [(ShakeId, Flavor)] -> Bool
customerOk (_, prefs) shakes = not $ null $ filter (==True) $ map match1 prefs
  where
    match1 pref = not $ null $ filter (\(s, fl) -> (Preference s fl) == pref) shakes

-- |List of Bools that denote whether the customer at this position is ok with the
-- batch or not.
checkAllCustomers :: [(CustomerId, [Preference])] -> [(ShakeId, Flavor)] -> [Bool]
checkAllCustomers custs shakes = map (flip customerOk shakes) custs

check :: [(CustomerId, [Preference])] -> [(ShakeId, Flavor)] -> [(ShakeId, Flavor)]
check custs shakes
  | null shakes = []
  | all (==True) satisfied = shakes -- all are satisfied, finished!
  | otherwise =
      if null unSatisfied
         then error "ERROR: Not all satisfied, but none unsatisfied"
         else
            let
              next = head unSatisfied
              cust = custs !! (next - 1)
              batch = checkCustomer cust shakes
            in check custs batch
  where
    satisfied = checkAllCustomers custs shakes
    iSatisfied = zipWith (,) [1..] satisfied
    unSatisfied = (map fst . filter (\(i, b) -> b == False)) iSatisfied
    allMalted = all (==Malted) $ map snd shakes

-- |If modifying the batches helps the customer to like something, return that batch.
-- Otherwise, return []
checkCustomer :: (CustomerId, [Preference]) -> [(ShakeId, Flavor)] -> [(ShakeId, Flavor)]
checkCustomer c@(s, prefs) shakes
  | null malted = []
  | otherwise =
    let
      (Preference shake _) = head malted
      prs = filter (\(i, _) -> i == shake) shakes
      (_, fl) = head prs
    in if null prs
          then []
          else if fl == Malted
                  then []
                  else changeShake shake Malted shakes
  where
    malted = getMalted c

changeShake :: Int -> Flavor -> [(ShakeId, Flavor)] -> [(ShakeId, Flavor)]
changeShake i fl = map chng
  where chng (i', fl') = if i' == i then (i, fl)
                                    else (i', fl')

-- |Get the customer's one malted preference, or []
getMalted :: (CustomerId, [Preference]) -> [Preference]
getMalted (_, prefs) = filter (\(Preference s fl) -> fl == Malted) prefs

-- Parser (variable part)

numToFlavor :: Int -> Flavor
numToFlavor 0 = Unmalted
numToFlavor 1 = Malted
numToFlavor _ = undefined

parsePref :: GenParser Char st Preference
parsePref = do
  char ' '
  shake <- parseInt
  char ' '
  malted <- parseInt
  return $ Preference shake (numToFlavor malted)

parsePrefLine :: GenParser Char st Likes
parsePrefLine = do
  numPrefMap <- parseInt
  prefs <- count numPrefMap parsePref
  eol
  return $ Likes numPrefMap prefs

parseSingleCase = do
  numFlavors <- parseInt
  eol
  numCustomers <- parseInt
  eol
  prefs <- count numCustomers parsePrefLine
  return $ TestCase numFlavors numCustomers prefs

--
-- constant part
--

-- Parsing (constant part)

type Count = Int

-- | First number is number of test cases
data TestInput = TestInput Count [TestCase]
  deriving (Show, Ord, Eq)

eol :: GenParser Char st Char
eol = char '\n'

parseInt :: GenParser Char st Int
parseInt = (read :: String -> Int) <$> many1 digit

parseTestCases = do
  numCases <- parseInt
  eol
  cases <- count numCases parseSingleCase
  return $ TestInput numCases cases

parseCases :: String -> Either ParseError TestInput
parseCases contents = parse parseTestCases "(stdin)" contents

-- ghci testing

testParse = bracket (openFile "test/B-small-practice.in" ReadMode) hClose process
  where
    process h = do
      contents <- hGetContents h
      let cases = parseCases contents
      print cases

testSolve = bracket (openFile "test/B-small-practice.in" ReadMode) hClose process
  where
    process h = do
      contents <- hGetContents h
      runOnContent contents

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
