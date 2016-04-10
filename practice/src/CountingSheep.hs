-- | Qualification Round 2016
-- https://code.google.com/codejam/contest/6254486/dashboard#s=p0
module CountingSheep where

import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Debug.Trace (trace)

type TestCase = Int

digits :: Integer -> [Int]
digits = map convert . show
 where
   convert :: Char -> Int
   convert d = read [d]

step s1 n1 n =
  if complete newSet
     then n
     else step newSet n1 (n + n1)
    where
      ds = digits n
      newSet = S.union s1 (S.fromList ds)

complete :: S.Set Int -> Bool
complete s = S.member 9 s && S.findIndex 9 s == 9

solve n = step S.empty n n

--- Parser

parseInt :: GenParser Char st Int
parseInt = many1 (oneOf ['0'..'9']) >>= return . (read :: String -> Int)

eol :: GenParser Char st String
eol = try (string "\n\r") <|> try (string "\r\n") <|> try (string "\n") <|>
      try (string "\r") <?> "eol"

parseNumberOfTestCases :: GenParser Char st Int
parseNumberOfTestCases = do
  n <- parseInt
  spaces
  eol
  return n

parseTestCase :: GenParser Char st Int
parseTestCase = parseInt

parseTestCases :: GenParser Char st [Int]
parseTestCases = do
  n <- parseNumberOfTestCases
  count n parseTestCase
