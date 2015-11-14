-- | Qualification Round Africa 2010 C
-- https://code.google.com/codejam/contest/351101/dashboard#s=p2
module T9Spelling where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import Control.Exception (bracket)
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Map.Strict as M
import Data.List (foldl')

-- variable Data

type TransTable = M.Map Char String

data TestCase = TestCase String
  deriving (Show, Eq, Ord)

alphaGroups = ["abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]

-- |'alphaGroups' indexed by their keyboard symbol
transList :: [(String, Char)]
transList = zipWith (,) alphaGroups (map (head . show) [2..])

-- |Generated translation table (uses 'transList' as input)
transTable :: TransTable
transTable = M.insert ' ' "0" m1
  where
    m1 = foldl' f1 (M.empty :: TransTable) transList
    f1 :: TransTable -> (String, Char) -> TransTable
    f1 m (s, c) = foldl' (f2 c) m (zipWith (,) [1..] s)
    f2 :: Char -> TransTable -> (Int, Char) -> TransTable
    f2 c m (i, ch) = M.insert ch (take i $ repeat c) m

translateCh :: Char -> String
translateCh ch =
  case (M.lookup ch transTable) of
    Nothing -> error "undefined char '" ++ (show ch) ++ "'"
    Just s -> s

translateSimple :: String -> [String]
translateSimple = map translateCh

glue :: [String] -> String
glue = fst . foldl' glueF ("", "")
  where
    glueF :: (String, String) -> String -> (String, String)
    glueF (s1, prev) s2
      | null s1 = (s2, s2)
      | otherwise =
          let
            h1 = head prev
            h2 = head s2
          in
            if h1 == h2 then (s1 ++ " " ++ s2, s2)
                        else (s1 ++ s2, s2)

translate :: String -> String
translate = glue . translateSimple

-- variable implementation

solveCase :: TestCase -> String
solveCase (TestCase w) = translate w

-- Parser (variable part)

parseWord = many1 $ oneOf "abcdefghijklmnopqrstuvwxyz "

parseSingleCase = do
  word <- parseWord
  eol
  return $ TestCase word

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
