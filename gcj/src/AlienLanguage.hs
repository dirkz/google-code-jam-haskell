-- | Qualification Round 2009 A
-- https://code.google.com/codejam/contest/90101/dashboard#s=p0
module AlienLanguage where

-- constant imports
import Text.ParserCombinators.Parsec
import Text.Parsec
import System.IO (openFile, hClose, hGetContents, hPutStrLn, IOMode(ReadMode), stderr)
import Debug.Trace (trace)

-- variable imports
import qualified Data.Set as S
import Data.List (sort, sortBy, foldl')

-- variable Data

type Dict = [String]

type Grammar = [S.Set Char]

data TestCase = TestCase
                  Dict
                  String -- ^ the combination to evaluate
  deriving (Show, Eq, Ord)

-- variable implementation

chars = ['a'..'z']

solveCase c@(TestCase words dict w) = show c

mkTokens :: Int -> [String] -> [Token]
mkTokens _ = map (S.fromList) . transpose

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
  where
    yss = cp xss

-- Parser (variable part)

parseTokens :: GenParser Char st [Token]
parseTokens = do
  tokens <- many parseToken
  eol <|> eof
  return tokens

parseToken :: GenParser Char st Token
parseToken = (char '(' >> parseMultis) <|> parseSingle

parseSingle :: GenParser Char st Token
parseSingle = do
  ch <- oneOf chars
  return $ S.fromList [ch]

parseMultis :: GenParser Char st Token
parseMultis = do
  chs <- many $ oneOf chars
  char ')'
  return $ S.fromList chs

parseWords :: GenParser Char st String
parseWords = do
  w <- parseWord
  eol
  return w

parseWord :: GenParser Char st String
parseWord = many1 (oneOf ['a'..'z'] <|> oneOf "()")

parseSingleCase words dict = do
  w <- parseWord
  eol
  return $ TestCase words dict w

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
                  Int -- ^ number of words
                  Int -- ^ word length (number of tokens)
                  [TestCase]
  deriving (Show, Ord, Eq)

parseTestCases = do
  numTokens <- parseInt
  char ' '
  numWords <- parseInt
  char ' '
  numCases <- parseInt
  eol
  words <- count numWords parseWords
  let tokens = mkTokens numTokens words
  cases <- count numCases (parseSingleCase (S.fromList words) tokens)
  return $ TestInput numCases numWords numTokens cases

parseCases :: String -> Either ParseError TestInput
parseCases contents = parse parseTestCases "(stdin)" contents

-- main

runOnContent :: String -> IO ()
runOnContent content = do
  let parsed = parseCases content
  case parsed of
    Right (TestInput _ _ _ cases) -> mapM_ putStrLn (output (solveCases cases))
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
