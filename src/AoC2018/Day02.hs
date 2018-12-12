module AoC2018.Day02
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Data.Foldable (asum)
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import Text.Parsec (Parsec, ParseError, char, parse, letter, endOfLine)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)
import qualified Data.MultiSet as MS
import qualified Data.Set as S

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2018/puzzles/day-02.txt"

run :: Parsec String () a -> IO (Either ParseError a)
run p = puzzleInput >>= pure <$> parse p ""

-- Tokens
tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

pnumber :: Parser Int
pnumber = fromInteger <$> natural tokenParser

-- Represents one line in puzzle
pline :: Parser String
pline = many letter <* endOfLine

-- Occurrence of letters
occurrence :: Functor f => f (MS.MultiSet a) -> f [(a, MS.Occur)]
occurrence = (<$>) MS.toOccurList

occur2and3 :: [(a, MS.Occur)] -> (Int, Int)
occur2and3 = foldl f (0, 0)
  where
    f (two, three) (_, x) =
      case x of
        2 -> (1, three)
        3 -> (two, 1)
        _ -> (two, three)

sumMult :: [(Int, Int)] -> Int
sumMult = multTuple . foldl1 sumTuples
  where
    sumTuples (x1, y1) (x2, y2) = (x1+x2, y1+y2)
    multTuple (x, y) = x * y

charSet :: Parser (MS.MultiSet Char)
charSet = MS.fromList <$> pline

multiLineOccur :: Parser [[(Char, MS.Occur)]]
multiLineOccur = many $ occurrence charSet

-- Checksum is multiply of counts of 2 and 3 letter occurrences
checksum :: Parser Int
checksum = sumMult . fmap occur2and3 <$> multiLineOccur

part1 :: IO (Either ParseError Int)
part1 = run checksum


-- PART 2
-- "abc" -> [(2,"ab"),(1,"ac"),(0,"bc")]
-- toLineData :: String -> [(Int, String)]
combinations :: Ord a => [a] -> [(Int, [a])]
combinations = snd . comb 0 ([], [])
  where
    comb i r@(pre, s) xs = case xs of
      []     -> r
      x:tail ->
        let v = (i, pre <> tail) in
        comb (i+1) (pre <> [x], v : s) tail

-- firstDupe :: [[(Int, String)]] -> Maybe (Int, String)
firstDupe :: (Foldable t, Ord a) => [t a] -> Maybe a
firstDupe xs = asum $ fst <$> scanl (foldl dupe) (Nothing, S.empty) xs
  where
    dupe (m, s) x =
      if S.member x s
      then (Just x, s)
      else (m, S.insert x s)

firstDupeParser :: Parser (Maybe String)
firstDupeParser = fmap snd . firstDupe . fmap combinations <$> many pline

part2 :: IO (Either ParseError (Maybe String))
part2 = run firstDupeParser
