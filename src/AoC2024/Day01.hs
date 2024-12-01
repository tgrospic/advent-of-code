module AoC2024.Day01
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Data.Foldable (Foldable(toList))
import Data.List (sort)
import Data.Set (fromList)
import Text.Parsec (ParseError, newline, sepBy1, many1, space)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)
import Util (parseIO, phead)
import qualified Data.MultiSet as MS

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2024/puzzles/day-01.txt"

pRow :: Parser (Int, Int)
pRow = (,) <$> int <* many1 space <*> int <* newline

pLists :: Parser ([Int], [Int])
pLists = unzip <$> many1 pRow

-- Part 1

diffAll :: Num a => Ord a => ([a], [a]) -> a
diffAll (xsl, xsr) =
  sum $ diff <$> zip (sort xsl) (sort xsr)
  where
    diff (x, y) = if x > y then x - y else y - x

-- Part 2

similarityScore :: ([Int], [Int]) -> Int
similarityScore (xsl, xsr) =
  let r_mset = MS.fromList xsr in
  sum $ numScore r_mset <$> xsl
  where
    numScore mset x = x * MS.occur x mset

-- Results

part1 :: IO Int
part1 = parseIO (diffAll <$> pLists) puzzleInput

part2 :: IO Int
part2 = parseIO (similarityScore <$> pLists) puzzleInput
