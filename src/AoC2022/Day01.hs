module AoC2022.Day01
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Text.Parsec.String (Parser)
import Util (parseIO, phead)
import Text.Parsec (ParseError, newline, sepBy1)
import Data.List (sort)
import Text.ParserCombinators.Parsec.Number (int)

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2022/puzzles/day-01.txt"

row :: Parser Int
row = int <* newline

elf :: Parser Int
elf = sum <$> many row

elfs :: Parser [Int]
elfs = sepBy1 elf newline

-- Elfs sorted by Calories (high top)
elfsSortedTop3 :: Parser [Int]
elfsSortedTop3 = take 3 . reverse . sort <$> elfs

-- Elf with max amount of Calories
elfMax :: Parser Int
elfMax = elfsSortedTop3 >>= phead

-- Elf with max amount of Calories
elfTop3Sum :: Parser Int
elfTop3Sum = sum <$> elfsSortedTop3

part1 :: IO Int
part1 = parseIO elfMax puzzleInput

part2 :: IO Int
part2 = parseIO elfTop3Sum puzzleInput
