module AoC2022.Day01
  ( part1
  , part2
  )
  where

import AoC.Util (ParserS, parseEff, phead, pnewline, pint, readFile)
import Data.Array (many)
import Data.List.NonEmpty (reverse, sort, take)
import Data.List.Types (List, NonEmptyList)
import Data.Traversable (sum)
import Effect (Effect)
import Parsing.Combinators (sepBy1)
import Prelude ((<$>), (<*), (<<<), (>>=))

-- Puzzle input
puzzleInput :: Effect String
puzzleInput = readFile "./src/AoC2022/puzzles/day-01.txt"

row :: ParserS Int
row = pint <* pnewline

elf :: ParserS Int
elf = sum <$> many row

elfs :: ParserS (NonEmptyList Int)
elfs = sepBy1 elf pnewline

-- Elfs sorted by Calories (high on top)
elfsSortedTop3 :: ParserS (List Int)
elfsSortedTop3 = take 3 <<< reverse <<< sort <$> elfs

-- Elf with max amount of Calories
elfMax :: ParserS Int
elfMax = elfsSortedTop3 >>= phead

-- Elf with max amount of Calories
elfTop3Sum :: ParserS Int
elfTop3Sum = sum <$> elfsSortedTop3

part1 :: Effect Int
part1 = parseEff elfMax puzzleInput

part2 :: Effect Int
part2 = parseEff elfTop3Sum puzzleInput
