module AoC2018.Day01
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Text.Parsec (Parsec, ParseError, char, parse, newline)
import Text.Parsec.String (Parser)
import qualified Data.Set as S
import Util (parseIO, liftMaybe)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Maybe (listToMaybe)
import Data.Foldable (find)

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2018/puzzles/day-01.txt"

pplus :: Parser (Int -> Int)
pplus = (+) <$> (char '+' *> int <* newline)

pminus :: Parser (Int -> Int)
pminus = flip (-) <$> (char '-' *> int <* newline)

poperations :: Parser [(Int -> Int)]
poperations = many $ pplus <|> pminus

-- Execute all operations
executeAll :: Int -> [(Int -> Int)] -> Int
executeAll = foldl $ flip ($)

-- Finds duplicate result
duplicates :: (Bool, Int, S.Set Int) -> [(Int -> Int)] -> [(Bool, Int, S.Set Int)]
duplicates = scanl execOp
  where
    execOp (_, acc, s) op =
      let r = op acc in
      (S.member r s, r, S.insert r s)

operationsResult :: Parser Int
operationsResult  = executeAll 0 <$> poperations

firstDuplicate :: Parser Int
firstDuplicate = poperations >>= liftMaybe "no duplicates" . firstDupe
  where
    firstDupe xs = sndOf3 <$> (find fstOf3 . searchDupe) xs
    searchDupe = filter fstOf3 . duplicates (False, 0, S.insert 0 S.empty) . cycle
    fstOf3 (b, _, _) = b
    sndOf3 (_, v, _) = v

part1 :: IO Int
part1 = parseIO operationsResult puzzleInput

part2 :: IO Int
part2 = parseIO firstDuplicate puzzleInput
