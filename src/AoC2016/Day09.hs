module AoC2016.Day09
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad (replicateM)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, char, parse)
import Text.Parsec.String (Parser)
import Util (parseIO)
import Text.ParserCombinators.Parsec.Number (int)

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2016/puzzles/input-2016-day09.txt"

anyString :: Int -> Parser String
anyString x = replicateM x anyChar

pword1 :: Parser String
pword1 = (:) <$> letter <*> many letter

pword :: Parser String
pword = pword1 <|> pure mempty

pIntPair :: Parser (Int, Int)
pIntPair = (,) <$> int <* char 'x' <*> int

pCode :: Parser (Int, Int)
pCode = char '(' *> pIntPair <* char ')'

mkSubP :: (Int, Int) -> Parser String
mkSubP (x, y) = foldl (<>) mempty <$> replicate y <$> anyString x

pCodeExpr :: Parser String
pCodeExpr = pCode >>= mkSubP

expr :: Parser String
expr = pword1 <|> ((<>) <$> pCodeExpr <*> pword)

grammar :: Parser String
grammar = foldl (<>) mempty <$> many expr

-- parse grammar "" "A(6x2)a(2x5)cdefg"
-- parse (length <$> grammar) "" "A(6x2)a(2x5)cdefg"

-------------------------------------

pStrLen :: Parser Int
pStrLen  = length <$> pword

pStr1Len :: Parser Int
pStr1Len = length <$> pword1

mkSubPLen :: (Int, Int) -> Parser Int
mkSubPLen (x, y) = subLength <$> anyString x
  where
  subLength s = case parse grammarLen "" s of
    Right x' | x' > 0 -> y * x'
    _ -> y * length s

pCodeLen :: Parser Int
pCodeLen = pCode >>= mkSubPLen

exprLen :: Parser Int
exprLen = pStr1Len <|> (+) <$> pCodeLen <*> pStrLen

grammarLen :: Parser Int
grammarLen = foldl (+) 0 <$> many exprLen

-- parse grammarLen "" "A(6x2)a(2x5)cdefg"
-- parse grammarLen "" "X(8x2)(3x3)ABCY"

part1 :: IO Int
part1 = parseIO (length <$> grammar) puzzleInput

part2 :: IO Int
part2 = parseIO grammarLen puzzleInput

