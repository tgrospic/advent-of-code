module AoC2016.Day09 where

import AoC.Util (ParserS, parseEff, pint, readFile)
import Control.Alt ((<|>))
import Control.Lazy (class Lazy)
import Data.Array (many, replicate, (:))
import Data.Either (Either(Right))
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Parsing (runParser)
import Parsing.String (anyChar, char)
import Parsing.Token (letter)
import Prelude (pure, ($), (*), (*>), (+), (<$>), (<*), (<*>), (<>), (>), (>>=))

-- Puzzle input
puzzleInput :: Effect String
puzzleInput = readFile "./src/AoC2016/puzzles/input-2016-day09.txt"

anyString :: Int → ParserS String
anyString x = fromCharArray <$> replicateA x anyChar

pword1 :: ParserS String
pword1 = fromCharArray <$> ((:) <$> letter <*> many letter)

pword :: ParserS String
pword = pword1 <|> pure mempty

pIntPair :: ParserS (Tuple Int Int)
pIntPair = Tuple <$> pint <* char 'x' <*> pint

pCode :: ParserS (Tuple Int Int)
pCode = char '(' *> pIntPair <* char ')'

mkSubP :: Tuple Int Int → ParserS String
mkSubP (Tuple x y) = foldl (<>) mempty <$> replicate y <$> anyString x

pCodeExpr :: ParserS String
pCodeExpr = pCode >>= mkSubP

expr :: ParserS String
expr = pword1 <|> ((<>) <$> pCodeExpr <*> pword)

grammar :: ParserS String
grammar = foldl (<>) mempty <$> many expr

-- runParser "A(6x2)a(2x5)cdefg" grammar
-- runParser puzzle $ S.length <$> grammar

-------------------------------------

pStrLen :: ParserS Number
pStrLen  = toNumber <$> S.length <$> pword

pStr1Len :: ParserS Number
pStr1Len = toNumber <$> S.length <$> pword1

mkSubPLen :: Tuple Int Int → ParserS Number
mkSubPLen (Tuple x y) = subLength <$> anyString x
  where
  subLength s = case runParser s grammarLen of
    Right x' | x' > 0.0 → toNumber y * x'
    _ → toNumber $ y * S.length s

pCodeLen :: Lazy (ParserS Number) ⇒ ParserS Number
pCodeLen = pCode >>= mkSubPLen

exprLen :: Lazy (ParserS Number) ⇒ ParserS Number
exprLen = pStr1Len <|> (+) <$> pCodeLen <*> pStrLen

grammarLen :: Lazy (ParserS Number) ⇒ ParserS Number
grammarLen = foldl (+) 0.0 <$> many exprLen

-- runParser "A(6x2)a(2x5)cdefg" grammarLen
-- runParser "X(8x2)(3x3)ABCY" grammarLen

part1 :: Effect Int
part1 = parseEff (S.length <$> grammar) puzzleInput

part2 :: Effect Number
part2 = parseEff grammarLen puzzleInput
