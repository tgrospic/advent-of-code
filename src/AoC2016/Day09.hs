module AoC2016.Day09
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad (replicateM)
import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, char, parse)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

pnumber :: Parser Int
pnumber = fromInteger <$> natural tokenParser

anyString :: Int -> Parser String
anyString x = replicateM x anyChar

pword1 :: Parser String
pword1 = (:) <$> letter <*> many letter

pword :: Parser String
pword = pword1 <|> pure mempty

pIntPair :: Parser (Int, Int)
pIntPair = (,) <$> pnumber <* char 'x' <*> pnumber

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

puzzleInput :: IO String -- Eff ( EXCEPTION )
puzzleInput = readFile "./src/2016/puzzles/input-2016-day09.txt"

run :: Parsec String () a -> IO (Either ParseError a)
run p = puzzleInput >>= pure <$> parse p ""

part1 :: IO (Either ParseError Int)
part1 = run (length <$> grammar)

part2 :: IO (Either ParseError Int)
part2 = run grammarLen

