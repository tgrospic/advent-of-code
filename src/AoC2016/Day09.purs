module AoC2016.Day09 where

import Control.Alt ((<|>))
import Control.Lazy (class Lazy)
import Data.Array (many, replicate, singleton, (:))
import Data.Either (Either(Right))
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (resolve)
import Node.Process (cwd)
import Parsing (ParseError, Parser, runParser)
import Parsing.Language (haskellDef)
import Parsing.String (anyChar, char)
import Parsing.Token (TokenParser, letter, makeTokenParser)
import Prelude (flip, pure, ($), (*), (*>), (+), (<$>), (<*), (<*>), (<>), (=<<), (>), (>>=))

type ParserS = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser haskellDef

pnumber :: ParserS Int
pnumber = tokenParser.integer

anyString :: Int → ParserS String
anyString x = fromCharArray <$> replicateA x anyChar

pword1 :: ParserS String
pword1 = fromCharArray <$> ((:) <$> letter <*> many letter)

pword :: ParserS String
pword = pword1 <|> pure mempty

pIntPair :: ParserS (Tuple Int Int)
pIntPair = Tuple <$> pnumber <* char 'x' <*> pnumber

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

puzzleInput :: Effect String
puzzleInput = readTextFile UTF8 =<< filePath
  where
  filePath = flip resolve "./src/AoC2016/puzzles/input-2016-day09.txt" <$> singleton =<< cwd

-- run :: ∀ a. ParserS a → IOProcFsEx (Either ParseError a)
run :: ∀ a. ParserS a → Effect (Either ParseError a)
run p = flip runParser p <$> puzzleInput

part1 :: Effect (Either ParseError Int)
part1 = run (S.length <$> grammar)

part2 :: Effect (Either ParseError Number)
part2 = run grammarLen
