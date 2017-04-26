module AoC2016.Day09 where

import Data.String as S
import Control.Alternative ((<|>))
import Control.Lazy (class Lazy)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (many, replicate, singleton, (:))
import Data.Either (Either(Right))
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Data.Sequence (Seq)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (resolve)
import Node.Process (PROCESS, cwd)
import Prelude (flip, pure, ($), (*), (*>), (+), (<$>), (<*), (<*>), (<>), (=<<), (>), (>>=))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (anyChar, char)
import Text.Parsing.Parser.Token (TokenParser, letter, makeTokenParser)

type ParserS = Parser String

tokenParser :: TokenParser
tokenParser = makeTokenParser haskellDef

pnumber :: ParserS Int
pnumber = tokenParser.integer

toString :: Seq Char → String
toString = foldl (\a b → a <> S.singleton b) mempty

anyString :: Int → ParserS String
anyString x = toString <$> replicateA x anyChar

pword1 :: ParserS String
pword1 = S.fromCharArray <$> ((:) <$> letter <*> many letter)

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

type IOProcFsEx a = ∀ eff. Eff ( process :: PROCESS, fs :: FS, exception :: EXCEPTION | eff ) a

puzzleInput :: IOProcFsEx String
puzzleInput = readTextFile UTF8 =<< filePath
  where
  filePath = flip resolve "./src/AoC2016/puzzles/input-2016-day09.txt" <$> singleton <$> cwd

run :: ∀ a. ParserS a → IOProcFsEx (Either ParseError a)
run p = flip runParser p <$> puzzleInput

part1 :: IOProcFsEx (Either ParseError Int)
part1 = run (S.length <$> grammar)

part2 :: IOProcFsEx (Either ParseError Number)
part2 = run grammarLen
