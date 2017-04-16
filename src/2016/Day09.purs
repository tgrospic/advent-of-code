module AoC2016.Day09 where

import Data.Sequence as Seq
import Data.String as S
import Data.Unfoldable as U
import Control.Alternative ((<|>))
import Control.Lazy (class Lazy)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (try)
import Data.Array (many, replicate, singleton, (:))
import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (resolve)
import Node.Process (PROCESS, cwd)
import Prelude (bind, const, flip, id, pure, ($), (*), (*>), (+), (<$>), (<*), (<*>), (<>), (>), (>>=))
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.String (anyChar, char)
import Text.Parsing.Parser.Token (TokenParser, letter, makeTokenParser)

toString :: Seq.Seq Char → String
toString = foldl (\a b → a <> S.singleton b) ""

tokenParser :: TokenParser
tokenParser = makeTokenParser haskellDef

anyString :: Int → ParserT String Identity String
anyString x = toString <$> U.replicateA x anyChar

pnumber :: ParserT String Identity Int
pnumber = tokenParser.integer

pword1 :: ParserT String Identity String
pword1 = S.fromCharArray <$> ((:) <$> letter <*> many letter)

pword :: ParserT String Identity String
pword = pword1 <|> pure ""

pIntPair :: ParserT String Identity (Tuple Int Int)
pIntPair = Tuple <$> pnumber <* char 'x' <*> pnumber

pCode :: ParserT String Identity (Tuple Int Int)
pCode = char '(' *> pIntPair <* char ')'

mkSubP :: Tuple Int Int → ParserT String Identity String
mkSubP (Tuple x y) = foldl (<>) "" <$> replicate y <$> anyString x

pCodeExpr :: ParserT String Identity String
pCodeExpr = pCode >>= mkSubP

expr :: ParserT String Identity String
expr = pword1 <|> ((<>) <$> pCodeExpr <*> pword)

grammar :: ParserT String Identity String
grammar = foldl (<>) "" <$> many expr

-- runParser "A(6x2)a(2x5)cdefg" grammar
-- runParser puzzle $ S.length <$> grammar

-------------------------------------

pStrLen :: ParserT String Identity Int
pStrLen  = S.length <$> pword

pStr1Len :: ParserT String Identity Int
pStr1Len = S.length <$> pword1

mkSubPLen :: Tuple Int Int → ParserT String Identity Number
mkSubPLen (Tuple x y) = subLength <$> anyString x
  where
  subLength s = case runParser s grammarLen of
    Right x' | x' > 0.0 → toNumber y * x'
    _ → toNumber $ y * S.length s

pCodeLen :: Lazy (ParserT String Identity Number) ⇒ ParserT String Identity Number
pCodeLen = pCode >>= mkSubPLen

exprLen :: Lazy (ParserT String Identity Number) ⇒ ParserT String Identity Number
exprLen = toNumber <$> pStr1Len <|> (+) <$> pCodeLen <*> (toNumber <$> pStrLen)

grammarLen :: Lazy (ParserT String Identity Number) ⇒ ParserT String Identity Number
grammarLen = foldl (+) 0.0 <$> many exprLen

-- runParser "A(6x2)a(2x5)cdefg" grammarLen
-- runParser "X(8x2)(3x3)ABCY" grammarLen

puzzleInput :: ∀ eff. Eff ( process :: PROCESS, fs :: FS | eff ) String
puzzleInput = do
  filePath <- flip resolve "./2016/puzzles/input-2016-day09.txt" <$> singleton <$> cwd
  s <- try $ readTextFile UTF8 filePath
  pure $ either (const "") id s

run :: ∀ eff a. ParserT String Identity a → Eff ( process :: PROCESS, fs :: FS | eff ) (Either ParseError a)
run parser = do
  str <- puzzleInput
  pure $ runParser str parser

part1 :: ∀ eff. Eff ( process :: PROCESS, fs :: FS | eff ) (Either ParseError Int)
part1 = run (S.length <$> grammar)

part2 :: ∀ eff. Eff ( process :: PROCESS , fs :: FS | eff ) (Either ParseError Number)
part2 = run grammarLen
