module AoC.Util where

import Prelude

import Data.Array (singleton)
import Data.Either (Either, either)
import Data.Int (fromString)
import Data.List (List, head)
import Data.List.NonEmpty (toUnfoldable)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (resolve)
import Node.Process (cwd)
import Parsing (ParseError, Parser, liftMaybe, runParser)
import Parsing.Combinators (many1)
import Parsing.String (char)
import Parsing.String.Basic (digit)

type ParserS = Parser String

-- Parsers

pint :: ParserS Int
pint = many1 digit >>= parseInt >>> liftMaybe (\_ -> "int")
  where
    parseInt = fromString <<< fromCharArray <<< toUnfoldable

pnewline :: ParserS Char
pnewline = char '\n'

phead :: ∀ a. List a -> ParserS a
phead = head >>> liftMaybe (\_->"empty list")

-- Effect helpers

parseEff :: ∀ a. ParserS a -> Effect String -> Effect a
parseEff p s = flip runParser p <$> s >>= liftEither

liftEither :: ∀ a. Either ParseError a -> Effect a
liftEither = either (throw <<< show) pure

readFile :: String -> Effect String
readFile path = readTextFile UTF8 =<< filePath
  where
  filePath = flip resolve path <$> singleton =<< cwd
