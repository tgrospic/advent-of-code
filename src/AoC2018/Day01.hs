module AoC2018.Day01
  ( part1
  , part2
  ) where

import Control.Applicative (many, (<|>))
import Data.Functor.Identity (Identity)
import Text.Parsec (Parsec, ParseError, char, parse)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)
import qualified Data.Set as S

-- Puzzle input
puzzleInput :: IO String
puzzleInput = readFile "./src/AoC2018/puzzles/day-01.txt"

run :: Parsec String () a -> IO (Either ParseError a)
run p = puzzleInput >>= pure <$> parse p ""

-- Tokens
tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

pnumber :: Parser Int
pnumber = fromInteger <$> natural tokenParser

pplus :: Parser (Int -> Int)
pplus = (+) <$> (char '+' *> pnumber)

pminus :: Parser (Int -> Int)
pminus = flip (-) <$> (char '-' *> pnumber)

poperations :: Parser [(Int -> Int)]
poperations = many $ pplus <|> pminus

-- Execute all operations
executeAll :: Int -> [(Int -> Int)] -> Int
executeAll = foldl $ flip ($)

-- Find duplicate result
duplicates :: (Bool, Int, S.Set Int) -> [(Int -> Int)] -> [(Bool, Int, S.Set Int)]
duplicates = scanl execOp
  where
    execOp (_, acc, s) op =
      let r = op acc in
      (S.member r s, r, S.insert r s)

operationsResult :: Parser Int
operationsResult  = executeAll 0 <$> poperations

firstDuplicate :: Parser Int
firstDuplicate = sndOf3 . head . filter fstOf3 . duplicates (False, 0, S.insert 0 S.empty) . cycle <$> poperations
  where
    fstOf3 (b, _, _) = b
    sndOf3 (_, v, _) = v

part1 :: IO (Either ParseError Int)
part1 = run operationsResult

part2 :: IO (Either ParseError Int)
part2 = run firstDuplicate
