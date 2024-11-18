module Util where

import Data.Maybe (listToMaybe)
import Text.Parsec.String (Parser)
import Text.Parsec (Parsec, ParseError, parse)

-- Parsers

phead :: [a] -> Parser a
phead = liftMaybe "empty list" . listToMaybe

liftMaybe :: String -> Maybe a -> Parser a
liftMaybe message f = case f of
  Nothing -> fail message
  Just x  -> pure x

-- IO

parseIO :: Parsec String () a -> IO String -> IO a
parseIO p s = parse p "" <$> s >>= liftEither

liftEither :: Either ParseError a -> IO a
liftEither = either (fail . show) pure
