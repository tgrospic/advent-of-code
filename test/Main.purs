module Test.Main where

import Prelude
import AoC2016.Day09 as Day09
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.Process (PROCESS)

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
  result1 <- Day09.part1
  result2 <- Day09.part2
  log "DAY 09 results"
  log $ " Part 1: " <> show result1
  log $ " Part 2: " <> show result2
