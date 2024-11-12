module Test.Main where

import Prelude

import AoC2016.Day09 as Day09
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "2016: Day 9" do
    it "Part 1" do liftEffect $ rightRes Day09.part1 >>= shouldEqual 97714
    it "Part 2" do liftEffect $ rightRes Day09.part2 >>= shouldEqual 10762972461.0

rightRes :: forall e a. Show e => Effect (Either e a) -> Effect a
rightRes ea = do
  pe <- ea
  either (\e -> throw $ show e) pure pe
