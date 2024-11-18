module Test.Main where

import Prelude

import AoC2016.Day09 as AoC2016Day09
import AoC2022.Day01 as AoC2022Day01
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "2016: Day 9" do
    it "Part 1" do liftEffect $ AoC2016Day09.part1 >>= shouldEqual 97714
    it "Part 2" do liftEffect $ AoC2016Day09.part2 >>= shouldEqual 10762972461.0

  describe "2022: Day 1" do
    it "Part 1" do liftEffect $ AoC2022Day01.part1 >>= shouldEqual 70116
    it "Part 2" do liftEffect $ AoC2022Day01.part2 >>= shouldEqual 206582
