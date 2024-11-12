import Test.Hspec

import AoC2016.Day09 as Day09_2016
import AoC2018.Day01 as Day01_2018
import AoC2018.Day02 as Day02_2018

import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = hspec $ do
  describe "2016: Day 9" $ do
    it "Part 1" $ liftIO (Day09_2016.part1) `shouldReturn` Right 97714
    it "Part 2" $ liftIO (Day09_2016.part2) `shouldReturn` Right 10762972461

  describe "2018: Day 1" $ do
    it "Part 1" $ liftIO (Day01_2018.part1) `shouldReturn` Right 538
    it "Part 2" $ liftIO (Day01_2018.part2) `shouldReturn` Right 77271

  describe "2018: Day 2" $ do
    it "Part 1" $ liftIO (Day02_2018.part1) `shouldReturn` Right 7872
    it "Part 2" $ liftIO (Day02_2018.part2) `shouldReturn` Right (Just "tjxmoewpdkyaihvrndfluwbzc")
