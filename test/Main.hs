import AoC2016.Day09 as Day09_2016
import AoC2018.Day01 as Day01_2018
import AoC2018.Day02 as Day02_2018
import Data.Monoid ((<>))

main :: IO ()
main = do
  -- AoC 2016
  putStrLn "\n------ 2016 ------"

  printDay "DAY 1" Day09_2016.part1 Day09_2016.part2

  -- AoC 2018
  putStrLn "\n------ 2018 ------"

  printDay "DAY 1" Day01_2018.part1 Day01_2018.part2
  printDay "DAY 2" Day02_2018.part1 Day02_2018.part2

  putStrLn "\n------------------"


printDay day p1 p2 = do
  putStrLn $ "\n" <> day
  p1' <- p1
  p2' <- p2
  putStrLn $ " Part 1: " <> show p1'
  putStrLn $ " Part 2: " <> show p2'
