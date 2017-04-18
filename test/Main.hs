import AoC2016.Day09 as Day09
import Data.Monoid ((<>))

main :: IO ()
main = do
  result1 <- Day09.part1
  result2 <- Day09.part2
  putStrLn "DAY 09 results"
  putStrLn $ " Part 1: " <> show result1
  putStrLn $ " Part 2: " <> show result2
