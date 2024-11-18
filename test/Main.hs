import qualified AoC2016.Day09
import qualified AoC2018.Day01
import qualified AoC2018.Day02
import qualified AoC2022.Day01
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Test.Hspec
import           Text.Printf            (printf)

main :: IO ()
main = hspec $ do
  describe (title "2016: Day 9") $ do
    it "Part 1" $ liftIO AoC2016.Day09.part1 `shouldReturn` 97714
    it "Part 2" $ liftIO AoC2016.Day09.part2 `shouldReturn` 10762972461

  describe (title "2018: Day 1") $ do
    it "Part 1" $ liftIO AoC2018.Day01.part1 `shouldReturn` 538
    it "Part 2" $ liftIO AoC2018.Day01.part2 `shouldReturn` 77271

  describe (title "2018: Day 2") $ do
    it "Part 1" $ liftIO AoC2018.Day02.part1 `shouldReturn` 7872
    it "Part 2" $ liftIO AoC2018.Day02.part2 `shouldReturn` "tjxmoewpdkyaihvrndfluwbzc"

  describe (title "2022: Day 1") $ do
    it "Part 1" $ liftIO AoC2022.Day01.part1 `shouldReturn` 70116
    it "Part 2" $ liftIO AoC2022.Day01.part2 `shouldReturn` 206582

title = printf "\x1b[1;35m%s\x1b[0m"
