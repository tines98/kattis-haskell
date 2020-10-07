import Data.Time
import Data.Time.Format

readinput = (map read) . words

main :: IO ()
main = interact (print . weekday . solve . readinput)

solve :: [Int] -> Day
solve (d:m:rest) = fromGregorian 2009 m d

weekday = formatTime defaultTimeLocale "%A"