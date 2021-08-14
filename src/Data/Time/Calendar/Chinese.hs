module Data.Time.Calendar.Chinese where

import Data.Time
import Data.Time.Calendar as Calendar
import Data.Text (Text)

import qualified Data.Text as T

newtype ChnDay = ChnDay Day

toGregorian :: ChnDay -> (Integer, Int, Int)
toGregorian = Calendar.toGregorian . toDay

fromGregorian :: Integer -> Int -> Int -> ChnDay
fromGregorian y m d = fromDay $ Calendar.fromGregorian y m d

fromDay :: Day -> ChnDay
fromDay = undefined

toDay :: ChnDay -> Day
toDay = undefined

data C = C
  { yearGregorian :: Int
  , springFestival :: Day
  , isLeapYear :: Bool
  , leapMonth :: Maybe Int
  , dayOfMonth :: [Bool]
  , solarTermOffset :: [Int]
  }




{-

1-4: leapMonth (0-12), 0 => Not a leap year
5-17: MonthLength, 0 => shorter month (30 days), 1 => longer month (31 days)
18-23: Spring Festival date offset to 01/01
24: empty
25-72: solar term offset

9 bytes


-}
