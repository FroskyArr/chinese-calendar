module Data.Time.Calendar.Chinese where

import Data.Time
import Data.Time.Calendar as Calendar
import Data.Text (Text)

import qualified Data.Text as T

newtype ChnDay = ChnDay Day

toGregorian :: ChnDay -> (Interger, Int, Int)
toGregorian = Calendar.toGregorian . toDay

fromGregorian :: Integer -> Int -> Int -> ChnDay
fromGregorian y m d = fromDay $ Calendar.fromGregorian y m d

fromDay :: Day -> ChnDay
fromDay = undefined

toDay :: ChnDay -> Day
toDay = undefined
