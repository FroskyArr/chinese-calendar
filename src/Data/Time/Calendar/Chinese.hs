{-# LANGUAGE RecordWildCards #-}
module Data.Time.Calendar.Chinese where

import Data.Text (Text)
import Data.Time
import Data.Time.Calendar as Calendar

import Data.Bits.Coded (runDecode)
import Data.Bits.Coding
import Data.Bytes.Get (runGetL)

import Control.Monad (void)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
  { springFestival :: Int
  , leapMonth :: Maybe Int
  , dayOfMonth :: [Bool]
  , solarTermOffset :: [Int]
  } deriving (Eq, Show)

encode :: C -> BSL.ByteString
encode = undefined

decode :: BSL.ByteString -> C
decode bs = flip runGetL bs $ runDecode $ do
  lm <- getBitsFrom 4 0
  let leapMonth = if lm == 0 then Nothing else Just lm
      monthNum = if lm == 0 then 12 else 13
  dayOfMonth <- sequence $ replicate monthNum getBit
  springFestival <- getBits 7 1 0
  void getBit
  solarTermOffset <- sequence $ replicate 24 (getBitsFrom 2 0)
  pure C{..}




{-

1-4: leapMonth (0-12), 0 => Not a leap year
5-17: MonthLength, 0 => shorter month (30 days), 1 => longer month (31 days)
18-23: Spring Festival date offset to 01/01
24: empty
25-72: solar term offset

9 bytes


-}
