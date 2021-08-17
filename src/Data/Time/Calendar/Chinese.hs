{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Time.Calendar.Chinese where

import Data.Text (Text)
import Data.Time
import Data.Time.Calendar as Calendar

import Data.Bits
import Data.Bits.Coded (runDecode, runEncode)
import Data.Bits.Coding
import Data.Bits.Extras (assignBit)
import Data.Bytes.Get (MonadGet, runGetL)
import Data.Bytes.Put (runPutL)
import Data.Word

import Control.Monad (replicateM, void, when)
import Data.Maybe

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
encode C{..} = runPutL . runEncode $ do
  putBitsFrom 3 $ fromMaybe 0 leapMonth
  mapM_ putBit dayOfMonth
  when (isNothing leapMonth) $ putBit False
  putBitsFrom 5 $ springFestival
  putBit False
  mapM_ (putBitsFrom 1) solarTermOffset

decode :: BSL.ByteString -> C
decode bs = flip runGetL bs $ runDecode $ do
  lm <- getBitsFrom 3 0
  bools13 <- sequence $ replicate 13 getBit
  let leapMonth =   if lm == 0 then Nothing else Just lm
      monthNum =    if lm == 0 then 12 else 13
      dayOfMonth = (if lm == 0 then init else id) bools13
  springFestival <- getBitsFrom 5 0
  void getBit
  solarTermOffset <- sequence $ replicate 24 (getBitsFrom 1 0)
  pure C{..}



{-

0-3: leapMonth (0-12), 0 => Not a leap year
4-16: MonthLength, 0 => shorter month (30 days), 1 => longer month (31 days)
17-22: Spring Festival date offset to 01/01
23: empty
24-71: solar term offset (starting from 小寒), 2 bits each

9 bytes


-}
