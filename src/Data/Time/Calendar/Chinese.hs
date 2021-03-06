{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Time.Calendar.Chinese (
  ChnDay,
  fromGregorian, fromGregorianValid, toGregorian,
  fromDay, fromDayValid, toDay,
  today,
  format, getSolarTerm
  ) where

import Data.Time
       ( Day(..)
       , LocalTime(localDay)
       , ZonedTime(zonedTimeToLocalTime)
       , addDays
       , diffDays
       , getCurrentTime
       , hoursToTimeZone
       , utcToZonedTime
       )
import qualified Data.Time.Calendar as Calendar

import Data.Bits.Coded (runDecode, runEncode)
import Data.Bits.Coding (getBit, getBitsFrom, putBit, putBitsFrom)
import Data.Bytes.Get (MonadGet, runGetL)
import Data.Bytes.Put (runPutL)

import Control.Monad (replicateM, void, when)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Vector (Vector, (!))
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (lift)
import Prelude hiding (lookup)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector

newtype ChnDay = ChnDay Day
  deriving (Eq, Ord, Show)

-- | It will clip an unsupported date to 1901-01-01 or 2100-12-31
fromGregorian :: Integer -> Int -> Int -> ChnDay
fromGregorian y m d = fromDay $ Calendar.fromGregorian y m d
{-# INLINE fromGregorian #-}

-- | It returns `Nothing` if given an unsupported date.
fromGregorianValid :: Integer -> Int -> Int -> Maybe ChnDay
fromGregorianValid y m d =
  if inRange yearRange (fromInteger y)
    then ChnDay <$> Calendar.fromGregorianValid y m d
    else Nothing
{-# INLINE fromGregorianValid #-}

toGregorian :: ChnDay -> (Integer, Int, Int)
toGregorian (ChnDay day) = Calendar.toGregorian day
{-# INLINE toGregorian #-}

fromDay :: Day -> ChnDay
fromDay =
  ChnDay . ModifiedJulianDay . clip modifiedJulianDayRange . toModifiedJulianDay
{-# INLINE fromDay #-}

fromDayValid :: Day -> Maybe ChnDay
fromDayValid day = do
  if inRange modifiedJulianDayRange (toModifiedJulianDay day)
    then Just (ChnDay day)
    else Nothing
{-# INLINE fromDayValid #-}

toDay :: ChnDay -> Day
toDay (ChnDay day) = day
{-# INLINE toDay #-}

-- | It uses UTC+8 timezone regardless of locale.
today :: IO ChnDay
today =
  fromDay . localDay . zonedTimeToLocalTime . utcToZonedTime (hoursToTimeZone 8)
  <$> getCurrentTime
{-# INLINE today #-}



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
  putBitsFrom 5 springFestival
  putBit False
  mapM_ (putBitsFrom 1) solarTermOffset

decode :: BSL.ByteString -> C
decode bs = flip runGetL bs $ runDecode $ do
  lm <- getBitsFrom 3 0
  bools13 <- replicateM 13 getBit
  let leapMonth  =  if lm == 0 then Nothing else Just lm
      monthNum   =  if lm == 0 then 12 else 13
      dayOfMonth = (if lm == 0 then init else id) bools13
  springFestival <- getBitsFrom 5 0
  void getBit
  solarTermOffset <- replicateM 24 (getBitsFrom 1 0)
  pure C{..}


-- |
-- >>> format (fromGregorian 2033 12 31)
-- "???????????????????????????"
format :: ChnDay -> String
format (ChnDay day0) =
  showYear year
  <> showMonth isLeap month
  <> showDay day
  where
  (y,_,_) = Calendar.toGregorian day0
  offset = fromInteger $ day0 `diffDays` Calendar.fromGregorian y 1 1
  yinfo = lookup y
  (year, yearInfo) =
    if springFestival yinfo > offset
      then (y-1, lookup (y-1))
      else (y, yinfo)
  ((isLeap, month), day) = go offsetToSF 1 (dayOfMonth yearInfo)

  sf = toInteger (springFestival yearInfo)
        `addDays` Calendar.fromGregorian year 1 1
  offsetToSF = fromInteger $ day0 `diffDays` sf

  go :: Int -> Int -> [Bool] -> ((Bool, Int), Int)
  go _ _ [] = error "internal error"
  go day month (dom:doms) =
    if x >= 0
      then go x (month+1) doms
      else (calcLeap month, day+1) -- day+1 because it's not offset anymore
    where x = day - 29 - fromEnum dom

  -- return (isLeap, num-part of a month)
  calcLeap :: Int -> (Bool, Int)
  calcLeap m = case leapMonth yearInfo of
    Just x | x == m -> (True, m-1)
    Just x | x < m -> (False, m-1)
    _ -> (False, m)

-- |
-- >>> getSolarTerm (fromGregorian 2021 8 23)
-- Just "??????"
getSolarTerm :: ChnDay -> Maybe String
getSolarTerm (ChnDay day0) =
  asum $ [m*2-1, m*2-2] <&> \idx -> do
    let d = solarTermOffsets ! idx + offsetsInYear !! idx
    if Calendar.fromGregorian y m d == day0
       then Just (solarTerm ! idx)
       else Nothing
  where
  offsetsInYear = solarTermOffset $ lookup y
  (y,m,d) = Calendar.toGregorian day0

solarTerm :: Vector String
solarTerm = $(lift (Vector.fromList
  ["??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ,"??????","??????"
  ]))

showYear :: Integer -> String
showYear y =
  [stems !! stem, branches !! branch, '???']
  where
  stems = "??????????????????????????????"
  branches = "????????????????????????????????????"
  o = fromInteger y - 4
  stem = o `mod` 10
  branch = o `mod` 12

showMonth :: Bool -> Int -> String
showMonth True month = '???' : chnNum ! month <> "???"
showMonth False month = chnNum ! month <> "???"

-- Well, it works.
showDay :: Int -> String
showDay day | 1 <= day && day <= 10  = '???' : chnNum ! day
showDay day | 11 <= day && day <= 19 = '???' : chnNum ! (day-10)
showDay 20 = "??????"
showDay day | 21 <= day && day <= 29 = '???' : chnNum ! (day-20)
showDay 30 = "??????"
showDay _ = ""

-- XD
chnNum :: Vector String
chnNum = $(lift (Vector.fromList
  ["???","???","???","???","???","???","???","???","???","???","???","??????","??????"]))
{-# NOINLINE chnNum #-}

yearRange :: (Int, Int)
yearRange = (1901, 2100)

modifiedJulianDayRange :: (Integer, Integer)
modifiedJulianDayRange = $(lift
  (toModifiedJulianDay (Calendar.fromGregorian (toInteger 1901) 1 1)
  ,toModifiedJulianDay (Calendar.fromGregorian (toInteger 2100) 12 31)
  ))

clip :: Ord a => (a, a) -> a -> a
clip (l,_) n | n < l = l
clip (_,r) n | n > r = r
clip _     n         = n
{-# INLINE clip #-}

inRange :: Ord a => (a, a) -> a -> Bool
inRange (l,r) n | l <= n && n <= r = True
inRange _ _ = False
{-# INLINE inRange #-}

-- won't check if year is in range (for a valid ChnDay it should be fine)
lookup :: Integer -> C
lookup year = index `seq` decode (BSL.fromStrict $ data_ ! index)
  where
  index = fromInteger year - fst yearRange

solarTermOffsets :: Vector Int
solarTermOffsets = $(lift (Vector.fromList
  [4,19, 3,18,4,19,4,19, 4,20,4,20,6,22, 6,22,6,22,7,22, 6,21,6,21] :: Vector Int
  ))

-- | 0-3: leapMonth (0-12), 0 => Not a leap year
-- 4-16: MonthLength, 0 => shorter month (30 days), 1 => longer month (31 days)
-- 17-22: Spring Festival date offset to 01/01
-- 23: empty
-- 24-71: solar term offset (starting from ??????), 2 bits each
--
-- 9 bytes in total
-- NB. the `bits` library storing bits starts with MSB of a byte
--
-- `data_` is encoded with chinese calendar data in 1901~2100
data_ :: Vector BS.ByteString
data_ = $(lift (Vector.fromList (fmap BS.pack
  [[4,174,98,165,166,170,154,170,169],     [10,87,76,169,170,174,170,170,170]
  ,[101,38,184,170,250,238,174,234,170],   [13,38,92,234,165,154,89,154,165]
  ,[13,149,68,165,166,170,154,170,169],    [86,170,176,169,170,170,170,170,170]
  ,[5,106,86,170,250,238,174,234,170],     [9,173,64,234,165,154,89,154,165]
  ,[52,174,170,165,166,170,154,170,169],   [4,174,80,169,170,170,170,170,170]
  ,[122,77,186,170,250,238,174,234,170],   [10,77,96,234,165,154,89,154,165]
  ,[13,37,72,149,166,170,154,170,169],     [109,82,178,165,166,170,170,170,170]
  ,[11,84,88,170,186,174,170,234,170],     [13,106,66,170,165,154,89,150,149]
  ,[57,109,44,149,166,154,154,154,165],    [9,91,82,165,166,170,170,170,169]
  ,[132,155,190,170,186,174,170,234,170],  [4,151,100,170,165,154,89,150,149]
  ,[10,75,76,149,165,154,154,154,165],     [107,37,182,165,166,170,170,170,169]
  ,[6,165,92,169,170,174,170,234,170],     [6,212,70,170,165,154,89,150,149]
  ,[90,218,174,149,165,154,154,154,165],   [2,182,86,165,166,170,154,170,169]
  ,[9,87,64,169,170,174,170,170,170],      [52,151,172,170,165,153,89,149,85]
  ,[4,151,80,149,165,154,89,154,165],      [118,75,58,165,166,170,154,170,169]
  ,[13,74,94,169,170,174,170,170,170],     [14,165,72,170,165,153,89,149,85]
  ,[102,212,178,149,165,154,89,154,165],   [5,173,88,165,166,170,154,170,169]
  ,[2,182,68,169,170,170,170,170,170],     [73,55,46,170,165,153,89,149,85]
  ,[9,46,82,149,165,154,89,154,165],       [140,150,188,165,166,170,154,170,169]
  ,[12,149,98,169,170,170,170,170,170],    [13,74,76,170,165,153,89,149,85]
  ,[125,165,52,149,165,154,89,154,165],    [11,85,90,165,166,170,154,170,169]
  ,[5,106,70,169,170,170,170,170,170],     [90,173,176,170,165,89,89,149,85]
  ,[2,93,86,149,165,154,89,150,149],       [9,45,64,149,166,170,154,154,169]
  ,[60,149,170,165,166,170,170,170,170],   [10,149,80,170,101,89,85,149,85]
  ,[139,74,184,85,165,154,89,150,149],     [6,202,94,149,165,154,154,154,169]
  ,[11,85,72,165,166,170,170,170,170],     [101,90,180,170,101,89,85,149,85]
  ,[4,218,88,85,165,154,89,150,149],       [10,91,66,149,165,154,154,154,165]
  ,[69,43,174,165,166,170,154,170,169],    [5,43,84,170,85,89,85,149,85]
  ,[154,149,60,85,165,154,89,150,149],     [14,149,96,149,165,154,89,154,165]
  ,[6,170,76,165,166,170,154,170,169],     [122,213,54,169,85,89,85,85,85]
  ,[10,181,90,85,165,153,89,149,85],       [4,182,70,149,165,154,89,154,165]
  ,[90,87,48,165,166,170,154,170,169],     [10,87,86,169,85,89,85,85,85]
  ,[5,38,64,85,165,153,89,149,85],         [78,147,40,149,165,154,89,154,165]
  ,[13,149,78,165,166,170,154,170,169],    [133,170,186,169,85,85,85,85,85]
  ,[5,106,94,85,165,153,89,149,85],        [9,109,72,149,165,154,89,154,165]
  ,[100,174,180,165,166,170,154,170,169],  [4,173,90,169,85,85,85,85,85]
  ,[10,77,66,85,165,89,89,149,85],         [93,38,172,149,165,154,89,154,165]
  ,[13,37,82,165,166,170,154,154,169],     [157,82,188,169,81,85,85,85,85]
  ,[11,84,96,85,165,89,85,149,85],         [11,106,74,149,165,154,89,150,165]
  ,[121,109,54,165,166,154,154,154,169],   [9,91,92,169,81,85,85,85,85]
  ,[4,155,70,85,165,89,85,149,85],         [90,75,176,149,165,154,89,150,149]
  ,[10,75,86,149,165,154,154,154,169],     [187,37,192,165,81,85,69,85,85]
  ,[6,165,100,85,101,89,85,149,85],        [6,212,78,85,165,154,89,150,149]
  ,[122,218,56,149,165,154,90,154,165],    [10,182,94,165,81,85,69,85,84]
  ,[9,87,72,85,85,89,85,85,85],            [100,151,180,85,165,153,89,150,149]
  ,[4,151,90,149,165,154,89,154,165],      [6,75,68,165,81,85,69,85,84]
  ,[70,165,44,84,85,89,85,85,85],          [14,165,80,85,165,153,89,149,85]
  ,[150,178,188,149,165,154,89,154,165],   [5,172,98,165,81,85,69,85,84]
  ,[10,182,74,84,85,85,85,85,85],          [105,54,182,85,165,153,89,149,85]
  ,[9,46,92,149,165,154,89,154,165],       [12,150,70,165,81,85,69,85,84]
  ,[93,74,174,84,85,85,85,85,85],          [13,74,84,85,165,153,89,149,85]
  ,[13,165,62,149,165,154,89,154,165],     [53,170,170,165,81,85,69,85,84]
  ,[5,106,78,84,85,85,85,85,85],           [138,173,184,85,165,89,85,149,85]
  ,[2,93,96,149,165,154,89,154,165],       [9,45,74,165,81,85,69,69,84]
  ,[108,149,178,84,81,85,85,85,85],        [10,149,88,85,165,89,85,149,85]
  ,[11,74,66,149,165,154,89,150,165],      [91,85,44,165,81,69,69,69,84]
  ,[10,213,80,84,81,85,69,85,85],          [165,90,188,85,165,89,85,149,85]
  ,[4,186,98,149,165,154,89,150,149],      [10,91,76,149,80,69,69,69,84]
  ,[117,43,182,80,81,85,69,85,85],         [5,43,92,85,101,89,85,149,85]
  ,[10,147,70,85,165,153,89,150,149],      [87,74,176,149,80,69,4,69,84]
  ,[6,170,84,80,81,85,69,85,84],           [10,213,62,85,85,89,85,85,85]
  ,[52,218,170,85,165,153,89,150,149],     [4,182,80,149,80,69,4,69,80]
  ,[122,87,56,80,81,85,69,85,84],          [10,78,94,84,85,85,85,85,85]
  ,[13,38,72,85,165,153,89,149,85],        [110,147,50,149,80,69,4,69,80]
  ,[13,83,86,80,81,85,69,85,84],           [5,170,66,84,85,85,85,85,85]
  ,[70,181,44,85,165,153,89,149,85],       [9,109,82,149,80,69,4,69,80]
  ,[196,174,188,80,81,85,69,85,84],        [4,173,98,84,85,85,85,85,85]
  ,[10,77,76,85,165,89,85,149,85],         [125,37,182,149,80,69,4,69,80]
  ,[13,37,90,80,81,85,69,85,84],           [13,82,68,84,85,85,85,85,85]
  ,[109,170,46,85,165,89,85,149,85],       [11,90,84,149,80,69,4,69,80]
  ,[5,109,62,80,81,69,69,69,84],           [52,173,170,84,81,85,85,85,85]
  ,[4,155,80,85,165,89,85,149,85],         [138,75,186,149,80,69,4,65,80]
  ,[10,75,94,80,80,69,69,69,84],           [10,165,72,84,81,85,69,85,85]
  ,[107,82,178,85,165,89,85,149,85],       [6,210,88,149,80,68,4,65,64]
  ,[10,218,64,64,80,69,4,69,84],           [69,91,44,80,81,85,69,85,85]
  ,[9,55,82,85,85,89,85,85,85],            [148,151,190,85,80,68,4,65,64]
  ,[4,151,98,64,80,69,4,69,84],            [6,75,76,80,81,85,69,85,85]
  ,[118,165,54,85,85,85,85,85,85],         [14,165,90,85,80,68,4,65,64]
  ,[6,170,68,64,80,69,4,69,80],            [90,182,46,80,81,85,69,85,84]
  ,[10,174,84,85,85,85,85,85,85],          [9,46,64,85,80,68,4,64,0]
  ,[76,151,40,64,80,69,4,69,80],           [12,150,78,80,81,85,69,85,84]
  ,[141,74,184,84,85,85,85,85,85],         [13,74,94,85,80,68,4,64,0]
  ,[13,165,70,64,80,69,4,69,80],           [101,170,178,80,81,85,69,85,84]
  ,[5,106,88,84,85,85,85,85,85],           [10,109,66,85,80,4,0,64,0]
  ,[85,46,172,64,80,69,4,69,80],           [5,45,82,80,81,69,69,69,84]
  ,[154,149,188,84,85,85,85,85,85],        [10,149,98,85,80,4,0,64,0]
  ,[11,74,74,64,80,69,4,65,80],            [123,85,52,80,81,69,69,69,84]
  ,[10,213,90,84,81,85,69,85,85],          [5,90,70,85,80,4,0,64,0]
  ,[90,93,46,64,80,69,4,65,80],            [10,91,84,80,80,69,5,69,84]
  ,[5,43,64,84,81,85,69,85,85],            [74,147,170,85,80,4,0,64,0]
  ,[6,147,78,64,80,68,4,65,64],            [135,41,184,80,80,69,4,69,84]
  ,[6,170,94,80,81,85,69,85,85],           [10,213,72,85,0,4,0,0,0]
  ,[100,218,178,0,80,68,4,65,64],          [4,182,88,64,80,69,4,69,84]
  ,[10,87,66,80,81,85,69,85,85],           [85,39,46,85,0,0,0,0,0]
  ,[13,22,80,0,80,68,4,65,64],             [158,147,58,64,80,69,4,69,80]
  ,[13,82,96,80,81,85,69,85,84],           [13,170,74,85,0,0,0,0,0]
  ,[118,181,52,0,80,68,4,64,0],            [5,109,90,64,80,69,4,69,80]
  ,[4,174,70,80,81,85,69,85,84],           [90,78,176,84,0,0,0,0,0]
  ,[10,45,84,0,80,68,0,64,0],              [13,21,62,64,80,69,4,69,80]
  ,[61,146,168,80,81,85,69,85,84],         [13,82,156,168,170,170,170,170]
  ])))
{-# NOINLINE data_ #-}
