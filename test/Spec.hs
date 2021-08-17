import Test.Hspec
import Test.QuickCheck

import Data.Time.Calendar.Chinese

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = print $ decode year2020

year2020 :: BSL.ByteString
year2020 = BSL.pack [0xE4, 0x52, 0x55, 0xB0, 0x69, 0, 0, 0, 0x4, 0]
