import Test.Hspec
import Test.QuickCheck

import Data.Time.Calendar.Chinese

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = print $ decode year2020

year2020 :: BSL.ByteString
year2020 = BSL.pack [0x47, 0x4A, 0xB0, 0x45, 0, 0, 0, 0, 0x4]
