import Test.Hspec
import Test.QuickCheck

import Data.Time.Calendar.Chinese

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = hspec $ do
  describe "decode" $
    it "decode bytes into calendar info" $
      decode year2020 `shouldBe` year2020'

  describe "encode" $ do
    it "encode calendar info into bytes" $
      encode year2020' `shouldBe` year2020
    it "decode . encode ≡ id" $
      (decode . encode) year2020' `shouldBe` year2020'
    it "encode . decode ≡ id" $
      (encode . decode) year2020 `shouldBe` year2020

year2020 :: BSL.ByteString
year2020 = BSL.pack [0x47, 0x4A, 0xB0, 0x45, 0, 0, 0, 0, 0x4]

year2020' :: C
year2020' = C
  { springFestival = 24
  , leapMonth = Just 4
  , dayOfMonth = [False,True,True,True,False,True,False,False,True,False,True,False,True]
  , solarTermOffset = [1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
  }
