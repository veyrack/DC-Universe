import Test.Hspec

import CarteTest as GT
import ModelTest as MT

main :: IO ()
main = hspec $ do
    -- Premiers Test 
    GT.testShouldBe
    MT.testShouldBe