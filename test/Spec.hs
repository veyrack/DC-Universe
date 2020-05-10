import Test.Hspec

import Gametest as GT

main :: IO ()
main = hspec $ do
    -- Premiers Test 
    GT.testShouldBe
