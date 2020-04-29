module RuneLink.Base.GameSpec (spec) where

import Test.Hspec
import RuneLink.Base.Game
import RuneLink.Base.Util
import Data.Matrix

spec :: Spec
spec =
    describe "createBoard" $
        context "with length and height parameters" $ do
            it "creates a board of empty Runes" $
                all (==Empty) $ createBoard 3 2
            it "creates a board with a width of 3" $
                ncols (createBoard 3 2) `shouldBe` 3 
            it "creates a board with a height of 2" $
                nrows (createBoard 3 2) `shouldBe` 2                

