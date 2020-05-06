module RuneLink.Base.GameSpec (spec) where

import Test.Hspec
import Data.Matrix (prettyMatrix, fromList, ncols, nrows, getRow, minorMatrix)

import RuneLink.Base.Game

testPlayer = Player 1
testBoard = createBoard 6 7

locations = zip [1,1,1,1] [1,2,3,4]
transforms = setRuneOnBoard <$> pure (Placed testPlayer) <*> locations
boardWithWin = transform transforms testBoard

transform :: [(a -> a)] -> a -> a
transform [] x = x
transform (f:fs) x  = transform fs $ f x

spec :: Spec
spec = do
    describe "isPlaced" $ do
        context "when placed" $
            it "returns true" $
                isPlaced $ Placed $ Player 1
        context "when unreachable" $
            it "returns false" $
                not $ isPlaced $ Unreachable
        context "when reachable" $
            it "returns false" $
                not $ isPlaced $ Unreachable
    describe "createBoard" $
        context "with length and height parameters" $ do
            it "creates a board with reachable runes on the bottom row" $
                all (==Reachable) $ getRow 1 $ createBoard 3 2
            it "creates a board with all runes unreachable not on the bottom row" $
                all (==Unreachable) $ minorMatrix 1 0 $ createBoard 3 2
            it "creates a board with a width of 3" $
                ncols (createBoard 3 2) `shouldBe` 3 
            it "creates a board with a height of 2" $
                nrows (createBoard 3 2) `shouldBe` 2
    describe "checkVictory" $ do
        context "with empty board" $
            it "returns nothing" $
                checkVictory 4 testPlayer 3 3 testBoard `shouldBe` Nothing
        context "with board that contains a victory" $
            it "returns one row" $
                checkVictory 4 testPlayer 1 3 boardWithWin `shouldBe` Just [locations]
    describe "isDraw" $ do
        context "with empty board" $ do
            it "returns false" $ do
                isDraw testBoard `shouldBe` False
