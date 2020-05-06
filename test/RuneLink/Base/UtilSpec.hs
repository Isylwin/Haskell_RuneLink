module RuneLink.Base.UtilSpec (spec) where

import           Control.Applicative
import qualified Data.Matrix as M (Matrix, fromLists)
import           RuneLink.Base.Util
import           Test.Hspec

-- type Expected = [Int]
-- type Input = M.Matrix Int
-- type TestFunc = Input -> Expected
-- type Test = (TestFunc, Input, Expected)
-- type TestCase = (String, Test)
-- type TestCases = [TestCase]
-- data TestResult = TestResult {name :: String, result :: Bool} deriving (Eq, Show)
-- type TestResults = [TestResult]

-- makeCase :: Input -> String -> TestFunc -> Expected -> TestCase
-- makeCase input name func expected = (,) name $ (,,) func matrix5x5 expected

-- makeCases :: (String -> TestFunc -> Expected -> TestCase) -> [(String, TestFunc, Expected)] -> TestCases
-- makeCases f [] = []
-- makeCases f ((s,t,e):xs) = f s t e : makeCases f xs

-- makeCase5x5 :: String -> TestFunc -> Expected -> TestCase
-- makeCase5x5 = makeCase matrix5x5

-- makeCases5x5 :: TestCases
-- makeCases5x5 = makeCases makeCase5x5 $
--     (,,) "gets diagonal of (1,2)(row,col)" (getMajorDiagonal 1 2) [2, 8, 14, 20]  :
--     (,,) "with diagonal of (2,1)(row,col)" (getMajorDiagonal 2 1) [6, 12, 18, 24] :
--     (,,) "with diagonal of (3,3)(row,col)" (getMajorDiagonal 3 3) [6, 12, 18, 24] :
--     (,,) "with diagonal of (4,1)(row,col)" (getMajorDiagonal 4 1) [6, 12, 18, 24] :
--     []

matrix5x5 :: M.Matrix Int
matrix5x5 = M.fromLists 
    [ [1 ,2 ,3 ,4 ,5 ]
    , [6 ,7 ,8 ,9 ,10]
    , [11,12,13,14,15]
    , [16,17,18,19,20]
    , [21,22,23,24,25] ]

matrix2x5 :: M.Matrix Int
matrix2x5 = M.fromLists 
    [ [1 ,2 ,3 ,4 ,5 ]
    , [6 ,7 ,8 ,9 ,10] ]

matrix5x2 :: M.Matrix Int
matrix5x2 = M.fromLists 
    [ [1 ,2 ]
    , [6 ,7 ]
    , [11,12]
    , [16,17]
    , [21,22] ]

locVector :: Int -> [Row] -> [Col] -> [(Row,Col)]
locVector n rows cols = take n $ zip rows cols

spec :: Spec
spec = do
    describe "getMajorDiagonal" $ do
        context "with 5x5 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMajorDiagonal 1 2 matrix5x5) `shouldBe` locVector 4 [1..] [2..]
            it "gets diagonal of (2,1)(row,col)" $
                (getMajorDiagonal 2 1 matrix5x5) `shouldBe` locVector 4 [2..] [1..]
            it "gets diagonal of (3,3)(row,col)" $
                (getMajorDiagonal 3 3 matrix5x5) `shouldBe` locVector 5 [1..] [1..]
            it "gets diagonal of (4,1)(row,col)" $
                (getMajorDiagonal 4 1 matrix5x5) `shouldBe` locVector 2 [4..] [1..]
            it "diagonals below main diagonal on same line are equal" $
                (getMajorDiagonal 3 2 matrix5x5) `shouldBe` (getMajorDiagonal 5 4 matrix5x5)
            it "diagonals above main diagonal on same line are equal" $
                (getMajorDiagonal 2 3 matrix5x5) `shouldBe` (getMajorDiagonal 4 5 matrix5x5)
        context "with 2x5 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMajorDiagonal 1 2 matrix2x5) `shouldBe` locVector 2 [1..] [2..]
            it "gets diagonal of (2,1)(row,col)" $
                (getMajorDiagonal 2 1 matrix2x5) `shouldBe` locVector 1 [2..] [1..]
        context "with 5x2 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMajorDiagonal 1 2 matrix5x2) `shouldBe` locVector 1 [1..] [2..]
            it "gets diagonal of (2,1)(row,col)" $
                (getMajorDiagonal 2 1 matrix5x2) `shouldBe` locVector 2 [2..] [1..]
    describe "getMinorDiagonal" $ do
        context "with 5x5 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMinorDiagonal 1 2 matrix5x5) `shouldBe` locVector 2 [1..] [2,1..1]
            it "gets diagonal of (2,1)(row,col)" $
                (getMinorDiagonal 2 1 matrix5x5) `shouldBe` locVector 2 [1..] [2,1..1]
            it "gets diagonal of (3,3)(row,col)" $
                (getMinorDiagonal 3 3 matrix5x5) `shouldBe` locVector 5 [1..] [5,4..1]
            it "gets diagonal of (4,1)(row,col)" $
                (getMinorDiagonal 4 1 matrix5x5) `shouldBe` locVector 4 [1..] [4,3..1]
            it "diagonals on same line are equal" $
                (getMinorDiagonal 3 2 matrix5x5) `shouldBe` (getMinorDiagonal 1 4 matrix5x5)
        context "with 2x5 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMinorDiagonal 1 2 matrix2x5) `shouldBe` locVector 2 [1..] [2,1..1]
            it "gets diagonal of (2,1)(row,col)" $
                (getMinorDiagonal 2 1 matrix2x5) `shouldBe` locVector 2 [1..] [2,1..1]
        context "with 5x2 matrix" $ do
            it "gets diagonal of (1,2)(row,col)" $
                (getMinorDiagonal 1 2 matrix5x2) `shouldBe` locVector 2 [1..] [2,1..1]
            it "gets diagonal of (2,1)(row,col)" $
                (getMinorDiagonal 2 1 matrix5x2) `shouldBe` locVector 2 [1..] [2,1..1]
    describe "getRow" $ do
        context "with 5x5 matrix" $ do
            it "gets the 3rd row for (3,1)(row,col)" $
                getRow 3 1 matrix5x5 `shouldBe` locVector 5 (repeat 3) [1..]
            it "gets the 3rd row for (3,2)(row,col)" $
                getRow 3 2 matrix5x5 `shouldBe` getRow 3 1 matrix5x5
    describe "getColumn" $ do
        context "with 5x5 matrix" $ do
            it "gets the 3rd col for (1,3)(row,col)" $
                getColumn 1 3 matrix5x5 `shouldBe` locVector 5 [1..] (repeat 3)
            it "gets the 3rd col for (2,3)(row,col)" $
                getColumn 1 3 matrix5x5 `shouldBe` getColumn 2 3 matrix5x5
    describe "getAxes" $ do
        context "with 5x5 matrix" $ do
            it "gets all axes for (3,3)(row,col)" $
                getAxes 3 3 matrix5x5 `shouldBe` [getRow 3 3 matrix5x5, getColumn 3 3 matrix5x5, getMajorDiagonal 3 3 matrix5x5, getMinorDiagonal 3 3 matrix5x5]
    describe "sublists" $ do
        context "with empty list" $
            it "returns an empty list" $
                sublists 4 [] `shouldBe` ([] :: [[Int]])
        context "with a list too small" $
            it "returns an empty list" $
                sublists 4 [1,2,3] `shouldBe` []
        context "with a list of the same size" $
            it "returns the given list" $
                sublists 4 [1,2,3,4] `shouldBe` [[1,2,3,4]]
        context "with a list one larger" $
            it "returns two lists" $
                sublists 4 [1,2,3,4,5] `shouldBe` [[1,2,3,4],[2,3,4,5]]