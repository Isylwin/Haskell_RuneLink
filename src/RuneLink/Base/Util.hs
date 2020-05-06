module RuneLink.Base.Util
    ( Axis(..)
    , Row
    , Col
    , Location
    , getMajorDiagonal
    , getMinorDiagonal
    , getRow
    , getColumn
    , getAxis
    , getAxes
    , sublists
    ) where

import           Control.Applicative
import qualified Data.Matrix as M (Matrix, ncols, nrows, (!), submatrix, toList)

type Col = Int
type Row = Int
type Location = (Row,Col)
data Axis = Row | Column | MajorDiagonal | MinorDiagonal deriving(Eq, Show, Enum, Bounded)

getAxis :: Axis -> Row -> Col -> M.Matrix a -> [Location]
getAxis Row             = getRow
getAxis Column          = getColumn
getAxis MajorDiagonal   = getMajorDiagonal
getAxis MinorDiagonal   = getMinorDiagonal

getAxes :: Row -> Col -> M.Matrix a -> [[Location]]
getAxes row col matrix = getAxis <$> [minBound..] <*> pure row <*> pure col <*> pure matrix

-- | Unsafe
getMajorDiagonal :: Row -> Col -> M.Matrix a -> [Location]
getMajorDiagonal row col matrix = 
    let distEnd = min (M.nrows matrix - row) (M.ncols matrix - col)
        distStart = min (row - 1) (col - 1)
        startCol = col - distStart
        startRow = row - distStart 
        endCol = col + distEnd
        endRow = row + distEnd
        diagonalList = zip [startRow..endRow] [startCol..endCol]
    in diagonalList

-- | Unsafe
getMinorDiagonal :: Row -> Col -> M.Matrix a -> [Location]
getMinorDiagonal row col matrix =
    let distStart = min (row - 1) (M.ncols matrix - col) 
        distEnd = min (M.nrows matrix - row) (col - 1)
        startRow = row - distStart 
        startCol = col + distStart 
        endRow = row + distEnd     
        endCol = col - distEnd    
        diagonalList = zip [startRow..endRow] (reverse [endCol..startCol])
    in diagonalList

-- | Unsafe
getRow :: Row -> Col -> M.Matrix a -> [Location]
getRow row _ matrix = liftA2 (,) (pure row) [1..(M.ncols matrix)] --M.toList $ M.submatrix row row 1 (M.ncols matrix) matrix

-- | Unsafe
getColumn :: Row -> Col -> M.Matrix a -> [Location]
getColumn _ col matrix = liftA2 (,) [1..(M.nrows matrix)] $ pure col  --M.toList $ M.submatrix 1 (M.nrows matrix) col col matrix

sublists :: Int -> [a] -> [[a]]
sublists n xs 
    | length xs < n = []
    | otherwise = (take n xs) : (sublists n $ tail xs)
