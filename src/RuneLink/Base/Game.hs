module RuneLink.Base.Game
    ( GameState(..)
    , Rune(..)
    , Player(..)
    , RuneLink
    , Board
    , isPlaced
    , createBoard
    , setRuneOnBoard
    , checkVictory
    , isDraw
    ) where

import           Control.Applicative (liftA2)
import qualified Data.Matrix as M (Matrix, (!), fromList, toList, getElem, setElem, ncols, nrows)
import           Data.Maybe (listToMaybe)
import           RuneLink.Base.Util (Row, Col, Location, getAxes, sublists)

newtype Player = Player {getId :: Int} deriving(Eq, Show)
data GameState = Playing | Victory Player | Draw deriving(Eq, Show)
data Rune = 
      Unreachable   
    | Reachable     
    | Placed        Player
    | VictoryRune   Player
    deriving(Eq, Show)

type Board = M.Matrix Rune
type Players = [Player]

data RuneLink = Game
    { players :: Players
    , gameState :: GameState
    , currentTurn :: Player
    , board :: Board
    } deriving(Eq, Show)

isPlaced :: Rune -> Bool
isPlaced (Placed _) = True
isPlaced _ = False

placeRune :: Player -> Rune -> Rune
placeRune player Reachable = Placed player
placeRune _ x = x

getPlayer :: Rune -> Maybe Player
getPlayer (Placed x) = Just x
getPlayer _ = Nothing

setRuneOnBoard :: Rune -> Location -> Board -> Board
setRuneOnBoard rune location board = M.setElem rune location board

createBoard :: Int -> Int -> Board
createBoard width height =
    let reachables = replicate width Reachable
        unreachables = repeat Unreachable
    in M.fromList height width $ reachables ++ unreachables

getRunes :: Board -> [Location] -> [Rune]
getRunes board = map $ (M.!) board

checkVictory :: Int -> Player -> Row -> Col -> Board -> Maybe [[Location]]
checkVictory n player row col board = 
    let axes = getAxes row col board
        listsOfN = concatMap (sublists n) axes
        verifyList xs = all (==Placed player) $ getRunes board xs
        verifiedList = filter verifyList listsOfN
    in if null verifiedList then Nothing else Just verifiedList

isDraw :: Board -> Bool
isDraw board = all (==Unreachable) board
