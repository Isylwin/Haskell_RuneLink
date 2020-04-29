module RuneLink.Base.Game
    ( GameState(..)
    , GameTurn(..)
    , Rune(..)
    , Board
    , RuneLink
    , createBoard
    ) where

import Data.Matrix

data GameState = Playing | VictoryPlayerOne | VictoryPlayerTwo | Draw deriving(Eq, Show)
data GameTurn = PlayerOneTurn | PlayerTwoTurn deriving(Eq, Show, Enum, Bounded)
data Rune = Empty | PlayerOne | PlayerTwo deriving(Eq, Show)
type Board = Matrix Rune
data RuneLink = Game
    { gameState :: GameState
    , gameTurn :: GameTurn
    , board :: Board
    } deriving(Eq, Show)

createBoard :: Int -> Int -> Board
createBoard y x = fromList x y $ repeat Empty