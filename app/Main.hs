module Main(main) where

import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import RuneLink.Base.Game

runeHoleRadius = 22.5
runeHoleOffset = 5
runeHoleXAmount = 7
runeHoleYAmount = 6
runeHoleDistance = runeHoleOffset + runeHoleRadius * 2

boardSizeX = boardSize runeHoleXAmount
boardSizeY = boardSize runeHoleYAmount

background =            makeColorI 72 62 51 255     -- RuneLink greyish background
runelinkBoardColor =    makeColorI 108 79 44 255    -- RuneLink brown

boardSize :: Float -> Float
boardSize x = runeHoleOffset + runeHoleDistance * x

window :: Display
window = InWindow "Better RuneLink" (700, 600) (500, 500)

runelinkBoard :: Picture
runelinkBoard = color runelinkBoardColor $ rectangleSolid boardSizeX boardSizeY

runeHole :: Point -> Picture
runeHole (x,y) = translate x y $ circleSolid runeHoleRadius

runeHoleLocs :: Float -> [Float]
runeHoleLocs n = 
    let shift = boardSize n / 2
        offset = runeHoleRadius + runeHoleOffset
        transform = (shift-) . (offset+) . (runeHoleDistance*)
    in map transform [0..n-1]

runeHoles :: [Picture]
runeHoles = map runeHole $ liftA2 (,) (runeHoleLocs runeHoleXAmount) (runeHoleLocs runeHoleYAmount)

drawing :: Temp -> Picture
drawing _ = pictures $ runelinkBoard : runeHoles

initBoard = createBoard 6 7
initLoc = (1,1)
player = Player 1
initTemp = Temp initBoard initLoc

data Temp = Temp {board :: Board, currentLoc :: (Int,Int)} deriving(Show)

update :: ViewPort -> Float -> Temp -> Temp
update _ _ (Temp b l) = 
    let newLoc = (fst l + 1, snd l + 1)
        newBoard = setRuneOnBoard (Placed player) newLoc b
        victories = checkVictory 4 player 1 3 newBoard
    in Temp newBoard newLoc
    
main :: IO ()
main = simulate window background 60 initTemp drawing update
