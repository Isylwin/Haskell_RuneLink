module Main(main) where

import Graphics.Gloss

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
    let translate = (-) $ (boardSize n) / 2
        offset = runeHoleRadius + runeHoleOffset
    in map (translate . (+offset) . (*runeHoleDistance)) [0..n-1]

runeHoles :: [Picture]
runeHoles = map runeHole $ (,) <$> (runeHoleLocs runeHoleXAmount) <*> (runeHoleLocs runeHoleYAmount)

drawing :: Picture
drawing = pictures $ runelinkBoard : runeHoles

main :: IO ()
main = display window background drawing
