module GridSDL

import Effects
import Effect.SDL
import Effect.State

import Grid

%access public export

gray : Colour
gray = MkCol 128 128 128 255

record Config where
    constructor MkConfig
    width, height : Int

%name Config config

record GridWindow where
    constructor MkGridWindow
    leftX, topX, columnN, rowN : Int

mouseToPoint : (Int, Int) -> Config -> GridWindow -> Point
mouseToPoint (x, y) config (MkGridWindow wLeft wTop wColumnN wRowN) =
    ((x * wColumnN `div` width config) + wLeft, (y * wRowN `div` height config) + wTop)

drawGrid : Config -> GridWindow -> Grid -> Eff () [SDL_ON]
drawGrid (MkConfig width height) (MkGridWindow wLeft wTop wColumnN wRowN) grid = do
    mapE (\column => drawColumn column) [(wLeft + 1) .. (wLeft + wColumnN)]
    pure ()
  where
    cellWidth : Int
    cellWidth = width `div` wColumnN

    cellHeight : Int
    cellHeight = height `div` wRowN

    cellMargin : Int
    cellMargin = 1

    --lifeMargin : Int
    --lifeMargin = 1 + cellMargin

    drawCell : Int -> Int -> Eff () [SDL_ON]
    drawCell column row =
        let left = width * (column - wLeft - 1) `div` wColumnN
            top = height * (row - wTop - 1) `div` wRowN
        in do
            rectangle gray (left) (top) (cellWidth) (cellHeight)
            when (inBounds (column - 1, row - 1) grid) $ rectangle
                    (if (containsLife (column - 1, row - 1) grid) then white else black)
                    (left + cellMargin) (top + cellMargin)
                    (cellWidth - 2 * cellMargin) (cellHeight - 2 * cellMargin)

            --rectangle gray (left + cellMargin) (top + cellMargin)
            --        (cellWidth - 2 * cellMargin) (cellHeight - 2 * cellMargin)
            --when (containsLife (column - 1, row - 1) grid) $ rectangle red
            --        (left + lifeMargin) (top + lifeMargin)
            --        (cellWidth - 2 * lifeMargin) (cellHeight - 2 * lifeMargin)

            --when (containsLife (column - 1, row - 1) grid) $ rectangle white
            --        (left + cellMargin) (top + cellMargin)
            --        (cellWidth - 2 * cellMargin) (cellHeight - 2 * cellMargin)

    drawColumn : Int -> Eff () [SDL_ON]
    drawColumn column = mapE (\row => drawCell column row) [(wTop + 1) .. (wTop + wRowN)] *> pure ()
