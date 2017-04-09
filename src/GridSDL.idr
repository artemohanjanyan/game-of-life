module GridSDL

import Effects
import Effect.SDL
import Effect.State

import Grid

%access public export

gray : Colour
gray = MkCol 192 192 192 255

record Config where
    constructor MkConfig
    width, height : Int

%name Config config

mouseToPoint : (Int, Int) -> Config -> Grid -> Point
mouseToPoint (x, y) config grid =
    (x * columnN grid `div` width config, y * rowN grid `div` height config)

drawGrid : Config -> Grid -> Eff () [SDL_ON]
drawGrid (MkConfig width height) grid = do
    mapE (\column => drawColumn column) [1..columnN grid]
    pure ()
  where
    cellWidth : Int
    cellWidth = width `div` columnN grid

    cellHeight : Int
    cellHeight = height `div` rowN grid

    cellMargin : Int
    cellMargin = 2

    lifeMargin : Int
    lifeMargin = 2 + cellMargin

    drawCell : Int -> Int -> Eff () [SDL_ON]
    drawCell column row =
        let left = width * (column - 1) `div` columnN grid
            top = height * (row - 1) `div` rowN grid
        in do
            rectangle gray (left + cellMargin) (top + cellMargin)
                    (cellWidth - 2 * cellMargin) (cellHeight - 2 * cellMargin)
            when (containsLife (column - 1, row - 1) grid) $ rectangle red
                    (left + lifeMargin) (top + lifeMargin)
                    (cellWidth - 2 * lifeMargin) (cellHeight - 2 * lifeMargin)

    drawColumn : Int -> Eff () [SDL_ON]
    drawColumn column = mapE (\row => drawCell column row) [1..rowN grid] *> pure ()
