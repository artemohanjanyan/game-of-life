module GridIO

import Effects
import Effect.File

import Grid

%access public export

LIFE_CHAR : Char
LIFE_CHAR = 'o'

readGrid : Int -> Int -> Eff Grid [FILE R]
readGrid columnN rowN = do
    lines <- mapE (\_ => tryReadLine) [1 .. rowN]
    let numberedLines = zip lines (the (List Int) [0 .. (columnN - 1)])
    pure $ foldl addLine (MkGrid 0 0 columnN rowN empty) numberedLines
  where
    tryReadLine : Eff String [FILE R]
    tryReadLine = do
        Result line <- readLine | FError _ => pure ""
        pure line

    addChar : Int -> Grid -> (Char, Int) -> Grid
    addChar row grid (c, column) =
        if c == 'o'
            then addLife (column, row) grid
            else grid

    addLine : Grid -> (String, Int) -> Grid
    addLine grid (line, row) = foldl (addChar row) grid $ zip (unpack line) (the (List Int) [0 .. (rowN - 1)])

writeGrid : Grid -> Eff () [FILE W]
writeGrid grid = do
    Success <- writeString toString | FError _ => pure ()
    pure ()
  where
    coords : List (List (Int, Int))
    coords = map (\row => map (\column => (column, row)) [0 .. (columnN grid - 1)]) [0 .. (rowN grid - 1)]

    lines : List String
    lines = map (\q => pack (map (\point => if containsLife point grid then 'o' else '.') q)) coords

    toString : String
    toString = unlines lines
