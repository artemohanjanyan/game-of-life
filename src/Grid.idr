module Grid

import Data.AVL.Set
import Data.AVL.Dict

%access export

Point : Type
Point = (Int, Int)

%name Point point

addPoints : Point -> Point -> Point
addPoints (a, b) (c, d) = (a + c, b + d)

record Grid where
    constructor MkGrid
    bounds : (Point, Nat, Nat)
    lifes : Set Point

%name Grid grid

inBounds : (Point, Nat, Nat) -> Point -> Bool
inBounds ((x, y), w, h) (a, b) =
    x <= a && a < x + toIntNat w && y <= b && b < y + toIntNat h

addLife : Point -> Grid -> Grid
addLife point grid =
    if inBounds (bounds grid) point
       then record { lifes $= insert point } grid
       else grid

-- TODO figure out how to erase
removeLife : Point -> Grid -> Grid
removeLife point grid = record { lifes = difference (lifes grid) $ fromList [point] } grid

nextGeneration : Grid -> Grid
nextGeneration grid =
    let neighbourCounts = toList $ foldl accountLife empty $ lifes grid
        nextLifes = map fst $ filter checkLife neighbourCounts
    in record { lifes = fromList nextLifes } grid
  where
    neighbourCells : List Point
    neighbourCells = [(-1, -1), (0, -1), (1, -1),
                      (-1,  0),          (1,  0),
                      (-1,  1), (0,  1), (1,  1)]

    lifeNeighbours : Point -> List Point
    lifeNeighbours point = filter (inBounds $ bounds grid) $ map (addPoints point) neighbourCells

    addNeighbour : Dict Point Nat -> Point -> Dict Point Nat
    addNeighbour dict point =
        case lookup point dict of
            Just count => insert point (count + 1) dict
            Nothing    => insert point          1  dict

    accountLife : Dict Point Nat -> Point -> Dict Point Nat
    accountLife dict point = foldl addNeighbour dict $ lifeNeighbours point

    checkLife : (Point, Nat) -> Bool
    checkLife (point, count) = count == 3 || (count == 2 && contains point (lifes grid))
