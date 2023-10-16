module H2048 where

import Data.List
import Text.Printf (printf)

type Tile = Int
type Grid = [[Tile]]

emptyGrid :: Grid
emptyGrid = replicate 4 (replicate 4 0)

data Direction = DirRight | DirLeft | DirUp | DirDown

showGrid :: Grid -> String
showGrid grid = concat (map showLine grid)
  where
    showLine line = concat (map showTile line) ++ "\n"
    showTile tile = printf "%4d " tile

-- Renvoie True si la tuile est bien une puissance de 2
-- isValidTile :: Tile -> Bool
-- Renvoie True si la grille est une grille valide, c'est-à-dire
-- aux bonnes dimensions et ne contenant que des tuiles valides
-- isValidGrid :: Grid -> Bool

moveLine :: [Tile] -> [Tile]
moveLine (0 : xs) = (moveLine xs) ++ [0]
moveLine (x : y : xs)
  | x == y = ((x + y) : (moveLine xs)) ++ [0]
  | otherwise = x : (moveLine $ y : xs)
moveLine (x : ys) = x : (moveLine ys)
moveLine [] = []

rotateRight :: [[a]] -> [[a]]
rotateRight grid = map reverse $ transpose grid

rotateLeft :: [[a]] -> [[a]]
rotateLeft grid = reverse $ transpose grid

move :: Direction -> Grid -> Grid
move DirLeft grid = map moveLine grid
move DirRight grid = map (reverse . moveLine . reverse) grid
move DirUp grid = rotateRight $ move DirLeft $ rotateLeft grid
move DirDown grid = rotateLeft $ move DirLeft $ rotateRight grid

addTwo :: [Tile] -> [Tile]
addTwo [] = []
addTwo (0 : xs) = 2 : xs
addTwo (x : xs) = x : addTwo xs

applyTransform :: [[Tile] -> [Tile]] -> Grid -> Grid
applyTransform transform grid = zipWith (\f line -> f line) transform grid

addTwoToLine :: Int -> Grid -> Grid
addTwoToLine n grid = applyTransform ((replicate n id) ++ [addTwo] ++ replicate (4 - n - 1) id) grid
