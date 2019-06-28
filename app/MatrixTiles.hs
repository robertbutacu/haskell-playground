module MatrixTiles where

import Data.List
import Data.Ord
{-
You are given an M by N matrix consisting of booleans that represents a board.
Each True boolean represents a wall.
Each False boolean represents a tile you can walk on.

Given this matrix, a start coordinate, and an end coordinate,
return the minimum number of steps required to reach the end coordinate from the start.

 If there is no possible path, then return null.

 You can move up, left, down, and right.

 You cannot move through walls.

 You cannot wrap around the edges of the board.
 For example, given the following board:

 [[f, f, f, f],
 [t, t, f, t],
 [f, f, f, f],
 [f, f, f, f]]
 and start = (3, 0) (bottom left) and end = (0, 0) (top left),
 the minimum number of steps required to reach the end is 7,
 since we would need to go through (1, 2) because there is a wall everywhere else on the second row.
-}

(???) = error "Not implemented"

canStepOn :: (Int, Int) -> [[Bool]] -> Bool
canStepOn (x, y) matrix =
  case (x >= 0 && x < length (head matrix), y >= 0 && y < length matrix) of
    (True, True) -> not (matrix !! y !! x)
    (_, _)       -> False


validStates :: (Int, Int) -> [[Bool]] -> [(Int, Int)] -> [(Int, Int)]
validStates currPos matrix currPath = up ++ down ++ left ++ right
  where
    up    = [upPath    | canStepOn upPath matrix    && notElem upPath currPath]
    down  = [downPath  | canStepOn downPath matrix  && notElem downPath currPath]
    left  = [leftPath  | canStepOn leftPath matrix  && notElem leftPath currPath]
    right = [rightPath | canStepOn rightPath matrix && notElem rightPath currPath]
    downPath  = (fst currPos, snd currPos + 1)
    upPath    = (fst currPos, snd currPos - 1)
    leftPath  = (fst currPos - 1, snd currPos)
    rightPath = (fst currPos + 1, snd currPos)


constructAllPaths :: (Int, Int) -> (Int, Int) -> [[Bool]] -> [(Int, Int)] -> [(Int, Int)]
constructAllPaths destination currPos matrix currPath =
  fst . minimumBy customCmp $ allPaths
  where
    allPaths                                    = map (withPathLength . nextPaths) (validStates currPos matrix currPath)
    nextPaths nextPos                           = constructAllPaths destination nextPos matrix (currPath ++ [currPos])
    withPathLength path                         = (path, length path)
    customCmp (path1, length1) (path2, length2) = compare length1 length2

findPath :: [[Bool]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findPath matrix start finish = constructAllPaths finish start matrix []

m :: [[Bool]]
m = [[False, False, False, False],
     [True,  True,  False, True],
     [False, False, False, False],
     [False, False, False, False]]
