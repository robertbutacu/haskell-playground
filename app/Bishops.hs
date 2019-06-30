module Bishops where

import Common ((???))
{-
On our special chessboard, two bishops attack each other if they share the same diagonal.
This includes bishops that have another bishop located between them, i.e. bishops can attack through pieces.

You are given N bishops, represented as (row, column) tuples on a M by M chessboard.
Write a function to count the number of pairs of bishops that attack each other.
 The ordering of the pair doesn't matter: (1, 2) is considered the same as (2, 1).

For example, given M = 5 and the list of bishops:

(0, 0)
(1, 2)
(2, 2)
(4, 0)
The board would look like this:

[b 0 0 0 0]
[0 0 b 0 0]
[0 0 b 0 0]
[0 0 0 0 0]
[b 0 0 0 0]
You should return 2, since bishops 1 and 3 attack each other, as well as bishops 3 and 4.
-}

filterMap :: (a -> (b, Bool)) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) =
  if snd result
    then fst result : filterMap f xs
    else filterMap f xs
  where
    result = f x

isSameDiagonal :: (Integer, Integer) -> (Integer, Integer) -> Bool
isSameDiagonal from to = abs (fst from - fst to) == abs (snd from - snd to)

filterOutDuplicates :: [(Integer, Integer)] -> [(Integer, Integer)]
filterOutDuplicates xs = filter (\v -> any (\ov -> fst v == snd ov && snd v == fst ov) xs) xs

attackingBishops :: [(Integer, Integer)] -> [(Integer, Integer)]
attackingBishops bishops = filterOutDuplicates . filter (\b -> any (isSameDiagonal b) bishops) $ bishops

bishops = [(0, 0), (1, 2), (2, 2), (4,0)]