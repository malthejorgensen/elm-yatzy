module Yatzy.Score where
import List exposing (sort, head, tail, repeat, drop, take, sum, filter, map, length)
import String exposing (fromList)
--import Graphics.Element exposing (show)
import Debug

eyeCounts ds =
  let eyeCheck n = (n, filter (\d -> d == n) ds |> length)
  in
  map eyeCheck [1..6] |> filter (\(n, c) -> c > 0)

upper : Int -> List Int -> Int
upper n ds =
  sum <| filter (\d -> d == n) ds

chance : List Int -> Int
chance ds =
  sum ds

yatzy : List Int -> Int
yatzy ds =
  case eyeCounts ds of
    [(p, 5)] -> 50
    otherwise -> 0
    --otherwise -> Debug.log (toString <| eyeCounts ds) 0

smallStraight : List Int -> Int
smallStraight ds =
  let sortedDice = sort ds
  in
  if sortedDice == [1,2,3,4,5] then
    15
  else
    0

largeStraight : List Int -> Int
largeStraight ds =
  let sortedDice = sort ds
  in
  if sortedDice == [2,3,4,5,6] then
    20
  else
    0

fourOfAKind : List Int -> Int
fourOfAKind ds =
  let fours = filter (\(eyes, count) -> count == 4) (eyeCounts ds)
  in
  if fours == [] then
    0
  else
    sum <| map (\(p,q) -> p*q) fours

threeOfAKind : List Int -> Int
threeOfAKind ds =
  let fours = filter (\(eyes, count) -> count == 3) (eyeCounts ds)
  in
  if fours == [] then
    0
  else
    sum <| map (\(p,q) -> p*q) fours

onePair : List Int -> Int
onePair ds =
  let pairs = filter (\(eyes, count) -> count == 2) (eyeCounts ds)
  in
  if pairs == [] then
    0
  else
    sum <| map (\(p,q) -> p*q) pairs

twoPair : List Int -> Int
twoPair ds =
  let pairs = filter (\(eyes, count) -> count == 2) (eyeCounts ds)
  in
  if length pairs == 2 then
    sum <| map (\(p,q) -> p*q) pairs
  else
    0

fullHouse : List Int -> Int
fullHouse ds =
  case eyeCounts ds of
    [(p, 2), (q, 3)] -> sum ds
    [(p, 3), (q, 2)] -> sum ds
    otherwise -> 0
