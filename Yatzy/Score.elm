module Yatzy.Score where

import Set
import Debug

upper : Int -> List Int -> Int
upper n ds =
  let
      isN x =
        x == n
  in
      ds
        |> List.filter isN
        |> List.sum

chance : List Int -> Int
chance ds =
  List.sum ds

yatzy : List Int -> Int
yatzy ds =
  let
      equalToFirst n =
        Just n == (List.head ds)
  in
      if List.all equalToFirst ds
      then 50
      else 0

smallStraight : List Int -> Int
smallStraight ds =
  if List.sort ds == [1,2,3,4,5]
  then 15
  else 0

largeStraight : List Int -> Int
largeStraight ds =
  if List.sort ds == [2,3,4,5,6]
  then 20
  else 0

fourOfAKind : List Int -> Int
fourOfAKind ds =
  someOfAKind 4 ds

threeOfAKind : List Int -> Int
threeOfAKind ds =
  someOfAKind 3 ds

onePair : List Int -> Int
onePair ds =
  case List.maximum (pairs ds) of
    Just x -> 2 * x
    Nothing -> 0

twoPair : List Int -> Int
twoPair ds =
  case pairs ds of
    [x, y] -> (x + y) * 2
    _ -> 0

fullHouse : List Int -> Int
fullHouse ds =
  let
      twoOfAKind = List.head (exactlyCountN 2 ds)
      threeOfAKind = List.head (exactlyCountN 3 ds)
  in
      case (twoOfAKind, threeOfAKind) of
        (Just x, Just y) -> x * 2 + y * 3
        _ -> 0

someOfAKind : Int -> List Int -> Int
someOfAKind count ds =
  let
      firstMatching =
        case List.head (atLeastCountN count ds) of
          Just n -> n
          Nothing -> 0
  in
      count * firstMatching

uniq : List comparable -> List comparable
uniq ds =
  ds
    |> Set.fromList
    |> Set.toList

pairs : List Int -> List Int
pairs ds =
  ds
    |> atLeastCountN 2
    |> List.sort
    |> uniq

countN : Int -> List Int -> Int
countN n ds =
  ds
    |> List.filter (\x -> x == n)
    |> List.length

exactlyCountN : Int -> List Int -> List Int
exactlyCountN count ds =
  List.filter (\n -> count == countN n ds) ds

atLeastCountN : Int -> List Int -> List Int
atLeastCountN count ds =
  List.filter (\n -> count <= countN n ds) ds
