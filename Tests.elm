module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import String

import Yatzy.Score


all : Test
all =
  suite "Scores"
    [
      --   upper n ds should return the score for the roll ds used in the
      --   slot for n number of eyes in the upper section of the score sheet.
      suite "upper"
        [
          test "ones" (assertEqual 2 (Yatzy.Score.upper 1 [2,3,1,2,1])),
          test "twos" (assertEqual 8 (Yatzy.Score.upper 2 [2,2,2,3,2]))
        ],

      --   chance ds should return the sum of the eyes on the dice.
      test "chance" (assertEqual 14 (Yatzy.Score.chance [3,4,2,4,1])),

      --   yatzy ds should return 50 if all dice are showing the same number
      --   of eyes.
      suite "yatzy"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.yatzy [2,3,3,3,3])),
          test "won"  (assertEqual 50 (Yatzy.Score.yatzy [4,4,4,4,4]))
        ],

      --   smallStraight ds should return 15 for any set of dice containing
      --   the sequence 1-2-3-4-5 in any order.
      suite "small straight"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.smallStraight [1,2,3,4,6])),
          test "won"  (assertEqual 15 (Yatzy.Score.smallStraight [2,4,3,1,5]))
        ],

      --   largeStraight ds should return 20 for any set of dice containing
      --   the sequence 2-3-4-5-6 in any order.
      suite "large straight"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.largeStraight [5,2,3,2,6])),
          test "won"  (assertEqual 20 (Yatzy.Score.largeStraight [6,2,4,3,5]))
        ],

      --   fourOfAKind ds has a value when there are four of a kind in the
      --   roll and then its value is the total number of eyes on the dice
      --   being four of a kind.
      suite "four of a kind"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.fourOfAKind [2,3,3,3,5])),
          test "won"  (assertEqual 20 (Yatzy.Score.fourOfAKind [5,5,2,5,5]))
        ],

      --   threeOfAKind ds has a value when there are three of a kind in
      --   the roll and then its value is the total number of eyes on the dice
      --   being three of a kind.
      suite "three of a kind"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.threeOfAKind [2,3,4,3,5])),
          test "won"  (assertEqual 12 (Yatzy.Score.threeOfAKind [4,1,4,2,4]))
        ],

      --   onePair ds has a value when there is a pair in the roll and then
      --   its value is the total number of eyes on the dice in the pair.
      suite "one pair"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.onePair [2,3,4,1,5])),
          test "won"  (assertEqual 12 (Yatzy.Score.onePair [4,6,6,2,5]))
        ],

      --   twoPairs ds has a value when there is two pairs in the roll and
      --   then its value is the total number of eyes on the dice in the two
      --   pairs.
      suite "two pairs"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.twoPair [2,3,3,3,3])),
          test "won"  (assertEqual 16 (Yatzy.Score.twoPair [2,4,6,2,6]))
        ],

      --   fullHouse ds has a value when there is three of a kind and a pair
      --   in the roll. The value is the sum of the eyes on the dice.
      suite "full house"
        [
          test "lost" (assertEqual  0 (Yatzy.Score.fullHouse [3,3,3,3,3])),
          test "won"  (assertEqual 21 (Yatzy.Score.fullHouse [3,3,6,3,6]))
        ]
    ]
