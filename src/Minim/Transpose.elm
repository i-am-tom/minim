module Minim.Transpose exposing
  ( Transposition

  , print
  , random
  , transpose
  )

import Minim.Interval as Interval exposing (Interval (..))
import Minim.Note exposing (Note (..))
import Random

type Direction
  = Up | Down

type Transposition =
  Transpose Interval Direction

print : Transposition -> String
print (Transpose interval direction) =
  case direction of
    Up   -> "up by "   ++ Interval.print interval
    Down -> "down by " ++ Interval.print interval

random : Random.Generator Transposition
random =
  Random.map2 Transpose Interval.random (Random.uniform Up [Down])

transpose : Transposition -> Note -> Note
transpose (Transpose interval direction) note =
  let
    initial =
      case note of
        C  ->  0
        Db ->  1
        D  ->  2
        Eb ->  3
        E  ->  4
        F  ->  5
        Gb ->  6
        G  ->  7
        Ab ->  8
        A  ->  9
        Bb -> 10
        B  -> 11

    movement =
      case interval of
        Minor2nd   ->  1
        Major2nd   ->  2
        Minor3rd   ->  3
        Major3rd   ->  4
        Perfect4th ->  5
        Tritone    ->  6
        Perfect5th ->  7
        Minor6th   ->  8
        Major6th   ->  9
        Minor7th   -> 10
        Major7th   -> 11
        Octave     -> 12

    toEnum index =
      case index of
        0  -> C
        1  -> Db
        2  -> D
        3  -> Eb
        4  -> E
        5  -> F
        6  -> Gb
        7  -> G
        8  -> Ab
        9  -> A
        10 -> Bb
        11 -> B

        overflow ->
          toEnum (modBy 12 overflow)

  in
    case direction of
      Down -> toEnum (initial - movement)
      Up   -> toEnum (initial + movement)
