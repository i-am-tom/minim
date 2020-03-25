module Minim.Test exposing
  ( print
  , random
  , solve
  )

import Minim.Interval as Interval
import Minim.Model exposing (..)
import Minim.Note as Note
import Random

random : Random.Generator Test
random =
  let
    note =
      Random.uniform C [ Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B ]

    chord =
      Random.uniform Major [ Minor, Dominant ]

    direction =
      Random.uniform Up [ Down ]

    interval = Random.uniform Minor2nd
      [ Major2nd
      , Minor3rd
      , Major3rd
      , Perfect4th
      , Tritone
      , Perfect5th
      , Minor6th
      , Major6th
      , Minor7th
      , Major7th
      , Octave
      ]

    absoluteInterval =
      Random.map3 AbsoluteIntervalTest note direction interval

    twoNoteVoicing =
      Random.map2 TwoNoteVoicingTest note chord

  in
    Random.uniform absoluteInterval [ twoNoteVoicing ]
      |> Random.andThen identity

intervalToInt : Interval -> Int
intervalToInt interval =
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

intToNote : Int -> Note
intToNote int =
  case int of
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
      intToNote (modBy 12 overflow)

print : Test -> String
print test =
  case test of
    AbsoluteIntervalTest note direction interval ->
      case direction of
        Down -> "Transpose " ++ Interval.print interval
             ++ " below "    ++ Note.print Note.Flat note

        Up -> "Transpose " ++ Interval.print interval
           ++ " above "    ++ Note.print Note.Flat note

    TwoNoteVoicingTest note chord ->
      case chord of
        Major ->
          "Select the 3rd and 7th of " ++ Note.print Note.Flat note ++ "Δ"

        Minor ->
          "Select the 3rd and 7th of " ++ Note.print Note.Flat note ++ "-7"

        Dominant ->
          "Select the 3rd and 7th of " ++ Note.print Note.Flat note ++ "7"

solve : Test -> List Note
solve test =
  case test of
    AbsoluteIntervalTest note direction interval ->
      [ case direction of
          Up   -> intToNote (Note.toInt note + intervalToInt interval)
          Down -> intToNote (Note.toInt note - intervalToInt interval)
      ]

    TwoNoteVoicingTest note chord ->
      case chord of
        Major ->
          [ intToNote (Note.toInt note + intervalToInt Major3rd)
          , intToNote (Note.toInt note + intervalToInt Major7th)
          ]

        Minor ->
          [ intToNote (Note.toInt note + intervalToInt Minor3rd)
          , intToNote (Note.toInt note + intervalToInt Minor7th)
          ]

        Dominant ->
          [ intToNote (Note.toInt note + intervalToInt Major3rd)
          , intToNote (Note.toInt note + intervalToInt Minor7th)
          ]
