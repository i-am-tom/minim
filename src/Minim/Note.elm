module Minim.Note exposing
  ( SharpOrFlat (..)
  
  , print
  , toInt
  )

import Minim.Model exposing (Note (..))

type SharpOrFlat =
  Sharp | Flat

print : SharpOrFlat -> Note -> String
print sharpOrFlat note =
  case sharpOrFlat of
    Flat ->
      case note of
        C  -> "C"
        Db -> "D♭"
        D  -> "D"
        Eb -> "E♭"
        E  -> "E"
        F  -> "F"
        Gb -> "G♭"
        G  -> "G"
        Ab -> "A♭"
        A  -> "A"
        Bb -> "B♭"
        B  -> "B"

    Sharp ->
      case note of
        C  -> "C"
        Db -> "C♯"
        D  -> "D"
        Eb -> "D♯"
        E  -> "E"
        F  -> "F"
        Gb -> "F♯"
        G  -> "G"
        Ab -> "G♯"
        A  -> "A"
        Bb -> "A♯"
        B  -> "B"

toInt : Note -> Int
toInt note =
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
