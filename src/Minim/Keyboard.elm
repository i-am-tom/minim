module Minim.Keyboard exposing
  ( label
  , read
  )

import Keyboard exposing (Key (..))
import Keyboard.Arrows
import Minim.Model exposing (Note (..))

label : Note -> String
label note =
  case note of
    C  -> "A"
    Db -> "W"
    D  -> "S"
    Eb -> "E"
    E  -> "D"
    F  -> "F"
    Gb -> "T"
    G  -> "G"
    Ab -> "Y"
    A  -> "H"
    Bb -> "U"
    B  -> "J"

read : Key -> Maybe Note
read key =
  case key of
    Character "A" -> Just C
    Character "W" -> Just Db
    Character "S" -> Just D
    Character "E" -> Just Eb
    Character "D" -> Just E
    Character "F" -> Just F
    Character "T" -> Just Gb
    Character "G" -> Just G
    Character "Y" -> Just Ab
    Character "H" -> Just A
    Character "U" -> Just Bb
    Character "J" -> Just B
    _             -> Nothing
