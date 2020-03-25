module Minim.Interval exposing
  ( print
  )

import Minim.Model exposing (Interval (..))

print : Interval -> String
print interval =
  case interval of
    Minor2nd   -> "a half step / minor 2nd"
    Major2nd   -> "a whole step / major 2nd"
    Minor3rd   -> "a minor 3rd"
    Major3rd   -> "a major 3rd"
    Perfect4th -> "a perfect 4th"
    Tritone    -> "a tritone / augmented 4th / diminished 5th"
    Perfect5th -> "a perfect 5th"
    Minor6th   -> "a minor 6th / augmented 5th"
    Major6th   -> "a major 6th"
    Minor7th   -> "a minor 7th / augmented 6th"
    Major7th   -> "a major 7th"
    Octave     -> "an octave"
