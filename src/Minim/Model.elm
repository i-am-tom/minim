module Minim.Model exposing (..)

import Time exposing (Posix)
import Keyboard exposing (Key)

type Challenge
  = AbsoluteInterval
  | TwoNoteVoicing

type Direction =
  Up | Down

type Interval
  = Minor2nd
  | Major2nd
  | Minor3rd
  | Major3rd
  | Perfect4th
  | Tritone
  | Perfect5th
  | Minor6th
  | Major6th
  | Minor7th
  | Major7th
  | Octave

type Note =
  C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B

type Chord =
  Major | Minor | Dominant

type Test
  = AbsoluteIntervalTest Note Direction Interval
  | TwoNoteVoicingTest Note Chord

type Verdict =
  Correct | ShouldHaveBeen (List Note)

type alias MenuModel
  = { lastScore : Maybe Int
    }

type alias TestingModel
  = { startTime    : Posix
    , currentTime  : Posix
    , currentScore : Int
    , lastResult   : Maybe Verdict
    , keyboard     : List Note
    , test         : Maybe Test
    }

type Interface
  = Menu MenuModel
  | Testing TestingModel

type alias Model
  = { challenges  : List Challenge
    , highscore   : Int
    , interface   : Interface
    , pressedKeys : List Key
    }
