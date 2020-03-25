module Minim.Update exposing
  ( Message (..)
  , update
  )

import Debug
import Keyboard
import Minim.Keyboard as Piano
import Minim.Model exposing (..)
import Minim.Note as Note
import Minim.Test as Test
import Random
import Task
import Time exposing (Posix)

type Message
  = RequestNewGame
  | StartGame Posix
  | Allow Challenge
  | Disallow Challenge

  | AssignTest Test
  | PressKeys Keyboard.Msg

  | Tick Posix
  | Validate
  | EndGame Int

updateKeys : List Keyboard.Key -> List Note -> List Note
updateKeys newKeys currentNotes =
  let
    newNotes =
      List.filterMap Piano.read newKeys

    toggler newNote acc =
      if List.member newNote acc then
        List.filter (\x -> x /= newNote) acc
      else
        newNote :: acc
  in
    List.foldr toggler currentNotes newNotes

update : Message -> Model -> ( Model, Cmd Message )
update message model =
  case message of
    RequestNewGame ->
      ( model
      , Task.perform StartGame Time.now
      )

    StartGame now ->
      ( { model
        | interface = Testing
            { startTime    = now
            , currentTime  = now
            , currentScore = 0
            , lastResult   = Nothing
            , keyboard     = []
            , test         = Nothing
            }
        }

      , Random.generate AssignTest Test.random
      )

    Allow challenge ->
      ( { model | challenges = challenge :: model.challenges }
      , Cmd.none
      )

    Disallow challenge ->
      ( { model
        | challenges = List.filter (\x -> x /= challenge) model.challenges
        }

      , Cmd.none
      )

    Tick tick ->
      case model.interface of
        Menu _ ->
          ( model, Cmd.none )

        Testing testingModel ->
          let
            updated =
              { model
              | interface = Testing
                  { testingModel
                  | currentTime = tick
                  }
              }

            currentTime =
              Time.posixToMillis tick

            startTime =
              Time.posixToMillis testingModel.startTime
          in
            if Debug.log "duration" (currentTime - startTime) >= 180000 then
              update (EndGame testingModel.currentScore) updated
            else
              ( updated, Cmd.none )

    AssignTest test ->
      case model.interface of
        Testing testingModel ->
          ( { model
            | interface = Testing
                { testingModel
                | test = Just test
                }
            }

          , Cmd.none
          )

        Menu _ ->
          ( model, Cmd.none )

    PressKeys msg ->
      let
        newKeys =
          Keyboard.update msg model.pressedKeys

        updated =
          { model | pressedKeys = newKeys }
      in
        if newKeys == [ Keyboard.Enter ] then
          update Validate updated
        else
          case model.interface of
            Menu _ ->
              ( updated, Cmd.none )

            Testing testingModel ->
              let
                selection =
                  updateKeys newKeys testingModel.keyboard 
              in
                ( { updated
                  | interface = Testing
                      { testingModel
                      | keyboard = selection
                      }
                  }

                , Cmd.none
                )

    Validate ->
      case model.interface of
        Menu _ ->
          ( model, Cmd.none )

        Testing testingModel ->
          case testingModel.test of
            Just test ->
              let
                guess =
                  List.sortBy Note.toInt testingModel.keyboard

                check =
                  List.sortBy Note.toInt (Test.solve test)

                updated =
                  if guess == check then
                    { testingModel
                    | currentScore = testingModel.currentScore + 1
                    , keyboard     = []
                    , lastResult   = Just Correct
                    , test         = Nothing
                    }
                  else
                    { testingModel
                    | keyboard     = []
                    , lastResult   = Just (ShouldHaveBeen check)
                    , test         = Nothing
                    }
              in
                ( { model | interface = Testing updated }
                , Random.generate AssignTest Test.random
                )

            Nothing ->
              ( model, Cmd.none )

    EndGame score ->
      let
        updated =
          { model | interface = Menu { lastScore = Just score } }
      in
        if model.highscore < score then
          ( { updated | highscore = score }, Cmd.none )
        else
          ( updated, Cmd.none )
