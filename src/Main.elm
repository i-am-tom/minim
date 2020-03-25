module Main exposing
  ( main
  )

import Browser
import Debug exposing (todo)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onSubmit)
import Keyboard
import Minim.Keyboard as Piano
import Minim.Model exposing (Model, Interface (..), Note (..), Verdict (..))
import Minim.Note as Note
import Minim.Test as Test
import Minim.Update exposing (Message (..), update)
import Task
import Time

main : Program () Model Message
main =
  Browser.element
    { init = \() ->
        ( { challenges  = []
          , highscore   = 0
          , interface   = Menu { lastScore = Nothing }
          , pressedKeys = []
          }

        , Cmd.none
        )

    , subscriptions = \model -> Sub.batch
        [ Sub.map PressKeys Keyboard.subscriptions

        , case model.interface of
            Menu _    -> Sub.none
            Testing _ -> Time.every 1000 Tick
        ]

    , update = update
    , view = \model ->
        case model.interface of
          Menu menuModel ->
            div [ class "menu" ]
              [ div [ class "scores" ]
                  [ div [ class "previous-score" ]
                      [ text "Previous score: "

                      , strong []
                          [ case menuModel.lastScore of
                              Just score ->
                                text (String.fromInt score)

                              Nothing ->
                                text "N / A"
                          ]
                      ]

                  , div [ class "high-score" ]
                      [ text "High score: "
                      , strong []
                          [ text (String.fromInt model.highscore)
                          ]
                      ]
                  ]

              , form [ onSubmit RequestNewGame ]
                  [ button [ class "play" ]
                      [ text "Play"
                      ]
                  ]
              ]

          Testing testingModel ->
            div []
              [ div [ class "countdown" ]
                  [ text "Remaining time: "
                  
                  , span [ class "remaining" ]
                      [ let
                          current = Time.posixToMillis testingModel.currentTime
                          start   = Time.posixToMillis testingModel.startTime

                          remainder = 180 - ((current - start) // 1000)
                        in
                          text (String.fromInt remainder)
                      ]
                  ]
              
              , div [ class "keyboard" ]
                  [ div [ class "naturals" ]
                      ( List.indexedMap (natural testingModel.keyboard)
                          [ C, D, E, F, G, A, B ]
                      )

                  , div [ class "accidentals" ]
                      ( List.indexedMap (accidental testingModel.keyboard)
                          [ Db, Eb, Gb, Ab, Bb ]
                      )

                  ]

              , case testingModel.test of
                  Just test ->
                    div [ class "instruction" ]
                      [ text (Test.print test)
                      ]

                  Nothing ->
                    text ""

              , case testingModel.lastResult of
                  Nothing ->
                    text ""

                  Just Correct ->
                    div [ class "result correct" ]
                      [ text "Yay!"
                      ]

                  Just (ShouldHaveBeen answers) ->
                    let
                      notes = List.map (Note.print Note.Flat) answers
                    in
                      div [ class "result incorrect" ]
                        [ text "Oops! The correct answer was: "
                        , text (String.join ", " notes)
                        ]

              , div [ class "current-score" ]
                  [ text "Current score: "

                  , strong []
                      [ text (String.fromInt testingModel.currentScore)
                      ]
                  ]
              ]
    }

accidental : List Note -> Int -> Note -> Html msg
accidental highlights index note =
  div
    [ class "key", class ("key-" ++ Note.print Note.Flat note)
    , class (if List.member note highlights then "key-active" else "")
    ]

    [ text (Piano.label note) ]

natural : List Note -> Int -> Note -> Html msg
natural highlights index note =
  div
    [ class "key", class ("key-" ++ Note.print Note.Flat note)
    , class (if List.member note highlights then "key-active" else "")
    ]

    [ text (Piano.label note) ]
