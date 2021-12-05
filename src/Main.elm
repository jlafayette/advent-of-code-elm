module Main exposing (..)

import Browser
import ColorUtils exposing (blue, darkBlue, white)
import Day1
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { day1Answer : Maybe Int }


init : ( Model, Cmd Msg )
init =
    ( { day1Answer = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SolveDay1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SolveDay1 ->
            ( { model | day1Answer = Just Day1.solve }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ width fill
            , height fill
            , paddingXY 16 12
            , spacing 12
            ]
            [ el [] (text "Advent of Code")
            , el [] (text "Day 1")
            , row
                [ spacing 16 ]
                [ button "Solve" SolveDay1
                , el [] (answerToEl model.day1Answer)
                ]
            ]


answerToEl : Maybe Int -> Element Msg
answerToEl maybeInt =
    case maybeInt of
        Just answer ->
            text (String.fromInt answer)

        Nothing ->
            text ""


button : String -> Msg -> Element Msg
button label msg =
    Input.button
        [ Background.color blue
        , Font.color white
        , Font.size 15
        , paddingXY 16 8
        , Border.rounded 3
        , mouseOver [ Background.color darkBlue ]
        ]
        { onPress = Just msg, label = text label }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
