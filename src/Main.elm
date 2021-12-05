module Main exposing (..)

import Browser
import ColorUtils exposing (blue, darkBlue, white)
import Day1
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { vis : Dict Int Bool }


init : ( Model, Cmd Msg )
init =
    ( { vis = Dict.fromList [ ( 1, False ) ] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ShowDay ( Int, Bool )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowDay ( day, show ) ->
            let
                newVis =
                    Dict.insert day show model.vis
            in
            ( { model | vis = newVis }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        maybeDay1Vis =
            Dict.get 1 model.vis

        day1Vis =
            case maybeDay1Vis of
                Nothing ->
                    False

                Just v ->
                    v

        elements =
            if day1Vis then
                [ Day1.view ]

            else
                []
    in
    layout [] <|
        column
            [ width fill
            , height fill
            , paddingXY 16 12
            , spacing 12
            ]
            [ el [] (text "Advent of Code")
            , row
                [ spacing 12 ]
                [ el [] (text "Day 1")
                , button 1 day1Vis
                ]
            , column
                [ spacing 4, paddingXY 16 0 ]
                elements
            ]


button : Int -> Bool -> Element Msg
button day current =
    let
        label =
            if current then
                "Hide"

            else
                "Show"
    in
    Input.button
        [ Background.color blue
        , Font.color white
        , Font.size 15
        , paddingXY 16 8
        , Border.rounded 3
        , mouseOver [ Background.color darkBlue ]
        ]
        { onPress = Just (ShowDay ( day, not current )), label = text label }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
