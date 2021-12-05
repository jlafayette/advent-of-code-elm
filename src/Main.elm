module Main exposing (..)

import Browser
import ColorUtils exposing (blue, darkBlue, white)
import Day1
import Day2
import Day3
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { vis : Dict Int Bool
    , views : Dict Int (Element Msg)
    }


init : ( Model, Cmd Msg )
init =
    ( { vis =
            Dict.fromList
                [ ( 1, False )
                , ( 2, False )
                , ( 3, True )
                ]
      , views =
            Dict.fromList
                [ ( 1, Day1.view )
                , ( 2, Day2.view )
                , ( 3, Day3.view )
                ]
      }
    , Cmd.none
    )



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
        title =
            el [] (text "Advent of Code")

        elements =
            title :: dayViews model
    in
    layout [] <|
        column
            [ width fill
            , paddingXY 16 12
            , spacing 12
            ]
            elements


dayViews : Model -> List (Element Msg)
dayViews model =
    List.range 1 (Dict.size model.vis)
        |> List.map (\n -> dayView n model)


getVis : Int -> Dict Int Bool -> Bool
getVis n dict =
    case Dict.get n dict of
        Nothing ->
            False

        Just v ->
            v


getView : Int -> Dict Int (Element Msg) -> Element Msg
getView n dict =
    case Dict.get n dict of
        Nothing ->
            el [] (text ("No view defined for day " ++ String.fromInt n))

        Just e ->
            e


dayView : Int -> Model -> Element Msg
dayView n model =
    let
        dayVis =
            getVis n model.vis

        elements =
            if dayVis then
                [ getView n model.views ]

            else
                []
    in
    column
        [ width fill
        , height fill
        , paddingXY 16 12
        , spacing 12
        ]
        [ row
            [ spacing 12 ]
            [ el [] (text ("Day " ++ String.fromInt n))
            , button n dayVis
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
