module Main exposing (..)

import Browser
import ColorUtils exposing (blue, darkBlue, white)
import Day1
import Day2
import Day3
import Day4
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Day =
    { visible : Bool
    , view : Element Msg
    }


type alias Model =
    { days : Dict Int Day }


init : ( Model, Cmd Msg )
init =
    ( { days =
            Dict.fromList
                [ ( 1, { visible = False, view = Day1.view } )
                , ( 2, { visible = False, view = Day2.view } )
                , ( 3, { visible = False, view = Day3.view } )
                , ( 4, { visible = True, view = Day4.view } )
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
                currentDay =
                    Dict.get day model.days

                newDays =
                    case currentDay of
                        Nothing ->
                            model.days

                        Just current ->
                            Dict.insert day { current | visible = show } model.days
            in
            ( { model | days = newDays }, Cmd.none )



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
    List.range 1 (Dict.size model.days)
        |> List.map (\n -> dayView n model)


getVis : Int -> Dict Int Day -> Bool
getVis n dict =
    case Dict.get n dict of
        Nothing ->
            False

        Just day ->
            day.visible


getView : Int -> Dict Int Day -> Element Msg
getView n dict =
    case Dict.get n dict of
        Nothing ->
            el [] (text ("No view defined for day " ++ String.fromInt n))

        Just day ->
            day.view


dayView : Int -> Model -> Element Msg
dayView n model =
    let
        dayVis =
            getVis n model.days

        elements =
            if dayVis then
                [ getView n model.days ]

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
