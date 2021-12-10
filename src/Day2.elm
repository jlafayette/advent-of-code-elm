module Day2 exposing (view)

import Day2Input exposing (exampleInput, input)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type Cmd
    = Forward Int
    | Down Int
    | Up Int


parse : String -> List Cmd
parse lines =
    lines
        |> String.trim
        |> String.lines
        |> List.map (\x -> String.trim x)
        |> List.map parseLine


parseLine : String -> Cmd
parseLine line =
    let
        parts =
            String.words line

        default =
            Forward 0
    in
    case parts of
        [ commandStr, intStr ] ->
            let
                amount =
                    Maybe.withDefault 0 (String.toInt intStr)
            in
            case commandStr of
                "forward" ->
                    Forward amount

                "down" ->
                    Down amount

                "up" ->
                    Up amount

                _ ->
                    default

        _ ->
            default


f1 : Cmd -> ( Int, Int ) -> ( Int, Int )
f1 cmd ( forward, depth ) =
    case cmd of
        Forward amount ->
            ( forward + amount, depth )

        Down amount ->
            ( forward, depth + amount )

        Up amount ->
            ( forward, depth - amount )


mult : ( Int, Int ) -> Int
mult ( x, y ) =
    x * y



-- expect 150


solveExample1 _ =
    exampleInput
        |> parse
        |> List.foldl f1 ( 0, 0 )
        |> mult


solvePart1 _ =
    input
        |> parse
        |> List.foldl f1 ( 0, 0 )
        |> mult



---- Part 2 ----


type alias SubState =
    { horizontalPos : Int
    , aim : Int
    , depth : Int
    }


f2 : Cmd -> SubState -> SubState
f2 cmd { horizontalPos, aim, depth } =
    case cmd of
        Forward amount ->
            { horizontalPos = horizontalPos + amount
            , aim = aim
            , depth = depth + (aim * amount)
            }

        Down amount ->
            { horizontalPos = horizontalPos
            , aim = aim + amount
            , depth = depth
            }

        Up amount ->
            { horizontalPos = horizontalPos
            , aim = aim - amount
            , depth = depth
            }


mult2 : SubState -> Int
mult2 { horizontalPos, aim, depth } =
    horizontalPos * depth


solveExample2 _ =
    exampleInput
        |> parse
        |> List.foldl f2 { horizontalPos = 0, aim = 0, depth = 0 }
        |> mult2


solvePart2 _ =
    input
        |> parse
        |> List.foldl f2 { horizontalPos = 0, aim = 0, depth = 0 }
        |> mult2


view : () -> Element msg
view _ =
    let
        ex1 =
            "Example 1: " ++ Debug.toString (solveExample1 ())

        ex2 =
            "Example 2: " ++ Debug.toString (solveExample2 ())

        part1 =
            "Part 1: " ++ Debug.toString (solvePart1 ())

        part2 =
            "Part 2: " ++ Debug.toString (solvePart2 ())
    in
    column
        []
        [ el [] (text ex1)
        , el [] (text ex2)
        , el [] (text part1)
        , el [] (text part2)
        ]
