module Day8 exposing (view)

import Day8Input exposing (exampleInput, input)
import Element exposing (..)


parse : String -> List String
parse str =
    str
        |> String.trim
        |> String.lines
        |> List.map (\x -> String.trim x)



---- Part 1 ----


solveExample1 _ =
    0


solvePart1 _ =
    0



---- Part 2 ----


solveExample2 _ =
    0


solvePart2 _ =
    0



---- View ----


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
