module Day3 exposing (view)

import Day3Input exposing (exampleInput, input)
import Element exposing (..)


parse : String -> List String
parse lines =
    lines
        |> String.trim
        |> String.lines
        |> List.map (\x -> String.trim x)



---- Part 1 ----


solveExample1 =
    0


solvePart1 =
    0



---- Part 2 ----


solveExample2 =
    0


solvePart2 =
    0



---- View ----


view : Element msg
view =
    let
        ex1 =
            "Example 1: " ++ Debug.toString solveExample1

        ex2 =
            "Example 2: " ++ Debug.toString solveExample2

        part1 =
            "Part 1: " ++ Debug.toString solvePart1

        part2 =
            "Part 2: " ++ Debug.toString solvePart2
    in
    column
        []
        [ el [] (text ex1)
        , el [] (text ex2)
        , el [] (text part1)
        , el [] (text part2)
        ]
