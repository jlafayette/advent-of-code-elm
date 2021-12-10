module Day1 exposing (view)

import Day1Input exposing (exampleInput, input)
import Element exposing (..)


parse : String -> List Int
parse input =
    input
        |> String.words
        |> List.map (\x -> String.toInt x)
        |> List.filterMap identity


countIncreases : Int -> Maybe Int -> List Int -> Int
countIncreases acc prev depths =
    case prev of
        Nothing ->
            case depths of
                [] ->
                    acc

                x :: xs ->
                    countIncreases acc (Just x) xs

        Just p ->
            case depths of
                [] ->
                    acc

                x :: xs ->
                    let
                        newAcc =
                            if x > p then
                                acc + 1

                            else
                                acc
                    in
                    countIncreases newAcc (Just x) xs


solveExample1 : () -> Int
solveExample1 _ =
    exampleInput
        |> parse
        |> countIncreases 0 Nothing


solvePart1 : () -> Int
solvePart1 _ =
    input
        |> parse
        |> countIncreases 0 Nothing


triples : List (List Int) -> List Int -> List Int -> List (List Int)
triples acc prev input =
    case prev of
        [] ->
            case input of
                [] ->
                    List.reverse acc

                x :: xs ->
                    triples acc [ x ] xs

        [ a ] ->
            case input of
                [] ->
                    List.reverse acc

                x :: xs ->
                    triples acc [ a, x ] xs

        [ a, b ] ->
            case input of
                [] ->
                    List.reverse acc

                x :: xs ->
                    triples ([ a, b, x ] :: acc) [ b, x ] xs

        _ ->
            List.reverse acc


solveExample2 : () -> Int
solveExample2 _ =
    exampleInput
        |> parse
        |> triples [] []
        |> List.map List.sum
        |> countIncreases 0 Nothing


solvePart2 : () -> Int
solvePart2 _ =
    input
        |> parse
        |> triples [] []
        |> List.map List.sum
        |> countIncreases 0 Nothing


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
