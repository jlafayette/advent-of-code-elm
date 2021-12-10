module Day7 exposing (view)

import Day7Input exposing (exampleInput, input)
import Element exposing (..)


parse : String -> List Int
parse str =
    str
        |> String.trim
        |> String.split ","
        |> List.filterMap String.toInt



---- Part 1 ----


fuelCost : Int -> Int -> Int
fuelCost p1 p2 =
    abs (p1 - p2)


solve : List Int -> Int
solve positions =
    let
        -- avg = Debug.log "avg" (List.sum positions // List.length positions)
        min =
            List.minimum positions |> Maybe.withDefault 9999999999999999

        max =
            List.maximum positions |> Maybe.withDefault -9999999999999999

        score : List Int -> Int -> Int
        score ps p =
            ps
                |> List.map (\x -> fuelCost x p)
                |> List.sum
    in
    List.range min max
        |> List.map (\x -> score positions x)
        |> List.minimum
        |> Maybe.withDefault 0


solveExample1 _ =
    exampleInput |> parse |> solve


solvePart1 _ =
    input |> parse |> solve



---- Part 2 ----


fuelCost2 : Int -> Int -> Int
fuelCost2 p1 p2 =
    let
        d =
            abs (p1 - p2)

        foldf : Int -> Int -> Int
        foldf n acc =
            acc + n
    in
    List.range 1 d
        |> List.foldl foldf 0


solve2 : List Int -> Int
solve2 positions =
    let
        lowestInt =
            -9999999999999999

        highestInt =
            9999999999999999

        min_ =
            List.minimum positions |> Maybe.withDefault highestInt

        max_ =
            List.maximum positions |> Maybe.withDefault lowestInt

        score : List Int -> Int -> Int
        score ps p =
            let
                result =
                    ps |> List.map (\x -> fuelCost2 x p) |> List.sum
            in
            Debug.log ("score" ++ String.fromInt p) result

        bisect : List Int -> List Int
        bisect remaining =
            let
                len =
                    List.length remaining

                loHalf =
                    List.take (len // 2) remaining

                hiHalf =
                    List.drop (len // 2) remaining

                maybeLo =
                    loHalf |> List.reverse |> List.head

                maybeHi =
                    hiHalf |> List.head
            in
            case maybeLo of
                Nothing ->
                    hiHalf

                Just lo ->
                    case maybeHi of
                        Nothing ->
                            loHalf

                        Just hi ->
                            let
                                hiScore =
                                    score positions hi

                                loScore =
                                    score positions lo
                            in
                            if hiScore < loScore then
                                hiHalf

                            else
                                loHalf

        helper best toCheck =
            case toCheck of
                [] ->
                    best

                [ x ] ->
                    min best (score positions x)

                _ ->
                    helper best (bisect toCheck)
    in
    helper highestInt (List.range min_ max_)


solveExample2 _ =
    exampleInput |> parse |> solve2


solvePart2 _ =
    input |> parse |> solve2



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
