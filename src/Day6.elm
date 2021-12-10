module Day6 exposing (view)

import Day6Input exposing (exampleInput, input)
import Element exposing (..)


parse : String -> List Int
parse str =
    str
        |> String.trim
        |> String.split ","
        |> List.filterMap String.toInt


lanternFishNewDay : Int -> List Int
lanternFishNewDay fish =
    case fish of
        0 ->
            [ 6, 8 ]

        _ ->
            [ fish - 1 ]



---- Part 1 ----


solve : Int -> List Int -> Int
solve days lanternFish =
    let
        f : Int -> List Int -> List Int
        f _ fish =
            -- return fish after one day
            fish
                |> List.map lanternFishNewDay
                |> List.concat
    in
    List.range 1 days
        |> List.foldl f lanternFish
        |> List.length


solveExample1 =
    exampleInput |> parse |> solve 80


solvePart1 =
    input |> parse |> solve 80



---- Part 2 ----
-- it takes too long to solve the list way above, instead
-- lets model the population as a dictionary with keys
-- of 0,1,2,3,4,5,6,7,8 and values that are the number
-- of fish that are that number
-- then we only have to shuffle around these dict values
-- instead of creating new lists all over the place


type alias Population =
    { p0 : Int
    , p1 : Int
    , p2 : Int
    , p3 : Int
    , p4 : Int
    , p5 : Int
    , p6 : Int
    , p7 : Int
    , p8 : Int
    }


newPop : List Int -> Population
newPop fishes =
    let
        foldf : Int -> Population -> Population
        foldf fish pop =
            case fish of
                0 ->
                    { pop | p0 = pop.p0 + 1 }

                1 ->
                    { pop | p1 = pop.p1 + 1 }

                2 ->
                    { pop | p2 = pop.p2 + 1 }

                3 ->
                    { pop | p3 = pop.p3 + 1 }

                4 ->
                    { pop | p4 = pop.p4 + 1 }

                5 ->
                    { pop | p5 = pop.p5 + 1 }

                6 ->
                    { pop | p6 = pop.p6 + 1 }

                7 ->
                    { pop | p7 = pop.p7 + 1 }

                8 ->
                    { pop | p8 = pop.p8 + 1 }

                _ ->
                    pop
    in
    fishes
        |> List.foldr foldf
            { p0 = 0
            , p1 = 0
            , p2 = 0
            , p3 = 0
            , p4 = 0
            , p5 = 0
            , p6 = 0
            , p7 = 0
            , p8 = 0
            }


newDay : Int -> Population -> Population
newDay _ pop =
    { p0 = pop.p1
    , p1 = pop.p2
    , p2 = pop.p3
    , p3 = pop.p4
    , p4 = pop.p5
    , p5 = pop.p6
    , p6 = pop.p7 + pop.p0
    , p7 = pop.p8
    , p8 = pop.p0
    }


popCount : Population -> Int
popCount pop =
    pop.p0 + pop.p1 + pop.p2 + pop.p3 + pop.p4 + pop.p5 + pop.p6 + pop.p7 + pop.p8


solve2 : Int -> List Int -> Int
solve2 days lanternFish =
    let
        initPop =
            newPop lanternFish
    in
    List.range 1 days
        |> List.foldl newDay initPop
        |> popCount


solveExample2 =
    exampleInput |> parse |> solve2 256


solvePart2 =
    input |> parse |> solve2 256



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
