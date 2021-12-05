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


{-| Represents a single bit that can be one or zero.

If one, then the value stored depends on the place of the bit in the
binary number (1, 2, 4, 8...)

-}
type Bit
    = Zero
    | One Int


type Strategy
    = MostCommon
    | LeastCommon


createPlaces : String -> List Int
createPlaces line =
    let
        x =
            2 ^ (String.length line - 1)

        f : List Int -> Int -> List Int
        f acc n =
            case n of
                0 ->
                    List.reverse acc

                _ ->
                    f (n :: acc) (n // 2)
    in
    f [] x


type alias BitCounter =
    { zero : Int
    , one : Int
    }


getBit : Strategy -> List String -> Int -> ( Bit, List String )
getBit strategy lines place =
    let
        foldf : String -> BitCounter -> BitCounter
        foldf line counter =
            case String.right 1 line of
                "0" ->
                    { zero = counter.zero + 1, one = counter.one }

                "1" ->
                    { zero = counter.zero, one = counter.one + 1 }

                _ ->
                    counter

        mc : Int -> BitCounter -> Bit
        mc p counter =
            case strategy of
                MostCommon ->
                    if counter.one >= counter.zero then
                        One p

                    else
                        Zero

                LeastCommon ->
                    if counter.one < counter.zero then
                        One p

                    else
                        Zero

        commonBit =
            lines
                |> List.foldl foldf { zero = 0, one = 0 }
                |> mc place

        newLines =
            lines |> List.map (\line -> String.dropRight 1 line)
    in
    ( commonBit, newLines )


addBits : List Bit -> Int
addBits bits =
    let
        f : Bit -> Int -> Int
        f bit n =
            case bit of
                Zero ->
                    n

                One place ->
                    n + place
    in
    List.foldl f 0 bits


compute : Strategy -> List String -> Int
compute strategy lines =
    let
        line =
            Maybe.withDefault "0" (List.head lines)

        recursive : List Bit -> List String -> Int -> List Bit
        recursive acc lines_ place =
            let
                lineLen =
                    Maybe.withDefault "" (List.head lines_) |> String.length
            in
            if lineLen == 0 then
                List.reverse acc

            else
                let
                    newPlace =
                        place * 2

                    -- TODO: pass in strategy here
                    ( bit, newLines ) =
                        getBit strategy lines_ place
                in
                recursive (bit :: acc) newLines newPlace
    in
    recursive [] lines 1 |> addBits


solveExample1 =
    let
        parsedInput =
            exampleInput |> parse

        gamma =
            compute MostCommon parsedInput

        epsilon =
            compute LeastCommon parsedInput
    in
    gamma * epsilon


solvePart1 =
    let
        parsedInput =
            input |> parse

        gamma =
            compute MostCommon parsedInput

        epsilon =
            compute LeastCommon parsedInput
    in
    gamma * epsilon



---- Part 2 ----


solveExample2 =
    0


solvePart2 =
    0


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
