module Day8 exposing (view)

import Day8Input exposing (exampleInput, input)
import Dict exposing (Dict)
import Element exposing (..)
import Parser exposing ((|.), (|=), Parser)


type alias Entry =
    { signalPatterns : List String
    , outputValues : List String
    }


parser : Parser (List Entry)
parser =
    Parser.loop [] parserHelp


parserHelp : List Entry -> Parser (Parser.Step (List Entry) (List Entry))
parserHelp revEntries =
    Parser.oneOf
        [ Parser.succeed (\e -> Parser.Loop (e :: revEntries))
            |= entryParser
        , Parser.succeed ()
            |. Parser.end
            |> Parser.map (\e -> Parser.Done (List.reverse revEntries))
        ]


entryParser : Parser Entry
entryParser =
    Parser.succeed Entry
        |. Parser.spaces
        |= wordParser 10
        |. Parser.spaces
        |. Parser.symbol "|"
        |. Parser.spaces
        |= wordParser 4
        |. Parser.spaces


wordParser : Int -> Parser (List String)
wordParser count =
    Parser.loop [] (wordParserHelp count)


wordParserHelp : Int -> List String -> Parser (Parser.Step (List String) (List String))
wordParserHelp count revWords =
    let
        f w =
            if List.length revWords < (count - 1) then
                Parser.Loop (w :: revWords)

            else
                Parser.Done (List.reverse (w :: revWords))
    in
    Parser.succeed f
        |. Parser.spaces
        |= parseWord
        |. Parser.spaces


parseWord : Parser String
parseWord =
    Parser.getChompedString <|
        Parser.succeed identity
            |. Parser.chompIf (\c -> Char.isLower c)
            |. Parser.chompWhile (\c -> Char.isLower c)



-- parse : String -> List String


parse str =
    Parser.run parser str



---- Part 1 ----


isUnique : String -> Bool
isUnique outputValue =
    case String.length outputValue of
        2 ->
            True

        3 ->
            True

        4 ->
            True

        7 ->
            True

        _ ->
            False


countUnique : List String -> Int
countUnique outputValues =
    outputValues
        |> List.filter isUnique
        |> List.length



-- solve : List Entry -> Result (List Parser.DeadEnd) Int


solve entries =
    entries
        |> List.map (\e -> countUnique e.outputValues)
        |> List.sum
        |> Ok


solveExample1 _ =
    exampleInput |> parse |> Result.andThen solve


solvePart1 _ =
    input |> parse |> Result.andThen solve



---- Part 2 ----


exampleInput21 =
    """
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
"""


{-| dddd
e a
e a
ffff
g b
g b
cccc

acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1

dddd is in 7(3) but not in 1(2)

b is in 1(2) and in all three of 9,6,0(6)
a is in 1(2) but only in two of 9,6,0(6)

e is in 4(4) and also all three of 9,6,0(6) (and not in 1(2))
f is in 4(4) but only in two of 9,6,0(6) (and not in 1(2)) (and is in 8(7))

g is in 2 of 9,6,0(6) but not in 1(2) and not in 4(4)

cccc is the other one

-}
type L
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Signal
    = Top
    | TopL
    | TopR
    | Mid
    | BotL
    | BotR
    | Bot


type alias Patterns =
    { p1_2 : List L
    , p7_3 : List L
    , p4_4 : List L
    , p8_7 : List L
    , p235_5 : List (List L)
    , p690_6 : List (List L)
    }


type Pattern
    = P1 String
    | P7 String
    | P4 String
    | P235 String
    | P690 String
    | P8 String


classify : List String -> List Pattern
classify values =
    let
        classify_ v =
            case String.length v of
                2 ->
                    Just <| P1 v

                7 ->
                    Just <| P8 v

                3 ->
                    Just <| P7 v

                4 ->
                    Just <| P4 v

                5 ->
                    Just <| P235 v

                6 ->
                    Just <| P690 v

                _ ->
                    Nothing
    in
    List.filterMap classify_ values


solveLookup : List String -> Dict String Int
solveLookup values =
    Dict.empty


type alias SolvedEntry =
    { signalPatterns : List String
    , lookup : Dict String Int
    , outputValues : List String
    }


solveEntry : Entry -> SolvedEntry
solveEntry entry =
    { signalPatterns = entry.signalPatterns
    , lookup = solveLookup entry.signalPatterns
    , outputValues = entry.outputValues
    }


toNumber : SolvedEntry -> Int
toNumber entry =
    let
        get k =
            case Dict.get k entry.lookup of
                Nothing ->
                    0

                -- Debug.todo "bad"
                Just n ->
                    n

        combine : Int -> List Int -> Int
        combine acc remaining =
            case remaining of
                [] ->
                    acc

                x :: xs ->
                    let
                        newAcc =
                            x * (10 ^ List.length xs)
                    in
                    combine newAcc xs
    in
    entry.outputValues
        |> List.map get
        |> combine 0


solve2 entries =
    entries
        |> List.map solveEntry
        |> List.map toNumber
        |> List.sum
        |> Ok


solveExample21 _ =
    exampleInput21 |> parse |> Result.andThen solve2


solveExample22 _ =
    0


solvePart2 _ =
    0



---- View ----


view : () -> Element msg
view _ =
    let
        ex1 =
            "Example 1: " ++ Debug.toString (solveExample1 ())

        ex21 =
            "Example 2-1: " ++ Debug.toString (solveExample21 ())

        ex22 =
            "Example 2-2: " ++ Debug.toString (solveExample22 ())

        part1 =
            "Part 1: " ++ Debug.toString (solvePart1 ())

        part2 =
            "Part 2: " ++ Debug.toString (solvePart2 ())
    in
    column
        []
        [ el [] (text ex1)
        , el [] (text ex21)
        , el [] (text ex22)
        , el [] (text part1)
        , el [] (text part2)
        ]
