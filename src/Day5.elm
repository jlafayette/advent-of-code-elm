module Day5 exposing (view)

import Day5Input exposing (exampleInput, input)
import Dict exposing (Dict)
import Element exposing (..)
import Parser exposing ((|.), (|=), Parser)


type alias VentLine =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }


ventMinCoord : VentLine -> ( Int, Int )
ventMinCoord vent =
    ( min vent.x1 vent.x2, min vent.y1 vent.y2 )


ventMaxCoord : VentLine -> ( Int, Int )
ventMaxCoord vent =
    ( max vent.x1 vent.x2, max vent.y1 vent.y2 )


ventLineHV : VentLine -> Bool
ventLineHV vent =
    vent.x1 == vent.x2 || vent.y1 == vent.y2


ventRecord : VentLine -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
ventRecord vent dict =
    let
        ( minX, minY ) =
            ventMinCoord vent

        ( maxX, maxY ) =
            ventMaxCoord vent

        dictUpdate : Maybe Int -> Maybe Int
        dictUpdate existing =
            case existing of
                Nothing ->
                    Just 1

                Just n ->
                    Just (n + 1)

        foldf : ( Int, Int ) -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
        foldf ( x, y ) d =
            Dict.update ( x, y ) dictUpdate d
    in
    if minX == maxX then
        -- vertical
        List.range minY maxY
            |> List.map (\y -> ( minX, y ))
            |> List.foldr foldf dict

    else if minY == maxY then
        -- horizontal
        List.range minX maxX
            |> List.map (\x -> ( x, minY ))
            |> List.foldr foldf dict

    else
        -- diagonal
        dict


parse : String -> Result String (List VentLine)
parse str =
    Parser.run parser str |> Result.mapError simplifyParseError


parser : Parser (List VentLine)
parser =
    Parser.loop [] parserHelp


parserHelp : List VentLine -> Parser (Parser.Step (List VentLine) (List VentLine))
parserHelp revVents =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop (v :: revVents))
            |= lineParser
        , Parser.succeed ()
            |. Parser.end
            |> Parser.map (\v -> Parser.Done (List.reverse revVents))
        ]


lineParser : Parser VentLine
lineParser =
    Parser.succeed VentLine
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "-"
        |. Parser.symbol ">"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.spaces


simplifyParseError : List Parser.DeadEnd -> String
simplifyParseError errs =
    Debug.toString errs



---- Part 1 ----
-- solve : List VentLine -> Result String Int


solve vents =
    let
        dict : Dict ( Int, Int ) Int
        dict =
            Dict.empty
    in
    vents
        |> List.foldr ventRecord dict
        |> Dict.filter (\_ v -> v >= 2)
        |> Dict.size
        |> Ok



-- solveExample1 : Result String Int


solveExample1 =
    exampleInput |> parse |> Result.andThen solve


solvePart1 =
    input |> parse |> Result.andThen solve



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
