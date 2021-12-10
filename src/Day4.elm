module Day4 exposing (view)

import Day4Input exposing (exampleInput, input)
import Element exposing (..)


type alias Board =
    { rows : List (List Int)
    , drawn : List Int
    }


parseBoard : String -> Board
parseBoard str =
    let
        parseRow : String -> List Int
        parseRow rowStr =
            rowStr
                |> String.trim
                |> String.words
                |> List.filterMap String.toInt

        rows =
            str
                |> String.trim
                |> String.lines
                |> List.map parseRow
    in
    { rows = rows
    , drawn = []
    }


getRows : Board -> List (List Int)
getRows board =
    board.rows


getColumns : Board -> List (List Int)
getColumns board =
    let
        r : List (List Int) -> List (List Int) -> List (List Int)
        r acc left =
            if List.any List.isEmpty left then
                acc

            else
                let
                    newCol =
                        left
                            |> List.map (\row -> List.head row |> Maybe.withDefault 0)

                    newLeft =
                        left
                            |> List.map (\row -> List.drop 1 row)
                in
                r (newCol :: acc) newLeft
    in
    r [] board.rows


boardRowsAndColumns : Board -> List (List Int)
boardRowsAndColumns board =
    List.append (getRows board) (getColumns board)


boardDraw : Int -> Board -> Board
boardDraw num board =
    { board | drawn = num :: board.drawn }


boardWin : Board -> Bool
boardWin board =
    let
        f : Int -> Bool -> Bool
        f n goodSoFar =
            if goodSoFar then
                List.member n board.drawn

            else
                False

        rowWin : List Int -> Bool -> Bool
        rowWin row won =
            if won then
                True

            else
                List.foldr f True row

        -- for row in rows
        -- check each row for a win
        -- if row wins, exit True
        -- else recursive call
    in
    List.foldr rowWin False (boardRowsAndColumns board)


score : Board -> Int
score board =
    let
        notCalled =
            List.concat board.rows
                |> List.filter (\x -> not (List.member x board.drawn))

        lastCalled =
            List.head board.drawn |> Maybe.withDefault 0
    in
    List.sum notCalled * lastCalled


parseNumbers : String -> List Int
parseNumbers str =
    str
        |> String.trim
        |> String.split ","
        |> List.map String.trim
        |> List.filterMap String.toInt


type alias State =
    { boards : List Board
    , toDraw : List Int
    }


draw : State -> ( State, Int )
draw state =
    case state.toDraw of
        [] ->
            Debug.todo "ran out of numbers to draw!"

        x :: xs ->
            ( { boards = state.boards |> List.map (boardDraw x)
              , toDraw = xs
              }
            , x
            )


parse : String -> State
parse str =
    let
        sections =
            str
                |> String.trim
                |> String.split "\n\n"

        numbers =
            Maybe.withDefault "0" (List.head sections)

        boardSections =
            Maybe.withDefault [] (List.tail sections)
    in
    { boards = boardSections |> List.map parseBoard
    , toDraw = parseNumbers numbers
    }


checkForWin : State -> Maybe Board
checkForWin state =
    let
        r : List Board -> Maybe Board
        r boards =
            case boards of
                [] ->
                    Nothing

                b :: bs ->
                    if boardWin b then
                        Just b

                    else
                        r bs
    in
    r state.boards



---- Part 1 ----


play : State -> Int
play state =
    let
        -- take a number
        -- apply it to boards
        ( newState, _ ) =
            draw state

        -- check if there is a winning board
        maybeWinning =
            checkForWin newState
    in
    case maybeWinning of
        -- if yes, exit with board score
        Just board ->
            score board

        -- if no, recursively call play
        Nothing ->
            play newState


solveExample1 _ =
    exampleInput |> parse |> play


solvePart1 _ =
    input |> parse |> play


type alias UnwonTracker =
    { unWonCount : Int
    , unWon : Maybe Board
    }


newTracker =
    { unWonCount = 0
    , unWon = Nothing
    }


trackerNextBoard : UnwonTracker -> Board -> UnwonTracker
trackerNextBoard tracker board =
    let
        won =
            boardWin board
    in
    if won then
        tracker

    else
        { unWonCount = tracker.unWonCount + 1
        , unWon = Just board
        }


trackerResolve : UnwonTracker -> Maybe Board
trackerResolve tracker =
    if tracker.unWonCount == 1 then
        tracker.unWon

    else
        Nothing



---- Part 2 ----


play2 : State -> Int
play2 state =
    let
        findLast : UnwonTracker -> List Board -> UnwonTracker
        findLast tracker boards =
            case boards of
                [] ->
                    tracker

                b :: bs ->
                    findLast (trackerNextBoard tracker b) bs

        -- loop advances the state, then checks
        loop : Maybe Board -> State -> Int
        loop maybeLast state_ =
            -- what is the exit state?
            -- there is only one unwon state left
            -- and... that board is bingo-ed
            let
                -- take a number and apply it to the boards
                ( newState, lastDrawn ) =
                    draw state_

                newMaybeLast =
                    case maybeLast of
                        Just board ->
                            -- need to do the draw
                            Just (boardDraw lastDrawn board)

                        Nothing ->
                            findLast newTracker newState.boards |> trackerResolve
            in
            case newMaybeLast of
                Just board ->
                    if boardWin board then
                        score board

                    else
                        loop (Just board) newState

                Nothing ->
                    loop Nothing newState
    in
    loop Nothing state


solveExample2 _ =
    exampleInput |> parse |> play2


solvePart2 _ =
    input |> parse |> play2



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
