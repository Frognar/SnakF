module SnakeTests

open SnakF.Snake
open Xunit
open FsCheck

[<Fact>]
let ``after move should be in different position`` () =
    let rec moveToDifferentPosition (startingPosition: position) (direction: direction) =
        move startingPosition direction direction <> startingPosition

    Check.QuickThrowOnFailure moveToDifferentPosition

[<Fact>]
let ``snake cannot turn 180 degrees`` () =
    let rec cannotTurn180 (startingPosition: position) (direction: direction) =
        let newDirection = match direction with
                            | North -> South
                            | South -> North
                            | East -> West
                            | West -> East
        move startingPosition direction newDirection = move startingPosition direction direction

    Check.QuickThrowOnFailure cannotTurn180

[<Fact>]
let ``can end in to starting position when turn right 4 times`` () =
    let nextRight (direction: direction) =
        match direction with
        | North -> East
        | South -> West
        | East -> South
        | West -> North

    let canGoBack (startingPosition: position) (initialDirection: direction) =
        let rec loop i pos prevDir dir =
            if i = 4 then pos
            else loop (i + 1) (move pos prevDir dir) dir (nextRight dir)
        let endingPosition = loop 0 startingPosition initialDirection initialDirection
        endingPosition = startingPosition

    Check.QuickThrowOnFailure canGoBack

[<Fact>]
let ``can end in starting position when turn left 4 times`` () =
    let nextLeft (direction: direction) =
        match direction with
        | North -> West
        | South -> East
        | East -> North
        | West -> South

    let canGoBack (startingPosition: position) (initialDirection: direction) =
        let rec loop i pos prevDir dir =
            if i = 4 then pos
            else loop (i + 1) (move pos prevDir dir) dir (nextLeft dir)
        let endingPosition = loop 0 startingPosition initialDirection initialDirection
        endingPosition = startingPosition

    Check.QuickThrowOnFailure canGoBack