﻿module SnakeTests

open SnakF.Snake
open Xunit
open FsCheck

[<Fact>]
let ``after move should be in different position`` () =
    let rec moveToDifferentPosition (startingPosition: position) (direction: direction) =
        move startingPosition direction <> startingPosition

    Check.QuickThrowOnFailure moveToDifferentPosition

[<Fact>]
let ``can end in to starting position when turn right 4 times`` () =
    let nextRight (direction: direction) =
        match direction with
        | North -> East
        | South -> West
        | East -> South
        | West -> North

    let canGoBack (startingPosition: position) (initialDirection: direction) =
        let rec loop i pos dir =
            if i = 4 then pos
            else loop (i + 1) (move pos dir) (nextRight dir)
        let endingPosition = loop 0 startingPosition initialDirection
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
        let rec loop i pos dir =
            if i = 4 then pos
            else loop (i + 1) (move pos dir) (nextLeft dir)
        let endingPosition = loop 0 startingPosition initialDirection
        endingPosition = startingPosition

    Check.QuickThrowOnFailure canGoBack

[<Fact>]
let ``move should always change only one coordinate`` () =
    let changeOnlyOne (startingPosition: position) (direction: direction) =
        let nextPosition = move startingPosition direction
        let x1, y1 = startingPosition
        let x2, y2 = nextPosition
        (x1 = x2 && y1 <> y2) || (y1 = y2 && x1 <> x2)

    Check.QuickThrowOnFailure changeOnlyOne

[<Fact>]
let ``move should change position by distance of 1`` () =
    let distanceOne (startingPosition: position) (direction: direction) =
        let nextPosition = move startingPosition direction
        let x1, y1 = startingPosition
        let x2, y2 = nextPosition
        let distance = abs (x1 - x2) + abs (y1 - y2)
        distance = 1

    Check.QuickThrowOnFailure distanceOne

[<Fact>]
let ``180 turns are invalid`` () =
    let invalidTurn (direction: direction) =
        let newDirection = match direction with
                            | North -> South
                            | South -> North
                            | East -> West
                            | West -> East
        isValidTurn direction newDirection = false

    Check.QuickThrowOnFailure invalidTurn

[<Fact>]
let ``90 turns are valid`` () =
    let validTurn (direction: direction) (left: bool) =
        let newDirection = match direction with
                            | North -> if left then West else East
                            | South -> if left then East else West
                            | East -> if left then North else South
                            | West -> if left then South else North
        isValidTurn direction newDirection

    Check.QuickThrowOnFailure validTurn