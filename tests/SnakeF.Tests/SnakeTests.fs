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