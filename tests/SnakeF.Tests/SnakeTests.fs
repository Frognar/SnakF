module SnakeTests

open SnakF.Snake
open Xunit
open FsCheck

[<Fact>]
let ``after move should be in different position`` () =
    let rec moveToDifferentPosition (startingPosition: position) (direction: direction) =
        move startingPosition direction <> startingPosition
    Check.QuickThrowOnFailure moveToDifferentPosition
