module SnakeTests

open SnakF.Snake
open Xunit

[<Fact>]
let ``after move should be in different position`` () =
    let startingPosition: position = (0, 0)
    let actualPosition = move startingPosition North
    Assert.NotEqual(startingPosition, actualPosition)
