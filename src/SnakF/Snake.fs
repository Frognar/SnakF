module SnakF.Snake

type direction = North | South | East | West

type position = int * int

let move (startingPosition: position) (direction: direction) : position =
    let x, y = startingPosition
    (x, y + 1)
