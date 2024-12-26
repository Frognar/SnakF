module SnakF.Snake

type direction = North | South | East | West

type position = int * int

let is180Turn (previousDirection: direction) (direction: direction) =
    match previousDirection, direction with
    | North, South
    | South, North
    | East, West
    | West, East -> true
    | _ -> false

let isValidTurn (previousDirection: direction) (direction: direction) =
    not (is180Turn previousDirection direction)

let move (startingPosition: position) (direction: direction) : position =
    let x, y = startingPosition
    match direction with
    | North -> x, y - 1
    | South -> x, y + 1
    | East -> x + 1, y
    | West -> x - 1, y
