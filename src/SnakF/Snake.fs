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

let move (startingPosition: position) (previousDirection: direction) (direction: direction) : position =
    let x, y = startingPosition
    match
        if is180Turn previousDirection direction
        then previousDirection
        else direction
    with
    | North -> x, y - 1
    | South -> x, y + 1
    | East -> x + 1, y
    | West -> x - 1, y
