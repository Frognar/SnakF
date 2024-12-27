module SnakeTests

open FsCheck.Xunit
open SnakF.Snake

let (-) (p1: position) (p2: position) = { x = p1.x - p2.x; y = p1.y - p2.y }
let abs (p: position) = { x = abs p.x; y = abs p.y }

let distanceFrom0 (p: position) : int =
    let absPosition = abs p
    absPosition.x + absPosition.y

let turnLeft (direction: direction) =
    match direction with
    | North -> West
    | South -> East
    | East -> North
    | West -> South

let turnRight (direction: direction) =
    match direction with
    | North -> East
    | South -> West
    | East -> South
    | West -> North

let turn180 (direction: direction) = direction |> turnLeft |> turnLeft

[<Property>]
let ``after move should be in different position`` (startingPosition: position) (direction: direction) =
    move startingPosition direction <> startingPosition

[<Property>]
let ``can end in starting position when turn right 4 times`` (startingPosition: position) (initialDirection: direction) =
    let rec loop i pos dir =
        match i with
        | 4 -> pos
        | _ -> loop (i + 1) (move pos dir) (turnRight dir)

    let endingPosition = loop 0 startingPosition initialDirection
    endingPosition = startingPosition

[<Property>]
let ``can end in starting position when turn left 4 times`` (startingPosition: position) (initialDirection: direction) =
    let rec loop i pos dir =
        match i with
        | 4 -> pos
        | _ -> loop (i + 1) (move pos dir) (turnLeft dir)

    let endingPosition = loop 0 startingPosition initialDirection
    endingPosition = startingPosition

[<Property>]
let ``move should always change only one coordinate`` (startingPosition: position) (direction: direction) =
    let nextPosition = move startingPosition direction
    let diff = startingPosition - nextPosition |> abs
    diff = { x = 1; y = 0 } || diff = { x = 0; y = 1 }

[<Property>]
let ``move should change position by distance of 1`` (startingPosition: position) (direction: direction) =
    let nextPosition = move startingPosition direction
    startingPosition - nextPosition |> distanceFrom0 = 1

[<Property>]
let ``180 turns are invalid`` (direction: direction) =
    let newDirection = turn180 direction
    isValidTurn direction newDirection = false

[<Property>]
let ``90 turns are valid`` (direction: direction) (left: bool) =
    let newDirection = (if left then turnLeft else turnRight) direction
    isValidTurn direction newDirection

[<Property>]
let ``snake should start in given position`` (startingPosition: position) =
    let snake = createSnake startingPosition
    snake.head = startingPosition

[<Property>]
let ``snake should start with length 3`` (startingPosition: position) =
    let snake = createSnake startingPosition
    snake.tail |> List.length = 2 // + head
