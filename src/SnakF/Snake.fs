module SnakF.Snake

type direction =
    | North
    | South
    | East
    | West

type position = { x: int; y: int }

type Snake =
    { head: position
      tail: position list
      direction: direction }

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
    match direction with
    | North ->
        { startingPosition with
            y = startingPosition.y - 1 }
    | South ->
        { startingPosition with
            y = startingPosition.y + 1 }
    | East ->
        { startingPosition with
            x = startingPosition.x - 1 }
    | West ->
        { startingPosition with
            x = startingPosition.x + 1 }

let createSnake (startingPosition: position) : Snake =
    { head = startingPosition
      tail =
        [ 1..2 ]
        |> List.map (fun i ->
            { startingPosition with
                y = startingPosition.y + i })
      direction = North }

let moveSnake (snake: Snake) (direction: direction) : Snake =
    let validDirection =
        if isValidTurn snake.direction direction then
            direction
        else
            snake.direction

    let newHead = move snake.head direction
    let newTail = snake.head :: (snake.tail |> List.take (List.length snake.tail - 1))

    { snake with
        head = newHead
        direction = validDirection
        tail = newTail }
