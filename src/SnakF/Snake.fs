module SnakF.Snake

type Direction =
    | North
    | South
    | East
    | West

type Position = { x: int; y: int }

type Snake =
    { head: Position
      tail: Position list
      direction: Direction }

type GameState =
    { snake: Snake
      score: int
      pointPosition: Position
      gameSize: int * int }

let is180Turn (previousDirection: Direction) (direction: Direction) =
    match previousDirection, direction with
    | North, South
    | South, North
    | East, West
    | West, East -> true
    | _ -> false

let isValidTurn (previousDirection: Direction) (direction: Direction) =
    not (is180Turn previousDirection direction)

let move (startingPosition: Position) (direction: Direction) : Position =
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

let createSnake (startingPosition: Position) : Snake =
    { head = startingPosition
      tail =
        [ 1..2 ]
        |> List.map (fun i ->
            { startingPosition with
                y = startingPosition.y + i })
      direction = North }

let moveSnake (snake: Snake) (direction: Direction) : Snake =
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

let growSnake (snake: Snake) : Snake =
    { snake with tail = snake.tail @ [ snake.head ] }

type PointGenerationStrategy = Position -> int * int -> Position
let gameTick
    (generateNewPoint: PointGenerationStrategy)
    (gameState: GameState)
    (direction: Direction)
    : GameState =

    let newSnake = moveSnake gameState.snake direction
    let shouldScore = newSnake.head = gameState.pointPosition

    if shouldScore then
        { gameState with
            snake = growSnake newSnake
            score = gameState.score + 1
            pointPosition = generateNewPoint newSnake.head gameState.gameSize }
    else
        { gameState with snake = newSnake }

let render (game: GameState) =
    let width, height = game.gameSize

    let renderPoint (point: Position) =
        if point = game.pointPosition then
            "#"
        elif point = game.snake.head then
            "S"
        elif game.snake.tail |> List.contains point then
            "x"
        else
            "."

    [ 0..height - 1 ]
    |> List.map (fun y ->
        [ 0..width - 1 ]
        |> List.map (fun x -> renderPoint { x = x; y = y })
        |> String.concat "")
    |> String.concat "\n"