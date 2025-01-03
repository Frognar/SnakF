module SnakF.Snake

type Direction =
    | Up
    | Down
    | Left
    | Right

type Position = { x: int; y: int }

type Snake =
    { head: Position
      tail: Position list
      direction: Direction }

type GameState =
    { snake: Snake
      score: int
      pointPosition: Position
      gameSize: int * int
      gameOver: bool }

let is180Turn (previousDirection: Direction) (direction: Direction) =
    match previousDirection, direction with
    | Up, Down
    | Down, Up
    | Left, Right
    | Right, Left -> true
    | _ -> false

let isValidTurn (previousDirection: Direction) (direction: Direction) =
    not (is180Turn previousDirection direction)

let move (startingPosition: Position) (direction: Direction) : Position =
    match direction with
    | Up ->
        { startingPosition with
            y = startingPosition.y - 1 }
    | Down ->
        { startingPosition with
            y = startingPosition.y + 1 }
    | Left ->
        { startingPosition with
            x = startingPosition.x - 1 }
    | Right ->
        { startingPosition with
            x = startingPosition.x + 1 }

let createSnake (startingPosition: Position) : Snake =
    { head = startingPosition
      tail =
        [ 1..2 ]
        |> List.map (fun i ->
            { startingPosition with
                y = startingPosition.y + i })
      direction = Up }

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

let exceedsWall (snake: Snake) (size: int * int) : bool =
    let width, height = size
    snake.head.x < 0 || snake.head.x >= width || snake.head.y < 0 || snake.head.y >= height

type PointGenerationStrategy = Position -> int * int -> Position
let gameTick
    (generateNewPoint: PointGenerationStrategy)
    (gameState: GameState)
    (direction: Direction)
    : GameState =

    let newSnake = moveSnake gameState.snake direction
    let shouldScore = newSnake.head = gameState.pointPosition
    let exceedsWall = exceedsWall newSnake gameState.gameSize

    if shouldScore then
        { gameState with
            snake = growSnake newSnake
            score = gameState.score + 1
            pointPosition = generateNewPoint newSnake.head gameState.gameSize
            gameOver = exceedsWall }
    else
        { gameState with
            snake = newSnake
            gameOver = exceedsWall }

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