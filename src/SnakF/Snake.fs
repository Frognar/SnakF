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

let getPositionChange direction =
    match direction with
    | Up -> { x = 0; y = -1 }
    | Down -> { x = 0; y = 1 }
    | Left -> { x = -1; y = 0 }
    | Right -> { x = 1; y = 0 }

let move (startingPosition: Position) (direction: Direction) : Position =
    let positionChange = getPositionChange direction

    { startingPosition with
        x = startingPosition.x + positionChange.x
        y = startingPosition.y + positionChange.y }

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

    let newHead = move snake.head validDirection
    let newTail = snake.head :: (snake.tail |> List.take (List.length snake.tail - 1))

    { snake with
        head = newHead
        direction = validDirection
        tail = newTail }

let exceedsWall (snake: Snake) (size: int * int) : bool =
    let width, height = size
    snake.head.x < 0 || snake.head.x >= width || snake.head.y < 0 || snake.head.y >= height

let hitItself (snake: Snake) : bool =
    snake.tail |> List.contains snake.head

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
        let grownSnake = { newSnake with tail = newSnake.tail @ [gameState.snake.tail |> List.last]}
        let hitItself = hitItself grownSnake
        { gameState with
            snake = grownSnake
            score = gameState.score + 1
            pointPosition = generateNewPoint newSnake.head gameState.gameSize
            gameOver = exceedsWall || hitItself }
    else
        let hitItself = hitItself newSnake
        { gameState with
            snake = newSnake
            gameOver = exceedsWall || hitItself }

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

    let renderedMap =
        [ 0..height - 1 ]
            |> List.map (fun y ->
            [ 0..width - 1 ]
            |> List.map (fun x -> renderPoint { x = x; y = y })
            |> String.concat "")
        |> String.concat "\n"
    renderedMap + $"\nScore {game.score}"