open System
open SnakF.Snake

let pointGenerationStrategy (snakeHead: Position) (size: int * int) : Position =
    match snakeHead with
    | { x = 5; y = 5 } -> { x = 4; y = 5 }
    | _ -> { x = fst size - snakeHead.x; y = snd size - snakeHead.y }

let gameTick = gameTick pointGenerationStrategy

let rec gameLoop (gameState: GameState) =
    if gameState.gameOver then Environment.Exit 0
    Console.Clear ()
    Console.WriteLine(render gameState)
    Threading.Thread.Sleep 200
    let keyPress = Console.KeyAvailable
    if keyPress then
        match Console.ReadKey(intercept = true).Key with
        | ConsoleKey.UpArrow -> gameLoop (gameTick gameState Up)
        | ConsoleKey.DownArrow -> gameLoop (gameTick gameState Down)
        | ConsoleKey.LeftArrow -> gameLoop (gameTick gameState Left)
        | ConsoleKey.RightArrow -> gameLoop (gameTick gameState Right)
        | _ -> gameLoop (gameTick gameState gameState.snake.direction)
    else
        gameLoop (gameTick gameState gameState.snake.direction)

[<EntryPoint>]
let main argv =
    gameLoop {
        snake = createSnake { x = 5; y = 5 }
        pointPosition = { x = 3; y = 2 }
        score = 0
        gameOver = false
        gameSize = (10, 10)
    }
    Console.ReadLine () |> ignore
    0