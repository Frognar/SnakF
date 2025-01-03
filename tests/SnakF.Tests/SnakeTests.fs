﻿namespace SnakeTests

open FsCheck.Xunit
open FsCheck
open SnakF.Snake

module private Helpers =
    let (-) (p1: Position) (p2: Position) = { x = p1.x - p2.x; y = p1.y - p2.y }
    let abs (p: Position) = { x = abs p.x; y = abs p.y }

    let distanceFrom0 (p: Position) : int =
        let absPosition = abs p
        absPosition.x + absPosition.y

    let turnLeft (direction: Direction) =
        match direction with
        | North -> West
        | South -> East
        | East -> North
        | West -> South

    let turnRight (direction: Direction) =
        match direction with
        | North -> East
        | South -> West
        | East -> South
        | West -> North

    let turn180 (direction: Direction) = direction |> turnLeft |> turnLeft

type CustomGenerator =
    static member Position() : Arbitrary<Position> =
        gen {
            let! x = Gen.choose (0, 10)
            let! y = Gen.choose (0, 10)
            return { x = x; y = y }
        }
        |> Arb.fromGen

    static member Snake() : Arbitrary<Snake> =
        gen {
            let! position = CustomGenerator.Position() |> Arb.toGen
            let! direction = Gen.elements [ North; South; East; West ]
            let! size = Gen.choose (2, 5)

            return
                { head = position
                  tail = [ 1..size ] |> List.map (fun i -> { position with y = position.y - i })
                  direction = direction }
        }
        |> Arb.fromGen

[<Properties(Arbitrary = [| typeof<CustomGenerator> |])>]
do ()

module MoveTests =
    open Helpers

    [<Property>]
    let ``after move should be in different position`` (startingPosition: Position) (direction: Direction) =
        move startingPosition direction <> startingPosition

    [<Property>]
    let ``can end in starting position when turn right 4 times``
        (startingPosition: Position)
        (initialDirection: Direction)
        =
        let rec loop i pos dir =
            match i with
            | 4 -> pos
            | _ -> loop (i + 1) (move pos dir) (turnRight dir)

        let endingPosition = loop 0 startingPosition initialDirection
        endingPosition = startingPosition

    [<Property>]
    let ``can end in starting position when turn left 4 times``
        (startingPosition: Position)
        (initialDirection: Direction)
        =
        let rec loop i pos dir =
            match i with
            | 4 -> pos
            | _ -> loop (i + 1) (move pos dir) (turnLeft dir)

        let endingPosition = loop 0 startingPosition initialDirection
        endingPosition = startingPosition

    [<Property>]
    let ``move should always change only one coordinate`` (startingPosition: Position) (direction: Direction) =
        let nextPosition = move startingPosition direction
        let diff = startingPosition - nextPosition |> abs
        diff = { x = 1; y = 0 } || diff = { x = 0; y = 1 }

    [<Property>]
    let ``move should change position by distance of 1`` (startingPosition: Position) (direction: Direction) =
        let nextPosition = move startingPosition direction
        startingPosition - nextPosition |> distanceFrom0 = 1


module TurnTests =
    open Helpers

    [<Property>]
    let ``180 turns are invalid`` (direction: Direction) =
        let newDirection = turn180 direction
        isValidTurn direction newDirection = false

    [<Property>]
    let ``90 turns are valid`` (direction: Direction) (left: bool) =
        let newDirection = (if left then turnLeft else turnRight) direction
        isValidTurn direction newDirection

module SnakeTests =
    open Helpers

    [<Property>]
    let ``snake should start in given position`` (startingPosition: Position) =
        let snake = createSnake startingPosition
        snake.head = startingPosition

    [<Property>]
    let ``snake should start with length 3`` (startingPosition: Position) =
        let snake = createSnake startingPosition
        snake.tail |> List.length = 2 // + head

    [<Property>]
    let ``snake after move should have same length`` (snake: Snake) (direction: Direction) =
        let newSnake = moveSnake snake direction
        newSnake.tail.Length = snake.tail.Length

    [<Property>]
    let ``snake after move should be in different position`` (snake: Snake) (direction: Direction) =
        let newSnake = moveSnake snake direction
        newSnake.head <> snake.head

    [<Property>]
    let ``snake can't turn 180`` (snake: Snake) =
        let direction = turn180 snake.direction
        let newSnake = moveSnake snake direction
        newSnake.direction = snake.direction

    [<Property>]
    let ``snake can turn 90`` (snake: Snake) (left: bool) =
        let direction = (if left then turnLeft else turnRight) snake.direction
        let newSnake = moveSnake snake direction
        newSnake.direction <> snake.direction && newSnake.direction = direction

    [<Property>]
    let ``snake tail should follow head`` (snake: Snake) (direction: Direction) =
        let newSnake = moveSnake snake direction
        newSnake.tail.Head = snake.head

    [<Property>]
    let ``snake segments should follow each other`` (snake: Snake) (direction: Direction) =
        let newSnake = moveSnake snake direction

        newSnake.tail
        |> List.windowed 2
        |> List.forall (fun pair -> pair[0] - pair[1] |> distanceFrom0 = 1)

module GameTests =
    let pointGenerationStrategy (snakeHead: Position) (size: int * int) : Position =
        match snakeHead with
        | { x = 5; y = 5 } -> { x = 4; y = 5 }
        | _ -> { x = fst size - snakeHead.x; y = snd size - snakeHead.y }

    let gameTick (gameState: GameState) (direction: Direction) =
        gameTick pointGenerationStrategy gameState direction
    
    [<Property>]
    let ``game tick should move snake`` (snake: Snake) (direction: Direction) =
        let game =
            { snake = snake
              score = 0
              pointPosition = { x = 4; y = 5 }
              gameSize = (10, 10)
              gameOver = false }

        let newGame = gameTick game direction
        newGame.snake <> game.snake

    let getPointNextToSnake (snake: Snake) (direction: Direction) : Position =
        (moveSnake snake direction).head

    [<Property>]
    let ``game tick should increase score if snake is in point`` (snake: Snake) (direction: Direction) =
        let pointPosition = getPointNextToSnake snake direction

        let game =
            { snake = snake
              score = 0
              pointPosition = pointPosition
              gameSize = (10, 10)
              gameOver = false }

        let newGame = gameTick game direction
        newGame.score = 1

    [<Property>]
    let ``snake should increase in length if snake is in point`` (snake: Snake) (direction: Direction) =
        let pointPosition = getPointNextToSnake snake direction

        let game =
            { snake = snake
              score = 0
              pointPosition = pointPosition
              gameSize = (10, 10)
              gameOver = false }

        let newGame = gameTick game direction
        List.length newGame.snake.tail > List.length snake.tail

    [<Property>]
    let ``new point should be generated if snake is in point`` (snake: Snake) (direction: Direction) =
        let pointPosition = getPointNextToSnake snake direction

        let game =
            { snake = snake
              score = 0
              pointPosition = pointPosition
              gameSize = (10, 10)
              gameOver = false }

        let newGame = gameTick game direction
        newGame.pointPosition <> pointPosition
