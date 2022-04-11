module App

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop

module List =

    let foldi folder state list =
        list
        |> List.fold (fun (prevState, i) elem -> (folder i prevState elem, i + 1)) (state, 0)
        |> fst

module TetrisGame =

    type Grid = int list list
    type Rotation = Clockwise | CounterClockwise

    module Rotation =

        let opposite dir =
            match dir with
            | Clockwise -> CounterClockwise
            | CounterClockwise -> Clockwise

    module Grid =
        let iter f grid =
            grid |> List.iteri (fun j row ->
                row |> List.iteri (fun i cell ->
                    f i j cell))

        let map f grid =
            grid |> List.mapi (fun j row ->
                row |> List.mapi (fun i cell ->
                    f i j cell))

        let exists f grid =
            grid |> List.foldi (fun j gridAcc row ->
                row |> List.foldi (fun i rowAcc cell ->
                    if not gridAcc && not rowAcc then
                        f i j cell
                    else
                        true) false) false

        let create (w, h) =
            [ for i in 1 .. h ->
                [ for j in 1 .. w ->
                    0 ] ]

        let width (grid: Grid) =
            let row = grid.[0]
            row |> List.length

        let height (grid: Grid) =
            grid |> List.length

        let rotate dir grid =
            let w = grid |> width
            let h = grid |> height
            let grid =
                [ for i in 0 .. w - 1 ->
                    [ for j in 0 .. h - 1 ->
                        grid.[j].[i] ] ]
            match dir with
            | Clockwise -> grid |> List.map List.rev
            | CounterClockwise -> grid |> List.rev

    module Piece =

        let I =
            [ [ 0; 1; 0; 0 ]
              [ 0; 1; 0; 0 ]
              [ 0; 1; 0; 0 ]
              [ 0; 1; 0; 0 ] ]

        let L =
            [ [ 0; 2; 0 ]
              [ 0; 2; 0 ]
              [ 0; 2; 2 ] ]

        let J =
            [ [ 0; 3; 0 ]
              [ 0; 3; 0 ]
              [ 3; 3; 0 ] ]

        let O =
            [ [ 4; 4 ]
              [ 4; 4 ] ]

        let Z =
            [ [ 5; 5; 0 ]
              [ 0; 5; 5 ]
              [ 0; 0; 0 ] ]

        let S =
            [ [ 0; 6; 6 ]
              [ 6; 6; 0 ]
              [ 0; 0; 0 ] ]

        let T =
            [ [ 0; 7; 0 ]
              [ 7; 7; 7 ]
              [ 0; 0; 0 ] ]

        let random =
            let rnd = Random()
            let choices = [I; L; J; O; Z; S; T]
            fun () ->
                choices |> List.item (rnd.Next 7)

    type Player =
        { x: int
          y: int
          piece: Grid }

    module Player =

        let reset board =
            let piece = Piece.random()
            let boardW = board |> Grid.width
            let pieceW = piece |> Grid.width
            let x = floor(float boardW / 2.) - floor(float pieceW / 2.)
            { piece = piece; x = int x; y = 0 }

    let merge board player =
        let w = player.piece |> Grid.width
        let h = player.piece |> Grid.height
        board |> Grid.map (fun i j cell ->
            let y = j - player.y
            let x = i - player.x
            if x >= 0 && x < w && y >= 0 && y < h && player.piece.[y].[x] <> 0 then
                player.piece.[y].[x]
            else
                cell)

    let collide board player =
        let w = board |> Grid.width
        let h = board |> Grid.height
        player.piece |> Grid.exists (fun i j cell ->
            if cell <> 0 then
                let x = i + player.x
                let y = j + player.y
                if x < 0 || x >= w || y < 0 || y >= h then
                    true
                else
                    board.[y].[x] <> 0
            else
                false)

    let drop board player =
        let player = { player with y = player.y + 1 }
        if collide board player then
            let player = { player with y = player.y - 1 }
            let board = merge board player
            let player = Player.reset board
            board, player
        else
            board, player

    let moveLeft board player =
        let player = { player with x = player.x - 1 }
        if collide board player then
            { player with x = player.x + 1 }
        else
            player

    let moveRight board player =
        let player = { player with x = player.x + 1 }
        if collide board player then
            { player with x = player.x - 1 }
        else
            player

    let rotate board player dir =
        let x = player.x
        let rec aux player offset =
            if collide board player then
                let player = { player with x = player.x + offset }
                let offset = -(offset + (if offset > 0 then 1 else -1))
                if offset > (player.piece |> Grid.width) then
                    { player with
                        piece = player.piece |> Grid.rotate (Rotation.opposite dir)
                        x = x }
                else
                    aux player offset
            else
                player
        let player = { player with piece = player.piece |> Grid.rotate dir }
        aux player 1

    let sweep board =
        let w = board |> Grid.width
        let h = board |> Grid.height
        let shrinkedBoard =
            board |> List.filter (List.exists ((=) 0))
        let missingLineCount = h - (shrinkedBoard |> Grid.height)
        let newEmptyLines =
            [ for _ in 1 .. missingLineCount ->
                List.replicate w 0 ]
        newEmptyLines @ shrinkedBoard

module Key =

    let mutable keysPressed = Set.empty<string>

    let keyHit x =
        if keysPressed.Contains x then
            keysPressed <- Set.remove x keysPressed
            true
        else
            false

    let update (e: KeyboardEvent, pressed) =
        let op =
            if pressed then
                Set.add
            else
                Set.remove
        keysPressed <- op e.key keysPressed

    let left() = keyHit "ArrowLeft"
    let right() = keyHit "ArrowRight"
    let down() = keyHit "ArrowDown"
    let q() = keyHit "q"
    let w() = keyHit "w"

    let init() =
        document.addEventListener("keydown", fun e -> update(!!e, true))
        document.addEventListener("keyup", fun e -> update(!!e, false))

module View =

    open TetrisGame

    let canvas: HTMLCanvasElement = !! document.getElementById "canvas"
    let ctx = canvas.getContext_2d()
    ctx.scale (20., 20.)

    let drawGrid grid x y =
        grid |> Grid.iter (fun i j cell ->
            if cell <> 0 then
                match cell with
                | 1 -> ctx.fillStyle <- !^"#FF0D72"
                | 2 -> ctx.fillStyle <- !^"#0DC2FF"
                | 3 -> ctx.fillStyle <- !^"#0DFF72"
                | 4 -> ctx.fillStyle <- !^"#F538FF"
                | 5 -> ctx.fillStyle <- !^"#FF8E0D"
                | 6 -> ctx.fillStyle <- !^"#FFE138"
                | 7 -> ctx.fillStyle <- !^"#3877FF"
                | _ -> ()

                ctx.fillRect (float (i + x), float (j + y), 1., 1.))

    let clear() =
        ctx.fillStyle <- !^"#000"
        ctx.fillRect (0., 0., canvas.width, canvas.height)

    let rec update dropCounter dropInterval board player lastTime time =
        clear()

        let deltaTime = time - lastTime

        let dropCounter, board, player =
            let counter = dropCounter + deltaTime
            if counter > dropInterval then
                let board, player = drop board player
                0., board, player
            else
                counter, board, player

        let dropCounter, board, player =
            if Key.down() then
                let board, player = drop board player
                0., board, player
            else
                dropCounter, board, player

        let player =
            if Key.left() then
                moveLeft board player
            else if Key.right() then
                moveRight board player
            else if Key.q() then
                rotate board player CounterClockwise
            else if Key.w() then
                rotate board player Clockwise
            else
                player

        let board = sweep board

        let board =
            if player.y = 0 && collide board player then
                Grid.create (12, 20)
            else
                board

        drawGrid board 0 0
        drawGrid player.piece player.x player.y
        window.requestAnimationFrame(update dropCounter dropInterval board player time) |> ignore

open TetrisGame
open View

let board = Grid.create (12, 20)
let player = Player.reset board

Key.init()
update 0. 1000. board player 0. 0.
