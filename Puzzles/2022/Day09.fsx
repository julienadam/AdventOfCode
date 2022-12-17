#load "../../Tools.fsx"

open System
open System.IO
open System.Collections.Generic
open Tools

let mapLine (dir : string, len) = 
  let c = match dir.[0] with | 'R' -> Compass.Right | 'L' -> Compass.Left | 'U' -> Compass.Up | 'D' -> Compass.Down
  c, len |> Int32.Parse

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map (split2 ' ') |> Seq.map mapLine

type HeadPosition = Compass option
type coords = int * int

let makeMove headDir (headPos, move) =
    match headPos, headDir with 
    | None,             Right   -> Some Right,       None
    | None,             Left    -> Some Left,        None              
    | None,             Up      -> Some Up,          None
    | None,             Down    -> Some Down,        None
    | Some Right,       Right   -> Some Right,       Right |> Some
    | Some Right,       Left    -> None,             None
    | Some Right,       Down    -> Some DownRight,   None
    | Some Right,       Up      -> Some UpRight,     None
    | Some Left,        Left    -> Some Left,        Left |> Some
    | Some Left,        Right   -> None,             None
    | Some Left,        Down    -> Some DownLeft,    None
    | Some Left,        Up      -> Some UpLeft,      None
    | Some Up,          Up      -> Some Up,          Up |> Some
    | Some Up,          Down    -> None,             None
    | Some Up,          Right   -> Some UpRight,     None
    | Some Up,          Left    -> Some UpLeft,      None
    | Some Down,        Down    -> Some Down,        Down |> Some
    | Some Down,        Up      -> None,             None
    | Some Down,        Right   -> Some DownRight,   None
    | Some Down,        Left    -> Some DownLeft,    None
    | Some UpRight,     Down    -> Some Right,       None
    | Some UpRight,     Up      -> Some Up,          UpRight |> Some
    | Some UpRight,     Right   -> Some Right,       UpRight |> Some
    | Some UpRight,     Left    -> Some Up,          None
    | Some UpLeft,      Down    -> Some Left,        None
    | Some UpLeft,      Up      -> Some Up,          UpLeft |> Some
    | Some UpLeft,      Right   -> Some Up,          None
    | Some UpLeft,      Left    -> Some Left,        UpLeft |> Some
    | Some DownRight,   Down    -> Some Down,        DownRight |> Some
    | Some DownRight,   Up      -> Some Right,       None
    | Some DownRight,   Left    -> Some Down,        None
    | Some DownRight,   Right   -> Some Right,       DownRight |> Some
    | Some DownLeft,    Down    -> Some Down,        DownLeft |> Some
    | Some DownLeft,    Up      -> Some Left,        None
    | Some DownLeft,    Left    -> Some Left,        DownLeft |> Some
    | Some DownLeft,    Right   -> Some Down,        None
    
let applyMove (x,y) (move: Compass option) =
    match move with
    | None -> (x,y)
    | Some Compass.Up -> (x, y-1)
    | Some Compass.Down -> (x, y+1)
    | Some Compass.Left -> (x-1, y)
    | Some Compass.Right -> (x+1, y)
    | Some Compass.UpRight -> (x+1, y-1)
    | Some Compass.UpLeft -> (x-1, y-1)
    | Some Compass.DownRight -> (x+1, y+1)
    | Some Compass.DownLeft -> (x-1, y+1)

let solve1 input =
    let instructions = getInput input |> Seq.toList
    let visited = new HashSet<coords>([(0,0)])
    let initialState = None, (0, 0)

    let finalState = 
        instructions |> Seq.fold (fun state (dir, len) ->
            [1..len] |> Seq.fold (fun (hp,coords) _ -> 
                let newHeadPos, move = makeMove dir (hp,coords)
                let newCoords = applyMove coords move
                visited.Add(newCoords) |> ignore
                newHeadPos, newCoords
            ) state
        ) initialState

    // finalState |> Dump

    visited.Count

    
solve1 "Day09.txt" |> Dump

//let solve2 input = 
//    let instructions = getInput input |> Seq.toList
//    let part2State = [1..10] |> List.map (fun i -> Cover, (0, 0))
//    let visited = new HashSet<coords>([(0,0)])
    
//    instructions |> Seq.fold (fun state (dir, len) ->
//        let node1' = makeMove dir state.[0]
//        // let node2' = makeMove dir node1'
//    ) part2State
