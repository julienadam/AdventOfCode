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
    | None,             dir       -> Some dir,       None
    | Some p, d when d = p        -> Some p,         p |> Some
    | Some Right,       Left      -> None,           None
    | Some Right,       Down      -> Some DownRight, None
    | Some Right,       Up        -> Some UpRight,   None
    | Some Right,       UpRight   -> Some Right,     UpRight |> Some
    | Some Right,       UpLeft    -> Some Up,        None
    | Some Right,       DownRight -> Some Right,     DownRight |> Some
    | Some Right,       DownLeft  -> Some Down,      None
    | Some Left,        Right     -> None,           None
    | Some Left,        Down      -> Some DownLeft,  None
    | Some Left,        Up        -> Some UpLeft,    None
    | Some Left,        UpRight   -> Some Up,        None
    | Some Left,        UpLeft    -> Some Left,      UpLeft |> Some
    | Some Left,        DownRight -> Some Down,      None
    | Some Left,        DownLeft  -> Some Left,      DownLeft |> Some
    | Some Up,          Down      -> None,           None
    | Some Up,          Right     -> Some UpRight,   None
    | Some Up,          Left      -> Some UpLeft,    None
    | Some Up,          UpRight   -> Some Up,        Some UpRight
    | Some Up,          UpLeft    -> Some Up,        Some UpLeft
    | Some Up,          DownRight -> Some Right,     None
    | Some Up,          DownLeft  -> Some Left,      None
    | Some Down,        Up        -> None,           None
    | Some Down,        Right     -> Some DownRight, None
    | Some Down,        Left      -> Some DownLeft,  None
    | Some Down,        UpRight   -> Some Right,     None
    | Some Down,        UpLeft    -> Some Left,      None
    | Some Down,        DownRight -> Some Down,      Some DownRight
    | Some Down,        DownLeft  -> Some Down,      Some DownLeft
    | Some UpRight,     Down      -> Some Right,     None
    | Some UpRight,     Up        -> Some Up,        UpRight |> Some
    | Some UpRight,     Right     -> Some Right,     UpRight |> Some
    | Some UpRight,     Left      -> Some Up,        None
    //| Some UpRight,     UpRight   -> Some UpRight,   Some UpRight
    | Some UpRight,     UpLeft    -> Some Up,        Some Up
    | Some UpRight,     DownRight -> Some Right,     Some Right
    | Some UpRight,     DownLeft  -> None,           None
    | Some UpLeft,      Down      -> Some Left,      None
    | Some UpLeft,      Up        -> Some Up,        UpLeft |> Some
    | Some UpLeft,      Right     -> Some Up,        None
    | Some UpLeft,      Left      -> Some Left,      UpLeft |> Some
    | Some UpLeft,      UpRight   -> Some Up,        Some Up
    //| Some UpLeft,      UpLeft    -> Some UpLeft,    Some UpLeft
    | Some UpLeft,      DownRight -> None,           None
    | Some UpLeft,      DownLeft  -> Some Left,      Some Left
    | Some DownRight,   Down      -> Some Down,      DownRight |> Some
    | Some DownRight,   Up        -> Some Right,     None
    | Some DownRight,   Left      -> Some Down,      None
    | Some DownRight,   Right     -> Some Right,     DownRight |> Some
    | Some DownRight,   UpRight     -> Some Right,   Right |> Some
    | Some DownRight,   UpLeft     -> None,   None
    //| Some DownRight,   DownRight     -> Some DownRight,   Some DownRight
    | Some DownRight,   DownLeft     -> Some Down,   Down |> Some
    | Some DownLeft,    Down      -> Some Down,      DownLeft |> Some
    | Some DownLeft,    Up        -> Some Left,      None
    | Some DownLeft,    Left      -> Some Left,      DownLeft |> Some
    | Some DownLeft,    Right     -> Some Down,      None
    | Some DownLeft,    UpRight     -> None, None
    | Some DownLeft,    UpLeft     -> Some Left, Some Left
    //| Some DownLeft,    DownLeft     -> Some DownLeft, Some DownLeft
    | Some DownLeft,    DownRight     -> Some Down, Some Down
    | p, d -> failwithf "Pos %A and dir %A is not handled" p d

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

let solve2 input = 
    let instructions = getInput input |> Seq.toList
    let part2State : (Compass option * ( int * int )) list = [1..10] |> List.map (fun i -> None, (0, 0))
    let visited = new HashSet<coords>([(0,0)])
    
    instructions |> Seq.fold (fun (state) (dir, len) ->
        let hp1, coord1 = state.[0]
        let hp1', m1 = makeMove dir hp1
        let coord1' = applyMove coord1 m1
        
        let hp2, coord2 = state.[1]
        let hp2', m2 = makeMove dir hp2
        
        
        let node2' = makeMove dir node1'
    ) part2State
