#load "../../Tools.fsx"
#r "nuget: XPlot.Plotly" 

open System
open System.IO
open System.Collections.Generic
open Tools

let mapLine (dir : string, len) = 
  let c = match dir.[0] with | 'R' -> Compass.Right | 'L' -> Compass.Left | 'U' -> Compass.Up | 'D' -> Compass.Down
  c, len |> Int32.Parse

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map (split2 ' ') |> Seq.map mapLine

type coords = int * int

let makeMove (headDir: Compass option) headPos =
    if headDir.IsNone then
        None, None
    else
        match headPos, headDir.Value with 
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
                let newHeadPos, move = makeMove (Some dir) hp
                let newCoords = applyMove coords move
                visited.Add(newCoords) |> ignore
                newHeadPos, newCoords
            ) state
        ) initialState

    // finalState |> Dump

    visited.Count

    
solve1 "Day09.txt" |> Dump

let getKnotPositions (positions: Compass option list) (tailX, tailY) = seq {
    yield (tailX, tailY), "H"
    let mutable currentX, currentY = tailX, tailY
    let mutable index = 0
    for p in positions |> List.rev do
        let c = sprintf "%c" ((char) ((int) '9' - index))
        let newX, newY = 
            match p with
            | None -> (currentX, currentY)
            | Some Up -> (currentX, currentY - 1)
            | Some Down -> (currentX, currentY + 1)
            | Some Left -> (currentX - 1, currentY)
            | Some Right -> (currentX + 1, currentY)
            | Some UpRight -> (currentX + 1, currentY - 1)
            | Some UpLeft -> (currentX - 1, currentY - 1)
            | Some DownRight -> (currentX + 1, currentY + 1)
            | Some DownLeft -> (currentX - 1, currentY + 1)
        index <- index + 1
        currentX <- newX
        currentY <- newY
        yield (newX, newY), c
}

open XPlot.Plotly

let display (positions: Compass option list) (tailX, tailY) =

    let knotPos = (getKnotPositions positions  (tailX, tailY)) |> Seq.toArray

    let xs = knotPos |> Array.map fst |> Array.map fst
    let ys = knotPos |> Array.map fst |> Array.map snd
    let labels = knotPos |> Array.map snd

    printfn "%A" xs
    printfn "%A" ys
    printfn "%A" labels

    let trace1 =
        Scatter(
            x = xs,
            y = ys,
            text = labels
        )

    let styledLayout =
        Layout(
            xaxis =
                Xaxis(
                    ticklen = 1
                ),
            yaxis =
                Yaxis(
                    ticklen = 1
                )
        )
    

    [trace1]
    |> Chart.Plot
    |> Chart.WithLayout styledLayout
    |> Chart.WithWidth 700
    |> Chart.WithHeight 500
    |> Chart.Show


let solve2 input = 
    let instructions = getInput input |> Seq.toList
    let part2State : Compass option list * (int * int) = [1..10] |> List.map (fun _ -> None), (0,0)
    
    let visited = new HashSet<coords>([(0,0)])
    
    let finalState = 
        instructions |> Seq.fold (fun (headPositions : Compass option list, tailCoords) (dir, len) ->
            let mutable positions, coords = headPositions, tailCoords
            for i = 0 to len - 1 do
                let hp1 = positions.[0]
                let hp1', m1 = makeMove (Some dir) hp1
        
                let hp2 = positions.[1]
                let hp2', m2 = makeMove m1 hp2
        
                let hp3 = positions.[2]
                let hp3', m3 = makeMove m2 hp3
        
                let hp4 = positions.[3]
                let hp4', m4 = makeMove m3 hp4
        
                let hp5 = positions.[4]
                let hp5', m5 = makeMove m4 hp5
        
                let hp6 = positions.[5]
                let hp6', m6 = makeMove m5 hp6
        
                let hp7 = positions.[6]
                let hp7', m7 = makeMove m6 hp7
        
                let hp8 = positions.[7]
                let hp8', m8 = makeMove m7 hp8
        
                let hp9 = positions.[8]
                let hp9', m9 = makeMove m8 hp9
        
                let hp10 = positions.[9]
                let hp10', m10 = makeMove m9 hp10
        
                let newTailCoords = applyMove tailCoords m10
                visited.Add(newTailCoords) |> ignore

                positions <- [hp1'; hp2'; hp3'; hp4';hp5';hp6';hp7';hp8';hp9';hp10';]
                coords <- newTailCoords
            
            display positions coords
            positions, coords
        
        ) part2State

    visited.Count


solve2 "Day09_sample2.txt" |> Dump

