#load "../../Tools.fsx"

open System
open System.IO
open System.Collections.Generic
open Tools

let mapLine (dir : string, len) = 
  let c = match dir.[0] with | 'R' -> Compass.Right | 'L' -> Compass.Left | 'U' -> Compass.Up | 'D' -> Compass.Down
  c, len |> Int32.Parse

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map (split2 ' ') |> Seq.map mapLine

type HeadPosition =
| Cover
| Right
| Left
| Up
| UpLeft
| UpRight
| Down
| DownLeft
| DownRight

type coords = int * int

let makeMove (headDir:Compass) (headPos, (tailX,tailY)) (placesVisited:HashSet<coords>) =
    let unchanged = tailX, tailY
    let newHeadPos, newTailCoords =
        match headPos, headDir with 
        | Cover,    Compass.Right   -> Right,       unchanged
        | Cover,    Compass.Left    -> Left,        unchanged
        | Cover,    Compass.Up      -> Up,          unchanged
        | Cover,    Compass.Down    -> Down,        unchanged
        | Right,    Compass.Right   -> Right,       (tailX+1, tailY)
        | Right,    Compass.Left    -> Cover,       unchanged
        | Right,    Compass.Down    -> DownRight,   unchanged
        | Right,    Compass.Up      -> UpRight,     unchanged
        | Left,     Compass.Left    -> Left,        (tailX-1, tailY)
        | Left,     Compass.Right   -> Cover,       unchanged
        | Left,     Compass.Down    -> DownLeft,    unchanged
        | Left,     Compass.Up      -> UpLeft,      (tailX, tailY)
        | Up,       Compass.Up      -> Up,          (tailX, tailY-1)
        | Up,       Compass.Down    -> Cover,       unchanged
        | Up,       Compass.Right   -> UpRight,     unchanged
        | Up,       Compass.Left    -> UpLeft,      unchanged
        | Down,     Compass.Down    -> Down,        (tailX, tailY+1)
        | Down,     Compass.Up      -> Cover,       unchanged
        | Down,     Compass.Right   -> DownRight,   unchanged
        | Down,     Compass.Left    -> DownLeft,    unchanged
        | UpRight,  Compass.Down    -> Right,       unchanged
        | UpRight,  Compass.Up      -> Up,          (tailX + 1, tailY - 1)
        | UpRight,  Compass.Right   -> Right,       (tailX + 1, tailY - 1)
        | UpRight,  Compass.Left    -> Up,          unchanged
        | UpLeft,   Compass.Down    -> Left,        unchanged
        | UpLeft,   Compass.Up      -> Up,          (tailX - 1, tailY - 1)
        | UpLeft,   Compass.Right   -> Up,          unchanged
        | UpLeft,   Compass.Left    -> Left,        (tailX - 1, tailY - 1)
        | DownRight,Compass.Down    -> Down,        (tailX + 1, tailY + 1)
        | DownRight,Compass.Up      -> Right,       unchanged
        | DownRight,Compass.Left    -> Down,        unchanged
        | DownRight,Compass.Right   -> Right,       (tailX + 1, tailY + 1)
        | DownLeft, Compass.Down    -> Down,        (tailX - 1, tailY + 1)
        | DownLeft, Compass.Up      -> Left,        unchanged
        | DownLeft, Compass.Left    -> Left,        (tailX - 1, tailY + 1)
        | DownLeft, Compass.Right   -> Down,        unchanged
    
    if not (newTailCoords = unchanged) then
        placesVisited.Add(newTailCoords) |> ignore
    
    newHeadPos, newTailCoords

let solve1 input =
    let instructions = getInput input |> Seq.toList
    let visited = new HashSet<coords>([(0,0)])
    let initialState = Cover, (0, 0)

    let finalState = 
        instructions |> Seq.fold (fun state (dir, len) ->
            [1..len] |> Seq.fold (fun s _ -> 
                makeMove dir s visited
            ) state
        ) initialState

    // finalState |> Dump

    visited.Count

    
solve1 "Day09.txt" |> Dump

