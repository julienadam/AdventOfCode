#load "../../Tools.fsx"

open System
open System.IO
open Tools

// type Instruction = {
//   Direction : Compass
//   Length : 
// }

let mapLine (dir : string, len) = 
  let c = match dir.[0] with | 'R' -> Compass.Right | 'L' -> Compass.Left | 'U' -> Compass.Up | 'D' -> Compass.Down
  c, len |> Int32.Parse

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map (split2 ' ') |> Seq.map mapLine

let i = getInput "Day09.txt" |> Seq.toList

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

let initialState = (0, 0), Cover

//let newHeadPos (dir:Compass) (x,y) =
//    match dir with
//    | Compass.Right -> (x+1,y)
//    | Compass.Left -> (x-1,y)
//    | Compass.Up -> (x,y-1)
//    | Compass.Down -> (x,y+1)

let makeMove (headDir:Compass) ((tailX,tailY), headPos) placesVisited =
    
    match headPos, headDir with 
    | Cover, Compass.Right -> Right, (tailX,tailY)
    | Cover, Compass.Left -> Left, (tailX,tailY)
    | Cover, Compass.Up -> Up, (tailX,tailY)
    | Cover, Compass.Down -> Down, (tailX,tailY)
    | Right, Compass.Right -> Right, (tailX+1,tailY)
    | Right, Compass.Left -> Cover, (tailX-1,tailY)
    | Right, Compass.Down -> UpLeft, (tailX,tailY)
    | Right, Compass.Up -> DownLeft, (tailX,tailY)
    | Left, Compass.Left -> Left, (tailX-1,tailY)
    | Left, Compass.Right-> Cover, (tailX+1,tailY)
    | Left, Compass.Down -> UpRight, (tailX,tailY)
    | Left, Compass.Up -> DownRight, (tailX,tailY)
    | Up, Compass.Up -> Up, (tailX,tailY-1)
    | Up, Compass.Down -> Cover, (tailX,tailY)
    | Up, Compass.Right -> DownLeft, (tailX,tailY)
    | Up, Compass.Left -> DownRight, (tailX,tailY)
    | Down, Compass.Down -> Cover, (tailX,tailY+1)