
#time
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode

type Pipes = 
    | NorthSouth         // |
    | EastWest           // -
    | NorthEast          // L
    | NorthWest          // J
    | SouthWest          // 7
    | SouthEast          // F
    | Start of (int*int) // S

let parseLine row (line:string) =
    line |> Seq.mapi (fun col c ->
        match c with 
        | '|' -> Some NorthSouth
        | '-' -> Some EastWest
        | 'L' -> Some NorthEast
        | 'J' -> Some NorthWest
        | '7' -> Some SouthWest
        | 'F' -> Some SouthEast
        | '.' -> None
        | 'S' -> Start(row, col) |> Some
        | _ -> failwithf "not a valid pipe %c" c
    )
    |> Seq.toArray

let printPipe = function
    | Some NorthSouth  -> '┃'
    | Some EastWest  -> '━' 
    | Some NorthEast  -> '┗'
    | Some NorthWest  -> '┛'
    | Some SouthWest  -> '┓'
    | Some SouthEast  -> '┏' 
    | None -> '░'
    | Some(Start _) -> '█'

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.mapi parseLine

let solve1 input =
    getInput input 
    |> array2D

//solve1 "Day10_sample1.txt" 
solve1 "Day10.txt" 
|> Array2DTools.printGridCustom printPipe 
|> ignore