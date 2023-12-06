#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode

let path = getInputPath "day13.txt"
//let path = getInputPath "day13_sample1.txt"

let mapPointLines (input:string) = 
    let split = input.Split(",")
    (split.[0] |> int), (split.[1] |> int)

type Fold =
| Horizontal of int
| Vertical of int

let mapFoldLines (input: string) =
    match input.[11] with
    | 'x' -> Vertical (input.Substring(13) |> int)
    | 'y' -> Horizontal (input.Substring(13) |> int)
    | c -> failwithf "Invalid fold direction %c" c

let lines = File.ReadLines(path) 
let points = lines |> Seq.takeWhile (fun l -> l <> "") |> Seq.map mapPointLines |> Set.ofSeq
let folds = lines |> Seq.skipWhile (fun l -> l <> "") |> Seq.skip 1 |> Seq.map mapFoldLines

let foldHorizontal yf input =
    let bottom, top = input |> Set.partition (fun (_,y) -> y > yf)
    let flippedBottomUp = bottom |> Set.map (fun (x,y) -> x, 2*yf-y)
    Set.union top flippedBottomUp

let foldVertical xf input =
    let right, left = input |> Set.partition (fun (x,_) -> x > xf)
    let flippedRightToLeft = right |> Set.map (fun (x,y) -> 2*xf-x, y)
    Set.union left flippedRightToLeft
    
let foldPaper fold =
    match fold with
    | Horizontal y -> foldHorizontal y
    | Vertical x -> foldVertical x
        
module Part1 =

    let Solve() = (foldPaper (folds |> Seq.head) points) |> Seq.length |> Dump
        
Part1.Solve()

module Part2 =

    let Solve() =
        let folded = 
            folds 
            |> Seq.fold (fun currentPoints fold -> (foldPaper fold currentPoints)) points
            |> Dump
        
        let display = Array2D.create (1 + (folded |> Seq.map snd |> Seq.max)) (1 + (folded |> Seq.map fst |> Seq.max)) ' '
        folded |> Set.iter (fun (x,y) -> Array2D.set display y x '#')
        display |> Array2DTools.printGrid

Part2.Solve()