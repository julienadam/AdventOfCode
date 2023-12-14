
#time
#load "../../Tools.fs"

open System.IO
open AdventOfCode
open System.Collections.Generic
open System.Text

let transposeText (lines:string[]) =
    let builders = 
        [|0..lines[0].Length-1|] 
        |> Array.map (fun c -> new StringBuilder())

    lines |> Seq.iteri (fun row line ->
        line |> Seq.iteri (fun col c -> builders[col].Append(c) |> ignore)
    )
    
    builders |> Array.map (fun sb -> sb.ToString())

transposeText [|"ABCDEF"; "ABCDEF"|]

let getInputText name = 
    File.ReadAllLines(getInputPath2023 name)

let rec compactLeft (line:string) =
    let replaced = line.Replace(".O", "O.")
    if replaced = line then line else compactLeft replaced
let rec compactRight (line:string) =
    let replaced = line.Replace("O.", ".O")
    if replaced = line then line else compactRight replaced

let tiltTextNorth (input:string[]) =
    input |> transposeText |> Array.map compactLeft |> transposeText

let printLines (lines:string seq) = lines |> Seq.iter (printfn "%s")

getInputText "Day14_sample1.txt" |> tiltTextNorth |> printLines

let calcLoadText (lines: string array) =
    lines 
    |> Seq.mapi (fun r line ->
        line 
        |> Seq.map (fun c -> match c with | 'O' -> lines.Length - r | _ -> 0)
        |> Seq.sum)
    |> Seq.sum
    
getInputText "Day14_sample1.txt" |> tiltTextNorth |> calcLoadText
getInputText "Day14.txt" |> tiltTextNorth |> calcLoadText

let tiltTextSouth (input:string[]) =
    input |> transposeText |> Array.map compactRight |> transposeText

let tiltTextWest (input:string[]) =
    input |> Array.map compactLeft

let tiltTextEast (input:string[]) =
    input |> Array.map compactRight


let cycleText = tiltTextNorth >> tiltTextWest >> tiltTextSouth >> tiltTextEast

getInputText "Day14_sample1.txt" |> tiltTextNorth |> tiltTextWest |> tiltTextSouth |> tiltTextEast |> printLines
getInputText "Day14_sample1.txt" |> cycleText |> printLines

let hashLines (lines:string array) = System.HashCode.Combine(lines |> Seq.map (fun s -> s.GetHashCode()))

let findCycleLength grid =
    let mutable grid = grid
    let states = new Dictionary<int, int>()
    let mutable cycleFound = false
    let mutable i = 0
    let mutable result = (0,0)
    while (cycleFound = false) do 
        i <- i + 1
        grid <- cycleText grid 
        let hash = hashLines grid
        match states.TryGetValue hash with
        | true, round ->
            cycleFound <- true
            result <- (round, i)
        | false, _ ->
            states.Add(hash, i)
    result

let solveText input =
    let mutable grid = getInputText input
    let cycleStart, cycleEnd = findCycleLength (grid.Clone() :?> string[])
    let length = (cycleEnd - cycleStart)
    let remaining = (cycleStart - 1) + (1000000000 - cycleStart + 1) % length
    for _ = 1 to remaining do 
        grid <- cycleText grid
    calcLoadText grid

solveText "Day14_sample1.txt"
