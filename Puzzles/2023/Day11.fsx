
#time
#load "../../Tools.fs"
#r "nuget: NFluent"
#load "../../Tools/SparseGrid.fs"

open System.IO
open NFluent
open AdventOfCode
open AdventOfCode.SparseGrid

type Coords = int*int
type Offset = int*int
type Image = Map<Coords, unit>

let getInput name : Image= 
    File.ReadAllLines(getInputPath2023 name)
    |> Seq.mapi (fun row line -> 
        line 
        |> Seq.mapi (fun col c -> if c = '#' then Some ((row, col)) else None ) 
        |> Seq.choose id
    )
    |> Seq.collect id
    |> Seq.map (fun coords -> coords, ())
    |> Map.ofSeq


let expand (image:Image) : Image =
    let incrementValueIfGreaterThanLimit current value limit = if current > limit then value + 1 else value

    let getEmptyRowOrCol max selector =
        [0..max] 
        |> Seq.filter (fun x ->
            image.Keys 
            |> Seq.exists (fun coords -> (selector coords) = x)
            |> not
        )
        |> Seq.toList

    let rowLimits = getEmptyRowOrCol (maxR image) fst
    let colLimits = getEmptyRowOrCol (maxC image) snd

    image 
    |> Map.keys 
    |> Seq.map (fun (row,col) ->
        let newR = rowLimits |> List.fold (incrementValueIfGreaterThanLimit row) row
        let newC = colLimits |> List.fold (incrementValueIfGreaterThanLimit col) col
        newR, newC
    )
    |> Seq.map (fun c -> c, ())
    |> Map.ofSeq

let printCell = function | Some _ -> '#' | _ -> '.'

let solve1 input =
    let image: Image = getInput input
    // image |> printGrid printCell |> ignore
    // printfn ""
    let expanded: Image = expand image
    expanded |> printGrid printCell |> ignore
    expanded

Check.That("Day11_sample1.txt" |> getInput |> expand).IsEqualTo("Day11_sample1_expanded.txt" |> getInput)

solve1 "Day11_sample1.txt"
