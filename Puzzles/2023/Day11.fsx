
#time
#r "nuget: NFluent"
#load "../../Tools.fs"
#load "../../Tools/SparseGrid.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Distance.fs"

open System.IO
open NFluent
open AdventOfCode
open AdventOfCode.SparseGrid

type Coords = int*int
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
    let expanded: Image = expand image

    SeqEx.comb 2 (expanded.Keys |> Seq.toList)
    |> Seq.map ltupleize2
    |> Seq.map (fun (p1, p2) -> manhattanDistPoints p1 p2)
    |> Seq.sum

Check.That("Day11_sample1.txt" |> getInput |> expand).IsEqualTo("Day11_sample1_expanded.txt" |> getInput)
Check.That(solve1 "Day11_sample1.txt").IsEqualTo(374)


solve1 "Day11.txt"