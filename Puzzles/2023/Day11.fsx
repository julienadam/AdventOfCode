
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

type Coords = int64*int64
type Image = Map<Coords, unit>

let getInput name : Image= 
    File.ReadAllLines(getInputPath2023 name)
    |> Seq.mapi (fun row line -> 
        line 
        |> Seq.mapi (fun col c -> if c = '#' then Some ((row |> int64, col |> int64)) else None ) 
        |> Seq.choose id
    )
    |> Seq.collect id
    |> Seq.map (fun coords -> coords, ())
    |> Map.ofSeq


let expand factor (image:Image) : Image =
    // This is the actual expander, if current is over the limit, adds the expansion factor
    let incrementValueIfGreaterThanLimit current value limit = if current > limit then value + (max 1L (factor-1L)) else value

    let getEmptyRowOrCol max selector =
        [0L..max] 
        |> Seq.filter (fun x ->
            image.Keys 
            |> Seq.exists (fun coords -> (selector coords) = x)
            |> not
        )
        |> Seq.toList

    // Find empty rows and colums
    let rowLimits = getEmptyRowOrCol (maxR image) fst
    let colLimits = getEmptyRowOrCol (maxC image) snd

    image 
    |> Map.keys 
    |> Seq.map (fun (row,col) ->
        // Apply expansions on both dimensions
        let newR = rowLimits |> List.fold (incrementValueIfGreaterThanLimit row) row
        let newC = colLimits |> List.fold (incrementValueIfGreaterThanLimit col) col
        newR, newC
    )
    |> Seq.map (fun c -> c, ())
    |> Map.ofSeq

let printCell = function | Some _ -> '#' | _ -> '.'

let solve factor input  =
    let image: Image = getInput input
    let expanded: Image = expand factor image 

    SeqEx.comb 2 (expanded.Keys |> Seq.toList)
    |> Seq.map ltupleize2
    |> Seq.map (fun (p1, p2) -> manhattanDistPoints p1 p2)
    |> Seq.sum

let solve1 = solve 1
Check.That("Day11_sample1.txt" |> getInput |> expand 1).IsEqualTo("Day11_sample1_expanded.txt" |> getInput)
Check.That(solve1 "Day11_sample1.txt").IsEqualTo(374)

Check.That(solve 10 "Day11_sample1.txt").IsEqualTo(1030)
Check.That(solve 100 "Day11_sample1.txt").IsEqualTo(8410)

let solve2 = solve 1000000

solve1 "Day11.txt"
solve2 "Day11.txt"