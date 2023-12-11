
#time
#load "../../Tools.fs"
#load "../../Tools/SparseGrid.fs"

open System.IO
open AdventOfCode
open AdventOfCode.SparseGrid

type Image = Map<int*int, bool>

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Seq.mapi (fun row line -> 
        line 
        |> Seq.mapi (fun col c -> if c = '#' then Some ((row, col)) else None ) 
        |> Seq.choose id
    )
    |> Seq.collect id
    |> Seq.map (fun coords -> coords, true)
    |> Map.ofSeq

let expand (image:Image) =
    image

let solve1 input =
    let image = getInput input
    (expand image) |> Dump

solve1 "Day11_sample1.txt"
