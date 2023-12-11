
#time
#load "../../Tools.fs"
#load "../../Tools/SparseGrid.fs"

open System.IO
open AdventOfCode
open AdventOfCode.SparseGrid

type Coords = int*int
type Offset = int*int
type Image = Map<Coords, Offset>

let getInput name : Image= 
    File.ReadAllLines(getInputPath2023 name)
    |> Seq.mapi (fun row line -> 
        line 
        |> Seq.mapi (fun col c -> if c = '#' then Some ((row, col)) else None ) 
        |> Seq.choose id
    )
    |> Seq.collect id
    |> Seq.map (fun coords -> coords, (0,0))
    |> Map.ofSeq

let expandLines (image:Image) =
    let m = maxR image
    [0..m] |> Seq.iter (fun lineRow ->
        image 
        |> Seq.filter (fun ((r,_), _)-> r = lineRow )
        
    )
    image

let solve1 input =
    let image = getInput input
    (expandLines image) |> Dump

solve1 "Day11_sample1.txt"
