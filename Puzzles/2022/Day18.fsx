#time
#load "../../Tools.fsx"

open System.IO
open Checked
open Tools


let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map splitIntList
    |> Seq.map tupleize3
    |> Set.ofSeq
    |> Dump

getInput "Day18.txt"

let enumAdjacentCoords3D (x,y,z) = seq {
    yield x+1,y,z
    yield x-1,y,z
    yield x,y+1,z
    yield x,y-1,z
    yield x,y,z+1
    yield x,y,z-1
}

let countAdjacentNeighbours3D grid coords =
    enumAdjacentCoords3D coords
    |> Seq.filter (fun c -> grid |> Set.contains c)
    |> Seq.length
