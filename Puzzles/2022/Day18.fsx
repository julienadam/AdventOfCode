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

let solve1 grid =
    grid |> Seq.map (fun c -> 6 - (countAdjacentNeighbours3D grid c)) |> Seq.sum

    
type Result =
    | Face
    | Explorable of (int*int*int)

let solve2 grid = 
    let outOfBounds i = i < -1 || i > 20

    let rec countFaces toVisit visited grid facesFound =
        if toVisit |> Set.isEmpty then
            facesFound
        else
            let pos = toVisit |> Seq.head

            let explorationResults = 
                enumAdjacentCoords3D pos
                |> Seq.map (fun (x,y,z) ->
                    if outOfBounds x || outOfBounds y || outOfBounds z then
                        None
                    else if grid |> Set.contains (x,y,z) then
                        Some Face
                    else if visited |> Set.contains (x,y,z) then
                        None
                    else
                        Some (Explorable (x,y,z))
                )
                |> Seq.toList

            let newFacesFound = explorationResults |> Seq.map(fun r -> match r with | Some Face -> 1 | _ -> 0) |> Seq.sum
            let nextToVisit = 
                explorationResults
                |> List.choose (fun r -> match r with | Some (Explorable p) -> Some p | _ -> None)
                |> Set.ofList
                |> Set.union toVisit
                |> Set.remove pos

            countFaces nextToVisit (visited |> Set.add pos) grid (newFacesFound + facesFound)
    countFaces ([(0,0,0)] |> Set.ofList) Set.empty grid 0

getInput "Day18.txt"
|> solve2



