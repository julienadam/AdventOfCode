#time "on"
#load "../../Tools.fs"
#load "../../Tools/Directions.fs"
#load "../../Tools/Distance.fs"
#r "nuget:NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

// Math stuff
// Shoelace formula : https://en.wikipedia.org/wiki/Shoelace_formula
//  Gives the interior area
// Pick's theorem   : https://en.wikipedia.org/wiki/Pick%27s_theorem
//  Compute the area given the interior area and the number of points on the edges

let getInput name =
    let parseLine line =
        let split = line |> ssplit " "
        let dir = 
            match split[0] with 
            | "U" -> North
            | "D" -> South
            | "L" -> West
            | "R" -> East
            | _ -> failwithf "invalid dir"
        let length = split[1] |> int
        dir, length

    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine

let vertices instructions =
    instructions
    |> Seq.fold (fun acc (dir:Direction, len) ->
        let (dr, dc) = dir.MoveBy(acc |> List.head, len)
        (dr, dc) :: acc
    ) [(0,0)]

let shoelace (points: (int*int) list) =
    // Put the head at the back, this will give us a list of vertices 
    // offset by one
    let next = List.append (points |> List.tail) [points |> List.head]

    // Apply the formula
    let total = 
        List.zip points next
        |> List.sumBy(fun ((x1,y1), (x2,y2)) ->
            (x1 |> int64)*(y2 |> int64) - (x2 |> int64)*(y1 |>int64)
        )

    abs(total / 2L)

/// External perimeter of point list
let exterior (points: (int*int) list) =
    points 
    |> Seq.pairwise 
    |> Seq.map (fun (p1,p2) -> (manhattanDistPoints p1 p2))
    |> Seq.sumBy int64

let digSize vertices = (vertices |> shoelace) + ((vertices |> exterior) / 2L + 1L)

let solve1 fileName = getInput fileName |> vertices |> digSize

Check.That(solve1 "Day18_sample1.txt").IsEqualTo(62)
solve1 "Day18.txt"

let getInput2 name =
    let parseLine (line:string) =
        let hexColor = line.Substring(line.IndexOf('#') + 1, 6)
        let dir = 
            match hexColor[5] with
            | '0' -> East
            | '1' -> South
            | '2' -> West
            | '3' -> North
            | x -> failwithf "Not a valid direction %A" x

        dir, Int32.Parse(hexColor.Substring(0,5), Globalization.NumberStyles.HexNumber)

    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine


let solve2 fileName = getInput2 fileName |> vertices |> digSize

Check.That(solve2 "Day18_sample1.txt").IsEqualTo(952408144115L)
solve2 "Day18.txt"