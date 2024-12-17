
#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Distance.fs"
#load "../../Tools/Directions.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

let getInput name = 
    let grid = File.ReadAllLines(getInputPath2024 name) |> array2D
    let (sr,sc,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    grid[sr,sc] <- '.'
    grid[er,ec] <- '.'
    grid, (sr,sc), (er,ec)

type Coords = int*int
type State = Coords * Direction

let cost ((p1,d1):State) ((p2,d2):State) =
    if p1 = p2 then 
        if d1 = d2 then
            failwithf "Should not have 2 steps with the same pos and dir"
        if d1.TurnLeft() = d2 || d1.TurnRight() = d2 then
            1000.0f
        else if d1.TurnLeft().TurnLeft() = d2 then
            2000.0f
        else
            failwithf("Not a valid turn")
    else
        if d1 <> d2 then
            failwithf "Should not have 2 steps differing by both position and direction "
        if manhattanDistPoints p1 p2 <> 1 then
            failwithf "Should not have 2 steps at a distance different than 1"
        1.0f

let dirToFun = function | North -> Array2DTools.tryGetUp | East -> Array2DTools.tryGetRight | South -> Array2DTools.tryGetDown | West -> Array2DTools.tryGetLeft
let dirToFunRev = function | South -> Array2DTools.tryGetUp | West -> Array2DTools.tryGetRight | North -> Array2DTools.tryGetDown | East -> Array2DTools.tryGetLeft

let getNeighbors grid doRev ((r,c), dir) = seq {
    let f = if doRev then dirToFunRev else dirToFun
    match grid |> (f dir) r c with
    | Some (ar,ac,v) when v = '.' -> yield ((ar,ac), dir) 
    | _ -> ()
    yield (r,c), dir.TurnLeft()
    yield (r,c), dir.TurnRight()
}

let getVertices grid =
    grid 
    |> Array2DTools.filteri (fun _ _ v -> v= '.')
    |> Seq.collect (fun (r,c,_) -> [North;East;South;West] |> Seq.map (fun d -> ((r,c),d)))
    |> Seq.toList

#r "nuget: OptimizedPriorityQueue"
#load "../../Tools/Dijkstra.fs"

let getDijkstraMatrix grid start =
    Dijkstra.getDistMatrix (start,East) (getVertices grid) ((getNeighbors grid false) >> Seq.toList) (fun a b -> cost a b |> float32)

open System.Collections.Generic

// Find the end state (position and direction) with the best score
let bestTile (dists:IDictionary<State,float32>) target =
    dists 
    |> Seq.filter(fun kvp -> 
        let ((r,c),_) = kvp.Key
        (r,c) = target)
    |> Seq.minBy (fun kvp -> kvp.Value)

let solve1Dij input =
    let grid, start, target = getInput input
    let dists = getDijkstraMatrix grid start
    let arrivalTile = bestTile dists target
    arrivalTile.Value

Check.That(solve1Dij "Day16_sample1.txt").Equals(7036.0)
Check.That(solve1Dij "Day16_sample2.txt").Equals(11048.0)
solve1Dij "Day16.txt"

let solve2 input =
    let grid, start, target = getInput input
    let dists = getDijkstraMatrix grid start
    let arrivalTile = bestTile dists target
    let (finalPos, finalDir) = arrivalTile.Key
    let visited = new System.Collections.Generic.HashSet<(int*int)>([finalPos])

    let rec walkBack (pos,dir) =
        if (pos,dir) = (start,East) then 
            ()
        else
            let score = dists[(pos,dir)]
            // Find neighbors going backwards
            // keep only the ones that would result in the same score as the current score
            // if we made the move from the neighbor to the current position
            // and apply recursion to all candidates until we reach the start position
            getNeighbors grid true (pos,dir)
            |> Seq.filter (fun n -> dists[n] + cost n (pos,dir) = score)
            |> Seq.iter (fun ((r,c), d) -> 
                visited.Add((r,c)) |> ignore
                walkBack ((r,c), d)
            )

    walkBack (finalPos, finalDir)

    visited.Count

Check.That(solve2 "Day16_sample1.txt").Equals(45)
Check.That(solve2 "Day16_sample2.txt").Equals(64)
solve2 "Day16.txt"

//// Cost function unit tests 

Check.That(cost ((0,0), East) ((0,1), East)).Equals(1.0)
Check.That(cost ((0,0), East) ((1,0), East)).Equals(1.0)
Check.That(cost ((1,0), East) ((0,0), East)).Equals(1.0)
Check.That(cost ((1,1), East) ((0,1), East)).Equals(1.0)
Check.That(cost ((0,0), East) ((0,0), North)).Equals(1000.0)
Check.That(cost ((0,0), East) ((0,0), West)).Equals(2000.0)
Check.That(cost ((0,0), East) ((0,0), South)).Equals(1000.0)
Check.That(cost ((0,0), North) ((0,0), East)).Equals(1000.0)
Check.That(cost ((0,0), North) ((0,0), South)).Equals(2000.0)
Check.That(cost ((0,0), North) ((0,0), West)).Equals(1000.0)
Check.That(cost ((0,0), West) ((0,0), North)).Equals(1000.0)
Check.That(cost ((0,0), West) ((0,0), East)).Equals(2000.0)
Check.That(cost ((0,0), West) ((0,0), South)).Equals(1000.0)
Check.That(cost ((0,0), South) ((0,0), East)).Equals(1000.0)
Check.That(cost ((0,0), South) ((0,0), North)).Equals(2000.0)
Check.That(cost ((0,0), South) ((0,0), West)).Equals(1000.0)

//let printBestSpots grid (spots:System.Collections.Generic.HashSet<(int*int)>) =
//    grid |> Array2D.mapi (fun r c v ->
//        match v with
//        | '#' -> '#'
//        | '.' ->
//            if spots.Contains((r,c)) then
//                'O'
//            else
//                '.'
//        | _ -> failwithf ("Invalid grid")
//    ) |> Array2DTools.printGrid

//let printPath grid (path:State seq) =
//    let lookup = path.ToLookup(fst, snd)
//    grid |> Array2D.mapi (fun r c v ->
//        match v with
//        | '#' -> '#'
//        | '.' ->
//            if lookup.Contains((r,c)) |> not then
//                ' '
//            else
//                match lookup[(r,c)] |> Seq.rev |> Seq.toList with
//                | [North] -> '↑'
//                | [East] -> '→'
//                | [South] -> '↓'
//                | [West] -> '←'
//                | [North;East] -> '/'
//                | [East;North] -> '/'
//                | [North;West] -> '\\'
//                | [West;North] -> '\\'
//                | [South;East] -> '\\'
//                | [East;South] -> '\\'
//                | [South;West] -> '/'
//                | [West;South] -> '/'
//                | _ -> failwith "more than 2 directions on a single spot, should never happen"
//        | _ -> failwithf ("Invalid grid")
//    ) |> Array2DTools.printGrid

