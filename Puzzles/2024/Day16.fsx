#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/AStar.fs"
#load "../../Tools/Distance.fs"
#load "../../Tools/Directions.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> array2D

type Coords = int*int
type State = Coords * Direction

let gCost ((p1,d1):State) ((p2,d2):State) =
    if p1 = p2 then 
        if d1 = d2 then
            failwithf "Should not have 2 steps with the same pos and dir"
        let l = (((360 + d1.GetDegrees() - d2.GetDegrees()) % 360 / 90) |> float32) 
        let r = (((360 + d2.GetDegrees() - d1.GetDegrees()) % 360 / 90) |> float32) 
        (min l r) * 1000.0f
    else
        if d1 <> d2 then
            failwithf "Should not have 2 steps differing by both position and direction "
        if manhattanDistPoints p1 p2 <> 1 then
            failwithf "Should not have 2 steps at a distance different than 1"
        1.0f

let dirToFun = function | North -> Array2DTools.tryGetUp | East -> Array2DTools.tryGetRight | South -> Array2DTools.tryGetDown | West -> Array2DTools.tryGetLeft

let getNeighbors grid ((r,c), dir) = seq {
    match grid |> (dirToFun dir) r c with
    | Some (ar,ac,v) when v = '.' -> yield ((ar,ac), dir) 
    | _ -> ()
    yield (r,c), dir.TurnLeft()
    yield (r,c), dir.TurnRight()
}

open System.Linq

let scorePath (p:State seq) = 
    let mutable score = 0.0
    let path = p |> Seq.rev |> Seq.toArray
    for (a,b) in path |> Seq.windowed 2 |> Seq.map tupleize2 do
        let cost = gCost a b
        score <- score + (cost |> float)
    score

let findBestPath grid (sr,sc) (er,ec) =
    let goal = (er,ec)
    let isGoal (position,_) = position = goal
    let initialState = ((sr,sc), Direction.East)

    let config : FullAStar.Config<State> = {
        neighbors = getNeighbors grid
        gCost = (fun a b -> (gCost a b) |> float)
        fCost = (fun _ _ -> 1.0) // Dijkstra mode
        maxIterations = None
    }

    FullAStar.searchWithGoalFunc initialState (goal, East) isGoal config

let solve1 input =
    let grid = getInput input
    let (sr,sc,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    grid[sr,sc] <- '.'
    grid[er,ec] <- '.'

    match findBestPath grid (sr,sc) (er,ec) with
    | Some p -> scorePath p// printPath grid p
    | _ -> failwithf "No path found"


Check.That(solve1 "Day16_sample1.txt").Equals(7036.0)
Check.That(solve1 "Day16_sample2.txt").Equals(11048.0)
solve1 "Day16.txt"

#r "nuget: OptimizedPriorityQueue"
#load "../../Tools/Dijkstra.fs"

let solve2 input =
    let grid = getInput input
    let (sr,sc,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    grid[sr,sc] <- '.'
    grid[er,ec] <- '.'

    let vertices = 
        grid 
        |> Array2DTools.enumArray2d 
        |> Seq.filter (fun (_,_,v) -> v= '.')
        |> Seq.collect (fun (r,c,_) -> 
            [North;East;South;West] |> Seq.map (fun d -> ((r,c),d))
        )
        |> Seq.toList

    let dists = Dijkstra.getDistMatrix ((sr,sc),East) vertices ((getNeighbors grid) >> Seq.toList) (fun a b -> gCost a b |> float32)
    dists 
    |> Seq.filter(fun kvp -> 
        let ((r,c),_) = kvp.Key 
        (r,c) = (er,ec))
    |> Seq.map (fun kvp -> kvp.Value)
    |> Seq.min

Check.That(solve2 "Day16_sample1.txt").Equals(45)
Check.That(solve2 "Day16_sample2.txt").Equals(64)
solve2 "Day16.txt"

// Unit tests

Check.That(gCost ((0,0), East) ((0,1), East)).Equals(1.0)
Check.That(gCost ((0,0), East) ((1,0), East)).Equals(1.0)
Check.That(gCost ((1,0), East) ((0,0), East)).Equals(1.0)
Check.That(gCost ((1,1), East) ((0,1), East)).Equals(1.0)
Check.That(gCost ((0,0), East) ((0,0), North)).Equals(1000.0)
Check.That(gCost ((0,0), East) ((0,0), West)).Equals(2000.0)
Check.That(gCost ((0,0), East) ((0,0), South)).Equals(1000.0)
Check.That(gCost ((0,0), North) ((0,0), East)).Equals(1000.0)
Check.That(gCost ((0,0), North) ((0,0), South)).Equals(2000.0)
Check.That(gCost ((0,0), North) ((0,0), West)).Equals(1000.0)
Check.That(gCost ((0,0), West) ((0,0), North)).Equals(1000.0)
Check.That(gCost ((0,0), West) ((0,0), East)).Equals(2000.0)
Check.That(gCost ((0,0), West) ((0,0), South)).Equals(1000.0)
Check.That(gCost ((0,0), South) ((0,0), East)).Equals(1000.0)
Check.That(gCost ((0,0), South) ((0,0), North)).Equals(2000.0)
Check.That(gCost ((0,0), South) ((0,0), West)).Equals(1000.0)



let printBestSpots grid (spots:HashSet<(int*int)>) =
    grid |> Array2D.mapi (fun r c v ->
        match v with
        | '#' -> '#'
        | '.' ->
            if spots.Contains((r,c)) then
                'O'
            else
                '.'
        | _ -> failwithf ("Invalid grid")
    ) |> Array2DTools.printGrid

let printPath grid (path:State seq) =
    let lookup = path.ToLookup(fst, snd)
    grid |> Array2D.mapi (fun r c v ->
        match v with
        | '#' -> '#'
        | '.' ->
            if lookup.Contains((r,c)) |> not then
                ' '
            else
                match lookup[(r,c)] |> Seq.rev |> Seq.toList with
                | [North] -> '↑'
                | [East] -> '→'
                | [South] -> '↓'
                | [West] -> '←'
                | [North;East] -> '/'
                | [East;North] -> '/'
                | [North;West] -> '\\'
                | [West;North] -> '\\'
                | [South;East] -> '\\'
                | [East;South] -> '\\'
                | [South;West] -> '/'
                | [West;South] -> '/'
                | _ -> failwith "more than 2 directions on a single spot, should never happen"
        | _ -> failwithf ("Invalid grid")
    ) |> Array2DTools.printGrid

