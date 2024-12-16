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
type State = Coords * Direction option

let gCost ((_,d1):State) ((_,d2):State) =
    match d1, d2 with
    | d1, d2 when d1 = d2 -> 1.0
    | Some d1, Some d2 -> 1.0 + (((360 + abs(d1.GetDegrees() - d2.GetDegrees())) % 360 / 90) |> float) * 1000.0
    | a,b -> failwithf "Unsupported case %A %A" a b

Check.That(gCost ((0,0), Some East) ((0,1), Some East)).Equals(1.0)
Check.That(gCost ((0,0), Some East) ((0,1), Some North)).Equals(1001.0)
Check.That(gCost ((0,0), Some East) ((0,1), Some West)).Equals(2001.0)
Check.That(gCost ((0,0), Some East) ((0,1), Some South)).Equals(1001.0)

let getNeighbors grid ((r,c), dir) = seq {
    match grid |> Array2DTools.tryGetUp r c with
    | Some (ar, ac, v) when v = '.' -> yield ((ar,ac), Some North) | _ -> ()
    match grid |> Array2DTools.tryGetRight r c with
    | Some (ar, ac, v) when v = '.' -> yield ((ar,ac), Some East) | _ -> ()
    match grid |> Array2DTools.tryGetDown r c with
    | Some (ar, ac, v) when v = '.' -> yield ((ar,ac), Some South) | _ -> ()
    match grid |> Array2DTools.tryGetLeft r c with
    | Some (ar, ac, v) when v = '.' -> yield ((ar,ac), Some West) | _ -> ()
}

let solve1 input =
    let grid = getInput input // |> Dump
    let (sr,sc,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec,_) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')

    grid[sr,sc] <- '.'
    grid[er,ec] <- '.'

    let goal = (er,ec)
    let isGoal (position,_) = position = goal
    let initialState = ((sr,sc),Some Direction.East)

    let config : FullAStar.Config<State> = {
        neighbors = getNeighbors grid// >> Dump
        gCost = gCost
        fCost = (fun (p1,d1) (p2,d2) -> 
            // First heuristic let's try half as many rotations as the total distance
            //let d = manhattanDistPoints p1 p2 |> float
            //d + (d / 2.0) * 1000.0
            1.0
        )
        maxIterations = None
    }

    let scorePath (p:State seq) = 
        let mutable score = 0.0
        let path = p |> Seq.rev |> Seq.toArray
        for (a,b) in path |> Seq.windowed 2 |> Seq.map tupleize2 do
            let cost = gCost a b
            // printfn "Cost from %A to %A : %f" a b cost
            score <- score + cost
        score

    //let scorePath (p:State seq) = 
    //    let path = p |> Seq.rev |> Seq.toArray
    //    let initial = p |> Seq.head
    //    path |> Seq.skip 1 |> Seq.fold (fun (current,s) next -> (next, s + gCost current next)) (initial, 0.0)

    printfn "Starting at %A. Ending at %A" (sr,sc) (er,ec)
    match FullAStar.searchWithGoalFunc initialState (goal, None) isGoal config with
    | Some p -> 
        // p |> Seq.toArray |> Dump |> ignore
        printfn "%i steps" ((p |> Seq.length) - 1)
        scorePath p
    | _ -> failwithf "No path found"
    
Check.That(solve1 "Day16_sample1.txt").Equals(7036.0)
Check.That(solve1 "Day16_sample2.txt").Equals(11048.0)
solve1 "Day16.txt" // 144488 too high
