open System
open System.IO
open System.Text.RegularExpressions

let getInputPath file = Path.Combine(__SOURCE_DIRECTORY__, "Input", "2021", file)
let getInputPath2022 file = Path.Combine(__SOURCE_DIRECTORY__, "Input", "2022", file)

let Dump obj =
    printfn "%A" obj
    obj

let ssplit (sep:string) (str:string) = str.Split([|sep|], StringSplitOptions.None)

let split2 (sep : char) (s : string) = 
    let split = s.Split(sep)
    split.[0], split.[1]

let inline tupleize2 (a:array<'a>) = a.[0], a.[1]

module SeqEx =
    let read n s =
        s |> Seq.take n, s |> Seq.skip n

[<AutoOpen>]
module TupleTools =
    let inline swap2 (a,b) = (b,a)

[<AutoOpen>]
module Distance =
    let inline manhattanDistance (x1:int) (y1:int) (x2:int) (y2:int) = Math.Abs(x1 - x2) + Math.Abs(y1 - y2)
    let inline manhattanDistPoints (x1,y1) (x2,y2) = manhattanDistance x1 y1 x2 y2

[<AutoOpen>]
module RegexTools =
    let inline mInt (groupName:string) (m:Match) = m.Groups.[groupName].Value |> int

module AStar =
    type AStarNode<'a> = {
        F: int64
        G: int64
        Data: 'a
    }
    
module Array2DTools =

    let getAdjacent row col (grid:'a[,]) = seq {
        if row > 0 then
            yield ((row - 1), col, grid.[(row - 1), col])
        if row < ((grid |> Array2D.length1) - 1) then
            yield ((row + 1), col, grid.[(row + 1), col])
        if col > 0 then
            yield (row, (col - 1), grid.[row, (col - 1)])
        if col < ((grid |> Array2D.length2) - 1) then
            yield (row, (col + 1), grid.[row, (col + 1)])
    }
    
    let getAdjacentWithDiagonals row col (grid:'a[,]) = seq {
        yield! getAdjacent row col (grid:'a[,])
        if row > 0 && col < ((grid |> Array2D.length2) - 1) then
            yield ((row - 1), (col + 1), grid.[(row - 1), (col + 1)])
        if row <((grid |> Array2D.length1) - 1) && col > 0 then
            yield ((row + 1), (col - 1), grid.[(row + 1), (col - 1)])
        if row <((grid |> Array2D.length1) - 1) && col < ((grid |> Array2D.length2) - 1) then
            yield ((row + 1), (col + 1), grid.[(row + 1), (col + 1)])
    }

    let enumArray2d (array:'a[,]) = seq {
        for i = 0 to (array |> Array2D.length1) - 1 do
            for j = 0 to (array |> Array2D.length2) - 1 do
                yield i,j, array.[i,j]
    }

    let printGrid (grid:'a[,]) =
        for i in [0..grid.GetLength(0) - 1] do
            for j in [0..grid.GetLength(1) - 1] do
                printf "%O" grid.[i,j]
                if j % 10 = 9 then
                    printf " "
            printfn ""
            if i % 10 = 9 then
                printfn ""
        
type Compass =
| Up
| Down
| Left
| Right
| UpRight
| UpLeft
| DownRight
| DownLeft

module FullAStar = 

    open System.Collections.Generic;

    type Config<'a> = 
        {
            /// <summary>
            /// A method that, given a source, will return its neighbours.
            /// </summary>
            neighbours: 'a -> seq<'a>
            /// <summary>
            /// Given two nodes that are next to each other, return the g cost between them.
            /// The g cost is the cost of moving from one to the other directly.
            /// </summary>
            gCost: 'a -> 'a -> float
            /// <summary>
            /// Given two nodes, return the f cost between them. This is a heuristic score used from a given node to the goal.
            /// Line-of-sight distance is an example of how this might be defined.
            /// </summary>
            fCost: 'a -> 'a -> float
            /// <summary>
            /// The maximum number of tiles to check - used to limit overly long searches when accuracy is not paramount
            /// </summary>
            maxIterations: int option
        }

    let search<'a when 'a : comparison> start goal config : seq<'a> option =

        let rec reconstructPath cameFrom current =
            seq {
                yield current
                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec crawler (closedSet:HashSet<'a>) (openSet, gScores : IDictionary<'a, float>, fScores : IDictionary<'a, float>, cameFrom) =
            match config.maxIterations with 
            | Some n when n = closedSet.Count -> None
            | _ ->
                // TODO: optimize sort using a better data structure
                match List.sortBy (fun n -> fScores.[n]) openSet with
                | current::_ when current = goal -> Some <| reconstructPath cameFrom current 
                | current::rest ->
                    let gScore = gScores.[current]
                    let next =
                        config.neighbours current 
                        |> Seq.filter (fun n -> closedSet.Contains(n) |> not)
                        |> Seq.fold (fun (openSet, gScores : IDictionary<'a, float>, fScores : IDictionary<'a, float>, cameFrom) neighbour ->
                            let tentativeGScore = gScore + config.gCost current neighbour
                            if List.contains neighbour openSet && tentativeGScore >= gScores.[neighbour]
                            then (openSet, gScores, fScores, cameFrom)
                            else
                                let newOpenSet = if List.contains neighbour openSet then openSet else neighbour::openSet
                                if gScores.ContainsKey neighbour then
                                    gScores.[neighbour] = tentativeGScore |> ignore
                                else
                                    gScores.Add(neighbour, tentativeGScore)

                                let ns =(tentativeGScore + config.fCost neighbour goal)
                                if fScores.ContainsKey neighbour then
                                    fScores.[neighbour] = ns |> ignore
                                else
                                    fScores.Add(neighbour, ns)

                                let newCameFrom = Map.add neighbour current cameFrom
                                newOpenSet, gScores, fScores, newCameFrom
                            ) (rest, gScores, fScores, cameFrom)
                    closedSet.Add(current) |> ignore
                    crawler closedSet next
                | _ -> None

        let gScores = new Dictionary<'a, float>()
        gScores.Add(start, 0.)
        let fScores = new Dictionary<'a, float>()
        fScores.Add(start, config.fCost start goal)
        crawler (new HashSet<'a>()) ([start], gScores, fScores, Map.empty)