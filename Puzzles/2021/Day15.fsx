#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Distance.fs"

open AdventOfCode
open System.IO
open AdventOfCode.Array2DTools

type Node = {
    X: int
    Y: int
    F: int 
    G: int
    Parent: Node option
}

let scorePath points (grid:int[,]) =
    points |> Seq.skip 1 |> Seq.sumBy (fun (x,y) -> grid.[x,y])

let rebuildPath (lastNode:Node)  =
    let rec walkRec node path =
        let nPos = (node.X, node.Y)
        match node.Parent with
        | Some p -> walkRec p (nPos::path)
        | None -> (nPos::path)
            
    walkRec lastNode []

let rec pathfindingAStarRec targetX targetY grid (openNodes:Node list) (closedNodes:Map<int*int, Node>) =
    match openNodes with
    | [] -> failwithf "No path found"
    | _ -> 
        let q = openNodes |> Seq.minBy (fun n -> n.F)
        if (q.X, q.Y) = (targetX, targetY) then
            rebuildPath q
        else
            let closed = closedNodes |> Map.add (q.X, q.Y) q
            let openn = openNodes |> List.filter (fun n -> n <> q)
            let adjacentNonClosed = getAdjacent q.X q.Y grid |> Seq.filter (fun (i,j,_) -> closedNodes |> Map.containsKey (i,j) |> not)
                
            let nextOpenNodes = 
                adjacentNonClosed 
                |> Seq.choose (fun (i,j,v) ->
                        
                    // Distance calc
                    let g = q.G + v
                        
                    // Simplistic heuristic
                    let h = manhattanDistance i j targetX targetY
                        
                    match openNodes |> List.tryFind (fun e -> e.X = i && e.Y = j && g > e.G) with
                    | Some _ -> None
                    | _ -> 
                        // Build child node
                        Some { X = i; Y = j; F = h + g; G = g; Parent = Some q }
                )
                |> Seq.toList

            let nextOpen = List.concat [nextOpenNodes; openn]
                
            pathfindingAStarRec targetX targetY grid nextOpen closed 

let pathfindingAStar (start: int*int) (goal: int*int) grid =
    let x,y = start
    let targetX, targetY = goal
    pathfindingAStarRec targetX targetY grid [{X = x; Y= y; F = 0; G = 0; Parent = None }] Map.empty

module Part1 =

    let Solve input = 
        let path = getInputPath input
        let mapline = Seq.map (string >> int)
        let input = File.ReadLines(path) |> Seq.map mapline |> array2D
        printfn "%A" input

        let bestPath = pathfindingAStar (0,0) (Array2D.length1 input - 1, Array2D.length2 input - 1) input 
        let score = scorePath bestPath input
        printfn "%i with path %A" score bestPath

//Part1.Solve "day15_sample1.txt"
//Part1.Solve "day15.txt"

module Part2 =
    let Solve input = 
        let path = getInputPath input
        let mapline = Seq.map (string >> int)
        let input = File.ReadLines(path) |> Seq.map mapline |> array2D
        let size = Array2D.length1 input 
        
        let finalInput = Array2D.create (size*5) (size*5) 0
        for i in [0..4] do
            for j in [0..4] do
                let inputRiskUpdated = input |> Array2D.copy |> Array2D.map (fun v -> 
                    let x = (v + i + j)
                    if x > 9 then (x % 10 + 1) else x)
                Array2D.blit inputRiskUpdated 0 0 finalInput (i * size) (j * size) size size

        // printGrid finalInput
        let bestPath = pathfindingAStar (0,0) (Array2D.length1 finalInput - 1, Array2D.length2 finalInput - 1) finalInput 
        let score = scorePath bestPath finalInput
        printfn "%i with path %A" score bestPath

Part2.Solve "day15.txt"