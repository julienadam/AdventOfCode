#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#r "nuget: XPlot.Plotly"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic

type Piece = {
    id : int
    cells : (int*int*int) list
    color: Drawing.Color
}

let parseLine line =
    let end1,end2 = line |> ssplit2 "~"
    end1 |> splitIntList |> tupleize3, end2 |> splitIntList |> tupleize3

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine

/// Transforms a brick expressed as its end points into a set of (x,y,z) cells
/// Hopefully this will work because the input bricks are at most 5 cells longs
let endsToList (x1,y1,z1) (x2,y2,z2) = 
    seq {
        for x in [min x1 x2 .. max x1 x2] do
            for y in [min y1 y2 .. max y1 y2] do
                for z in [min z1 z2 .. max z1 z2] do
                    yield (x,y,z)
    } |> List.ofSeq |> List.sortBy thd3

let getHeightAt x y (heightMap:IDictionary<int*int, int>) =
    match heightMap.TryGetValue((x,y)) with
    | true, z -> z
    | false, _ -> 0

let downBy height = List.map (fun (x,y,z) -> (x,y,z-height))

open XPlot.Plotly

let display (bricks:Piece list) = 
    
    let traces = bricks |> List.map (fun piece ->
        let x = piece.cells |> List.map fst3
        let y = piece.cells  |> List.map snd3
        let z = piece.cells  |> List.map thd3

        Scatter3d(
            x = x, y = y, z = z,
            name = sprintf "Piece #%i" piece.id,
            mode = "markers",
            marker =
                Marker(
                    symbol = "square",
                    size = 20,
                    color = sprintf "rgba(%i, %i, %i,0.8)" piece.color.R piece.color.G piece.color.B,
                    line = Line(width = 0.5),
                    opacity = 0.8
                )
        )
    )

    let chart =
        traces
        |> Chart.Plot
        |> Chart.WithWidth 1900
        |> Chart.WithHeight 900

    chart.Show()

// display bricksByAscZOrder
let letAllPiecesFall pieces isCount stopAtFirst =
    let heightMap = new Dictionary<int*int, int>()
    let mutable falls = 0
    let rec letAllPiecesFallRec (processing: Piece list) (remaining: Piece list)=
        match processing with
        | [] -> 
            remaining, falls
        | head::tail ->
            // Find the collision points with the height map
            let (_,_,colz), h = 
                head.cells 
                |> List.map (fun (x,y,z) ->(x,y,z), heightMap |> getHeightAt x y)
                |> Seq.maxBy snd
            let fallBy = colz-h-1
            if stopAtFirst && fallBy > 0 then
                [], 1
            else
                if isCount && fallBy > 0 then
                    falls <- falls + 1
                // Put the brick down
                let restingBrick = (head.cells |> downBy fallBy)
                // Update the heightmap
                restingBrick |> Seq.iter(fun (x,y,z) -> heightMap[(x,y)] <- z)
                let restingPiece = { id = head.id; cells = restingBrick; color = head.color }
                // Rinse and repeat
                letAllPiecesFallRec tail (restingPiece::remaining)
    letAllPiecesFallRec pieces []

let getRestingState input = 
    let rnd = new Random(12345)
    let mutable idGen = 0

    let bricksByAscZOrder = 
        getInput input 
        |> Seq.map (fun (a,b) -> 
            idGen <- idGen + 1
            {
                id = idGen
                cells = endsToList a b
                color = System.Drawing.Color.FromArgb(rnd.Next()) 
            })
        |> Seq.sortBy (fun p -> p.cells.Head |> thd3)
        |> Seq.toList

    let allRestingPiecesInZOrder = 
        letAllPiecesFall bricksByAscZOrder false false 
        |> fst 
        |> List.sortBy (fun p -> p.cells.Head |> thd3)
    
    allRestingPiecesInZOrder
    // display allRestingPiecesInZOrder

let solve1 input =
    let allRestingPiecesInZOrder = input |> getRestingState
    allRestingPiecesInZOrder 
    |> Seq.mapi(fun i p -> p.id, allRestingPiecesInZOrder |> List.removeAt i)
    |> Seq.filter(fun (idRemoved, pieces) -> 
        match letAllPiecesFall pieces false true with
        | [], 1 -> false
        | _ -> true)
    |> Seq.length
    
Check.That(solve1 "Day22_sample1.txt").IsEqualTo(5) |> Dump
solve1 "Day22.txt" |> Dump

let solve2 input =
    let allRestingPiecesInZOrder = input |> getRestingState
    allRestingPiecesInZOrder 
    |> Seq.mapi(fun i p -> p.id, allRestingPiecesInZOrder |> List.removeAt i)
    |> Seq.sumBy(fun (_, pieces) -> 
        letAllPiecesFall pieces true false |> snd
    )

Check.That(solve2 "Day22_sample1.txt").IsEqualTo(7) |> Dump
solve2 "Day22.txt" |> Dump
