#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic

type Piece = {
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

#r "nuget: XPlot.Plotly"
open XPlot.Plotly

let display (bricks:Piece list) = 
    
    let traces = bricks |> List.map (fun piece ->
        let x = piece.cells |> List.map fst3
        let y = piece.cells  |> List.map snd3
        let z = piece.cells  |> List.map thd3
        
        Scatter3d(
            x = x,
            y = y,
            z = z,
            mode = "markers",
            marker =
                Marker(
                    symbol = "square",
                    size = 20,
                    color = sprintf "rgba(%i, %i, %i,0.8)" piece.color.R piece.color.G piece.color.B,
                    line =
                        Line(
                            //color = "rgba(217, 217, 217, 0.14)",
                            // color = "rgba(217, 217, 217,0)", //sprintf  piece.color.R piece.color.G piece.color.B,
                            width = 0.5
                        ),
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

let solve1 input =
    let rnd = new Random(12345)

    let bricksByAscZOrder = 
        getInput input 
        |> Seq.map (fun (a,b) -> 
            {
                cells = endsToList a b
                color = System.Drawing.Color.FromArgb(rnd.Next()) 
            })
        |> Seq.sortBy (fun p -> p.cells.Head |> thd3)
        |> Seq.toList
    
    // display bricksByAscZOrder
    let heightMap = new Dictionary<int*int, int>()

    let rec fall (processing: Piece list) (remaining: Piece list) =
        // display (List.concat [processing;remaining])
        // Console.ReadLine() |> ignore
        match processing with
        | [] -> 
            remaining
        | head::tail ->
            printfn "Processing piece %A" head.cells
            // Find the collision points with the height map
            let (colx,coly,colz), h = 
                head.cells 
                |> List.map (fun (x,y,z) ->(x,y,z), heightMap |> getHeightAt x y)
                |> Seq.maxBy snd
            printfn "Height map collision at (%i,%i,%i) height %i" colx coly colz h
            printfn "Down by %i" (colz-h-1)

            // Put the brick down
            let restingBrick = (head.cells |> downBy (colz-h-1))
            printfn "Processed piece %A" restingBrick

            // Update the heightmap
            restingBrick |> Seq.iter(fun (x,y,z) -> heightMap[(x,y)] <- z)
            let restingPiece = { cells = restingBrick; color = head.color }
            // Rinse and repeat
            fall tail (restingPiece::remaining)
    
    let allRestingPieces = fall bricksByAscZOrder []
    // display allRestingPieces
    allRestingPieces |> Dump

solve1 "Day22_sample1.txt"

