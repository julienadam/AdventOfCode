open System.Collections.Generic

#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: Colorful.Console"

open System
open System.IO
open AdventOfCode
open Array2DTools
open System.Drawing

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let mutable idGenerator = 0

type Region = {
    mutable area: int64
    mutable perimeter: int64
    plant: char
    id: int
}

type Cell = { plant: char; regionId: int option }

let printMapColor (cells:Cell[,]) =
    let r = new Random();
    let colorMap = 
        cells 
        |> Array2DTools.enumArray2d 
        |> Seq.map (fun (_,_,cell) -> cell.regionId) 
        |> Seq.distinct
        |> Seq.map (fun regionId -> regionId.Value, Color.FromArgb(r.Next(256), r.Next(256), r.Next(256)))
        |> Map.ofSeq

    for r = 0 to cells |> maxR do
        for c = 0 to cells |> maxC do
            let cell = cells[r,c]
            let color = colorMap[cell.regionId.Value]
            Colorful.Console.Write(cell.plant, color)
        printfn ""
    ()

let solve1 input =
    let regions = new System.Collections.Generic.Dictionary<int, Region>();
    let map = getInput input |> Array2D.map (fun v -> { plant = v; regionId = None})
    let otherPlots = new HashSet<int*int>()
    
    let rec walkRec (r,c) =
        otherPlots.Remove(r,c) |> ignore
        let cell = map[r,c]
        match cell.regionId with
        | None -> 
            let neighbors = 
                map
                |> Array2DTools.getAdjacent r c  |> Seq.toArray
            let otherNeighbors = neighbors |> Seq.filter (fun (_,_,av) -> av.plant <> cell.plant)
            otherNeighbors |> Seq.iter (fun (ar,ac,_) -> otherPlots.Add(ar,ac) |> ignore)
            let neighboringPlots = neighbors |> Seq.filter (fun (_,_,av) -> av.plant = cell.plant)
            let region = 
                match neighboringPlots |> Seq.tryPick (fun (ar,rc,av) -> av.regionId) with
                | Some regId -> regions[regId]
                | None ->
                    idGenerator <- idGenerator + 1
                    let region = { id = idGenerator; area = 0; perimeter = 0; plant = cell.plant }
                    regions.Add(region.id, region)
                    region
            region.area <- region.area + 1L
            region.perimeter <- region.perimeter + (4L - (neighboringPlots |> Seq.length |> int64))
            Array2D.set map r c { cell with regionId = Some region.id }
            neighboringPlots |> Seq.iter(fun (ar,ac,_) -> walkRec (ar,ac))
        | Some _ -> ()
     
    otherPlots.Add(0,0) |> ignore
    while otherPlots.Count <> 0 do
        walkRec(otherPlots |> Seq.head)

    // printMapColor map

    regions  //|> Seq.iter (fun kvp -> printfn "Region %i of type %c area %i perimeter %i and price %i" kvp.Key kvp.Value.plant kvp.Value.area kvp.Value.perimeter (kvp.Value.area * kvp.Value.perimeter))
    |> Seq.sumBy (fun kvp -> kvp.Value.perimeter * kvp.Value.area)


solve1 "Day12_sample2.txt"
