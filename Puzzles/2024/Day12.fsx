open System.Collections.Generic

#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: Colorful.Console"
#r "nuget: NFluent"

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


solve1 "Day12.txt"


type Region2 = {
    mutable segments : ((int*int)*(int*int)) list
    mutable area: int
    plant: char
    id: int
}

// Merges contiguous segments
let reduceSegments (segments:((int*int)*(int*int)) seq) =
    let rec reduceRec (remaining:((int*int)*(int*int)) Set) =
        let alignedSegments = 
            remaining |> Seq.tryPick (fun ((sr1,sc1), end1) ->
                // If there's a fork, don't merge with segments, it's an 8 shape like the last example
                match remaining |> Set.filter (fun (s, e) -> s = end1) |> Seq.length with
                | 1 -> 
                    // If not, find another segment starting at the end of the first that is on the same row or column
                    let found = 
                        remaining 
                        |> Seq.tryFind(fun (start2, (er2,ec2)) ->(end1 = start2) && (sr1=er2 || sc1 = ec2))
                    match found with
                    | None -> 
                        None
                    | Some (start2, end2) -> 
                        // we found adjacent contiguous segments !
                        Some (((sr1,sc1), end1), (start2, end2))
                | _ -> 
                    None
                )
        match alignedSegments with
        | Some ((s1,e1), (s2,e2)) -> 
            // remove the individual segments
            // add the merged segment
            let next = 
                remaining 
                |> Set.remove (s1,e1) 
                |> Set.remove (s2,e2) 
                |> Set.add((s1,e2))
            // Rinse & repeat
            reduceRec next
        | None -> remaining

    reduceRec (segments |> Set.ofSeq)

let solve2 input =
    let regions = new System.Collections.Generic.Dictionary<int, Region2>();
    let map = getInput input |> Array2D.map (fun v -> { plant = v; regionId = None})
    let otherPlots = new HashSet<int*int>()
    
    // treat each plot as a square defined by 4 segments, one for each side
    // walk in the same way as part 1
    // add each non adjacent side segment to a list maintained at the region level
    // in the end, merge contiguous segments and count

    let rec walkRec (r,c) =
        otherPlots.Remove(r,c) |> ignore
        let cell = map[r,c]
        match cell.regionId with
        | None ->
            let region = 
                match map |> Array2DTools.getAdjacent r c |> Seq.filter(fun (ar,ac,av) -> av.plant = cell.plant) |> Seq.tryPick (fun (ar,rc,av) -> av.regionId) with
                | Some regId -> regions[regId]
                | None ->
                    idGenerator <- idGenerator + 1
                    let newRegion = { id = idGenerator; segments = []; area = 0; plant = cell.plant }
                    regions.Add(newRegion.id, newRegion)
                    newRegion

            region.area <- region.area + 1
            Array2D.set map r c { cell with regionId = Some region.id }

            match map |> Array2DTools.tryGetUp r c with
            | Some (ar,ac, av) when av.plant = cell.plant -> 
                walkRec (ar,ac)
            | None ->
                region.segments <- ((r,c),(r,c+1))::region.segments
            | Some (ar,ac, _) -> 
                region.segments <- ((r,c),(r,c+1))::region.segments
                otherPlots.Add((ar,ac)) |> ignore
            
            match map |> Array2DTools.tryGetDown r c with
            | Some (ar,ac, av) when av.plant = cell.plant -> 
                walkRec (ar,ac)
            | None ->
                region.segments <- ((r+1,c),(r+1,c+1))::region.segments
            | Some (ar,ac, _) -> 
                region.segments <- ((r+1,c),(r+1,c+1))::region.segments
                otherPlots.Add((ar,ac)) |> ignore
            
            match map |> Array2DTools.tryGetLeft r c with
            | Some (ar,ac, av) when av.plant = cell.plant -> 
                walkRec (ar,ac)
            | None ->
                region.segments <- ((r,c),(r+1,c))::region.segments
            | Some (ar,ac, _) -> 
                region.segments <- ((r,c),(r+1,c))::region.segments
                otherPlots.Add((ar,ac)) |> ignore

            match map |> Array2DTools.tryGetRight r c with
            | Some (ar,ac, av) when av.plant = cell.plant -> 
                walkRec (ar,ac)
            | None ->
                region.segments <- ((r,c+1),(r+1,c+1))::region.segments
            | Some (ar,ac, _) -> 
                region.segments <- ((r,c+1),(r+1,c+1))::region.segments
                otherPlots.Add((ar,ac)) |> ignore
        | Some _ -> ()
     
    otherPlots.Add(0,0) |> ignore
    while otherPlots.Count <> 0 do
        walkRec(otherPlots |> Seq.head)
    
    regions
    |> Seq.sumBy (fun kvp -> (kvp.Value.segments |> reduceSegments |> Seq.length) * kvp.Value.area)

open NFluent

// ABCDE one
Check.That(solve2 "Day12_sample1.txt").Equals(80)

// Larger example
Check.That(solve2 "Day12_sample2.txt").Equals(1206)

// O & X
Check.That(solve2 "Day12_sample3.txt").Equals(436)

// E & X
Check.That(solve2 "Day12_sample4.txt").Equals(236)

// A & B
Check.That(solve2 "Day12_sample5.txt").Equals(368)

solve2 "Day12.txt"
