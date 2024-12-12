#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: Colorful.Console"

open System
open System.IO
open AdventOfCode
open Array2DTools

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let mutable idGenerator = 0

type Region = {
    mutable area: int64
    mutable perimeter: int64
    plant: char
    id: int
}

type Cell = { plant: char; regionId: int option }

open System.Drawing

let printMapColor (cells:Cell[,]) =
    let r = new Random(42);
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
    map
    |> Array2D.iteri (fun r c v ->
        let neighboringPlots = 
            map
            |> Array2DTools.getAdjacent r c 
            |> Seq.filter (fun (_,_,av) -> av.plant = v.plant)
        
        let region = 
            match neighboringPlots |> Seq.tryPick (fun (ar,rc,av) -> av.regionId) with
            | Some regId -> regions[regId]
            | None ->
                idGenerator <- idGenerator + 1
                let region = { id = idGenerator; area = 0; perimeter = 0; plant = v.plant }
                regions.Add(region.id, region)
                region

        region.area <- region.area + 1L
        region.perimeter <- region.perimeter + (4L - (neighboringPlots |> Seq.length |> int64))
        Array2D.set map r c { v with regionId = Some region.id }
    )

    printMapColor map

    regions 
    |> Seq.iter (fun kvp -> 
        printfn "Region %i of type %c area %i perimeter %i and price %i" kvp.Key kvp.Value.plant kvp.Value.area kvp.Value.perimeter (kvp.Value.area * kvp.Value.perimeter))
    // |> Seq.sumBy (fun kvp -> kvp.Value.perimeter * kvp.Value.area)


solve1 "Day12_sample2.txt"

// cell
  // plant type i.e. character
  // region id option, if no region, it was not checked yet

// region
  // area
  // perimeter

//match v.regionId with
     //| Some reg -> 
     //    let region = regions[reg]
     //    // TODO : handle case where another region is next to this one ?
     //    let neighboringPlots = 
     //        map 
     //        |> Array2DTools.getAdjacent r c 
     //        |> Seq.filter (fun (_,_,av) -> av.plant = v.plant)
     //        |> Seq.length
     //    region.area <- region.area + 1L
     //    region.perimeter <- region.perimeter + (neighboringPlots |> int64)
     //| None -> 
     //    let id = Guid.NewGuid()
     //    let region = { area = 0; perimeter = 0; plant = v.plant; id = id}
     //    regions.Add(id,region) |> ignore
         

         
         //let mutable r1 = { area = 0; perimeter = 0; plant = 'C' }
         
         //let cA = { plant = 'C'; region = ref r1}
         
         //cA.region.contents.area <- 1
         
         //r1 |> Dump
         //cA |> Dump