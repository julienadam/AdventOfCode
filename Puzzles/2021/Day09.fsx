#load "../../Tools.fsx"

open System
open System.IO
open Tools

let path = getInputPath "day09.txt"
//let path = getInputPath "day09_sample1.txt"

let input = 
    File.ReadAllLines(path) 
    |> Array.map (fun s -> s |> Seq.map (fun c -> (int c) - (int '0')) |> Seq.toArray) 
    |> array2D 

let getAdjacent row col (grid:int[,]) = seq {
    if row > 0 then
        yield ((row - 1), col, grid.[(row - 1), col])
    if row < ((grid |> Array2D.length1) - 1) then
        yield ((row + 1), col, grid.[(row + 1), col])
    if col > 0 then
        yield (row, (col - 1), grid.[row, (col - 1)])
    if col < ((grid |> Array2D.length2) - 1) then
        yield (row, (col + 1), grid.[row, (col + 1)])
}
    
let enumArray2d (array:'a[,]) = seq {
    for i = 0 to (array |> Array2D.length1) - 1 do
        for j = 0 to (array |> Array2D.length2) - 1 do
            yield i,j, array.[i,j]
}

module Part1 =
   
    let Solve () =
        input 
        |> enumArray2d 
        |> Seq.filter (fun (i,j,v) -> (getAdjacent i j input) |> Seq.forall (fun (_,_,x) -> x > v))
        |> Seq.map (fun (_,_,v) -> v + 1)
        |> Seq.sum
        |> Dump
    ()
    
Part1.Solve()

module Part2 =

    let rec floodfillRec (visited:Set<int*int>) i j (grid:int[,]) =
        
        let updatedVisited = visited |> Set.add (i,j)
        
        grid 
        |> getAdjacent i j 
        |> Seq.where (fun (x,y,v) -> v < 9 && (updatedVisited |> Set.contains (x,y) |> not))
        |> Seq.fold (fun currentVisited (nextI, nextJ, _) -> Set.union currentVisited (floodfillRec currentVisited nextI nextJ grid)) updatedVisited
        
    let floodfill i j grid = floodfillRec Set.empty i j grid |> Set.count

    let Solve () =
        let lowPoints = 
            input 
            |> enumArray2d 
            |> Seq.filter (fun (i,j,v) -> (getAdjacent i j input) |> Seq.forall (fun (_,_,x) -> x > v))
            // |> Dump
        
        let top3Basins = lowPoints |> Seq.map (fun (i,j,_) -> input |> floodfill i j ) |> Seq.sortDescending |> Seq.take 3 |> Seq.toArray
        (top3Basins.[0] * top3Basins.[1] * top3Basins.[2]) |> Dump
        
Part2.Solve()
