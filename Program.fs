
open System.IO
open AdventOfCode
open AdventOfCode.Array2DTools
open System.Collections.Generic

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

type AdjacentNode<'a when 'a:comparison> = {
    id: 'a
    weight : int
}

type Graph<'a when 'a:comparison> = {
    id: 'a
    adj : Map<'a, AdjacentNode<'a> list>
}

let addEdge graph src dst weight =
    let prev = match graph.adj |> Map.tryFind src with | None -> [] | Some s -> s
    { 
        graph with adj = graph.adj |> Map.add src ({ id = dst; weight = weight} :: prev)
    }

let getNonSlopeAdjacent row col (grid:char[,]) = seq {
    if row > 0 && grid.[(row - 1), col] <> 'v' then
        yield ((row - 1), col, grid.[(row - 1), col])
    if row < ((grid |> Array2D.length1) - 1) && grid.[(row + 1), col] <> '^' then
        yield ((row + 1), col, grid.[(row + 1), col])
    if col > 0 && grid.[row, (col - 1)] <> '>' then
        yield (row, (col - 1), grid.[row, (col - 1)])
    if col < ((grid |> Array2D.length2) - 1) && grid.[row, (col + 1)] <> '<' then
        yield (row, (col + 1), grid.[row, (col + 1)])
}

let buildGraph grid =
    let target = (grid |> maxR, (grid |> maxC) - 1)
    let visited = HashSet<int*int>()
    let mutable graph = { id = (0,1); adj = Map.empty }
    let rec walk r c =
        visited.Add((r,c)) |> ignore
        if (r,c) = target then
            ()
        else
            match grid[r,c] with
            | '>' -> walk r (c+1)
            | '<' -> walk r (c-1)
            | '^' -> walk (r-1) c
            | 'v' -> walk (r+1) c
            | '.' ->
                let adjacent = 
                    grid 
                    |> getNonSlopeAdjacent r c
                    |> Seq.where (fun (ar,ac,v) -> visited.Contains(ar,ac) = false && v <> '#')
                    |> Seq.map (fun (ar,ac, v) -> ar, ac)
                    |> Seq.toList

                if adjacent.IsEmpty then 
                    ()
                else
                    adjacent 
                    |> Seq.iter(fun (ar,ac) -> 
                        graph <- addEdge graph (r,c) (ar,ac) 1
                        walk ar ac)
            | _ -> failwithf "Invalid walk"
    walk 0 1

    // grid |> printGridCustom2 (fun v r c ->
    //     match longestPath.Contains (r,c) with
    //     | true -> 'O'
    //     | false -> v
    // ) |> ignore
    
let solve1 name = 
    let grid = getInput name
    buildGraph grid

solve1 "Day23_sample1.txt"

    
 
