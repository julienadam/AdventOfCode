#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open NFluent
open AdventOfCode
open AdventOfCode.Array2DTools
open System.Collections.Generic
open System.IO

let getInput name = 
    let grid = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (fun line -> line.ToCharArray())
        |> array2D
    // Plug the entrance and exit cells so that
    // bounds checking becomes irrelevant
    grid[0,1] <- '#'
    grid[(grid |> maxR), ((grid |> maxC) - 1)] <- '#'
    grid

let adjacentWithSlopes (grid:char array2d) (r, c) =
    match grid[r,c] with
    | '^' -> [ r - 1, c ]
    | '>' -> [ r, c + 1 ]
    | 'v' -> [ r + 1, c ]
    | '<' -> [ r, c - 1 ]
    | '.' -> [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]
    | x -> failwithf "Not a valid cell %c at (%i,%i)" x r c
    |> List.filter (fun (r, c) -> grid[r,c] <> '#')

type Candidate = {
    path: Set<int*int>
    length: int
    current: int*int
}

let findLongest grid getAdjacent getCost source dest =
    let queue = Queue<Candidate>()
    queue.Enqueue({ path = Set.empty; length = 0; current = source})
    let mutable longest = 0

    let rec walk () =
        match queue.TryDequeue() with
        | false, _ -> 
            // No more steps
            longest + 2 // add 2 because we removed the entrance and exit cells
        | true, candidate when candidate.current = dest ->
            // Destination reached, update maximum
            longest <- max longest candidate.length
            walk ()
        | true, candidate when candidate.path.Contains(candidate.current) -> 
            // We already visited this one, skip it
            walk ()
        | true, candidate ->
            // Get the adjacent cells and walk
            getAdjacent grid candidate.current
            |> Seq.iter (fun next -> 
                let n = { 
                        candidate with 
                            path = candidate.path |> Set.add candidate.current
                            length = candidate.length + getCost candidate.current next
                            current = next
                        }
                queue.Enqueue(n)
            )
            walk ()
    walk ()

let solve1 name =
    let grid = getInput name
    let start = 1,1
    let target = (grid |> maxR) - 1, (grid |> maxC) - 1
    findLongest grid adjacentWithSlopes (fun _ _ -> 1) start target

Check.That(solve1 "Day23_sample1.txt").IsEqualTo(94)
solve1 "Day23.txt"

let solve2 name =
    let grid = getInput name
    let start = 1,1
    let target = (grid |> maxR) - 1, (grid |> maxC) - 1

    let getAllAdjacent (grid:char array2d) r c =
        [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]
        |> List.filter (fun (r, c) -> grid[r,c] <> '#')

    // Intersections are all cells with more than 2 adjacent cells
    let intersections =
        seq {
            for r in 1 .. (grid |> maxR) - 1 do
                for c in 1 .. (grid |> maxC) - 1 do
                    if grid[r,c] <> '#' && List.length (getAllAdjacent grid r c) > 2 then
                        yield  r, c
        }
        |> Set.ofSeq
        |> Set.add start
        |> Set.add target

    // Find the distance between a cell and its closest intersection neighbors
    let computeDistanceToAllNeighbors source =
        let distances = Dictionary<int * int, int>()
        let queue = Queue<Set<int * int> * (int * int)>()
        queue.Enqueue(Set.empty, source)

        let rec walk () =
            match queue.TryDequeue() with
            | false, _ -> distances
            | _, (path, current) when path.Contains(current) -> walk ()
            | _, (path, current) when current <> source && intersections |> Set.contains current ->
                distances[current] <- Set.count path
                walk ()
            | _, (path, (r,c)) ->
                getAllAdjacent grid r c |> Seq.iter (fun next -> queue.Enqueue(Set.add (r,c) path, next))
                walk ()

        walk ()

    // Pre-compute all distances between all neighbors intersections
    let distanceMap = intersections |> Seq.map (fun src -> src, computeDistanceToAllNeighbors src) |> dict

    // Gets the neighbor intersections using the precomputed map
    let getAdjacentIntersections _ (r,c) = distanceMap[r,c].Keys |> seq

    // And call the path finder
    findLongest grid getAdjacentIntersections (fun src dest -> distanceMap[src][dest]) start target

Check.That(solve2 "Day23_sample1.txt").IsEqualTo(154)
solve2 "Day23.txt"