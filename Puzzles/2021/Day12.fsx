#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let path = getInputPath "day12.txt"
//let path = getInputPath "day12_sample1.txt"
//let path = getInputPath "day12_sample2.txt"
//let path = getInputPath "day12_sample3.txt"

type Cave =
| Start
| Small of string
| Large of string
| End

let mapLine (input:string) = 
    let mapCave s = 
        match s with 
        | "start" -> Start
        | "end" -> End
        | s when s.ToUpper() = s -> Large s
        | s when s.ToLower() = s -> Small s
        | s -> failwithf "Invalid cave id %s" s
    
    let split = input.Split("-")
    (split.[0] |> mapCave), (split.[1] |> mapCave)
    
let caveMap = 
    File.ReadLines(path) 
    |> Seq.map mapLine
    |> Seq.collect (fun (x,y)-> [x,y;y,x])
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.toList)
    |> Map.ofSeq

let printPath (p:Cave list) =
    p |> List.rev |> List.iter (fun c ->
        match c with
        | Start -> printf "Start->"
        | End -> printfn "End"
        | Small c -> printf "%s->" c
        | Large c -> printf "%s->" c)

let printPaths (ps:Cave list list) = ps |> Seq.iter printPath
    

module Part1 =
    
    let rec exploreMap (currentPath:Cave list) (caveMap:Map<Cave, Cave list>) (exploredPaths:Cave list list)=
        match currentPath with
        | [] ->
            exploreMap [Start] caveMap exploredPaths
        | End :: _ ->
            currentPath :: exploredPaths
        | cave:: _ ->
            let nextCaves = caveMap.[cave]
            let exploring =
                nextCaves |> List.collect (fun p ->
                match p with
                | Start -> []
                | Large _ -> exploreMap (p :: currentPath) caveMap exploredPaths
                | End -> exploreMap (p :: currentPath) caveMap exploredPaths
                | Small _ -> 
                    if (currentPath |> List.contains p) then
                        []
                    else
                        exploreMap (p :: currentPath) caveMap exploredPaths
                )
            
            List.concat [exploredPaths;exploring]
    
    let Solve() = 
        let exploredPaths = exploreMap List.empty caveMap List.empty
        exploredPaths |> printPaths
        exploredPaths |> Seq.length |> Dump
        ()
        
Part1.Solve()

module Part2 =
    
    let rec exploreMap (currentPath:Cave list) (caveMap:Map<Cave, Cave list>) (exploredPaths:Cave list list) smallExploDone =
        match currentPath with
        | [] ->
            exploreMap [Start] caveMap exploredPaths smallExploDone
        | End :: _ ->
            currentPath :: exploredPaths
        | cave:: _ ->
            let nextCaves = caveMap.[cave]
            let exploring =
                nextCaves |> List.collect (fun p ->
                match p with
                | Start -> []
                | Large _ -> exploreMap (p :: currentPath) caveMap exploredPaths smallExploDone
                | End -> exploreMap (p :: currentPath) caveMap exploredPaths smallExploDone
                | Small smol -> 
                    let timesExplored = currentPath |> List.filter (fun x -> x = p) |> List.length
                    if (timesExplored = 2) || (timesExplored = 1 && smallExploDone) then
                        // Already explored twice or already explored another small cave twice
                        []
                    else if timesExplored = 1 && (not smallExploDone) then
                        // Explore this cave twice
                        exploreMap (p :: currentPath) caveMap exploredPaths true
                    else if timesExplored > 2 then
                        // Should never happen
                        failwithf "passed in %s %i times" smol timesExplored
                    else
                        // Otherwise, explore normally
                        exploreMap (p :: currentPath) caveMap exploredPaths smallExploDone
                )
            
            List.concat [exploredPaths;exploring]
    
    let Solve() = 
        let exploredPaths = exploreMap List.empty caveMap List.empty false
        // exploredPaths |> printPaths
        exploredPaths |> Seq.length |> Dump
        ()
        
Part2.Solve()
