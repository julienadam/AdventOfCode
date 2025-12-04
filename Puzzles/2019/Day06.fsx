#time "on"
#load "../../Tools.fs"

#r "nuget: NFluent"
open System
open System.IO
open System.Security.Cryptography.X509Certificates
open NFluent
open AdventOfCode
open Checked

let getInput name =
    File.ReadLines(getInputPath2019 name)
    |> Seq.map (ssplit2 ")")
    |> Seq.groupBy fst
    |> Seq.map (fun (a, b) -> a, b |> Seq.map snd |> Seq.toArray)
    |> Map.ofSeq

let solve1 input =
    let orbitsMap = input |> getInput
    let rec countOrbits location (count:int) =
        match orbitsMap |> Map.tryFind location with
        | None ->
            count
        | Some satellites ->
            let result = count + (satellites |> Seq.sumBy (fun s -> countOrbits s (count + 1)))
            result
    countOrbits "COM" 0

Check.That(solve1 "Day06_sample1.txt").IsEqualTo(42)

solve1 "Day06.txt"

let getInput2 name =
    // Build a map of body -> parent body
    File.ReadLines(getInputPath2019 name)
    |> Seq.map (ssplit2 ")")
    |> Seq.map (fun (a,b) -> b,a)
    |> Map.ofSeq

let getPathFrom (orbits:Map<string,string>) origin =
    // Build a list of bodies from the origin to "COM"
    // each body is mapped to its distance from origin
    let rec fly location (path:Map<string, int>) distance =
        match location with
        | "COM" -> (path |> Map.add location distance)
        | l -> fly orbits[l] (path |> Map.add location distance) (distance + 1) 
    fly origin Map.empty 0
    
let transfersToOtherBody (orbits:Map<string,string>) otherPath origin =
    // Fly from the origin to the first body that was on the other path
    let rec fly location distance =
        match (otherPath |> Map.tryFind location) with
        | Some distToOther ->
            // Found a body that was on the other's path
            // Compute the total distance to find the number of
            // orbit transfers (remove 2 because origin and target are not bodies)
            (distToOther - 1) + (distance - 1)
        | None -> fly orbits[location] (distance + 1) 
            
    fly origin 0

let solve2 input =
    let orbits = getInput2 input
    
    let santaPath = getPathFrom orbits "SAN"
    transfersToOtherBody orbits santaPath "YOU"
    
Check.That(solve2 "Day06_sample2.txt").IsEqualTo(4)
solve2 "Day06.txt"