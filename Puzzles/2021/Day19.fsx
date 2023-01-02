#r "nuget: MathNet.Numerics.FSharp"

// Brute force with _some_ pruning
// Needs a heuristic to find closest scanners

open System.Diagnostics
open System
open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

#load "../../Tools.fs"

open AdventOfCode
open System.IO

type CoordinatesTransform = {
    Rotation: Matrix<float>
    Translation: Vector<float>
}

type Scanner = {
    Id: int
    Beacons: Vector<float> list
    Position : Vector<float> option
}

let mapPosLine (l:string) =
    l.Split(",") 
        |> Array.map (fun l -> 
            let r, i = Int32.TryParse(l)
            if not r then
                failwithf "Invalid input %s" l
            else
                i |> float
            )
    |> vector

let getInput fileName = 
    let inputStr = getInputPath fileName |> File.ReadAllText
    inputStr.Split("\r\n\r\n")
    |> Array.map (fun lines -> lines.Split("\r\n") |> Array.skip 1)
    |> Array.map (fun positions -> positions |> Array.map mapPosLine |> Array.toList)
    |> Array.mapi (fun id beacons -> { Beacons = beacons; Id = id; Position = None})
    |> Array.toList

let racos multipleOfPi =
    match multipleOfPi with
    | 0 -> 1.0
    | 1 -> 0.0
    | 2 -> -1.0
    | 3 -> 0.0
    | _ -> failwithf "Invalid multiplier of pi"

let rasin multipleOfPi =
    match multipleOfPi with
    | 0 -> 0.0
    | 1 -> 1.0
    | 2 -> 0.0
    | 3 -> -1.0
    | _ -> failwithf "Invalid multiplier of pi"

let genRotations ()= seq {
    for rx = 0 to 3 do
        let mx = [
            [1.0; 0.0     ; 0.0]
            [0.0; racos rx; -(rasin rx)]
            [0.0; rasin rx; racos rx]
        ] 
        for ry = 0 to 3 do
            let my = [
                [racos ry   ; 0.0; rasin ry]
                [0.0        ; 1.0; 0.0]
                [-(rasin ry); 0.0; racos ry]
            ]
            for rz = 0 to 3 do
                let mz = [
                    [racos rz; -(rasin rz); 0.0]
                    [rasin rz; racos rz   ; 0.0]
                    [0.0     ; 0.0        ; 1.0]
                ]

                yield (matrix mx) * (matrix my) * (matrix mz)
}

let rotations = genRotations() |> Seq.distinct

let have12BeaconsInCommonForRotation (scannerA: Vector<float> list) (scannerB: Vector<float> list) (rot:Matrix<float>) =
    let rotatedB = scannerB |> List.map (fun v -> v * rot)
    List.allPairs scannerA rotatedB |> List.tryPick (fun (pA, pB) -> 
        let trans = pB - pA
        let mappedA =
            scannerA 
            |> Seq.where (fun p -> p <> pA) 
            |> Seq.map (fun p -> p + trans)
            |> Seq.map(fun p -> rotatedB |> List.contains p)
            |> Seq.filter id
            |> Seq.length
        
        if mappedA >= 11 then
            Some (-trans, rot)
        else
            None
    )

let findTransformationMatrixIfAny scannerA scannerB =
    rotations |> Seq.tryPick (fun rot -> have12BeaconsInCommonForRotation scannerA scannerB rot)
    
let sprintVec (v:Vector<float>) = sprintf "(%0.0f,%0.0f,%0.0f)" v.[0] v.[1] v.[2]
let printVec v = printfn "%s" (sprintVec v)

let applyTransformation t v = (v * t.Rotation + t.Translation)

let printScanner scanner =
    printfn "Scanner %i" scanner.Id
    scanner.Beacons |> Seq.iter printVec

let transformScanner scanner transform =
    { scanner with Beacons = scanner.Beacons |> List.map(fun p -> applyTransformation transform p) }


let mutable iteration = 0
let rec findAllBeacons (scannersFound:Scanner list) (remainingScanners: Scanner list) (visited:(int*int) Set)=
    iteration <- iteration + 1
    printfn "Iteration %i Scanners found %i Scanners remaining %i. Visited : %i" iteration scannersFound.Length remainingScanners.Length visited.Count

    match remainingScanners with
    | [] -> scannersFound
    | _ ->
        let mutable newVisited = Set.empty
        let filteredPairs = 
            List.allPairs scannersFound remainingScanners
            |> List.filter (fun (s1,s2) -> visited.Contains((min s1.Id s2.Id), (max s1.Id s2.Id)) |> not)
        
        printfn "Searching in %i pairs" filteredPairs.Length

        let matchFound =
            filteredPairs
            |> List.tryPick (fun (scannerA, scannerB) -> 
                let ids = (min scannerA.Id scannerB.Id), (max scannerA.Id scannerB.Id)
                if newVisited |> Set.contains ids then
                    printfn "Already visited"
                    None
                else
                    newVisited <- visited |> Set.add ids
                    match findTransformationMatrixIfAny scannerA.Beacons scannerB.Beacons with
                    | Some (trans, rot) ->
                        printfn "Found match between scanner %i and %i" scannerA.Id scannerB.Id
                        Some (scannerA, scannerB, trans,rot)
                    | None ->
                        None
            )

        match matchFound with
        | Some (_, scannerB, trans,rot) -> 
            let newRemainingScanners = remainingScanners |> List.filter (fun s -> s.Id <> scannerB.Id)
            let transformedB = transformScanner scannerB { Rotation = rot; Translation = trans}
            let positionnedB = { transformedB with Position = trans |> Some }
            findAllBeacons (positionnedB :: scannersFound) newRemainingScanners newVisited
        | None  -> failwithf "No match found in iteration %i" iteration



let solve1 fileName =
    let input = getInput fileName
    let initialBeacon = { input.Head with Position = [0.0;0.0;0.0] |> vector |> Some }
    let allScannersLocated = findAllBeacons [initialBeacon] input.Tail Set.empty
 
    // Put all beacons in the result set
    let allBeacons = new HashSet<Vector<float>>(input.Head.Beacons)
    allScannersLocated |> Seq.iter (fun scanner -> scanner.Beacons |> Seq.iter (fun b -> allBeacons.Add(b) |> ignore))
    allBeacons |> Seq.iter (fun v -> printfn "(%0.0f,%0.0f,%0.0f)" v.[0] v.[1] v.[2])
    allBeacons |> Seq.length

//assert(solve1 "Day19_sample.txt" = 79)

//let sw = Stopwatch.StartNew()
//solve1 "Day19.txt"
//printfn "Runtime : %A" sw.Elapsed

let manhattanDistVec (v1:Vector<float>) (v2:Vector<float>) =
    Math.Abs(v1.[0] - v2.[0]) + Math.Abs(v1.[1] - v2.[1]) + Math.Abs(v1.[2] - v2.[2]) 

let solve2 fileName =
    let input = getInput fileName
    let initialBeacon = { input.Head with Position = [0.0;0.0;0.0] |> vector |> Some }
    let allScannersLocated = findAllBeacons [initialBeacon] input.Tail Set.empty
    
    let scannerPositions = allScannersLocated |> List.map (fun s -> s.Position.Value)

    List.allPairs scannerPositions scannerPositions
    |> List.map (fun (p1, p2) -> manhattanDistVec p1 p2)
    |> Seq.max

////assert(solve1 "Day19_sample.txt" = 79)

let sw = Stopwatch.StartNew()
let result = solve2 "Day19.txt"
printfn "Part 2 solution : %f Runtime : %A" result sw.Elapsed