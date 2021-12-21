#r "nuget: MathNet.Numerics.FSharp"

open System.Diagnostics
open System
open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

#load "../../Tools.fsx"

open Tools
open System.IO

type CoordinatesTransform = {
    Rotation: Matrix<float>
    Translation: Vector<float>
}

type Scanner = {
    Name: string
    Beacons: Vector<float> list
    // CoordinatesTransform: CoordinatesTransform option
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

let inputStr = getInputPath "Day19_sample1.txt" |> File.ReadAllText
let input = 
    inputStr.Split("\r\n\r\n")
    |> Array.map (fun lines -> lines.Split("\r\n") |> Array.skip 1)
    |> Array.map (fun positions -> positions |> Array.map mapPosLine |> Array.toList)
    |> Array.mapi (fun id beacons -> { Name = sprintf "Scanner %i" id; Beacons = beacons })
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
    printfn "%s" scanner.Name
    //match scanner.CoordinatesTransform with
    //| Some t ->
    //    printf "Found at" 
    //    printVec t.Translation
    //    scanner.Beacons |> Seq.iter (fun p ->
    //        printfn "%s -> %s" (sprintVec p) (sprintVec (applyTransformation t p)))
    //| _ -> 
    scanner.Beacons |> Seq.iter printVec

let transformScanner scanner transform =
    { scanner with Beacons = scanner.Beacons |> List.map(fun p -> applyTransformation transform p) }


let mutable iteration = 0
let rec findAllBeacons (scannersFound:Scanner list) (remainingScanners: Scanner list) (beacons: HashSet<Vector<float>>)=
    iteration <- iteration + 1
    printfn "Iteration %i Total beacons %o" iteration beacons.Count
    scannersFound |> Seq.iter printScanner

    match remainingScanners with
    | [] -> beacons
    | _ ->
        let matchesFound =
            List.allPairs scannersFound remainingScanners
            |> List.choose (fun (scannerA, scannerB) -> 
                findTransformationMatrixIfAny scannerA.Beacons scannerB.Beacons
                |> Option.map (fun (trans,rot) -> 
                    printfn "Found match between scanner %s and %s" scannerA.Name scannerB.Name
                    scannerA, scannerB, trans,rot))

        let newScanners = (matchesFound |> List.map (fun (_,b,_,_) -> b))
        let newRemainingScanners = remainingScanners |> List.filter (fun s -> newScanners |> List.contains s |> not)

        let newScannersFound = 
            matchesFound |> List.map (fun (scannerA, scannerB, trans, rot) ->
                let transformedB = transformScanner scannerB { Rotation = rot; Translation = trans}
                transformedB.Beacons |> Seq.iter (fun p -> beacons.Add(p) |> ignore)
                transformedB
                //match scannerA.CoordinatesTransform with
                //| None -> failwithf "Found match on scanner that hasn't been fixed yet"
                //| Some transform -> 
                //    // Add all the beacons of scanner B to the beacon list, after transformation
                //    let totalRotation, totalTranslation = rot * transform.Rotation, trans + transform.Translation
                //    scannerB.Beacons 
                //    |> List.map (fun p -> (p * totalRotation) + totalTranslation)
                //    |> Seq.iter (fun p -> beacons.Add(p) |> ignore)
                //    // Update the scanner we just found with their coords transform
                //    { scannerB with CoordinatesTransform = Some { Rotation = totalRotation; Translation = totalTranslation}}
            )

        findAllBeacons newScannersFound newRemainingScanners beacons



//let initialRotation = Matrix<float>.Build.DenseIdentity(3,3)
//let initialTranslation = ([0.0;0.0;0.0] |> vector)


let initialBeacons = new HashSet<Vector<float>>(input.Head.Beacons)
let allBeacons = findAllBeacons [input.Head ] input.Tail initialBeacons

allBeacons |> Seq.iter (fun v -> printfn "(%0.0f,%0.0f,%0.0f)" v.[0] v.[1] v.[2])
allBeacons |> Seq.length
