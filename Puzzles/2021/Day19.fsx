#r "nuget: MathNet.Numerics.FSharp"

open System.Diagnostics
open System
open MathNet.Numerics.LinearAlgebra

#load "../../Tools.fsx"

open Tools
open System.IO

let mapPosLine (l:string) =
    
    l.Split(",") 
        |> Array.map (fun l -> 
            let r, i = Int32.TryParse(l)
            if not r then
                failwithf "Invalid input %s" l
            else
                i |> float
            )
        //|> Array.toList
    |> vector

let inputStr = getInputPath "Day19_sample1.txt" |> File.ReadAllText
let input = 
    inputStr.Split("\r\n\r\n")
    |> Array.map (fun lines -> lines.Split("\r\n") |> Array.skip 1)
    |> Array.map (fun positions -> positions |> Array.map mapPosLine |> Array.toList)
    |> Array.toList


// Possibles transformations 
// x is -x
// x is y
// x is -y
// x is z
// x is -z


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

                // if rx <> ry && rx <> rz && ry <> rz  then 
                yield (matrix mx) * (matrix my) * (matrix mz)
}

let rotations = genRotations() |> Seq.distinct

// rotations |> Dump

let scanner0, otherScanners = input |> List.head, input |> List.tail

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
        
        //printfn "%i matches found" mappedA
       
        if mappedA >= 11 then
            Some (trans, rot)
        else
            None
    )

let findTransformationMatrixIfAny scannerA scannerB =
    match rotations |> Seq.tryPick (fun rot -> 
        //printfn "Trying rotation %O" rot
        have12BeaconsInCommonForRotation scannerA scannerB rot) with
    | Some (trans, rot) -> 
        printfn "Scanners match on 12 beacons or more for rotation : %O and translation : %O" rot trans
    | None ->
        printfn "No match between scanners, tried all rotations"

findTransformationMatrixIfAny scanner0 (otherScanners |> List.head)

