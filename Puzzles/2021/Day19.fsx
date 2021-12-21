open MathNet.Numerics.LinearAlgebra


#r "nuget: MathNet.Numerics.FSharp"

open System.Diagnostics
open System

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

let inputStr = getInputPath "Day19.txt" |> File.ReadAllText
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

rotations |> Dump