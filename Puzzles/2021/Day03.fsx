#load "../../Tools.fsx"
open System
open System.IO
open Tools

let path = getInputPath "day03.txt"
//let path = getInputPath "day03_sample1.txt"

let mapLine l = l |> Seq.map (fun c -> match c with | '0' -> 0 | '1' -> 1 | _ -> failwith "Invalid bit") |> Seq.toArray
    
let input = File.ReadAllLines(path) |> Seq.map mapLine |> List.ofSeq

module Part1 =

    let getMostCommonBit bits = 
        let totalLength = bits |> Seq.length
        let numOfOnes = bits |> Seq.filter (fun c -> c = 1) |> Seq.length
        if (totalLength - numOfOnes) < numOfOnes then 1 else 0

    let bitsToInts bits =
         bits
         |> Seq.rev
         |> Seq.indexed
         |> Dump
         |> Seq.fold (fun (gamma, epsilon) (i,b) -> 
            let inverseBit = if b = 0 then 1 else 0
            gamma ||| (b <<< i), epsilon ||| (inverseBit <<< i)) (0, 0)

    let Solve() = 
        let (gamma, epsilon) = 
            input 
            |> Seq.transpose 
            |> Seq.map getMostCommonBit
            |> bitsToInts
        
        gamma * epsilon |> Dump

        

Part1.Solve() |> ignore


module Part2 =

    let bitsToInts bits =
        bits
        |> Seq.rev
        |> Seq.indexed
        |> Seq.fold (fun n (i,b) -> n ||| (b <<< i)) 0


    let decideGamma (m: Map<int, int[] list>) = if (m.[1] |> Seq.length) >= (m.[0] |> Seq.length) then m.[1] else m.[0]
    let decideEpsilon (m: Map<int, int[] list>) = if (m.[1] |> Seq.length) < (m.[0] |> Seq.length) then m.[1] else m.[0]

    let rec SolveRec (numbers: int[] list) index decide =
        match numbers with
        | x::[] -> 
            x
        | _ -> 
            let grouped = 
                numbers
                |> List.groupBy (fun bits -> bits.[index]) 
                |> Map.ofList
            
            SolveRec (grouped |> decide) (index + 1) decide

    let Solve () =
        let gamma = SolveRec input 0 decideGamma |> Dump |> bitsToInts |> Dump
        let epsilon = SolveRec input 0 decideEpsilon |> Dump |> bitsToInts |> Dump
        
        gamma * epsilon |> Dump

Part2.Solve() |> ignore