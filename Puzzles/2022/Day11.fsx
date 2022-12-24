#load "../../Tools.fsx"

open System
open System.IO
open Tools

type Op =
    | Add of int
    | Multiply of int
    | Square

let mapMonkey (lines: string array) =
    let monkeyId = Int32.Parse(lines.[0].[7] |> string)
    let items = 
        lines.[1].Substring(18).Split([|", "|], StringSplitOptions.None)
        |> Seq.map Int32.Parse
    let op = 
        let rStr = lines.[2].Substring(25)
        match lines.[2].[23], rStr with
        | '*', "old" -> Square
        | '*', _ -> Multiply (Int32.Parse(rStr))
        | '+', _ -> Add (Int32.Parse(rStr))
        | o, _ -> failwithf "unknown op %c" o
    let test = Int32.Parse(lines.[3].Substring(21))
    let targetIfTrue = Int32.Parse(lines.[4].Substring(29))
    let targetIfFalse = Int32.Parse(lines.[5].Substring(30))
    monkeyId, items, op, test, targetIfTrue, targetIfFalse

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.chunkBySize 7
    |> Seq.map mapMonkey
    |> Seq.toList

getInput "Day11.txt"

// let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map mapLine