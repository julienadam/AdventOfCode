#load "../../Tools.fsx"

open System
open System.IO
open Tools

type Op =
    | Add of int
    | Multiply of int
    | Square

type Monkey = {
    id : int
    mutable items : int list
    op : Op
    test : int
    onTrue : int
    onFalse : int
    mutable itemsInspected : int
}

let mapOp (line:string) =
    let rStr = line.Substring(25)
    match line.[23], rStr with
    | '*', "old" -> Square
    | '*', _ -> Multiply (Int32.Parse(rStr))
    | '+', _ -> Add (Int32.Parse(rStr))
    | o, _ -> failwithf "unknown op %c" o

let mapMonkey (lines: string array) =
    let monkeyId = Int32.Parse(lines.[0].[7] |> string)
    let items = 
        lines.[1].Substring(18).Split([|", "|], StringSplitOptions.None)
        |> Seq.map Int32.Parse
        |> Seq.toList
    let op = mapOp lines.[2]
    let test = Int32.Parse(lines.[3].Substring(21))
    let targetIfTrue = Int32.Parse(lines.[4].Substring(29))
    let targetIfFalse = Int32.Parse(lines.[5].Substring(30))
    { id = monkeyId; items = items; test = test; op = op; onTrue = targetIfTrue; onFalse = targetIfFalse; itemsInspected = 0 }

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.chunkBySize 7
    |> Seq.map mapMonkey
    |> Seq.toArray

let updateWorry i op =
    match op with
    | Square -> i * i
    | Add j -> i + j
    | Multiply j -> i * j

let round i (monkeys:Monkey array) =
    monkeys |> Array.iter (fun monkey ->
        monkey.items |> Seq.iter (fun item ->
            monkey.itemsInspected <- monkey.itemsInspected + 1
            let worry = updateWorry item monkey.op
            let reducedWorry = Math.Round((worry |> double) / 3.0) |> int
            printfn "Worry for %i was updated to %i and rounded to %i" item worry reducedWorry
            let target = 
                if reducedWorry % monkey.test = 0 then
                    monkey.onTrue
                else 
                    monkey.onFalse
            monkeys.[target].items <- List.append monkeys.[target].items [reducedWorry]
        )
        monkey.items <- []
    )
    printfn ""
    printfn "After round %i" i
    monkeys |> Dump


let solve1 monkeys =
    [1..1] 
    |> Seq.iter (fun i -> round i monkeys |> ignore)
    
    let mostActive = 
        monkeys 
        |> Array.map(fun m -> m.itemsInspected) 
        |> Array.sortDescending 
        |> Array.take 2
        |> Dump

    mostActive.[0] * mostActive.[1]

getInput "Day11_sample1.txt"
|> solve1
|> Dump

