#load "../../Tools.fs"

open System
open System.IO
open Checked // Replaces normal, unchecked arithmetic ops with checked ones. Clever !
open AdventOfCode

type Op =
    | Add of int64
    | Multiply of int64
    | Square

type Monkey = {
    id : int
    mutable items : int64 list
    op : Op
    test : int64
    onTrue : int
    onFalse : int
    mutable itemsInspected : int64
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
        |> Seq.map Int64.Parse
        |> Seq.toList
    let op = mapOp lines.[2]
    let test = Int64.Parse(lines.[3].Substring(21))
    let targetIfTrue = Int32.Parse(lines.[4].Substring(29))
    let targetIfFalse = Int32.Parse(lines.[5].Substring(30))
    { id = monkeyId; items = items; test = test; op = op; onTrue = targetIfTrue; onFalse = targetIfFalse; itemsInspected = 0 }

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.chunkBySize 7
    |> Seq.map mapMonkey
    |> Seq.toArray

let updateWorry i =
    function
    | Square -> (i * i)
    | Add j -> (i + j)
    | Multiply j -> i * j


let updateWorryLcm lcm i =
    function
    | Square -> (i * i) % lcm
    | Add j -> (i + j) % lcm
    | Multiply j -> i * j % lcm


let round i (lcm: int64 option) (monkeys:Monkey array) =
    monkeys |> Array.iter (fun monkey ->
        monkey.items |> Seq.iter (fun item ->
            monkey.itemsInspected <- monkey.itemsInspected + 1L
            let reducedWorry = 
                if lcm.IsNone then
                    let worry = updateWorry item monkey.op
                    Math.Floor((worry |> double) / 3.0) |> int64
                else 
                    updateWorryLcm lcm.Value item monkey.op
            let target = 
                if reducedWorry % monkey.test = 0L then
                    monkey.onTrue
                else 
                    monkey.onFalse
            monkeys.[target].items <- List.append monkeys.[target].items [reducedWorry]
        )
        monkey.items <- []
    )
    if i = 1 || i = 20 || i % 1000 = 0 then
        printfn "== After round %i ==" i
        monkeys |> Seq.iter (fun m -> 
            printfn "Monkey %i inspected items %i times" m.id m.itemsInspected
        )


let solve1 monkeys =
    [1..20] 
    |> Seq.iter (fun i -> round i None monkeys |> ignore)
    
    let mostActive = 
        monkeys 
        |> Array.map(fun m -> m.itemsInspected)
        |> Array.sortDescending
        |> Array.take 2
        |> Dump

    mostActive.[0] * mostActive.[1]
    
//getInput "Day11_sample1.txt"
//|> solve1
//|> Dump

let solve2 monkeys =
    let lcm = monkeys |> Seq.fold (fun state m -> state * m.test) 1L
    printfn "Least common denominator is %i" lcm

    [1..10000] 
    |> Seq.iter (fun i -> round i (Some lcm) monkeys |> ignore)
    
    let mostActive =
        monkeys
        |> Array.map(fun m -> m.itemsInspected)
        |> Array.sortDescending 
        |> Array.take 2
        |> Dump

    mostActive.[0] * mostActive.[1]

getInput "Day11.txt"
|> solve2
|> Dump
