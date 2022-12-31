#time
#load "../../Tools.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools

let regex = new Regex (@"(?<monkey>\w+)\: (((?<m1>\w+) (?<op>[\+\-\*\/]{1}) (?<m2>\w+))|(?<num>\d+))")
type Op = int64 -> int64 -> int64

type MonkeyYell =
    | Number of int64
    | MathOp of string * Op * string

type Monkey = {
    name: string
    yell: MonkeyYell
}

let parseOp o =
    match o with 
    | "+" -> (+)
    | "-" -> (-)
    | "*" -> (*)
    | "/" -> (/)
    | _ -> failwithf "Invalid operator %s" o

let parseMonkey line =
    let m = regex.Match line
    if not m.Success then
        failwithf "Could not parse line %s" line
    let yell = 
        if m.Groups.["m1"].Success = false then
           m|> mInt64 "num" |> Number
        else
            (m |> mStr "m1", m|>mStr "op" |> parseOp, m |> mStr "m2") |> MathOp
    { 
        name = m |> mStr "monkey"
        yell = yell
    }

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map parseMonkey
    |> Seq.map (fun m -> m.name, m)
    |> Map.ofSeq

let rec compute (monkeys:Map<string,Monkey>) current =
    match current.yell with
    | Number n -> n
    | MathOp (l, op, r) ->
        let leftNumber = compute monkeys monkeys.[l]
        let rightNumber = compute monkeys monkeys.[r]
        op leftNumber rightNumber

let solve1 (monkeys:Map<string,Monkey>) =
    let root = monkeys["root"]
    compute monkeys root

getInput "Day21.txt"
|> solve1