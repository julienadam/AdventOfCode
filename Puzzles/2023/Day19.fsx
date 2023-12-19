#time "on"
#load "../../Tools.fs"
#load "../../Tools/RegexTools.fs"

open System
open System.IO
open AdventOfCode
open System.Text.RegularExpressions

type Part = { x: int; m: int; a: int; s: int }
type Result = | Goto of string | Accepted | Rejected
let re = new Regex(@"^(?<else>\w+)|(?<category>[xmas])(?<op>[><])(?<int>\d+)\:(?<res>\w+)$")

let parseCondition = function
    | ParseRegex re m -> 
        if m.ContainsKey("else") then
            match m["else"] with
            | "A" -> (fun (_:Part) -> Some Accepted)
            | "R" -> (fun (_:Part) -> Some Rejected)
            | x   -> (fun (_:Part) -> Some (Goto x))
        else
            let op = match m["op"] with | ">" -> (>) | "<" -> (<) | x -> failwithf "invalid op %s" x
            let right = m["int"] |> int
            let res = 
                match m["res"] with
                | "A" -> Accepted
                | "R" -> Rejected
                | x   -> Goto x
            match m["category"] with
            | "x" -> (fun (p:Part) -> if op p.x right then Some res else None)
            | "m" -> (fun (p:Part) -> if op p.m right then Some res else None)
            | "a" -> (fun (p:Part) -> if op p.a right then Some res else None)
            | "s" -> (fun (p:Part) -> if op p.s right then Some res else None)
            | x   -> failwithf "Invalid part %s" x
    | x -> failwithf "Could not parse %s" x

let parseLine (line:string) =
    let i = line.IndexOf('{')
    let name = line.Substring(0,i)
    let workflows = line.Substring(i + 1, line.Length - i - 2) |> ssplit ","
    name, workflows |> Array.map (fun c -> parseCondition c, c)

let parsePart (p:string) = 
    let values = p.Substring(1, p.Length - 2) |> ssplit "," |> Array.map (fun decl -> decl.Substring(2) |> int)
    { x = values[0]; m = values[1]; a = values[2]; s = values[3]}

let getInput name =
    let parseWorkflows w = w |> ssplit "\n" |> Array.map parseLine
    let parseParts p = p |> ssplit "\n" |> Array.map parsePart
    let workflows, ratings = File.ReadAllText(getInputPath2023 name) |> ssplit2 "\n\n"

    parseWorkflows workflows, parseParts ratings

let solve1 input =
    getInput input 
    |> Dump



solve1 "Day19_sample1.txt"
