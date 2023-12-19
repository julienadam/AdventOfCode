#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System.IO
open AdventOfCode
open NFluent

type Part = { x: int; m: int; a: int; s: int }
type Result = | Goto of string | Accepted | Rejected

let parseCondition (c:string) = 
    if c.Contains(":") then
        let spl = c.Split([|'>'; '<'; ':'|])
        let right = spl[1] |> int
        let res = match spl[2] with | "A" -> Accepted | "R" -> Rejected | x -> Goto x
        let op = if c.Contains(">") then (>) else (<)
        match spl[0] with
        | "x" -> (fun (p:Part) -> if op p.x right then Some res else None)
        | "m" -> (fun (p:Part) -> if op p.m right then Some res else None)
        | "a" -> (fun (p:Part) -> if op p.a right then Some res else None)
        | "s" -> (fun (p:Part) -> if op p.s right then Some res else None)
        | x   -> failwithf "Invalid part %s" x
    else
        match c with
        | "A" -> (fun _ -> Some Accepted)
        | "R" -> (fun _ -> Some Rejected)
        | x   -> (fun _ -> Some (Goto x))

let parseLine (line:string) =
    let i = line.IndexOf('{')
    let name = line.Substring(0,i)
    let workflows = line.Substring(i + 1, line.Length - i - 2) |> ssplit ","
    name, workflows |> Array.map parseCondition

let parsePart (p:string) = 
    let values = p.Substring(1, p.Length - 2) |> ssplit "," |> Array.map (fun decl -> decl.Substring(2) |> int)
    { x = values[0]; m = values[1]; a = values[2]; s = values[3]}

let getInput name =
    let parseWorkflows w = w |> ssplit "\n" |> Array.map parseLine
    let parseParts p = p |> ssplit "\n" |> Array.map parsePart
    let workflows, ratings = File.ReadAllText(getInputPath2023 name) |> ssplit2 "\n\n"

    workflows |> parseWorkflows |> Map.ofArray , parseParts ratings

let solve1 input =
    let workflows, parts = getInput input 
    let inw = workflows["in"]

    let rec runSystem conditions (part:Part) =
        match conditions |> Array.pick (fun condition -> condition part) with
        | Accepted -> Accepted
        | Rejected -> Rejected
        | Goto x   -> runSystem workflows[x] part
    
    parts 
    |> Seq.map (fun p -> p, runSystem inw p)
    |> Seq.filter (fun (_, r) -> r = Accepted)
    |> Seq.sumBy (fun (p,_) -> p.x + p.m + p.a + p.s)


Check.That(solve1 "Day19_sample1.txt").IsEqualTo(19114)
solve1 "Day19.txt"
