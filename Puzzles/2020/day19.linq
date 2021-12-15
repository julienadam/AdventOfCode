<Query Kind="FSharpProgram" />

let inline splitLines (s:string) = s.Split("\r\n")
let inline splitByEmptyLines (s:string) = s.Split("\r\n\r\n")
let inline tuple2 (array:'a[]) = array.[0], array.[1]
let inline parseIntList (l:string) = l.Split(' ') |> Array.map int |> Array.toList
let toString : char seq -> string = Seq.map string >> String.concat ""
  
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", file)
let path = getInputPath "day19.txt"

type Rule =
    | Simple of int list
    | Or of int list * int list
    | Definition of char

let extractLineNumber (rule: string) =
    let split = rule.Split(": ")
    split.[0] |> int, split.[1]

let parseRule (rule:String) =
    let lineNo, ruleDef = extractLineNumber rule
    let mDef = Regex.Match(rule, "\"(\w)\"")
    if mDef.Success then
        lineNo, Definition mDef.Groups.[1].Value.[0]
    else 
        if ruleDef.Contains(" | ") then
            let split = ruleDef.Split(" | ")
            lineNo , Or (parseIntList split.[0], parseIntList split.[1])
        else 
            lineNo, Simple (parseIntList ruleDef)

let (rulesStr, inputStr) = 
    File.ReadAllText path
    |> splitByEmptyLines
    |> tuple2

let (rulesOrig, input) =
    rulesStr |> splitLines |> Seq.map parseRule |> Map.ofSeq , inputStr |> splitLines

module Puzzle1 =
    let rules = rulesOrig
    let toRules ints = ints |> Seq.map (fun i -> rules.[i])
    
    let rec validate rule input =
        
        let ruleFold (b, s) r =
            if not b then (false, s) else validate r s
        
        match rule with
        | Definition c -> 
            ((input |> Seq.head) = c, input |> Seq.tail)
        | Simple ints ->
            ints 
            |> toRules 
            |> Seq.fold ruleFold (true, input)
        | Or (ints1, ints2) ->
            let (b1, s1) = 
                ints1 
                |> toRules
                |> Seq.fold ruleFold (true, input)
            
            if b1 then
                (b1, s1)
            else
                ints2
                |> toRules
                |> Seq.fold ruleFold (true, input)
        

    let solution() =
        input 
        |> Seq.map (fun input -> input, input |> validate rules.[0]) 
        |> Seq.filter (fun (_, (b, s)) -> b && s |> Seq.isEmpty)
        |> Seq.map fst
        |> Seq.length

module Puzzle2 =
    let replacementRule8 = Or ([42], [42; 8]) // 8: 42 | 42 8
    let replacementRule11 = Or ([42; 31], [42; 11; 31]) //11: 42 31 | 42 11 31

    let rules = rulesOrig |> Map.add 8 replacementRule8 |> Map.add 11 replacementRule11
    let toRules ints = ints |> Seq.map (fun i -> rules.[i])
    
    let rec validate rule (input:seq<char> list) =
        
        let ruleFold (b, s) r =
            if not b then (false, s) else validate r s
        
        match rule with
        | Definition c -> 
            let r =
                input 
                |> List.choose (fun s ->
                    if s |> Seq.isEmpty then
                        None
                    else if (s |> Seq.head) <> c then
                        None
                    else    
                        Some (s |> Seq.tail))
            if r.Length > 0 then
                (true, r)
            else
                (false, [])
        | Simple ints ->
            let r = 
                ints 
                |> toRules 
                |> Seq.fold ruleFold (true, input)
            r
        | Or (ints1, ints2) ->
            let (b1, s1) = 
                ints1 
                |> toRules
                |> Seq.fold ruleFold (true, input)
            
            let (b2, s2) = 
                ints2 
                |> toRules
                |> Seq.fold ruleFold (true, input)
            
            match b1, b2 with
            | true, true ->
                true, s1 @ s2
            | true, _ -> 
                true, s1
            | _, true -> 
                true, s2
            | _, _ ->
                false, []

    let solution() =

        input 
        |> Seq.map (fun input -> input, validate rules.[0] [input]) 
        |> Seq.filter (fun (_, (b, s)) -> b && s |> Seq.exists Seq.isEmpty)
        |> Seq.length

Puzzle2.solution() |> Dump