#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let path = getInputPath "day10.txt"
//let path = getInputPath "day10_sample1.txt"

let input = File.ReadAllLines(path) 

//let printStack (stack: char list) = new String(stack |> List.rev |> Seq.toArray)

let rec findErrorCharRec s (stack:char list) =
    match s with
    | [] -> None
    | c::rest ->
        match c, stack with
        | ')', top::_ when top <> '(' -> Some ')'
        | ']', top::_ when top <> '[' -> Some ']'
        | '}', top::_ when top <> '{' -> Some '}'
        | '>', top::_ when top <> '<' -> Some '>'
        | ')', _::stackRest -> findErrorCharRec rest stackRest
        | ']', _::stackRest -> findErrorCharRec rest stackRest
        | '}', _::stackRest -> findErrorCharRec rest stackRest
        | '>', _::stackRest -> findErrorCharRec rest stackRest
        | _, _ -> findErrorCharRec rest (c::stack)
        
let findErrorChar (s:string) = findErrorCharRec (s |> Seq.toList) List.empty

module Part1 =
    
    let scoreError c =
        match c with
        | Some ')' -> 3
        | Some ']' -> 57
        | Some '}' -> 1197
        | Some '>' -> 25137
        | Some x -> failwithf "invalid char %c" x
        | None -> 0
        
    let Solve () =
        input |> Seq.map findErrorChar |> Seq.map scoreError |> Seq.sum |> Dump
        ()
        
Part1.Solve()

module Part2 =

    let getUnclosedChars input =
        input 
        |> Seq.fold (fun stack c ->
            match c, stack with
            | ')', _::stackRest -> stackRest
            | ']', _::stackRest -> stackRest
            | '}', _::stackRest -> stackRest
            | '>', _::stackRest -> stackRest
            | _ -> c::stack) List.empty

    let scoreCompletion (chars: char list) =
        chars |> Seq.fold (fun score c -> 
            let sc =
                match c with
                | '(' -> 1L
                | '[' -> 2L
                | '{' -> 3L
                | '<' -> 4L
                | _ -> failwithf "Unexpected char %c" c
            (score * 5L) + sc
            ) 0L

    let Solve () =
        let scores = 
            input 
            |> Seq.where (fun l -> findErrorChar l |> Option.isNone) 
            |> Seq.map getUnclosedChars
            |> Seq.map scoreCompletion
            |> Seq.sort
            |> Seq.toList
        
        scores.[((scores |> List.length) / 2)] |> Dump
        
Part2.Solve()

