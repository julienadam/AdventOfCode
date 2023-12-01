#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput p = File.ReadAllLines(getInputPath2023 p)

let solve1 (input: string array) =
    input 
        |> Array.map (fun line -> 
            let chars = line.ToCharArray()
            let firstDigit = chars |> Array.find(fun c -> System.Char.IsAsciiDigit(c))
            let lastDigit = chars |> Array.findBack(fun c -> System.Char.IsAsciiDigit(c))
            sprintf "%c%c" firstDigit lastDigit |> int
        )
        |> Array.sum
        |> Dump

let digits = 
    [
        "one", 1
        "two", 2
        "three", 3
        "four",4
        "five",5
        "six", 6
        "seven", 7
        "eight", 8
        "nine", 9
        "zero", 0
    ] |> Map.ofList

let inline digitToInt (c:char) = (c |> int) - ('0' |> int)

let rec getFirstDigit (line:string) = 
    if String.IsNullOrEmpty(line) then
        failwithf "could not find digit"

    if Char.IsDigit(line.[0]) then 
        line.[0] |> digitToInt
    else
        match digits |> Map.tryPick (fun k v -> if line.StartsWith(k) then Some v else None)
        with
        | Some v -> v
        | None -> getFirstDigit (line.Substring(1))

let rec getLastDigit (line:string) = 
    if String.IsNullOrEmpty(line) then
        failwithf "could not find digit"
        
    if Char.IsDigit(line.[line.Length - 1]) then
        line.[line.Length - 1] |> digitToInt
    else
        match digits |> Map.tryPick (fun k v -> if line.EndsWith(k) then Some v else None)
        with
        | Some v -> v
        | None -> getLastDigit (line.Substring(0, line.Length - 1))

let solve2(input : string array) =
    input 
    |> Seq.map (fun line -> getFirstDigit line, getLastDigit line)
    |> Seq.map (fun (a, b) -> a * 10 + b)
    |> Seq.sum
    |> Dump

getInput "day01_sample.txt" |> solve1
getInput "day01.txt" |> solve1 
getInput "day01_sample2.txt" |> solve2
getInput "day01.txt" |> solve2 
