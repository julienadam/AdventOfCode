open System
open System.IO
open System.Net.Http

let args = fsi.CommandLineArgs |> Array.tail
let now = DateTime.Now
let day = 
    if args.Length > 0 then
        args[0] |> int
    else
        now.Day

let year = 
    if args.Length > 1 then
        args[1] |> int
    else
        now.Year
        

let createIfMissing path (contents:string) =
    printfn $"%s{path}"
    if File.Exists(path) = false then
        File.WriteAllText(path, contents)

let getCookie () =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Input", year.ToString(), "session.cookie")
    if File.Exists(path) = false then
        failwithf $"No session cookie found at %s{path}"
    File.ReadAllText(path)

let downloadPuzzleInput () =
    let puzzleUrl = $"https://adventofcode.com/%i{year}/day/%i{day}/input"
    let client = new HttpClient()
    let message = new HttpRequestMessage(HttpMethod.Get, Uri(puzzleUrl))
    message.Headers.Add("Cookie", $"session=%s{getCookie ()}")
    let response = client.Send(message);
    match response.IsSuccessStatusCode with
    | true ->
        (new StreamReader(response.Content.ReadAsStream())).ReadToEnd()
    | false ->
        printfn $"Could not read puzzle from AoC url %s{puzzleUrl}"
        ""

let fsxPath = Path.Combine(__SOURCE_DIRECTORY__, "Puzzles", year.ToString(), $"Day%02i{day}.fsx")

let fsxContents =
    $"""#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name = File.ReadAllLines(getInputPath%i{year} name)

let solve1 input =
    getInput input |> Dump

solve1 "Day%02i{day}_sample1.txt"
"""

createIfMissing fsxPath fsxContents

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input", year.ToString(), $"Day%02i{day}.txt")
createIfMissing inputPath (downloadPuzzleInput())

let sample1Path = Path.Combine(__SOURCE_DIRECTORY__, "Input", year.ToString(), $"Day%02i{day}_sample1.txt")
createIfMissing inputPath ""
