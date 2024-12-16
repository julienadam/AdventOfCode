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

let createIfMissing path contents =
    printfn "%s" path
    if File.Exists(path) = false then
        File.WriteAllText(path, contents)

let getCookie () =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Input", (now.Year.ToString()), "session.cookie")
    if File.Exists(path) = false then
        failwithf "No session cookie found at %s" path
    File.ReadAllText(path)

let downloadPuzzleInput () =
    let puzzleUrl = sprintf "https://adventofcode.com/%i/day/%i/input" now.Year day
    let client = new HttpClient()
    let message = new HttpRequestMessage(HttpMethod.Get, Uri(puzzleUrl))
    message.Headers.Add("Cookie", sprintf "session=%s" (getCookie ()))
    let response = client.Send(message);
    match response.IsSuccessStatusCode with
    | true ->
        (new StreamReader(response.Content.ReadAsStream())).ReadToEnd()
    | false ->
        printfn "Could not read puzzle from AoC url %s" puzzleUrl
        ""

let fsxPath = Path.Combine(__SOURCE_DIRECTORY__, "Puzzles", (now.Year.ToString()), sprintf "Day%02i.fsx" day)

let fsxContents = 
    sprintf 
        """#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllLines(getInputPath%i name)

let solve1 input =
    getInput input |> Dump

solve1 "Day%02i_sample1.txt"
""" 
        now.Year day

createIfMissing fsxPath fsxContents

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input", (now.Year.ToString()), sprintf "Day%02i.txt" day)

createIfMissing inputPath (downloadPuzzleInput())
