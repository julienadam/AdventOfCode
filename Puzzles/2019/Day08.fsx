#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Digits.fs"

#r "nuget: NFluent"

open System
open System.IO
open NFluent
open AdventOfCode
open Checked

let getInput name =
    File.ReadAllText(getInputPath2019 name)

let solve1 input =
    getInput input |> Dump    
    
solve1 "Day08.txt"

let solve2 input =
    getInput input |> Dump    

solve2 "Day08.txt"