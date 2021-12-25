#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Tools

let lines = getInputPath "Day23_sample1.txt" |> File.ReadAllLines 
let roomA = lines.[2].[3], lines.[3].[3]
let roomB = lines.[2].[5], lines.[3].[5]
let roomC = lines.[2].[7], lines.[3].[7]
let roomD = lines.[2].[9], lines.[3].[9]

printfn "%A %A %A %A" roomA roomB roomC roomD