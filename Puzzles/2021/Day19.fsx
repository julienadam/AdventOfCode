open System.Diagnostics
open System

#load "../../Tools.fsx"

open Tools
open System.IO

let mapPosLine (l:string) =
    l.Split(",") 
    |> Array.map (fun l -> 
        let r, i = Int32.TryParse(l)
        if not r then
            failwithf "Invalid input %s" l
        else
            i
        ) 
        |> Array.toList

let inputStr = getInputPath "Day19.txt" |> File.ReadAllText
inputStr.Split("\r\n\r\n")
|> Array.map (fun lines -> lines.Split("\r\n") |> Array.skip 1)
|> Array.map (fun positions -> positions |> Array.map mapPosLine |> Array.toList)
|> Array.toList
|> Dump