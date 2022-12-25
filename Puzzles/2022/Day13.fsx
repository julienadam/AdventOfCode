#load "../../Tools.fsx"
#r "nuget: FSharp.Data"

open System
open System.IO
open Tools
open FSharp.Data

let parseData (l:string) = FSharp.Data.JsonValue.Parse(l)

let getInput p = 
    File.ReadAllText(getInputPath2022 p).Split([|"\r\n\r\n"|], StringSplitOptions.None)
    |> Seq.map (fun s -> 
        let split = s.Split([|"\r\n"|], StringSplitOptions.None)
        split.[0] |> parseData, split[1] |> parseData)

let rec compare (left:FSharp.Data.JsonValue) (right:FSharp.Data.JsonValue) =
    match left, right with
    | FSharp.Data.JsonValue.Number l, FSharp.Data.JsonValue.Number r ->
        if l < r then
            //printfn "True because %A < %A" l r
            Some true
        else if l > r then
            //printfn "false because %A > %A" l r
            Some false
        else
            None
    | FSharp.Data.JsonValue.Array leftItems, FSharp.Data.JsonValue.Array rightItems ->
        let result = 
            Seq.zip leftItems rightItems
            |> Seq.tryPick (fun (l,r) -> compare l r)
        match result with
        | Some b -> Some b
        | None ->
            if leftItems.Length < rightItems.Length then
                //printfn "True because %A is smaller than %A" leftItems rightItems
                Some true
            else if leftItems.Length > rightItems.Length then
                //printfn "False because %A is bigger than %A" leftItems rightItems
                Some false
            else
                None
    | FSharp.Data.JsonValue.Array _, FSharp.Data.JsonValue.Number _ ->
        let wrappedRight = FSharp.Data.JsonValue.Array([|right|])
        compare left wrappedRight
    | FSharp.Data.JsonValue.Number _, FSharp.Data.JsonValue.Array _ ->
        let wrappedLeft = FSharp.Data.JsonValue.Array([|left|])
        compare wrappedLeft right
    | _ -> failwithf "Invalid json value"

let solve1 pairs =
    pairs 
    |> Seq.mapi (fun i (l,r) -> i + 1, compare l r)
    |> Seq.filter (fun (idx,b) -> b.IsSome && b.Value)
    |> Seq.map fst
    |> Seq.sum

//let input = getInput "Day13_sample1.txt"
//input
//|> solve1
//|> Dump

let dividerPacket2 = parseData "[[2]]"
let dividerPacket6 = parseData "[[6]]"

let getInput2 p = 
    File.ReadAllText(getInputPath2022 p)
        .Replace("\r\n\r\n", "\r\n")
        .Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseData
    |> Seq.append [dividerPacket2; dividerPacket6]
    |> Seq.toArray

let input2 = getInput2 "Day13_sample1.txt" |> Dump


let solve2 input = 
    let compareBound l r = 
        match compare l r with
        | Some b -> if b then -1 else 1
        | None -> 0

    let sorted = 
        input
        |> Seq.sortWith compareBound
        |> Seq.toArray

    sorted |> Seq.iter (fun a -> 
        printfn "%s" (a.ToString(FSharp.Data.JsonSaveOptions.DisableFormatting)))

    let i1 = (Array.IndexOf(sorted, dividerPacket2) + 1)
    let i2 = (Array.IndexOf(sorted, dividerPacket6) + 1)
    i1 * i2
// |> Dump

input2 |> solve2