#load "../../Tools.fsx"
#r "nuget: FSharp.Data"
open FSharp.Data
open System
open System.IO
open Tools

let parseData (l:string) = JsonValue.Parse(l)

let getInput p = 
    File.ReadAllText(getInputPath2022 p).Split([|"\r\n\r\n"|], StringSplitOptions.None)
    |> Seq.map (fun s -> 
        let split = s.Split([|"\r\n"|], StringSplitOptions.None)
        split.[0] |> parseData, split[1] |> parseData)

let rec compare (left:JsonValue) (right:JsonValue) =
    match left, right with
    | JsonValue.Number l, JsonValue.Number r ->
        if l < r then
            //printfn "True because %A < %A" l r
            Some true
        else if l > r then
            //printfn "false because %A > %A" l r
            Some false
        else
            None
    | JsonValue.Array leftItems, JsonValue.Array rightItems ->
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
    | JsonValue.Array _, JsonValue.Number _ ->
        let wrappedRight = JsonValue.Array([|right|])
        compare left wrappedRight
    | JsonValue.Number _, JsonValue.Array _ ->
        let wrappedLeft = JsonValue.Array([|left|])
        compare wrappedLeft right
    | _ -> failwithf "Invalid json value"

let solve1 pairs =
    pairs 
    |> Seq.mapi (fun i (l,r) -> i + 1, compare l r)
    |> Seq.filter (fun (idx,b) -> b.IsSome && b.Value)
    |> Seq.map fst
    |> Seq.sum

let input = getInput "Day13.txt"
input
|> solve1
|> Dump

let dividerPacket2 = parseData "[[2]]"
let dividerPacket6 = parseData "[[6]]"

let getInput2 p = 
    File.ReadAllText(getInputPath2022 p)
        .Replace("\r\n\r\n", "\r\n")
        .Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseData
    |> Seq.append [dividerPacket2; dividerPacket6]
    |> Seq.toArray

let solve2 input = 
    let comparer l r = 
        match compare l r with
        | Some b -> if b then -1 else 1
        | None -> 0

    let sorted = 
        input
        |> Seq.sortWith comparer
        |> Seq.toArray

    //sorted |> Seq.iter (fun a -> 
    //    printfn "%s" (a.ToString(JsonSaveOptions.DisableFormatting)))

    let i1 = (Array.IndexOf(sorted, dividerPacket2) + 1)
    let i2 = (Array.IndexOf(sorted, dividerPacket6) + 1)
    i1 * i2

getInput2 "Day13.txt"
|> solve2