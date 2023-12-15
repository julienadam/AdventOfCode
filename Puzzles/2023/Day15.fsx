#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

let getInput name = File.ReadAllText(getInputPath2023 name)

let hash (s:string) : int =
    s |> Seq.fold (fun v c -> (v + (c |> int)) * 17 % 256) 0 

Check.That(hash "HASH").IsEqualTo(52)

let solve1 input =
    getInput input
    |> ssplit ","
    |> Seq.sumBy hash

Check.That(solve1 "Day15_sample1.txt").IsEqualTo(1320)

solve1 "Day15.txt"

type Op =
    | Remove of string * int
    | Add of string * int * int

let parseOp (str:string) =
    if str.EndsWith("-") then
        let label = str.Substring(0, str.Length - 1)
        (label, label |> hash) |> Remove
    else
        let l, r = str |> ssplit2 "="
        (l, l |> hash, r |> int) |> Add

let solve2 input =
    let ops = 
        getInput input
        |> ssplit ","
        |> Seq.map parseOp
    
    let boxes = 
        ops |> Seq.fold (fun (m:Map<int, (string*int) list>) op -> 
            match op with
            | Remove (label, box) ->
                match m |> Map.tryFind box with
                | Some lenses -> 
                    let filtered = lenses |> List.filter (fun (lens, _) -> lens <> label)
                    m |> Map.add box (filtered)
                | _ -> 
                    m
            | Add (label, box, focal) ->
            match m |> Map.tryFind box with
                | Some lenses -> 
                    let newBox = 
                        match lenses |> List.tryFindIndex (fun (l,f) -> l = label) with
                        | Some i -> lenses |> List.updateAt i (label, focal)
                        | None -> (label, focal) :: lenses
                    m |> Map.add box newBox
                | None ->
                    m |> Map.add box [(label,focal)]

        ) Map.empty

    boxes 
    |> Map.toSeq 
    |> Seq.sumBy (fun (box, lenses) ->
        lenses 
        |> Seq.rev 
        |> Seq.mapi (fun pos (label, focal) ->
            (box + 1) * (pos + 1) * focal
        )
        |> Seq.sum
    )

Check.That(solve2 "Day15_sample1.txt").IsEqualTo(145)

solve2 "Day15.txt"