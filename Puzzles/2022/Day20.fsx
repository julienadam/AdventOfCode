#time
#load "../../Tools.fsx"

open System
open System.IO
open Checked
open Tools

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map Int64.Parse
    |> Seq.toArray

let solve1 mixCount (input:int64 array) =

    let encryptedFile = new System.Collections.Generic.List<int64*int>(
        input |> Seq.indexed |> Seq.map (fun (index, value) -> value, index)
    )

    let listToMix = new System.Collections.Generic.List<int64*int>(encryptedFile)
    let count = encryptedFile.Count

    for _ = 1 to mixCount do
        encryptedFile |> Seq.iter(fun (value, index) ->
            let oldIndex = listToMix.IndexOf((value, index))
            let ni = ((oldIndex |> int64) + value) % int64(count - 1)
            let newIndex = if ni < 0 then int64(count) + ni - 1L else ni
            listToMix.Remove((value, index)) |> ignore
            listToMix.Insert((int)newIndex, (value, index))
        )

    let indexZero = listToMix.FindIndex(fun (value,_) -> value = 0)
    let index1000 = (1000 + indexZero) % count
    let index2000 = (2000 + indexZero) % count
    let index3000 = (3000 + indexZero) % count

    (listToMix[index1000] |> fst) + 
    (listToMix[index2000] |> fst) + 
    (listToMix[index3000] |> fst)

let solve2 (input:int64 array) =
    input 
    |> Array.map (fun i -> i * 811589153L)
    |> solve1 10

getInput "Day20.txt"
|> solve1 1

getInput "Day20.txt"
|> solve2
