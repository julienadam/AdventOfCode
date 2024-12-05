#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    let toPair s = s |> ssplit "|" |> Array.map int |> tupleize2

    let ordering, rawUpdates = 
        File.ReadAllText(getInputPath2024 name)
        |> ssplit "\n\n"
        |> tupleize2
    // map the page rules by source page
    let orderingRules = ordering |> splitLines |> Seq.map toPair |> Seq.groupBy fst |> Seq.map (fun (k,v) -> k, v |> Seq.map snd |> Seq.toArray) |> Map.ofSeq
    orderingRules, rawUpdates |> splitLines |> Seq.map splitIntList


let isSingleUpdateCorrectlyOrdered (ordering:Map<int, int array>) (update:int array) =
    let posMap = update |> Seq.mapi (fun idx v -> (v,idx)) |> Map.ofSeq

    // for each page in update
    // get its position in the update
    // for each rule specifying the page and another page, get its position (if any)
    // if there's a position it must be after the current page, if not, the update is not correctlt ordered
    update 
    |> Seq.mapi (fun idx currPage -> (idx, currPage)) 
    |> Seq.exists (fun (idx, currPage) ->
        match ordering.TryFind(currPage) with
        | Some pagesThatShouldBeAfter -> 
            pagesThatShouldBeAfter 
            |> Seq.exists (fun p -> 
                // Try to find the page specified in the rule and if it exists, check the ordering rule
                // If we find a page that fails the ordering rule, return true
                match posMap.TryFind p with | Some otherPageIndex -> idx > otherPageIndex | None -> false)
        | None -> false
    )

let solve1 input =
    let ordering, updates = 
        getInput input // |> Dump

    updates 
    |> Seq.filter (isSingleUpdateCorrectlyOrdered ordering >> not)
    |> Seq.map (fun a -> a[a.Length / 2])
    |> Seq.sum

solve1 "Day05.txt"

let fixSingleUpdate (ordering:Map<int, int array>) (update:int array) =

    let rec fixSingleUpdaterec (currentUpdate:int array) = 
        let posMap = currentUpdate |> Seq.mapi (fun idx v -> (v,idx)) |> Map.ofSeq
        let errors = seq {
            for i in [0..update.Length-1] do
                let currentPage = currentUpdate[i]
                match ordering.TryFind(currentPage) with
                | Some pagesThatShouldBeAfter -> 
                    for j in [0..pagesThatShouldBeAfter.Length-1] do
                        let otherPage = pagesThatShouldBeAfter[j]
                        match posMap.TryFind otherPage with
                        | Some otherIndex ->
                            if i > otherIndex then
                                // printfn "Page %i at %i is incorrectly positionned after page %i at %i " currentPage i otherPage otherIndex
                                yield i,otherIndex
                        | None -> ()
                | None -> ()
                ()
            ()
        }

        match errors |> Seq.tryHead with
        | Some (source, dest) -> 
            // printfn "Swapping %i with %i in %A" source dest currentUpdate
            let sourceVal = currentUpdate[source]
            let destVal = currentUpdate[dest]
            Array.set currentUpdate source destVal 
            Array.set currentUpdate dest sourceVal 
            fixSingleUpdaterec currentUpdate
        | None -> currentUpdate

    fixSingleUpdaterec update

let solve2 input =
    let ordering, updates = 
        getInput input // |> Dump

    let incorrectlyOrderedUpdate = updates |> Seq.filter (isSingleUpdateCorrectlyOrdered ordering) // |> Seq.toArray |> Dump
    incorrectlyOrderedUpdate 
    |> Seq.map (fixSingleUpdate ordering) // |> Seq.toArray |> Dump
    |> Seq.map (fun a -> a[a.Length / 2]) // |> Dump
    |> Seq.sum

solve2 "Day05.txt"
