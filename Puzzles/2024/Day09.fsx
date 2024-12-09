open System.Text

#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllText(getInputPath2024 name)

let sample1 = "2333133121414131402"

let ctoi (c:char) = (c |> int) - ('0' |> int)

let expand (input:char array) =
    let debug = new StringBuilder()
    // keep track of 
    // - position in input
    // - position in output
    // - position of next file blocks to move from the end after the current blocks are moved
    // - next blocks to move from the end (how many and their id)
    let rec expandRec srcIdx outIdx movIdx (mov:(int*int) option) (checksum:int64) =
        if srcIdx > movIdx then
            match mov with
            | Some (qty, id) ->
                // Add the remaining quantity
                let mutable checksum = checksum
                for x = 0 to qty - 1 do
                    //printfn "Adding %i at position %i (remaining)" id (outIdx + x)
                    debug.Append(id) |> ignore
                    checksum <- checksum + ((id * (outIdx + x)) |> int64)
                checksum
            | None -> checksum
        else
            let repeats = ctoi input[srcIdx]
            if srcIdx % 2 = 0 then
                // File blocks, get their id
                let id = srcIdx / 2
                let mutable nc = checksum
                // Update the checksum
                for x = 0 to repeats - 1 do
                    // Add the id multiplied by the position in the output
                    // Do that for each repetition in the output
                    //printfn "Adding %i at position %i (file)" id (outIdx + x)
                    debug.Append(id) |> ignore
                    nc <- nc + ((id * (outIdx + x)) |> int64)

                // Advance the input by one, the output by the number of repeats and update the checksum
                expandRec (srcIdx + 1) (outIdx + repeats) movIdx mov nc
            else
                // Free blocks
                let mutable checksum = checksum
                let mutable repeats = repeats
                let mutable outIdx = outIdx
                let mutable mov = mov
                let mutable movIdx = movIdx

                while repeats > 0 do 
                    match mov with
                    | Some (qty, id) when qty > 0 ->
                        debug.Append(id) |> ignore
                        //printfn "Adding %i at position %i (free)" id outIdx
                        checksum <- checksum + ((id * outIdx) |> int64)
                        outIdx <- outIdx + 1
                        mov <- Some (qty - 1, id)
                        repeats <- repeats - 1
                    | _ ->
                        let nextId = movIdx / 2
                        let qty = ctoi input[movIdx]
                        movIdx <- movIdx - 2
                        mov <- Some (qty, nextId)

                expandRec (srcIdx + 1) outIdx movIdx mov checksum

    let result = expandRec 0 0 (input.Length - 1) None 0
    printfn "%s" (debug.ToString())
    result

let solve1 (input:char array) = expand input

// solve1 (sample1.ToCharArray())

solve1 ((getInput "Day09.txt").ToCharArray())

let printblocks blocks =
    let mutable currP = 0
    blocks |> Seq.iter (fun (p,r,id) ->
        for _ = currP to p - 1 do
            printf "."
            currP <-currP + 1
        for _ = 1 to r do
            printf "%i" id
            currP <-currP + 1
    )
    printfn ""
    blocks

let expand2 (input:char array) =
    let diskMap = 
        input 
        |> Seq.mapi (fun id c -> (id, ctoi c)) 
        |> Seq.scan (fun (index, repeats, _) (id, r) -> ((index + repeats), r, id)) (0,0,0)
        |> Seq.skip 1

    let holes = 
        diskMap 
        |> Seq.filter (fun (_,_,i) -> i % 2 = 1) 
        |> Seq.map (fun (index, size, _) -> index, size) 
        |> Seq.toArray
        // |> Dump

    let files = 
        diskMap 
        |> Seq.filter (fun (_,_,i) -> i % 2 = 0) 
        |> Seq.map (fun (index, size, id) -> index, size, id / 2) 
        // |> Seq.toArray |>Dump
        |> printblocks

    // check remaining block
    // check holes
    // if a fitting hole is found
    //  remove the block from the block list
    //  resize the hole or remove it altogether
    // if it does not
    //  remove the block from the block list
    //  put it in the positionned block list
    // stop when no remaining blocks

    let rec fillHoles holes remainingBlocks positionnedBlocks =
        // positionnedBlocks |> Seq.sortBy fst3 |> printblocks |> ignore
        match remainingBlocks with 
        | [] -> positionnedBlocks
        | (blockPos, blockSize, fileId)::tailBlocks ->
            printfn "Trying to move block %A" (blockPos, blockSize, fileId)
            match holes |> Array.tryFindIndex (fun (_, holeSize) -> holeSize >= blockSize) with
            | Some holeIndex ->
                let (holePos, holeSize) = holes[holeIndex]
                printfn "Found matching hole %A" (holePos, holeSize)
                if blockSize = holeSize then
                    printfn "Removing hole %A" (holePos, holeSize)
                    fillHoles (holes |> Array.removeAt holeIndex) tailBlocks ((holePos, blockSize, fileId)::positionnedBlocks)
                else
                    printfn "Reducing hole %A to %A" (holePos, holeSize) (holePos + blockSize, holeSize - blockSize)
                    Array.set holes holeIndex (holePos + blockSize, holeSize - blockSize)
                    fillHoles holes tailBlocks ((holePos, blockSize, fileId)::positionnedBlocks)
            | None ->
                printfn "No hole found for %A, fixing in place" (blockPos, blockSize, fileId)
                fillHoles holes tailBlocks ((blockPos, blockSize, fileId)::positionnedBlocks)
           
    fillHoles holes (files |> Seq.skip 1 |> Seq.rev |> Seq.toList) [files |> Seq.head]

let checksum blocks = blocks |> Seq.sumBy (fun (p,r,id) -> [0..r-1] |> Seq.sumBy (fun offset -> ((p+offset)*id) |> int64))

let solve2 (input:char array) = 
    let blocks = expand2 input
    blocks 
    |> Seq.sortBy fst3 
    // |> Seq.toArray |> Dump
    // |> printblocks
    |> checksum

solve2 (sample1.ToCharArray())

solve2 ((getInput "Day09.txt").ToCharArray())


//if blockPos < holePos then
//    fillHoles holes remainingBlocks ((holePos, blockSize, fileId)::positionnedBlocks)
//else if blockSize = holeSize then
//    fillHoles remainingHoles remainingBlocks ((holePos, blockSize, fileId)::positionnedBlocks)
//else if blockSize < holeSize then
//    fillHoles ((holePos+blockSize, holeSize - blockSize)::remainingHoles) remainingBlocks ((holePos, blockSize, fileId)::positionnedBlocks)
//else
//    fillHoles holes remainingBlocks ((blockPos, blockSize, fileId)::positionnedBlocks)

//match remainingBlocks |> Array.tryFindIndex (fun (_,blockSize,_) -> blockSize <= holeSize) with
//| None -> List.concat [positionnedBlocks; remainingBlocks |> Array.toList]
//| Some blockIndex ->
//    let (_, blockSize, fileId) = remainingBlocks[blockIndex]
//    if blockSize = holeSize then
//        fillHoles tail (remainingBlocks |> Array.removeAt blockIndex) ((holePos, blockSize, fileId)::positionnedBlocks)
//    else
//        fillHoles ((holePos+blockSize, holeSize - blockSize)::tail) (remainingBlocks |> Array.removeAt blockIndex) ((holePos, blockSize, fileId)::positionnedBlocks)
