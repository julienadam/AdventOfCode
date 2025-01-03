open System.Text
open System.Diagnostics

#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllText(getInputPath2024 name)

let sample1 = "2333133121414131402"

let inline ctoi (c:char) = (c |> int) - ('0' |> int)

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

let defrag (input:char array) =
    let diskMap = 
        input 
        |> Seq.mapi (fun id c -> (id, ctoi c)) 
        |> Seq.scan (fun (index, repeats, _) (id, r) -> ((index + repeats), r, id)) (0,0,0)
        |> Seq.skip 1

    // Put the index and size of each free space in an array
    let freeSpaces = 
        diskMap 
        |> Seq.filter (fun (_,_,i) -> i % 2 = 1) 
        |> Seq.map (fun (index, size, _) -> index, size) 
        |> Seq.toArray

    // Put file index, size and id in a sequence
    let files = 
        diskMap 
        |> Seq.filter (fun (_,_,i) -> i % 2 = 0) 
        |> Seq.map (fun (index, size, id) -> index, size, id / 2) 

    // check remaining files list
    //  stop when no remaining files
    // take the first remaining file
    // check free spaces
    // if a free space big enough to fit the file is found
    //  remove the file from the remaining file list
    //  resize the free space or remove it altogether
    // else
    //  remove the file from the file list
    //  put it in the defragged file list

    let rec defragRec freeSpaces remainingFiles defraggedFiles =
        match remainingFiles with 
        | [] -> defraggedFiles
        | (filePos, fileSize, fileId)::tail ->
            match freeSpaces |> Array.tryFindIndex (fun (freePos, freeSize) -> freeSize >= fileSize && freePos < filePos) with
            | Some freeIndex ->
                let (freePos, freeSize) = freeSpaces[freeIndex]
                if fileSize = freeSize then
                    defragRec (freeSpaces |> Array.removeAt freeIndex) tail ((freePos, fileSize, fileId)::defraggedFiles)
                else
                    Array.set freeSpaces freeIndex (freePos + fileSize, freeSize - fileSize)
                    defragRec freeSpaces tail ((freePos, fileSize, fileId)::defraggedFiles)
            | None ->
                defragRec freeSpaces tail ((filePos, fileSize, fileId)::defraggedFiles)

    // start recursion with the first file set in place and the rest in reverse order is our remaining block list
    defragRec freeSpaces (files |> Seq.skip 1 |> Seq.rev |> Seq.toList) [files |> Seq.head]

let checksum blocks = blocks |> Seq.sumBy (fun (p,r,id) -> [0..r-1] |> Seq.sumBy (fun offset -> ((p+offset)*id) |> int64))

let solve2 (input:char array) = 
    let files = defrag input
    files 
    |> Seq.sortBy fst3 // |> Seq.toArray |> Dump
    |> checksum
    
// solve2 (sample1.ToCharArray())

solve2 ((getInput "Day09.txt").ToCharArray())
