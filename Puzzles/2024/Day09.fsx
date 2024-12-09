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

// 0099811188827773336446555566
// 0099811188827773336446555566

solve1 (sample1.ToCharArray())

solve1 ((getInput "Day09.txt").ToCharArray())
