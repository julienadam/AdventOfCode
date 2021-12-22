#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Tools

let tuple3ToMapOfTuple2 (s:(int*int*int) seq) =
    s
    |> Seq.map (fun (a,b,c) -> ((a,b), c))
    |> Map.ofSeq

let getInput fileName = 
    let inline mapPixel c = match c with | '#' -> 1 | '.' -> 0 | _ -> failwithf "Invalid input char %c" c
    let lines = File.ReadAllLines (getInputPath fileName)
    let enhancement = lines.[0] |> Seq.map mapPixel |> Seq.toList

    assert(enhancement.Length = 512)

    let inputImage =
        lines 
        |> Seq.skip 2 
        |> Seq.map (fun line -> line |> Seq.map mapPixel)
        |> array2D
        |> Array2DTools.enumArray2d
        |> tuple3ToMapOfTuple2

    enhancement, inputImage

let inline getPixelAt (row:int) (col:int) defaultVal (image: Map<int*int, int>)  =
    match image.TryFind (row, col) with
    | Some i -> i
    | None -> defaultVal

let getPixelsAround (row:int) (col:int) (image: Map<int*int, int>) defaultVal = seq {

    yield image |> getPixelAt (row - 1) (col - 1) defaultVal
    yield image |> getPixelAt (row - 1) col defaultVal
    yield image |> getPixelAt (row - 1) (col + 1) defaultVal
    yield image |> getPixelAt row (col - 1) defaultVal
    yield image |> getPixelAt row col defaultVal
    yield image |> getPixelAt row (col + 1) defaultVal
    yield image |> getPixelAt (row + 1) (col - 1) defaultVal
    yield image |> getPixelAt (row + 1) col defaultVal
    yield image |> getPixelAt (row + 1) (col + 1) defaultVal
}

let boolsToIntRev bools = bools |> Array.rev |> Array.mapi (fun i b -> b <<< i) |> Array.sum

let getBoundsOfNextImage (image:Map<int*int, int>) =
    let xs = image |> Map.keys |> Seq.map fst |> Seq.toList
    let ys = image |> Map.keys |> Seq.map snd |> Seq.toList
    (((xs |> Seq.min) - 2), ((xs |> Seq.max) + 2)), (((ys |> Seq.min) - 2), ((ys |> Seq.max) + 2)) 

let getDefaultValueForStep step (enhancement:int list) =
    match step % 2, enhancement.[0] with
    | _, 0 -> 0
    | 1, _ -> enhancement.[0]
    | 0, _ -> enhancement.[511]
    | _ -> failwithf "unsupported default value situation"


let buildNextImage image step (enhancement:int list) = seq {
    let ((minR, maxR), (minC, maxC)) = getBoundsOfNextImage image
    //printfn "New bounds : row [%i to %i], col [%i to %i]"  minR maxR minC maxC

    let defaultVal = getDefaultValueForStep step enhancement

    for r = minR to maxR do
        for c = minC to maxC do
            let enhanceIndex = getPixelsAround r c image defaultVal |> Seq.toArray |> boolsToIntRev
            yield (r, c, enhancement.[enhanceIndex])
}

let printImage image step enhancement =
    let ((minR, maxR), (minC, maxC)) = getBoundsOfNextImage image

    let defaultVal = getDefaultValueForStep step enhancement
    
    for r = minR to maxR do
        for c = minC to maxC do
            match getPixelAt r c defaultVal image with
            | 1 -> printf "#"
            | 0 -> printf "."
            | i -> failwithf "invalid pixel value %i" i
        printfn ""

let solve1 fileName =
    let enhancement, image = getInput fileName
    let mutable step = 0
    
    let defaultVal = getDefaultValueForStep step enhancement
    //printfn "Default val %i" defaultVal
    //printImage image step enhancement

    let afterStep1 = 
        buildNextImage image step enhancement
        |> tuple3ToMapOfTuple2
    
    step <- step + 1

    //printImage afterStep1 step enhancement
    //printfn "After step 1 : %i pixels lit" afterStep1.Count

    let afterStep2 =
        buildNextImage afterStep1 step enhancement
        |> tuple3ToMapOfTuple2

    step <- step + 1
   
    // printImage afterStep2 step enhancement
    // printfn "After step 1 : %i pixels lit" afterStep2.Count
    
    afterStep2 |> Map.values |> Seq.filter (fun c -> c = 1) |> Seq.length

assert(solve1 "Day20_Sample1.txt" = 35)

let sw = Stopwatch.StartNew()
printfn "Day19 Part 1: %i. Solved in %A" (solve1 "Day20.txt") sw.Elapsed


