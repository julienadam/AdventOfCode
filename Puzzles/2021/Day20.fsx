#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open AdventOfCode

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
    // Expand the image bounds by 2 in all directions for the next step
    let xs = image |> Map.keys |> Seq.map fst |> Seq.toList
    let ys = image |> Map.keys |> Seq.map snd |> Seq.toList
    (((xs |> Seq.min) - 2), ((xs |> Seq.max) + 2)), (((ys |> Seq.min) - 2), ((ys |> Seq.max) + 2)) 

let getDefaultValueForStep step (enhancement:int list) =
    // Special case when enhancement.[0] is 1, all the "infinite" pixels
    // switch on, and possibly off again if enhancement.[511] is 0
    match step % 2, enhancement.[0] with
    | _, 0 -> 0
    | 1, _ -> enhancement.[0]
    | 0, _ -> enhancement.[511]
    | _ -> failwithf "unsupported default value situation"


let buildNextImage image step (enhancement:int list) = seq {
    let ((minR, maxR), (minC, maxC)) = getBoundsOfNextImage image
    
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

let enhance times image enhancement =
    [0..times - 1] |> Seq.fold (fun state step -> 
        printfn "Step %i" step
        buildNextImage state step enhancement
        |> tuple3ToMapOfTuple2
        )
        image

let solve1 fileName =
    let enhancement, image = getInput fileName
    let finalImage = enhance 2 image enhancement

    printImage finalImage 1 enhancement

    finalImage 
    |> Map.values 
    |> Seq.filter (fun c -> c = 1) |> Seq.length


assert(solve1 "Day20_Sample1.txt" = 35)

let sw = Stopwatch.StartNew()
printfn "Day20 Part 1: %i. Solved in %A" (solve1 "Day20.txt") sw.Elapsed

let solve2 fileName =
    let enhancement, image = getInput fileName
    let finalImage = enhance 50 image enhancement

    printImage finalImage 49 enhancement

    finalImage 
    |> Map.values 
    |> Seq.filter (fun c -> c = 1) |> Seq.length

// assert(solve2 "Day20_Sample1.txt" = 3351)


sw.Restart()
printfn "Day20 Part 1: %i. Solved in %A" (solve2 "Day20.txt") sw.Elapsed
