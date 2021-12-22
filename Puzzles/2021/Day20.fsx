#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Tools

let a = [[1;2;3;4;5];[6;7;8;9;10]] |> array2D
a |> Dump
a.[1,2] |> Dump

let getInput fileName = 
    let inline mapPixel c = match c with | '#' -> 1 | '.' -> 0 | _ -> failwithf "Invalid input char %c" c
    let lines = File.ReadAllLines (getInputPath fileName)
    let enhancement = lines.[0] |> Seq.map mapPixel |> Seq.toList
    let inputImage =
        lines 
        |> Seq.skip 2 
        |> Seq.map (fun line -> line |> Seq.map mapPixel)
        |> array2D
        |> Array2DTools.enumArray2d
        |> Seq.filter (fun (r,c,v) -> v = 1)
        |> Seq.map (fun (r,c,_)-> r,c)
        |> Set.ofSeq

    enhancement, inputImage

let enhancement, image = getInput "Day20_Sample1.txt"

let inline getPixelAt (row:int) (col:int) (image: Set<int*int>) =
    if image |> Set.contains (row, col) then 1 else 0

let getPixelsAround (row:int) (col:int) (image: Set<int*int>) = seq {

    yield image |> getPixelAt (row - 1) (col - 1)
    yield image |> getPixelAt (row - 1) col
    yield image |> getPixelAt (row - 1) (col + 1)
    yield image |> getPixelAt row (col - 1)
    yield image |> getPixelAt row col
    yield image |> getPixelAt row (col + 1)
    yield image |> getPixelAt (row + 1) (col - 1)
    yield image |> getPixelAt (row + 1) col
    yield image |> getPixelAt (row + 1) (col + 1)
}

let boolsToIntRev bools = bools |> Array.rev |> Array.mapi (fun i b -> b <<< i) |> Array.sum
getPixelsAround 2 2 image |> Seq.toArray |> boolsToIntRev

