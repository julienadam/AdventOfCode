
#time
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

// let parseLine line = line |> Seq.map (fun c -> match c with |'.' )

let getInput name = 
    File.ReadAllText(getInputPath2023 name)
    |> ssplit "\r\n\r\n"
    |> Seq.map (fun lines -> 
        lines 
        |> ssplit "\r\n" 
        |> Array.map (fun line -> line.ToCharArray())
    )
    |> Seq.map (fun g -> array2D g)
    |> Seq.toList

let isReflectionPoint (cell : char array) i  =
    let left, right = cell |> Array.splitAt(i)
    left 
    |> Seq.rev 
    |> Seq.zip right
    |> Seq.forall (fun (a,b) -> a = b)

Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 5).IsTrue()
Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 4).IsFalse()
Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 3).IsFalse()

let findPossibleReflectionPoints (cell : char array) =
    [1..cell.Length-1] |> Seq.filter (isReflectionPoint cell) |> Set.ofSeq

let findVerticalReflectionPoint (grid : char array2d) =
    [0..(grid |> Array2D.length1)-1]
    |> Seq.map (fun i -> findPossibleReflectionPoints grid[i,*])
    |> Set.intersectMany
    |> Seq.tryExactlyOne

let findHorizontalReflectionPoint (grid : char array2d) =
    [0..(grid |> Array2D.length2)-1]
    |> Seq.map (fun i -> findPossibleReflectionPoints grid[*,i])
    |> Set.intersectMany
    |> Seq.tryExactlyOne

let sampleGrids = getInput "Day13_sample.txt"
Check.That(findVerticalReflectionPoint sampleGrids[0]).IsEqualTo(Some 5)
Check.That(findHorizontalReflectionPoint sampleGrids[0]).IsEqualTo(None)
Check.That(findVerticalReflectionPoint sampleGrids[1]).IsEqualTo(None)
Check.That(findHorizontalReflectionPoint sampleGrids[1]).IsEqualTo(Some 4)

let solve1 input =
    getInput input 
    |> Seq.map (fun grid ->
        let h = (findHorizontalReflectionPoint grid) |> Option.defaultValue 0
        let v = (findVerticalReflectionPoint grid) |> Option.defaultValue 0
        h, v
    )
    |> Seq.sumBy(fun (h, v) -> v + 100 * h)
    
Check.That(solve1 "Day13_sample.txt").IsEqualTo(405)

solve1 "Day13.txt"