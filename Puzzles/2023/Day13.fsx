
#time
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent
open System.Collections.Generic

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

let isReflectionPoint (cells : char array) i  =
    let left, right = cells |> Array.splitAt(i)
    left 
    |> Seq.rev 
    |> Seq.zip right
    |> Seq.forall (fun (a,b) -> a = b)

Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 5).IsTrue()
Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 4).IsFalse()
Check.That(isReflectionPoint (("#.##..##.").ToCharArray()) 3).IsFalse()

let findPossibleReflectionPoints (cache:Dictionary<string, Set<int>>) (cells : char array) =
    let str = String(cells)
    if cache.ContainsKey(str) then
        // printfn "cache hit for %s" str
        cache[str]
    else
        let r = [1..cells.Length-1] |> Seq.filter (isReflectionPoint cells) |> Set.ofSeq
        cache.Add(str, r) |> ignore
        r

let findVerticalReflectionPoints cache (grid : char array2d)  =
    [0..(grid |> Array2D.length1)-1]
    |> Seq.map (fun i -> findPossibleReflectionPoints cache grid[i,*])
    |> Set.intersectMany
    |> Set.toList

let findHorizontalReflectionPoints cache (grid : char array2d) =
    [0..(grid |> Array2D.length2)-1]
    |> Seq.map (fun i -> findPossibleReflectionPoints cache grid[*,i])
    |> Set.intersectMany
    |> Set.toList


let memo = new Dictionary<string, Set<int>>()
let sampleGrids = getInput "Day13_sample.txt"
Check.That(findVerticalReflectionPoints memo sampleGrids[0]).IsEqualTo([5])
Check.That(findHorizontalReflectionPoints memo sampleGrids[0]).IsEqualTo([])
Check.That(findVerticalReflectionPoints memo sampleGrids[1]).IsEqualTo([])
Check.That(findHorizontalReflectionPoints memo sampleGrids[1]).IsEqualTo([4])


let findReflectionPoints cache grid =
        let h = (findHorizontalReflectionPoints cache grid)
        match h with
        | a::_ -> a,0
        | [] -> 
            let v = (findVerticalReflectionPoints cache grid)
            match v with
            | b::_ -> 0, b
            | [] -> (0,0)

let solve1 input =
    let cache = new Dictionary<string, Set<int>>()

    getInput input 
    |> Seq.map (findReflectionPoints cache)
    |> Seq.toList |> Dump
    |> Seq.sumBy(fun (h, v) -> v + 100 * h)
    
Check.That(solve1 "Day13_sample.txt").IsEqualTo(405)
solve1 "Day13.txt"

open Array2DTools

let findReflectionPointsExcept cache grid excludedH excludedV =
        let h = (findHorizontalReflectionPoints cache grid)
        match h |> List.filter (fun h -> h <> excludedH) with
        | a::_ -> a,0
        | [] -> 
            let v = (findVerticalReflectionPoints cache grid)
            match v |> List.filter (fun v -> v <> excludedV) with
            | b::_ -> 0, b
            | [] -> (0,0)


let solve2 input =
    let cache = new Dictionary<string, Set<int>>()
    let grids = getInput input 

    let findReflectionWithSmudge (grid:char array2d) =
        let (origH, origV) = findReflectionPoints cache grid

        grid 
        |> enumArray2d
        |> Seq.pick (fun (r,c,v) ->
            let old = grid[r,c]
            let repl = match old with | '.' -> '#' | '#' -> '.' | _ -> failwith "nope"
            Array2D.set grid r c repl
            let result = 
                match findReflectionPointsExcept cache grid origH origV with 
                | 0,0 -> None 
                | x -> Some x
            Array2D.set grid r c old
            result
        )

    grids 
    |> Seq.map findReflectionWithSmudge
    |> Seq.sumBy(fun (h, v) -> v + 100 * h)

Check.That(solve2 "Day13_sample.txt").IsEqualTo(400)
solve2 "Day13.txt"

