open System.Text

#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent
open Checked

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Seq.map (fun s -> 
        let split = s |> ssplit " "
        let (pc,pr) = split[0].Substring(2).Split(",") |> Array.map int |> tupleize2
        let (vc,vr) = split[1].Substring(2).Split(",") |> Array.map int |> tupleize2
        (pr,pc),(vr,vc)
    )

let (++) (r1,c1) (r2,c2) = (r1+r2, c1+c2)
let mul (r1,c1) (i:int) = (i*r1, i*c1)

let move (pos:int*int) (vel:int*int) (iterations:int) (height,width)=
    let r,c = pos ++ (mul vel iterations)
    let boundedR = if r < 0 then r + (-r / height + 1) * height else r
    let boundedC = if c < 0 then c + (-c / width + 1) * width else c
    (boundedR % height), (boundedC % width)

Check.That(move (4,2) (-3,2) 1 (7, 11)).Equals((1,4))
Check.That(move (4,2) (-3,2) 2 (7, 11)).Equals((5,6))
Check.That(move (4,2) (-3,2) 3 (7, 11)).Equals((2,8))
Check.That(move (4,2) (-3,2) 4 (7, 11)).Equals((6,10))
Check.That(move (4,2) (-3,2) 5 (7, 11)).Equals((3,1))

let solve1 input area =
    let finalPositions = 
        getInput input 
        |> Seq.map (fun (p, v) -> move p v 100 area)

    let h, w = area
    let midR = h / 2
    let midC = w / 2

    finalPositions 
    |> Seq.filter (fun (r,c) -> r <> midR && c <> midC) // remove bots on the middle row and columns
    |> Seq.groupBy (fun (r,c) -> (r < midR, c < midC)) // split into quadrants
    |> Seq.map (fun (k,g) -> g |> Seq.length) // count # in each quadrant
    |> Seq.fold (*) 1 // and multiply everything

solve1 "Day14_sample1.txt" (7, 11)

solve1 "Day14.txt" (103, 101)

let printGrid (h,w) (positions:(int*int) seq) (s:int)=
    let setOfPos = positions |> Set
    let sb = new StringBuilder()
    sb.AppendLine().Append(s).Append("seconds").AppendLine() |> ignore
    for r = 0 to h - 1 do
        for c = 0 to w - 1 do
            if setOfPos.Contains(r,c) then
                sb.Append('█') |> ignore
            else
                sb.Append(' ') |> ignore

        sb.AppendLine() |> ignore
    sb

let solve2 input area =
    let initialPos = getInput input 
    use sw = File.CreateText(@"c:\temp\day14.txt")
    // This is based on trial and error
    // I started by dumping the first 2000 patterns for each second
    // there are 2 patterns that emerge every 101 and 103 seconds after a while
    // a vertical one and an horizontal one, I noted the first occurence for both patterns
    // 97s for the 101 repeats and 50s for the 103 repeats in my case
    // and used that to print the first 2000 patterns for 97+s*101 and 50+s*103 in a file
    // the christmas tree is clearly visible in the output file just by scrolling
    for s = 0 to 2000 do
        printf "."
        let sVerticalPattern = (97+s*101)
        let finalPositions = initialPos |> Seq.map (fun (p, v) -> move p v sVerticalPattern area)
        sw.WriteLine(printGrid area finalPositions sVerticalPattern)
        
        let sHorizontalPattern = (50+s*103)
        let finalPositions = initialPos |> Seq.map (fun (p, v) -> move p v sHorizontalPattern area)
        sw.WriteLine(printGrid area finalPositions sHorizontalPattern)
    ()

solve2 "Day14.txt" (103, 101)

//(7672 - 50) % 103 = 0
//(7672 - 97) % 101 = 0
