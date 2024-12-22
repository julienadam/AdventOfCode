#time "on"
#load "../../Tools.fs"
# r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Array.map int

let inline mix (secret:int64) spice = spice ^^^ secret
let inline prune a = a % 16777216L
let inline divideBy32 (a:int64) = a / 32L

let pseudorandom (r:int64) =
    let s1 = prune (mix r (r * 64L))
    let s2 = prune (mix s1 (divideBy32 s1))
    let s3 = prune (mix s2 (s2 * 2048L))
    s3

let nth initial n =
    let mutable secret = initial;
    for _ = 1 to n do
        secret <- pseudorandom secret
    secret

Check.That(nth 123L 10).Equals(5908254L)
Check.That(nth 1 2000).Equals(8685429L)
Check.That(nth 10 2000).Equals(4700978)
Check.That(nth 100 2000).Equals(15273692)
Check.That(nth 2024 2000).Equals(8667524)

let solve1 input =
    getInput input 
    |> Seq.map (fun secret -> nth secret 2000)
    |> Seq.sum

Check.That(solve1 "Day22_sample1.txt").Equals(37327623)
solve1 "Day22.txt"
