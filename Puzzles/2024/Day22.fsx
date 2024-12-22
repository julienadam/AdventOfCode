open System.Collections.Generic

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

let enumPseudoRandom seed quantity = seq {
    let mutable secret = seed;
    yield secret
    for _ = 1 to quantity do
        secret <- pseudorandom secret
        yield secret
}

let inline getPrice x = x % 10L

// Debug mode, use the ints themselves as the hash
//type Hash = (int64*int64*int64*int64)
//let hash i1 i2 i3 i4 = (i1,i2,i3,i4)
//let hashNext (i1,i2,i3,i4) i = (i2,i3,i4, i)

type Hash = int64
// build a hashcode from the price changes
// a price change is -9 to +9 so 5 bits for a total of 20 bits
// it is a very bad hashcode, but very convenient in our case
let inline hash i1 i2 i3 i4 =
    ((i1 + 10L) <<< 15) ||| ((i2 + 10L) <<< 10) ||| ((i3 + 10L) <<< 5) ||| (i4 + 10L)

// compute the next hash from the previous one
// mask the last 15 bits, shift left and OR the next bits
let mask = 0b111111111111111L
let inline hashNext prevHash i = (prevHash &&& mask) <<< 5 ||| (i + 10L)

let mapBestDeals secret = seq {
    let visited = new HashSet<Hash>()
    let workingSet = 
        enumPseudoRandom secret 2000 
        |> Seq.map getPrice
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> b, b - a)
        |> Seq.toArray

    // initial hash from the first 4 price changes
    let idx = 3
    let _, i1 = workingSet[idx-3]
    let _, i2 = workingSet[idx-2]
    let _, i3 = workingSet[idx-1]
    let price, i4 = workingSet[idx]
    let mutable h = hash i1 i2 i3 i4
    yield h, price
    visited.Add(h) |> ignore
    // then iterate and build hashes from the previous one
    for idx = 4 to workingSet.Length - 1 do
        let p, i = workingSet[idx]
        h <- hashNext h i
        if visited.Contains(h) |> not then
            yield h, p
            visited.Add(h) |> ignore
}

#r "nuget: FSharp.Collections.ParallelSeq"

open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let solve2 input =
    let buyers = getInput input
    let deals = new ConcurrentDictionary<Hash, int64>()
    buyers |> PSeq.iter(fun b ->
        for (changes, score) in mapBestDeals b do
            deals.AddOrUpdate(changes, score, (fun _ v -> v + score)) |> ignore
    )

    let bestDeal = 
        deals 
        |> Seq.maxBy (fun kvp -> kvp.Value)

    bestDeal.Value

// solve2 "Day22_sample2.txt"
solve2 "Day22.txt"
