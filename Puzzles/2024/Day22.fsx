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

let mapBestDeals secret =
    enumPseudoRandom secret 2000 
    |> Seq.map getPrice
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> b, b - a)
    |> Seq.windowed 4
    |> Seq.map (fun window -> 
        window |> Array.map snd |> tupleize4, window |> Array.last |> fst
    )

getInput "Day22.txt" 
|> Seq.sumBy(fun s -> (mapBestDeals s |> Seq.length))

let solve2 input =
    let buyers = getInput input
    let deals = new Dictionary<int64*int64*int64*int64, int64>()
    for b in buyers do
        let visited = new HashSet<int64*int64*int64*int64>()
        // printfn "Adding prices from buyer %i" b
        for (changes, score) in mapBestDeals b do
            if visited.Contains(changes) then
                ()
            else
                visited.Add(changes) |> ignore
                match deals.TryGetValue(changes) with
                | true, c ->
                    deals[changes] <- c + score
                | false, _ -> 
                    deals[changes] <- score

    let bestDeal = 
        deals 
        |> Seq.maxBy (fun kvp -> kvp.Value)

    bestDeal

solve2 "Day22_sample2.txt"
solve2 "Day22.txt"

// scoreChanges |> Map.find [|-9L;0L;3L;2L|]
