#time "on"
#load "../../Tools.fs"
#load "../../Tools/Digits.fs"
#r "nuget: NFluent"

open System
open System.IO
open System.Linq
open AdventOfCode
open Checked
open NFluent

let isValidPassword1 (digits: int64 seq) =
    let mutable hasDouble = false
    let hasDecreasingPair =
        digits
        |> Seq.pairwise
        |> Seq.exists (fun (a,b) ->
            if a = b then hasDouble <- true
            b < a)
    (hasDecreasingPair |> not) && hasDouble
    
Check.That(isValidPassword1 (digits 111111)).IsTrue();
Check.That(isValidPassword1 (digits 223450)).IsFalse();
Check.That(isValidPassword1 (digits 123789)).IsFalse();
    
let solve1 () =
    [347312L..805915L]
    |> Seq.map digits
    |> Seq.filter isValidPassword1
    |> Seq.length

solve1()

let isValidPassword2 digits =
    if not (isValidPassword1 digits) then
        false
    else
        digits
        |> Seq.groupBy id
        |> Seq.exists (fun (_, g) -> g |> Seq.length = 2)

let solve2 () =
    [347312L..805915L]
    |> Seq.map digits
    |> Seq.filter isValidPassword2
    |> Seq.length
 
solve2()