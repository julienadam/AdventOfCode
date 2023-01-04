#time
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open Checked
open AdventOfCode
open NFluent

let getInput p =
    File.ReadAllLines(getInputPath2022 p)

let snafuToDec (snafu:string) =
    snafu 
    |> Seq.rev
    |> Seq.mapi (fun i c ->
        let digit = 
            match c with
            | '2' -> 2.0
            | '1' -> 1.0
            | '0' -> 0.0
            | '-' -> -1.0
            | '=' -> -2.0
            | _ -> failwithf "Invalid snafu digit %c" c
        digit * (5.0 ** i)
    )
    |> Seq.sum

Check.That(snafuToDec "1=-0-2").IsEqualTo(1747)
Check.That(snafuToDec "12111").IsEqualTo(906)
Check.That(snafuToDec "2=0=").IsEqualTo(198)
Check.That(snafuToDec "21").IsEqualTo(11)
Check.That(snafuToDec "2=01").IsEqualTo(201)
Check.That(snafuToDec "111").IsEqualTo(31)
Check.That(snafuToDec "20012").IsEqualTo(1257)
Check.That(snafuToDec "112").IsEqualTo(32)
Check.That(snafuToDec "1=-1=").IsEqualTo(353)
Check.That(snafuToDec "1-12").IsEqualTo(107)
Check.That(snafuToDec "12").IsEqualTo(7)
Check.That(snafuToDec "1=").IsEqualTo(3)
Check.That(snafuToDec "122").IsEqualTo(37)
Check.That(snafuToDec "1-0---0").IsEqualTo(12345)

let decToSnafu (i:int64) =

    let powersOf5 = new ResizeArray<int>(Array.create 64 0)

    let rec decToSnafuRec (i:int64) index =
        let n =  i + int64(powersOf5[index])
        match n % 5L with 
        | 2L -> powersOf5.[index] <- 2
        | 1L -> powersOf5.[index] <- 1
        | 0L -> powersOf5.[index] <- 0
        | 3L -> 
            powersOf5.[index + 1] <- 1
            powersOf5.[index] <- -2
        | 4L -> 
            powersOf5.[index + 1] <- 1
            powersOf5.[index] <- -1
        | _ -> failwithf "not a modulo 5"
        
        let next = n / 5L
        if next = 0 then
            powersOf5
        else 
            decToSnafuRec next (index + 1)

    let p = decToSnafuRec i 0

    let chars = 
        p 
        |> Seq.rev
        |> Seq.skipWhile (fun i -> i = 0)
        |> Seq.map(fun i -> 
            match i with
            | 2 -> '2'
            | 1 -> '1'
            | 0 -> '0'
            | -1 -> '-'
            | -2 -> '='
            | _ -> failwithf "Invalid snafu digit value %i" i
        )
        |> Seq.toArray
    new String(chars)
    // sprintf "%i" (i % 5)

Check.That(decToSnafu 1).IsEqualTo("1")
Check.That(decToSnafu 2).IsEqualTo("2")
Check.That(decToSnafu 3).IsEqualTo("1=")
Check.That(decToSnafu 4).IsEqualTo("1-")
Check.That(decToSnafu 5).IsEqualTo("10")
Check.That(decToSnafu 6).IsEqualTo("11")
Check.That(decToSnafu 7).IsEqualTo("12")
Check.That(decToSnafu 8).IsEqualTo("2=")
Check.That(decToSnafu 9).IsEqualTo("2-")
Check.That(decToSnafu 10).IsEqualTo("20")
Check.That(decToSnafu 15).IsEqualTo("1=0")
Check.That(decToSnafu 20).IsEqualTo("1-0")
Check.That(decToSnafu 2022).IsEqualTo("1=11-2")
Check.That(decToSnafu 12345).IsEqualTo("1-0---0")
Check.That(decToSnafu 314159265).IsEqualTo("1121-1110-1=0")

getInput "Day25_sample1.txt"
|> Seq.map snafuToDec
|> Seq.sum
|> int64
|> decToSnafu

getInput "Day25.txt"
|> Seq.map snafuToDec
|> Seq.sum
|> int64
|> decToSnafu