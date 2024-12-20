﻿namespace AdventOfCode

open System.Runtime.CompilerServices

[<AutoOpen>]
module Digits =
    open System
    open System.Numerics

    /// Counts the digits in an int32 (log10 based)
    let inline numDigits n = if n = 0 then 1 else (Math.Floor(Math.Log10(n) + 1.0)) |> int
    
    /// Counts the digits in an int32 (log10 based)
    let inline numDigits64 (n:int64) = if n = 0 then 1 else (Math.Floor(Math.Log10(n |> float) + 1.0)) |> int

    // Counts the digits using good old fashioned divisions
    let inline countDigits64 (n:int64) = 
        if n = 0L then 1 else 
            let mutable d = 0L
            let mutable n = n
            while n <> 0 do
                d <- d + 1L
                n <- n / 10L
            d |> int

    /// Enumerates the digits of an integer, in reverse order
    let digitsRev (a:int64) : int64 seq = seq {
        if a = 0 then
            yield 0
        else
            let mutable a = a
            while a > 0 do 
                yield a % 10L
                a <- a / 10L
            }

    /// Enumerates the digits of an integer, in order
    let digits = digitsRev >> Seq.rev

    /// Concatenates the digits of 2 integers
    let (||||) (a:int64) (b:int64) : int64 =
        if b = 0 then a * 10L
        else
            let mutable pow = 1L
            while pow <= b do pow <- pow * 10L
            a * pow + b