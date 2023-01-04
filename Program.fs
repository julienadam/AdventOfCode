open System
open Checked

let decToSnafu i =

    let powersOf5 = new ResizeArray<int>(Array.create 64 0)

    let rec decToSnafuRec i index =
        let n = powersOf5[index] + i
        match n % 5 with 
        | 2 -> powersOf5.[index] <- 2
        | 1 -> powersOf5.[index] <- 1
        | 0 -> powersOf5.[index] <- 0
        | 3 -> 
            powersOf5.[index + 1] <- 1
            powersOf5.[index] <- -2
        | 4 -> 
            powersOf5.[index + 1] <- 1
            powersOf5.[index] <- -1
        | _ -> failwithf "not a modulo 5"
        
        let next = n / 5
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

let result = decToSnafu 12345
printfn "%s" result
if not (result = "1-0---0") then
    failwithf "Incorrect result"