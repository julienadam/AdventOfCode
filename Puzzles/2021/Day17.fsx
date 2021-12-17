open System.Diagnostics
open System

#load "../../Tools.fsx"

open Tools
open System.IO

type Range = {
    Start: int
    End: int
}
with
    member this.IsInRange x = x >= this.Start && x <= this.End
    

let calcXi startX startXvel iteration =
    startX + ([(max 0 (startXvel - iteration + 2)) ..startXvel] |> Seq.sum)
    
let calcYi startY startYvel iteration =
    startY + (iteration - 1) * startYvel - ([1..iteration - 2] |> Seq.sum)

let hitsTarget xVel yVel (xRange:Range) (yRange:Range) =
    //printfn "Verifying xVel %i yVel %i" xVel yVel
    let mutable m = yRange.Start
    Seq.initInfinite id |> Seq.pick (fun iteration ->
        let x,y = calcXi 0 xVel iteration, calcYi 0 yVel iteration
        m <- max y m
        if xRange.IsInRange x && yRange.IsInRange y then
            //printfn "Hit at iteration %i" iteration
            // Hit
            Some (Some m)
        else if x > xRange.End || y < yRange.Start then
            //printfn "Overshoot at iteration %i" iteration
            // Overshoot
            Some (None)
        else
            None
    )
     
let enumVelocities (xRange:Range) (yRange:Range) = seq {
    for x = 1 to xRange.End do
        // Arbitrary Y max. Maths should tell us what to expect
        // I just brute force the whole thing
        // Basically when xVel is 0 there should be an easy math way to determine
        // if there is an iteration where a hit occurs
        // Because the function becomes simpler
        for y = yRange.Start to 100000 do
            yield x, y
}

module Part1 =

    let Solve xRange yRange = 
        // increment iterations
        enumVelocities xRange yRange
        |> Seq.choose (fun (xVel,yVel) -> 
            hitsTarget xVel yVel xRange yRange 
            |> Option.map (fun m -> 
                printfn "(%i,%i) hits" xVel yVel
                xVel,yVel, m))
        |> Seq.maxBy (fun (xVel,yVel, m) -> m)
        |> Dump
        

module Part2 =
    
    let Solve xRange yRange = 
        // increment iterations
        enumVelocities xRange yRange
        |> Seq.choose (fun (xVel,yVel) -> 
            hitsTarget xVel yVel xRange yRange 
            |> Option.map (fun m -> 
                printfn "(%i,%i) hits" xVel yVel
                xVel,yVel, m))
        |> Seq.length

assert(hitsTarget 7 2 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 6 3 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 9 0 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 17 -4 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isNone)

//Part1.calcYi 0 -10 1
//Part1.calcYi 0 -10 2
//Part1.calcYi 0 -10 3

//Part1.Solve { Start = 20 ; End = 30 } { Start = -10; End = -5 }
//Part1.Solve { Start = 281 ; End = 311 } { Start = -74; End = -54 }

//Part2.Solve { Start = 20 ; End = 30 } { Start = -10; End = -5 }
Part2.Solve { Start = 281 ; End = 311 } { Start = -74; End = -54 }
