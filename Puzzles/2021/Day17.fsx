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
    let mutable m = yRange.Start
    Seq.initInfinite id |> Seq.pick (fun iteration ->
        let x,y = calcXi 0 xVel iteration, calcYi 0 yVel iteration
        m <- max y m
        if xRange.IsInRange x && yRange.IsInRange y then
            Some (Some m)
        else if x > xRange.End || y < yRange.Start then
            Some (None)
        else
            None
    )

let hitsTargetNoMax xVel yVel (xRange:Range) (yRange:Range) =
    Seq.initInfinite id |> Seq.pick (fun iteration ->
        let x,y = calcXi 0 xVel iteration, calcYi 0 yVel iteration
        if xRange.IsInRange x && yRange.IsInRange y then
            Some true
        else if x > xRange.End || y < yRange.Start then
            Some false
        else
            None
    )
     
let inverseTriangularNumber x =
    ((x * 2.0) ** 0.5) |> int

let enumVelocities (xRange:Range) (yRange:Range) = seq {
    for x = (inverseTriangularNumber xRange.Start) to xRange.End do
        for y = yRange.Start to Math.Abs(yRange.Start) do
            yield x, y
}

module Part1 =

    let Solve xRange yRange = 
        let sw = Stopwatch.StartNew()
        let xVelMax, yVelMax, maxY = 
            enumVelocities xRange yRange
            |> Seq.choose (fun (xVel,yVel) -> 
                hitsTarget xVel yVel xRange yRange 
                |> Option.map (fun m -> xVel,yVel, m))
            |> Seq.maxBy (fun (_, _, m) -> m)
        
        printfn "Best shot for %i %i, reached %i. Tooks %O" xVelMax yVelMax maxY sw.Elapsed
        
module Part2 =
    
    let Solve xRange yRange = 
        let sw = Stopwatch.StartNew()
        let numSolutions = 
            enumVelocities xRange yRange
            |> Seq.filter (fun (xVel,yVel) -> hitsTargetNoMax xVel yVel xRange yRange)
            |> Seq.length
        printfn "%i solutions. Tooks %O" numSolutions sw.Elapsed

assert(hitsTarget 7 2 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 6 3 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 9 0 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isSome)
assert(hitsTarget 17 -4 { Start = 20 ; End = 30 } { Start = -10; End = -5 } |> Option.isNone)

Part1.Solve { Start = 20 ; End = 30 } { Start = -10; End = -5 }
Part1.Solve { Start = 281 ; End = 311 } { Start = -74; End = -54 }

Part2.Solve { Start = 20 ; End = 30 } { Start = -10; End = -5 }
Part2.Solve { Start = 281 ; End = 311 } { Start = -74; End = -54 }
