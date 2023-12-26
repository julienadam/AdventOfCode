open System
open System.IO
open AdventOfCode

let parseFloatList l = l |> ssplit ", " |> Array.map(fun f -> Double.Parse(f))

let inline parseLine line= 
    let pos, vec = line |> ssplit2 " @ "
    pos |> parseFloatList |> tupleize3, vec |> parseFloatList |> tupleize3

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine

// Intersection of 2 lines
// https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
// Converting the time-based equations into y=f(x) form

// Extract the coefficients of the ax+c equations
let inline getPolynomials (ox,oy) (vx,vy) = 
    // x = ox+vx*t -> t = (x-ox)/vx
    // y = oy+vy*t -> y = oy+vy*((x-ox)/vx) -> y = vy*x/vx+(oy-vy*ox/vx)
    (vy/vx),(oy-vy*ox/vx)

// Apply the line crossing formula
let findCrossingPoint (p1,v1) (p2,v2) =
    let (a,c) = getPolynomials p1 v1
    let (b,d) = getPolynomials p2 v2
    if a = b then
        None
    else
        let x = (d-c)/(a-b)
        let y = a*x+c
        Some (x,y)

// Remove Z coordinate
let truncZ ((x,y,_),(vx,vy,_))  = ((x,y), (vx,vy))
//
// // Unit tests
// let verifySome s1 s2 ex ey =
//     let x,y = (findCrossingPoint (parseLine(s1) |> truncZ) (parseLine(s2) |> truncZ)) |> Option.get
//     Check.That(x).IsCloseTo(ex, 0.0000001) |> ignore
//     Check.That(y).IsCloseTo(ey, 0.0000001) |> ignore
//
// let verifyNone s1 s2 = Check.That((findCrossingPoint (parseLine(s1) |> truncZ) (parseLine(s2) |> truncZ))).IsEqualTo(None)
//
// verifySome "19, 13, 30 @ -2, 1, -2" "18, 19, 22 @ -1, -1, -2" 14.33333333 15.33333333
// verifySome "19, 13, 30 @ -2, 1, -2" "20, 25, 34 @ -2, -2, -4" 11.66666667 16.66666667
// verifySome "19, 13, 30 @ -2, 1, -2" "12, 31, 28 @ -1, -2, -1" 6.2 19.4
// verifySome "19, 13, 30 @ -2, 1, -2" "20, 19, 15 @ 1, -5, -3"  21.44444444 11.77777777
// verifyNone "18, 19, 22 @ -1, -1, -2" "20, 25, 34 @ -2, -2, -4"

// Check the bounds
let inline isCrossingPointInsideBounds x y rangeMin rangeMax = 
    x >= rangeMin && x <= rangeMax && y >= rangeMin && y <= rangeMax

// Verify that t is in the future for the given solution
let inline isInTheFuture (ox,oy) (vx,vy) x y = // x = ox + vx*t -> t = (x - ox) / vx
    (x - ox)/vx >= 0. && (y - oy)/vy >= 0.

let solve1 input rangeMin rangeMax =
    let hailstones = getInput input |> Array.map truncZ
    SeqEx.autoProduct hailstones
    |> Seq.filter (fun (a,b) -> a <> b)
    |> Seq.filter (fun ((p1,v1),(p2,v2)) ->
        match findCrossingPoint (p1,v1) (p2,v2) with
        | None -> 
            // printfn "Failure : No crossing point for \n%O\n%O" (p1,v1) (p2,v2)
            false
        | Some (x,y) ->
            if isCrossingPointInsideBounds x y rangeMin rangeMax then
                if (isInTheFuture p1 v1 x y) && (isInTheFuture p2 v2 x y) then
                    // printfn "Success : Crossing point (%f,%f) OK for \n%O\n%O" x y (p1,v1) (p2,v2) 
                    true
                else
                    // printfn "Failure : Crossing point (%f,%f) in the past for \n%O\n%O" x y (p1,v1) (p2,v2) 
                    false
            else
                // printfn "Failure : Crossing point (%f,%f) outside bounds for \n%O\n%O" x y (p1,v1) (p2,v2) 
                false
            
    )
    |> Seq.length

// Check.That(solve1 "Day24_sample1.txt" 7 27).IsEqualTo(2)

// solve1 "Day24.txt" 200000000000000. 400000000000000.

open Microsoft.Z3.Bool
open Microsoft.Z3.Real
open Microsoft.Z3

// (x y z) (vx vy vz) must match
// for every hailstone (hx hy hz) (hvx hvy hvz)
// hx+hvx*t = x+vx*t
// hy+hvy*t = y+vy*t
// hz+hvz*t = z+vz*t
// t > 0

let perfectHailstone (hailstones:((float*float*float)*(float*float*float)) array) =
    let x = Real("x")
    let y = Real("y")
    let z = Real("z")
    let vx = Real("vx")
    let vy = Real("vy")
    let vz = Real("vz")
    let t = Real("t")
    let u = Real("u")
    let v = Real("v")

    let getTruths hailstone time =
        let (hx,hy,hz),(hvx,hvy,hvz) = hailstone
        [|
            hx + hvx * time =. x + vx*time
            hy + hvy * time =. y + vy*time
            hz + hvz * time =. z + vz*time
            time >. 0.
        |]

    let truths0 = getTruths hailstones[0] t
    let truths1 = getTruths hailstones[1] u
    let truths2 = getTruths hailstones[2] v

    match Z3.Solve(Array.concat [|truths0;truths1;truths2|]) with
    | NoSolution 
    | Unknown -> failwithf "no solution found"
    | Solution s -> 
        let solution = 
            s |> List.map (fun (symbol, func, res) -> 
                match res with 
                | Const x -> symbol.ToString(), (x :?> RatNum).Double |> int64
                | _ -> failwithf "Not supported"
            )
            |> Map.ofList
        solution["x"],solution["y"],solution["z"]
        
let solve2 input =
    let hailstones = getInput input
    let x,y,z = perfectHailstone hailstones
    x + y + z

// Check.That(solve2 "Day24_sample1.txt").IsEqualTo(47)
solve2 "Day24.txt"
