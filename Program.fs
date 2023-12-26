
open System
open System.IO
open AdventOfCode

let intArrayToFloatArray ints = ints |> Array.map float

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (fun line -> 
        let pos, vec = line |> ssplit2 " @ "
        pos |> splitIntList |> intArrayToFloatArray |> tupleize3, vec |> splitIntList |> intArrayToFloatArray |> tupleize3
    )

open Microsoft.Z3.Bool
open Microsoft.Z3.Real
open Microsoft.Z3

let doesCross (rangeMin:float) (rangeMax:float) ((ox1:float,oy1:float,oz1:float),(vx1:float, vy1:float,vz1:float)) ((ox2:float,oy2:float,oz2:float),(vx2:float, vy2:float,vz2:float))=
    let x1 = Real("x")
    let y1 = Real("y")
    let x2 = Real("x")
    let y2 = Real("y")
    let t = Real("t")
    // let t2 = Real("t")

    let result = 
        Z3.Solve(
            // x1 =. ox1 + t*vx1,
            // y1 =. oy1 + t*vy1,
            // x2 =. ox2 + t*vx2,
            // y2 =. oy2 + t*vy2,
            // //t >. 0.,
            // x1 =.x2,
            // y1=.y2
            //
            y1 =. oy1 + ((x1 - ox1)/vx1)*x1,
            y2 =. oy2 + ((x2 - ox2)/vx2)*x1
            // x1 >=. rangeMin,
            // x1 <=. rangeMax,
            // y1 >=. rangeMin,
            // y1 <=. rangeMax,
            // x2 >=. rangeMin,
            // x2 <=. rangeMax,
            // y2 >=. rangeMin,
            // y2 <=. rangeMax,
            
            // x2 =. ox2 + t2 * vx2,
            // y2 =. oy2 + t2 * vy2,
            // x1 =. x2,
            // y1 =. y2,
            // t1 >. 0.,
            // t2 >. 0.
            //.,new Bool(Gs.context().MkIsInteger(t.Expr :?> RealExpr))
            )
    match result with
    | NoSolution -> false
    | Unknown -> false
    | Solution s -> 
        let solution = 
            s |> List.map (fun (symbol, func, res) -> 
                match res with 
                | Const x -> symbol.ToString(), (x :?> RatNum).Double
                | _ -> failwithf "Not supported"
            )
            |> Map.ofList
        //
        //printfn "%f is inside [%f,%f]" (solution["x"] + solution["t"] * ovx1) rangeMin rangeMax
        // printfn "%f is inside [%f,%f]" (solution["y"] + solution["t"] * ovy) rangeMin rangeMax
        //
        printfn "%O" solution
        true

doesCross 7 27 ((19,13,30),(-2,1,-2)) ((18,19,22),(-1,-1,-2))
// doesCross 7 27 ((19,13,30),(-2,1,-2)) ((20,25,34),(-2,-2,-4))
// doesCross 7 27 ((19,13,30),(-2,1,-2)) ((12,31,28),(-1,-2,-1))
// doesCross 7 27 ((19,13,30),(-2,1,-2)) ((20,19,15),(1,-5,-3))

// let solve1 rangeMin rangeMax input  =
//     getInput input 
//     |> Array.filter (fun (pos, vec) -> (doesCross rangeMin rangeMax (pos,vec)))
//     |> Seq.length
//
//
//
//
// // Check.That(solve1 7 27 "Day24_sample1.txt").IsEqualTo(2)
//
// // solve1 "Day24_sample1.txt"
