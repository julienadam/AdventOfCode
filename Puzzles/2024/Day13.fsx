#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

type Machine = {
    ax : int64; ay : int64
    bx : int64; by :int64
    px : int64; py: int64
}

let getInput name = 
    File.ReadAllText(getInputPath2024 name).Replace("\r\n", "\n")
    |> ssplit "\n\n"
    |> Seq.map (fun paragraph -> 
        let split = paragraph |> splitLines |> Seq.toArray
        let btnALine = split[0].Split(',')
        let btnBLine = split[1].Split(',')
        let prizeLine = split[2].Split(',')
        { 
            ax = (btnALine[0].Substring(12) |> int64)
            ay = (btnALine[1].Substring(3) |> int64)
            bx = (btnBLine[0].Substring(12) |> int64)
            by = (btnBLine[1].Substring(3) |> int64)
            px = (prizeLine[0].Substring(9) |> int64)
            py = (prizeLine[1].Substring(3) |> int64)
        }
    )

let solve1 input =
    getInput input 
    |> Seq.choose (fun m ->
        // Solve the equations
        // a*ax + b*bx = px
        // a*ay + b*by = py
        let b = (m.py*m.ax - (m.px * m.ay)) / (-m.bx*m.ay + m.ax*m.by)
        let a = (m.px - m.bx * b) / m.ax
        if a > 100 || b > 100 then
            None
        else if (a * m.ax + b * m.bx) <> m.px then 
            None // Can't reach X with those inputs
        else if (a * m.ay + b * m.by) <> m.py then 
            None // Can't reach Y with those inputs
        else
            Some (a,b)
    )
    |> Seq.sumBy (fun (a,b) -> a*3L + b)

solve1 "Day13_sample1.txt"

solve1 "Day13.txt"

let solve2 input =
    getInput input 
    |> Seq.map (fun m -> { m with px = m.px + 10000000000000L; py = m.py + 10000000000000L })
    |> Seq.choose (fun m ->
        // Solve the equations
        // a*ax + b*bx = px
        // a*ay + b*by = py
        let b = (m.py*m.ax - (m.px * m.ay)) / (-m.bx*m.ay + m.ax*m.by)
        let a = (m.px - m.bx * b) / m.ax
        if (a * m.ax + b * m.bx) <> m.px then 
            None // Can't reach X with those inputs
        else if (a * m.ay + b * m.by) <> m.py then 
            None // Can't reach Y with those inputs
        else
            Some (a,b)
    )
    |> Seq.sumBy (fun (a,b) -> a*3L + b)

solve2 "Day13.txt"
