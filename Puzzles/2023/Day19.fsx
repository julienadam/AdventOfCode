#time "on"
#load "../../Tools.fs"
#load "../../Tools/Ranges.fs"
#r "nuget: NFluent"

open System.IO
open AdventOfCode
open NFluent

type Result = | Goto of string | Accepted | Rejected

module Part1 =

    type Part = { x: int; m: int; a: int; s: int }

    let solve1 input =
        let parseCondition (c:string) = 
            if c.Contains(":") then
                let spl = c.Split([|'>'; '<'; ':'|])
                let right = spl[1] |> int
                let res = match spl[2] with | "A" -> Accepted | "R" -> Rejected | x -> Goto x
                let op = if c.Contains(">") then (>) else (<)
                match spl[0] with
                | "x" -> (fun (p:Part) -> if op p.x right then Some res else None)
                | "m" -> (fun (p:Part) -> if op p.m right then Some res else None)
                | "a" -> (fun (p:Part) -> if op p.a right then Some res else None)
                | "s" -> (fun (p:Part) -> if op p.s right then Some res else None)
                | x   -> failwithf "Invalid part %s" x
            else
                match c with
                | "A" -> (fun _ -> Some Accepted)
                | "R" -> (fun _ -> Some Rejected)
                | x   -> (fun _ -> Some (Goto x))

        let parseLine (line:string) =
            let i = line.IndexOf('{')
            let name = line.Substring(0,i)
            let workflows = line.Substring(i + 1, line.Length - i - 2) |> ssplit ","
            name, workflows |> Array.map parseCondition

        let parsePart (p:string) = 
            let values = p.Substring(1, p.Length - 2) |> ssplit "," |> Array.map (fun decl -> decl.Substring(2) |> int)
            { x = values[0]; m = values[1]; a = values[2]; s = values[3]}

        let getInput name =
            let parseWorkflows w = w |> ssplit "\n" |> Array.map parseLine
            let parseParts p = p |> ssplit "\n" |> Array.map parsePart
            let workflows, ratings = File.ReadAllText(getInputPath2023 name) |> ssplit2 "\n\n"

            workflows |> parseWorkflows |> Map.ofArray , parseParts ratings


        let workflows, parts = getInput input 
        let inw = workflows["in"]

        let rec runSystem conditions (part:Part) =
            match conditions |> Array.pick (fun condition -> condition part) with
            | Accepted -> Accepted
            | Rejected -> Rejected
            | Goto x   -> runSystem workflows[x] part
        
        parts 
        |> Seq.map (fun p -> p, runSystem inw p)
        |> Seq.filter (fun (_, r) -> r = Accepted)
        |> Seq.sumBy (fun (p,_) -> p.x + p.m + p.a + p.s)

Check.That(Part1.solve1 "Day19_sample1.txt").IsEqualTo(19114)
Part1.solve1 "Day19.txt"

module Part2 =

    type Op = | GreaterThan | LessThan

    type Condition = {
        index : int
        limit : int
        op : Op
    }

    type Workflow = {
        condition : Condition option
        result : Result
    }

    let countCombinations (ranges:(int*int)array) = 
        let inline rangeLength (a,b) = (int64 b)-(int64 a)+1L
        ranges 
        |> Seq.map rangeLength
        |> Seq.reduce (*)

    let parseCondition (c:string) = 
            let strToRes = function | "A" -> Accepted | "R" -> Rejected | x -> Goto x
            if c.Contains(":") then
                let spl = c.Split([|'>'; '<'; ':'|])
                let res = strToRes spl[2]
                { 
                    condition = { 
                        index = match spl[0] with | "x" -> 0 | "m" -> 1 | "a" -> 2 | "s" -> 3 | _ -> failwithf "invalid" 
                        limit = spl[1] |> int
                        op = if c.Contains(">") then GreaterThan else LessThan
                    } |> Some
                    result = res
                }
            else
                { condition = None; result = strToRes c}

    let parseLine (line:string) =
        let i = line.IndexOf('{')
        let name = line.Substring(0,i)
        let workflows = line.Substring(i + 1, line.Length - i - 2) |> ssplit ","
        name, workflows |> Seq.map parseCondition |> Seq.toList

    let parseWorkflows w = w |> ssplit "\n" |> Array.map parseLine

    let findCombinations workflowsText = 
        let workflows = workflowsText |> parseWorkflows |> Map.ofArray
        let init = workflows["in"]
        let startRanges = [|(1,4000); (1,4000); (1,4000); (1,4000) |]

        let rec solveRec (ranges:(int*int) array) steps : int64 =
            match steps with
            | [] -> 0L
            | head::tail ->
                match head.condition with
                | None ->
                    match head.result with
                    | Accepted -> countCombinations ranges
                    | Rejected -> 0L
                    | Goto x -> solveRec ranges (workflows[x])
                | Some condition ->
                    match condition.op with
                    | GreaterThan ->
                        let low,high = ranges[condition.index]
                        let matchRange = ranges |> Array.copy 
                        matchRange[condition.index] <- (condition.limit + 1, high)

                        let nonMatchRange = ranges |> Array.copy 
                        nonMatchRange[condition.index] <- (low, condition.limit)

                        solveRec matchRange [ { condition = None; result = head.result } ] + solveRec nonMatchRange tail
                    | LessThan ->
                        let low,high = ranges[condition.index]
                        let matchRange = ranges |> Array.copy 
                        matchRange[condition.index] <- (low, condition.limit-1)

                        let nonMatchRange = ranges |> Array.copy 
                        nonMatchRange[condition.index] <- (condition.limit, high)

                        solveRec matchRange [ { condition = None; result = head.result } ] + solveRec nonMatchRange tail
        solveRec startRanges init

    Check.That(findCombinations "in{A}").IsEqualTo(4000L*4000L*4000L*4000L)
    Check.That(findCombinations "in{R}").IsEqualTo(0L)
    Check.That(findCombinations "in{qs}\nqs{A}").IsEqualTo(4000L*4000L*4000L*4000L)
    Check.That(findCombinations "in{qs}\nqs{R}").IsEqualTo(0L)
    Check.That(findCombinations "in{x>2000:A,R}").IsEqualTo(2000L*4000L*4000L*4000L)
    Check.That(findCombinations "in{x<2001:A,R}").IsEqualTo(2000L*4000L*4000L*4000L)
    Check.That(findCombinations "in{qs}\nqs{x<2001:A}").IsEqualTo(2000L*4000L*4000L*4000L)
    Check.That(findCombinations "in{x>1000:A,m>3000:A,R}").IsEqualTo(
        countCombinations [|(1001,4000);(1,4000);(1,4000);(1,4000)|] +
        countCombinations [|(1,1000);(3001,4000);(1,4000);(1,4000)|]
    )
    Check.That(findCombinations "in{x<1001:A,m<3001:A,R}").IsEqualTo(
        countCombinations [|(1,1000);(1,4000);(1,4000);(1,4000)|] +
        countCombinations [|(1001,4000);(1,3000);(1,4000);(1,4000)|]
    )

    let solve2 input =
        File.ReadAllText(getInputPath2023 input) 
        |> ssplit2 "\n\n" 
        |> fst
        |> findCombinations



Check.That(Part2.solve2 "Day19_sample1.txt").IsEqualTo(167409079868000L)
Part2.solve2 "Day19.txt"