#time
#load "../../Tools.fsx"
#r "nuget: MathNet.Symbolics"

open System.IO
open System.Text.RegularExpressions
open MathNet.Symbolics
open Operators
open Checked
open Tools

let regex = new Regex (@"(?<monkey>\w+)\: (((?<m1>\w+) (?<op>[\+\-\*\/]{1}) (?<m2>\w+))|(?<num>\d+))")

module Part1 =

    type Op = int64 -> int64 -> int64

    type MonkeyYell =
        | Number of int64
        | MathOp of string * Op * string

    type Monkey = {
        name: string
        yell: MonkeyYell
    }

    let parseOp o =
        match o with 
        | "+" -> (+)
        | "-" -> (-)
        | "*" -> (*)
        | "/" -> (/)
        | _ -> failwithf "Invalid operator %s" o

    let parseMonkey line =
        let m = regex.Match line
        if not m.Success then
            failwithf "Could not parse line %s" line
        let yell = 
            if m.Groups.["m1"].Success = false then
               m|> mInt64 "num" |> Number
            else
                (m |> mStr "m1", m|>mStr "op" |> parseOp, m |> mStr "m2") |> MathOp
        { 
            name = m |> mStr "monkey"
            yell = yell
        }

    let getInput p =
        File.ReadAllLines(getInputPath2022 p)
        |> Seq.map parseMonkey
        |> Seq.map (fun m -> m.name, m)
        |> Map.ofSeq

    let rec compute (monkeys:Map<string,Monkey>) current =
        match current.yell with
        | Number n -> n
        | MathOp (l, op, r) ->
            let leftNumber = compute monkeys monkeys.[l]
            let rightNumber = compute monkeys monkeys.[r]
            op leftNumber rightNumber
        | _ -> failwithf "Not for this part"
    
    let solve1 (monkeys:Map<string,Monkey>) =
    
        let root = monkeys["root"]
        compute monkeys root

module Part2 =
    
    type MonkeyYell =
        | Number of int64
        | MathOp of string * string * string
        | Root of string * string
        | Human

    type Monkey = {
        name: string
        yell: MonkeyYell
    }
    
    let parseMonkey line =
        let m = regex.Match line
        if not m.Success then
            failwithf "Could not parse line %s" line
        let yell = 
            if m.Groups.["m1"].Success = false then
               m|> mInt64 "num" |> Number
            else
                (m |> mStr "m1", m|>mStr "op", m |> mStr "m2") |> MathOp
        { 
            name = m |> mStr "monkey"
            yell = yell
        }

    let getInput p =
        File.ReadAllLines(getInputPath2022 p)
        |> Seq.map parseMonkey
        |> Seq.map (fun m -> m.name, m)
        |> Map.ofSeq

    let rec express (monkeys:Map<string,Monkey>) current =
        match current.yell with
        | Number n -> sprintf "%i" n
        | MathOp (l, op, r) ->
            sprintf "(%s%s%s)" (express monkeys monkeys.[l]) op (express monkeys monkeys.[r])
        | Human -> "x"
        | Root (l,r) -> sprintf "%s=%s" (express monkeys monkeys.[l]) (express monkeys monkeys.[r])
        
    let solveLinear variable expr =
            // To polynomial
            let simple = Algebraic.expand(Rational.numerator(Rational.simplify variable expr))
            // calculate coefficients
            let coeff = Polynomial.coefficients variable simple
            match coeff with
            | [|num;den|] -> Rational.simplify variable (Algebraic.expand -num / den)
            | _ -> failwith "Cannot solve"


    let solve2 (monkeys:Map<string,Monkey>) =
        let root = monkeys["root"]
        let humn = monkeys["humn"]
        let moddedMonkeys = 
            monkeys 
            |> Map.add 
                "root" 
                (match root.yell with 
                    | MathOp (l,_,r) -> { root with yell = Root (l,r) } 
                    | _ -> failwithf "Root is not a math op ?")
            |> Map.add "humn" { humn with yell = Human }
    
        let left, right = express moddedMonkeys moddedMonkeys.["root"] |> Dump |> ssplit "=" |> tupleize2

        let x = symbol "x"
        let aLeft = Infix.parseOrThrow left
        let aRight = Infix.parseOrThrow right
        solveLinear x (aLeft - aRight)

Part1.getInput "Day21.txt"
|> Part1.solve1

Part2.getInput "Day21.txt"
|> Part2.solve2

