<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

let getInputPath file = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        @"CloudStation\Data\AdventOfCode\", 
        file)
        
let input = File.ReadAllLines(getInputPath "day18.txt")

module WeirdCalculator =
    let getCalculator addPrec multPrec =
        // Setup FParsec for simple int64 ops with custom precedence
        let ws = spaces 
        let str_ws s = pstring s >>. ws
        let number = pint64 .>> ws
        let opp = new OperatorPrecedenceParser<int64,unit,unit>()
        let expr = opp.ExpressionParser
        opp.TermParser <- number <|> between (str_ws "(") (str_ws ")") expr
        opp.AddOperator(InfixOperator("+", ws, addPrec, Associativity.Left, (+)))
        opp.AddOperator(InfixOperator("*", ws, multPrec, Associativity.Left, (*)))
        let completeExpression = ws >>. expr .>> eof 
        let calculate s = run completeExpression s
        let calculateValue s = 
            match calculate s with 
            | Success (v, _,_) -> v 
            | _ -> failwithf "Calculation failed"
        
        calculateValue


module Puzzle1 =
    
    let samples = 
        [
            "1 + (2 * 3) + (4 * (5 + 6))", 51L
            "2 * 3 + (4 * 5)", 26L
            "5 + (8 * 3 + 9 + 3 * 4 * 3)", 437L
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240L
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632L
        ]
    
    let calculator = WeirdCalculator.getCalculator 1 1

    let runTests() =
        samples 
        |> Seq.iter (fun (expr, expected) -> 
            let res = calculator expr
            if res = expected then 
                printfn "%s yielded %i, as expected" expr expected
            else
                printfn "%s yielded %i, expected %i" expr res expected
            ) 
    
    let solution() = 
        input
        |> Seq.map calculator
        |> Seq.sum
        |> Dump

printfn "Puzzle1"
Puzzle1.runTests()
Puzzle1.solution()

module Puzzle2 =
    let samples = 
        [
            "1 + (2 * 3) + (4 * (5 + 6))", 51L
            "2 * 3 + (4 * 5)", 46L
            "5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445L
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060L
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340L
        ]
    
    let calculator = WeirdCalculator.getCalculator 2 1

    let runTests() =
        samples 
        |> Seq.iter (fun (expr, expected) -> 
            let res = calculator expr
            if res = expected then 
                printfn "%s yielded %i, as expected" expr expected
            else
                printfn "%s yielded %i, expected %i" expr res expected
            ) 
    
    let solution() = 
        input
        |> Seq.map calculator
        |> Seq.sum
        |> Dump

printfn "Puzzle2"
Puzzle2.runTests()
Puzzle2.solution()
