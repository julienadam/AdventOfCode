
#time
#load "../../Tools.fs"
#r "nuget: faqt"

open System
open System.IO
open AdventOfCode
open Faqt

type Card =
|_A = 14 |_K = 13 |_Q = 12 |_J = 11 
|_T = 10 |_9 = 9  |_8 = 8  |_7 = 7  |_6 = 6 
|_5 = 5  |_4 = 4  |_3 = 3  |_2 = 2

type HandType = 
    | FiveOf    = 10 | FourOf    = 9 | FullHouse = 8 | ThreeOf   = 7  
    | TwoPair   = 6  | Pair      = 5 | Nothing   = 4

let readCardsInHand (str:string) =
    str 
    |> Seq.map (fun c -> Enum.Parse<Card>("_" + c.ToString())) 
    |> Array.ofSeq

[<CustomComparison>]
[<StructuralEquality>]
type Hand = {
    Type : HandType
    Cards : Card array
} with
    interface IComparable<Hand> with
        member this.CompareTo(other) =
            match (this.Type |> int).CompareTo(other.Type |> int) with
            | 0 ->
                let firstDiff = 
                    Seq.zip this.Cards other.Cards
                    |> Seq.tryFind (fun (a, b) -> a <> b)
                match firstDiff with
                | None -> 0
                | Some (a,b) -> (a |> int).CompareTo(b |> int)
            | v -> v
    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Hand as h -> (this :> IComparable<Hand>).CompareTo(h)
            | _ -> failwith("Invalid comparison")
    static member Classify(cards : Card array) =
        if cards.Length <> 5 then failwithf "Should have 5 cards"
        let grouped = 
            cards 
            |> Array.groupBy id 
            |> Array.sortByDescending (fun (g, cards) -> cards.Length)
        match grouped.Length with
        | 1 -> { Type = HandType.FiveOf; Cards = cards}
        | 2 -> 
            match grouped[0] with
            | _, (c: Card array) when c.Length = 4 ->
                { Type = HandType.FourOf; Cards = cards}
            | _, c when c.Length = 3 ->
                { Type = HandType.FullHouse; Cards = cards}
            | _ -> failwithf "2 groups can only be FourOf or FullHouse"
        | 3 -> 
            match grouped[0] with
            | (_,cards1) when cards1.Length = 3 ->
                { Type = HandType.ThreeOf; Cards = cards}
            | (_,cards1) when cards1.Length = 2 ->
                { Type = HandType.TwoPair; Cards = cards}
            | _ -> failwithf "3 groups can only be ThreeOf or TwoPair"
        | 4 -> { Type = HandType.Pair; Cards = cards}
        | 5 -> { Type = HandType.Nothing; Cards = cards}
        | _ -> failwithf "Cannot have more than 5 groups with 5 cards..."

(fun () -> readCardsInHand "AAAA" |> Hand.Classify).Should().Throw<Exception, _>()
(fun () -> readCardsInHand "AAAAAA" |> Hand.Classify).Should().Throw<Exception, _>()
(readCardsInHand "AAAAA" |> Hand.Classify).Type.Should().Be(HandType.FiveOf)
(readCardsInHand "AAAA2" |> Hand.Classify).Type.Should().Be(HandType.FourOf)
(readCardsInHand "AA22A" |> Hand.Classify).Type.Should().Be(HandType.FullHouse)
(readCardsInHand "AA32A" |> Hand.Classify).Type.Should().Be(HandType.ThreeOf)
(readCardsInHand "A332A" |> Hand.Classify).Type.Should().Be(HandType.TwoPair)
(readCardsInHand "A342A" |> Hand.Classify).Type.Should().Be(HandType.Pair)
(readCardsInHand "AKQT4" |> Hand.Classify).Type.Should().Be(HandType.Nothing)

(readCardsInHand "33333" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "22222" |> Hand.Classify)
(readCardsInHand "33333" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "AAAA2" |> Hand.Classify)
(readCardsInHand "88883" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "88882" |> Hand.Classify)
(readCardsInHand "88883" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "99922" |> Hand.Classify)
(readCardsInHand "44433" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "44422" |> Hand.Classify)
(readCardsInHand "55533" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "66643" |> Hand.Classify)
(readCardsInHand "44453" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "44452" |> Hand.Classify)
(readCardsInHand "44453" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "44332" |> Hand.Classify)
(readCardsInHand "44332" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "3322A" |> Hand.Classify)
(readCardsInHand "44332" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "AA432" |> Hand.Classify)
(readCardsInHand "22KQT" |> Hand.Classify).Should().BeGreaterThan(readCardsInHand "KQTJ9" |> Hand.Classify)

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (ssplit2 " ")
    |> Array.map (fun (a,b) -> a |> readCardsInHand, b |> int)

let solve1 input =
    getInput input 
    |> Seq.map (fun (cards, bid) -> Hand.Classify(cards), bid)
    |> Seq.sortBy(fst)
    |> Seq.mapi (fun i (h,bid) ->
        let rank = i + 1
        bid * rank
    )
    |> Seq.sum

solve1 "Day07.txt"
