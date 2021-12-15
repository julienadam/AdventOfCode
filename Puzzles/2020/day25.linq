<Query Kind="FSharpProgram" />

let magicNo = 20201227L
let realCardPk = 17773298L
let realDoorPk = 15530095L
let sampleCardPk = 5764801L
let sampleDoorPk = 17807724L

let transform subjectNumber loopSize =
    [1L..loopSize] 
    |> List.fold (fun current _ -> 
        (current * subjectNumber) % magicNo) 1L

// Could precalculate and store, depending on part 2.
let infiniteTransforms subjectNumber = seq {
    let mutable loopSize = 1L
    let mutable current = subjectNumber
    while true do
        yield loopSize, current
        current <- (current * subjectNumber) % magicNo
        loopSize <- loopSize + 1L
}
  
let inferLoopSize output subjectNumber =
    let sw = Stopwatch.StartNew()
    infiniteTransforms subjectNumber
    |> Seq.find (fun (loopSize, transformed) -> transformed = output)
    |> fst

//let cardPk = sampleCardPk
//let doorPk = sampleDoorPk
let cardPk = realCardPk
let doorPk = realDoorPk

let cardLoopSize = inferLoopSize cardPk 7L |> Dump
let doorLoopSize = inferLoopSize doorPk 7L |> Dump

let cardEncryptionKey = transform doorPk cardLoopSize
printfn "Card encryption key : %i" cardEncryptionKey
let doorEncryptionKey = transform cardPk doorLoopSize
printfn "Door encryption key : %i" doorEncryptionKey
