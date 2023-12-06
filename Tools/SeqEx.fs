namespace AdventOfCode

module SeqEx =
    let read n s =
        s |> Seq.take n, s |> Seq.skip n

    let crossproduct l1 l2 = seq {
        for el1 in l1 do
            for el2 in l2 do
                yield el1, el2 }

    let rec combinations acc size set = seq {
      match size, set with 
      | n, x::xs -> 
          if n > 0 then yield! combinations (x::acc) (n - 1) xs
          if n >= 0 then yield! combinations acc n xs 
      | 0, [] -> yield acc 
      | _, [] -> () }
    
    let inline product64 (numbers : int64 seq) = numbers |> Seq.fold (fun a b -> a * b) 1L
