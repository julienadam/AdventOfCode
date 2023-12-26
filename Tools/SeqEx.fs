namespace AdventOfCode

module SeqEx =
    let read n s =
        s |> Seq.take n, s |> Seq.skip n

    let crossproduct l1 l2 = seq {
        for el1 in l1 do
            for el2 in l2 do
                yield el1, el2 }

    let distinctCrossProduct l1 l2 = seq {
        for el1 in l1 do
            for el2 in l2 do
                if el1 <> el2 then
                    yield el1, el2 
        }

    let autoProduct (src:'a array) = seq {
        for i = 0 to src.Length - 1 do
            for j = i+1 to src.Length - 1 do
                yield src[i], src[j]
    }

    let rec combinations acc size set = seq {
      match size, set with 
      | n, x::xs -> 
          if n > 0 then yield! combinations (x::acc) (n - 1) xs
          if n >= 0 then yield! combinations acc n xs 
      | 0, [] -> yield acc 
      | _, [] -> () }

    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    let inline product64 (numbers : int64 seq) = numbers |> Seq.fold (fun a b -> a * b) 1L
