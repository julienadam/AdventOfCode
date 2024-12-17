module Dijkstra

open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open System.Collections.Generic
open Priority_Queue

type private Node<'a>(value: 'a) =
    inherit FastPriorityQueueNode()
    member this.Value = value

let inline private createOrUpdate (key:'a) (value:'b) (dic:Dictionary<'a,'b>) =
    if dic.TryAdd(key, value) = false then
        dic[key] <- value

let private solveAll start vertices neighbors distanceBetween =

    let mutable queue = FastPriorityQueue(vertices |> List.length)
    let dists = new Dictionary<'a, float32>()
    let prevs = new Dictionary<'a, 'a option>()
    let nodeByVertex = new Dictionary<'a, Node<'a>>()

    vertices
    |> List.iter (fun v ->
        let node = Node(v)
        if v <> start then
            dists |> createOrUpdate v Single.MaxValue
        else
            dists |> createOrUpdate v 0f

        prevs |> createOrUpdate v None
        queue.Enqueue(node,  dists.[v])
        nodeByVertex |> createOrUpdate v node
    )

    while not <| (queue.Count = 0) do
        let u = queue.Dequeue()
        
        u.Value
        |> neighbors
        |> List.map (fun v -> nodeByVertex.[v])
        |> List.filter queue.Contains
        |> List.iter (fun v ->
            let alt = dists.[u.Value] + distanceBetween u.Value v.Value

            if alt < dists.[v.Value] then
                dists |> createOrUpdate v.Value alt
                prevs |> createOrUpdate v.Value (Some u.Value)
                queue.UpdatePriority(v, alt)
        )

    prevs, dists


let getDistMatrix start vertices neighbors distance = solveAll start vertices neighbors distance |> snd

let solve start goal vertices neighbors distance =

    let prevs, dists = solveAll start vertices neighbors distance

    let rec makePath current =
        match prevs.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal, dists