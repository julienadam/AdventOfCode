#r "nuget: OptimizedPriorityQueue"


open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Priority_Queue

type private Node<'a>(value: 'a) =
    inherit FastPriorityQueueNode()
    member this.Value = value

let private solveAll start vertices neighbors distanceBetween =

    let mutable queue = FastPriorityQueue(vertices |> List.length)
    let mutable dists = Map.empty
    let mutable prevs = Map.empty
    let mutable nodeByVertex = Map.empty

    vertices
    |> List.iter
        (fun v ->
            let node = Node(v)
            if v <> start then
                dists <- dists |> Map.add v Single.MaxValue
            else
                dists <- dists |> Map.add v 0f

            prevs <- prevs |> Map.add v None
            queue.Enqueue(node,  dists.[v])
            nodeByVertex <- nodeByVertex |> Map.add v node)

    while not <| (queue.Count = 0) do
        let u = queue.Dequeue()
        
        u.Value
        |> neighbors
        |> List.map (fun v -> nodeByVertex.[v])
        |> List.filter queue.Contains
        |> List.iter
            (fun v ->
                let alt = dists.[u.Value] + distanceBetween u.Value v.Value

                if alt < dists.[v.Value] then
                    dists <- dists |> Map.add v.Value alt
                    prevs <- prevs |> Map.add v.Value (Some u.Value)
                    queue.UpdatePriority(v, alt))

    prevs

let dijkstra start goal vertices neighbors distance =

    let prevs = solveAll start vertices neighbors distance

    let rec makePath current =
        match prevs.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal