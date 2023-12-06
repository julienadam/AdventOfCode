namespace AdventOfCode

module Queues =
    type queue<'a> =
        | Queue of 'a list * 'a list
    
    let empty = Queue([], [])
    
    let enqueue q e = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)
    
    let dequeue q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)
