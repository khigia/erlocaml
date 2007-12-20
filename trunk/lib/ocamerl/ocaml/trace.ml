let level = 5

let trace lvl anyLazy =
    if lvl >= level then (Lazy.force anyLazy)

let info anyLazy =
    trace 20 anyLazy

let debug anyLazy =
    trace 10 anyLazy


let printf f =
    let thrID = Thread.id (Thread.self ()) in
    let fmt = "Thread %i: " ^^ f in
    Printf.printf fmt thrID



