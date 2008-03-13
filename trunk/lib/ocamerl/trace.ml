let lvl_debug   = 10
let lvl_todo    = 15
let lvl_info    = 20
let lvl_warning = 30
let lvl_error   = 40

let init () =
    (* ensure Thread.self is ok *)
    Thread.yield ()

let now () =
    let tm = Unix.localtime (Unix.time ()) in
    (* TODO need standard datetime format *)
    Printf.sprintf
        "%02d:%02d:%02d"
        tm.Unix.tm_hour
        tm.Unix.tm_min
        tm.Unix.tm_sec

let level lvl = match lvl with
    | n when n = lvl_debug   -> "DEBUG  "
    | n when n = lvl_todo    -> "!TODO! "
    | n when n = lvl_info    -> "INFO   "
    | n when n = lvl_warning -> "WARNING"
    | n when n = lvl_error   -> "ERROR  "
    | n -> Printf.sprintf "%d" n

let p lvl who f =
    let thrID = Thread.id (Thread.self ()) in
    let fmt = "%s: %s: thr %03i: %s: " ^^ f in
    Printf.printf fmt (now ()) (level lvl) thrID who

let dbg who f = p lvl_debug who f

let todo who f = p lvl_todo who f

let inf who f = p lvl_info who f

let wrn who f = p lvl_warning who f

let err who f = p lvl_error who f

let flush () =
    flush stdout
