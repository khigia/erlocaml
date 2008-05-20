let lvl_debug   = 10
let lvl_todo    = 15
let lvl_info    = 20
let lvl_warning = 30
let lvl_error   = 40

type output = Default | File

let _channel = ref stderr
let _channelType = ref Default
let _curlvl = ref lvl_warning
let _mutex = Mutex.create ()

let set_level lvl =
    _curlvl := lvl

let _clean_channel () =
    match !_channelType with
    | File ->
        close_out_noerr !_channel;
        _channel := stderr;
        _channelType := Default
    | _ ->
        ()

let to_file ?(fn="") () =
    let rec find_suffix b c =
        let cur = b ^ "." ^ (string_of_int c) in
        if Sys.file_exists cur
        then find_suffix b (c + 1)
        else cur
    in
    let fname = match fn with
    | "" ->
        (* TODO Tools.ensure_dir *)
        let logdir = Filename.concat Filename.current_dir_name "log" in
        if Sys.file_exists logdir && Sys.is_directory logdir
        then ()
        else Unix.mkdir logdir 0o750;
        let logfn = Filename.basename Sys.executable_name  ^ ".log" in
        let b = Filename.concat logdir logfn in
        find_suffix b 0
    | name ->
        name
    in
    let s = open_out fname in
    Printf.eprintf "LOG FILE = %s\n%!" fname;
    _clean_channel ();
    _channelType := File;
    _channel := s

let _init =
    (* ensure Thread.self is ok *)
    Thread.yield ();
    at_exit _clean_channel

let _now () =
    let tm = Unix.localtime (Unix.time ()) in
    (* TODO need standard datetime format *)
    Printf.sprintf
        "%02d:%02d:%02d"
        tm.Unix.tm_hour
        tm.Unix.tm_min
        tm.Unix.tm_sec

let _level_to_string lvl = match lvl with
    | n when n = lvl_debug   -> "DEBUG  "
    | n when n = lvl_todo    -> "!TODO! "
    | n when n = lvl_info    -> "INFO   "
    | n when n = lvl_warning -> "WARNING"
    | n when n = lvl_error   -> "ERROR  "
    | n -> Printf.sprintf "%d" n

let p lvl who f =
    let lock () = Mutex.lock _mutex in
    let unlock channel = Mutex.unlock _mutex in
    let thrID = Thread.id (Thread.self ()) in
    let fmt = "%s: %s: thr %03i: %s: " ^^ f in
    let printer = if !_curlvl <= lvl then begin lock (); Printf.kfprintf unlock end else Printf.ifprintf in
    printer !_channel fmt (_now ()) (_level_to_string lvl) thrID who

let flush () =
    flush !_channel

let dbg who f = p lvl_debug who f

let todo who f = p lvl_todo who f

let inf who f = p lvl_info who f

let wrn who f = p lvl_warning who f

let err who f = p lvl_error who f

