let get_host_addr () =
    let hosts = Unix.gethostbyname(Unix.gethostname()) in
    hosts.Unix.h_addr_list.(0)

let inet_addr sock =
    match Unix.getsockname sock with
        Unix.ADDR_INET(host_inet_addr, port) ->
            (host_inet_addr, port)
        | _ -> failwith "not an inet address (file descriptor)"

let client_addr fd =
    match fd with
        Unix.ADDR_INET(host,_) -> Unix.string_of_inet_addr host
        | _ -> "Unexpected client"


let make_handler f id sd =
    (*TODO could have a handler type ...*)
    let _ = try
        f id sd
    with
        exn ->
            Trace.err
                "Serv"
                "handler of connection %i: %s\n"
                id
                (Printexc.to_string exn);
    in
    try
        Unix.close sd
    with
        exn ->
            Trace.err
                "Serv"
                "handler finalizer:%s\n"
                (Printexc.to_string exn)

let trace_handler handler id sd =
    Trace.inf "Serv" "Handle connection %i" id;
    print_newline ();
    let r = handler id sd in
    Trace.inf "Serv" "End of connection %i" id;
    print_newline ();
    r

let handle_in_thread f id sd =
    Thread.create (fun () -> f id sd) ()

let listen port =
    let addr = get_host_addr () in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    let inet_addr = Unix.ADDR_INET(addr, port) in
    Unix.bind sock inet_addr;
    Unix.listen sock 3;
    sock

let accept_once id sock handler =
    Trace.dbg "Serv" "Blocked on accept\n";
    Trace.flush ();
    let (sd, sa) = Unix.accept sock in 
    Trace.dbg
        "Serv"
        "New connection from %s\n"
        (client_addr sa)
    ;
    handler id sd

let rec accept_loop id sock handler_f =
    let _ = accept_once id sock handler_f in
    accept_loop (id + 1) sock handler_f

let serve port handler =
    let sock = listen port in
    Trace.inf "Serv" "Listening on port %i" port;
    Trace.flush ();
    try
        accept_loop
            0
            sock
            handler
    with
        exn ->
            Trace.err "Serv" "Exception in server";

