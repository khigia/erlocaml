let get_host_addr () =
    let hosts = Unix.gethostbyname(Unix.gethostname()) in
    hosts.Unix.h_addr_list.(0)

let client_addr sock =
    match sock with
        Unix.ADDR_INET(host,_) -> Unix.string_of_inet_addr host
        | _ -> "Unexpected client"

let make_handler f id sd =
    (*TODO could have a handler type ...*)
    begin
        try
            f id sd
        with exn -> Printf.printf "ERROR:handler exec:%s\n" (Printexc.to_string exn)
    end;
    try
        Unix.close sd
    with exn -> Printf.printf "ERROR:handler finalizer:%s\n" (Printexc.to_string exn)

let trace_handler handler id sd =
    Printf.printf "TRACE: handle connection %i" id;
    print_newline ();
    let r = handler id sd in
    Printf.printf "TRACE: end of connection %i" id;
    print_newline ();
    r

let handle_in_thread handler id sd =
    ignore (Thread.create (fun () -> handler id sd) ())

let rec acceptor id sock handler =
    let (sd, sa) = Unix.accept sock in 
    Printf.printf "TRACE: new connection from %s" (client_addr sa) ;
    print_newline ();
    handler id sd;
    acceptor (id + 1) sock handler

let serve port handler =
    let addr = get_host_addr() in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (Unix.ADDR_INET(addr, port));
    Unix.listen sock 3;
    Printf.printf "Listening on port %i" port;
    print_newline ();
    try
        acceptor 0 sock handler
    with
        exn ->
            Printf.printf "ERROR: exception in acceptor";
            print_newline ()

