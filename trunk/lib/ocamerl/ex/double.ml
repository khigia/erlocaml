let send sock_addr t =
    let ic, oc = Unix.open_connection sock_addr in
    output_string oc (Eterm.to_binary t);
    flush oc;
    print_endline "OCAML.Double: Sent!";
    Unix.shutdown_connection ic

let rec transform t =
    match t with
        | Eterm.ET_int n -> Eterm.ET_int (Int32.mul n 2l)
        | Eterm.ET_float f -> Eterm.ET_float (f *. 2.0)
        | Eterm.ET_string s -> Eterm.ET_string (s ^ s)
        | Eterm.ET_tuple l ->
            Eterm.ET_tuple (Eterm.ETuple.make (List.rev (Eterm.ETuple.fold_left (fun acc e -> (transform e) :: acc) [] l)))
        | x -> x

let term sock_addr t =
    (* transform the term *)
    let newT = transform t in
    let newS = Eterm.to_string newT in
    let newB = Eterm.to_binary newT in
    Printf.printf "OCAML.Double: Transformed term: %s\n" newS;
    Printf.printf "OCAML.Double: Transformed binary form: %s\n" (Eterm.EBinary.to_string newB);
    flush_all ();
    (* send back the transformed term *)
    send sock_addr newT

let rec erl_terms f connection_id istream oc =
    let t = Eterm.of_stream istream in
    let s = Eterm.to_string t in
    let b = Eterm.to_binary t in
    Printf.printf "OCAML.Double: Read term: %s\n" s;
    Printf.printf "OCAML.Double: Binary form: %s\n" (Eterm.EBinary.to_string b);
    flush_all ();
    f t;
    erl_terms f connection_id istream oc

let erl_handler f id fd =
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    try
        erl_terms f id istream oc
    with
        Stream.Failure ->
            print_endline "OCAML.Double: Stream failure (connection closed?)"


let main () =
    let recvPort = ref 12345 in
    let sendPort = ref 54321 in
    Arg.parse
        [
            (
                "-recv",
                Arg.Int (fun i -> recvPort := i; ()),
                Printf.sprintf "PORT port to listen (default %i)" !recvPort
            );
            (
                "-send",
                Arg.Int (fun i -> sendPort := i; ()),
                Printf.sprintf "PORT port to send data (default %i)" !sendPort
            )
        ]
        (fun anon ->
            Printf.printf "unrecognized anonymous argument: %s\n" anon;
            print_string "Try option -help";
            exit(1)
        )
        "Serve a raw socket connection."
    ;
    Trace.init ();
    let addr = Serv.get_host_addr () in
    let sock_addr = Unix.ADDR_INET(addr, !sendPort) in
    let core_handler = erl_handler (term sock_addr) in
    let handler = Serv.trace_handler (Serv.make_handler core_handler) in
    Serv.serve !recvPort handler

let _ = main ()

