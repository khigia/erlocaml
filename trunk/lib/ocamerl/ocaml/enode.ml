type node = {
    name: string;
    mutable epmc: Epmc.t;
    server: server;
    pids: pids;     (* PID management *)
}
and server = {
    addr: Unix.inet_addr;
    port: int;
    thread: Thread.t;
}
and pids = {
    mutable creation: int option;
    mutable pid_count: int;
    mutable serial: int;
    pid_to_mbox: (Eterm.e_pid, mbox) Hashtbl.t;
    mbox_to_pid: (mbox, Eterm.e_pid) Hashtbl.t;
}
and mbox = {
    pid: Eterm.e_pid;
}

type node_server_state = {
    node_name: string;
    cookie: string;
    flags: Int32.t;
}

let distr_version = 5

(* node server *)

let handshake_recv_name = 'n'
let handshake_challenge_reply = 'r'

let generate_challenge () =
    Random.int32 Int32.max_int
    
type msg_handshake =
    | Msg_handshake_recv_name of
          int     (* distr version *)
        * Int32.t (* peer flags *)
        * string  (* peer name *)
    | Msg_handshake_status_ok
    | Msg_handshake_challenge_req of
          int     (* distr version *)
        * Int32.t (* node flags *)
        * Int32.t (* challenge *)
        * string  (* node name *)
    | Msg_handshake_challenge_reply of
          Int32.t (* peer challenge *)
        * string  (* peer digest *)


let handshake_message_to_string msg = match msg with
    | Msg_handshake_recv_name (distr, flags, name) ->
        Printf.sprintf
            "HanshakeRecvName(%i, %s, %s)"
            distr
            (Int32.to_string flags)
            name
    | Msg_handshake_challenge_reply (challenge, digest) ->
        Printf.sprintf
            "HandshakeChallengeReply(%s, %s)"
            (Int32.to_string challenge)
            digest


let rec handshake_message_of_stream =
    parser [< len = eint 2; 'tag; msg = tag_parse len tag >] -> msg

and tag_parse len tag =
    match tag with
        | n when n = handshake_recv_name -> parse_handshake_recv_name len
        | n when n = handshake_challenge_reply -> parse_handshake_challenge_reply len
        | _ ->
            failwith "handshake message tag not recognized"

and parse_handshake_recv_name len =
    parser [<
        peerDistrVsn = eint 2;
        peerFlags = eint32 4;
        peerName = string_n (len - 7)
    >] ->
        Msg_handshake_recv_name (
            peerDistrVsn,
            peerFlags,
            peerName
        )

and parse_handshake_challenge_reply len =
    parser [<
        peerChallenge = eint32 4;
        peerDigest = string_n (len - 5)
    >] ->
        Msg_handshake_challenge_reply (
            peerChallenge,
            peerDigest
        )

and string_n n =
  parser [< s = Tools.nnext n [] >] -> Tools.implode s

and eint n =
  parser [< s = Tools.nnext n [] >] -> Tools.int_of_chars s 0

and eint32 n =
  parser [< s = Tools.nnext n [] >] -> Tools.int32_of_chars s 0l

let handshake_message_to_chars msg = match msg with
    | Msg_handshake_status_ok ->
        Tools.explode "sok"
    | Msg_handshake_challenge_req (
            distr_version,
            flags,
            challenge,
            node_name
        ) ->
        'n'
        :: (Tools.chars_of_int distr_version [] 2)
        @  (Tools.chars_of_int32 flags [] 4)
        @  (Tools.chars_of_int32 challenge [] 4)
        @  (Tools.explode node_name)


let pack_handshake_msg msg =
    let chars = handshake_message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int len [] 2 in
    let r = Tools.implode (head @ chars) in
    Trace.printf "Packed handshake msg: %s\n"
        (Tools.dump_dec r "<<" ">>");
    r


(*
let rec _node_server_state_dump state ic oc =
    let buf = String.create 30 in
    let len = input ic buf 0 30 in
    Trace.printf
        "Got data (%i bytes): %s\n"
        len
        (Tools.dump_dec (String.sub buf 0 len) "<<" ">>")
    ;
    Trace.flush;
    _node_server_state_dump state ic oc
*)

let _node_server_handshake_recv_name state is oc =
    let msg = try
        handshake_message_of_stream is
    with
        Stream.Failure ->
            Stream.dump print_char is;
            failwith "got stream failure"
    in
    Trace.printf
        "Receive handshake message: %s\n"
        (handshake_message_to_string msg)
    ;
    output_string
        oc
        (pack_handshake_msg Msg_handshake_status_ok);
    let challenge = generate_challenge () in
    output_string
        oc
        (pack_handshake_msg (Msg_handshake_challenge_req (
            distr_version,
            state.flags,
            challenge,
            state.node_name
        )));
    flush oc;
    challenge

let _node_server_handshake_recv_challenge_reply state is oc challenge =
    let msg = try
        handshake_message_of_stream is
    with
        Stream.Failure ->
            Stream.dump print_char is;
            failwith "got stream failure"
    in
    Trace.printf
        "Receive handshake message: %s\n"
        (handshake_message_to_string msg)
    (* TODO
    match msg with Msg_handshake_challenge_reply peerChallenge peerDigest ->
        match check_digest peerDigest challenge state.cookie with
            | true ->
                send ack,
                use tick manager,
                send node msg that peer is ok
                go in tick manager function
            | false ->
                debug message (unauthorized node)
                close connection

    *)
    

let _node_server_handshake state istream oc =
    let challenge = _node_server_handshake_recv_name state istream oc in
    _node_server_handshake_recv_challenge_reply state istream oc challenge;
    ()

let _node_server_handler state id fd =
    Random.self_init ();
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    _node_server_handshake state istream oc

let _create_node_server nodeName =
    let sock = Serv.listen 0 in
    let addr, port = Serv.inet_addr sock in
    Trace.debug (lazy (Trace.printf
        "Node server listening on port %i (node '%s')\n"
        port
        nodeName
    ));
    Trace.flush;
    let handler =
        Serv.handle_in_thread (
            Serv.trace_handler (
                Serv.make_handler (
                    _node_server_handler
                        {
                            flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                            cookie = "crap"; (*TODO cookie as node option! *)
                            node_name = nodeName;
                        }
                )
            )
        )
    in
    let serving = fun () -> Serv.accept_loop
        0
        sock
        handler
    in
    let thr = Thread.create serving () in
    {
        addr = addr;
        port = port;
        thread = thr;
    }


(* PID management *)

let _create_pid_manager count serial creation =
    {
        creation = creation;
        pid_count = count;
        serial = serial;
        pid_to_mbox = Hashtbl.create 10;
        mbox_to_pid = Hashtbl.create 10;
    }

let _create_pid pids nodeName =
    match pids.creation with
        | None -> failwith "node not published"
        | Some n ->
            let pid = Eterm.make_pid
                nodeName
                pids.pid_count
                pids.serial
                n
            in
            pids.pid_count <- pids.pid_count + 1;
            if pids.pid_count > 0x7fff then (
                pids.pid_count <- 0;
                pids.serial <- pids.serial + 1;
                if pids.serial > 0x07 then pids.serial <- 0
            );
            pid

let _create_mbox pids pid =
    let mbox =
    {
        pid = pid;
    } in
    Hashtbl.add pids.pid_to_mbox pid mbox;
    Hashtbl.add pids.mbox_to_pid mbox pid;
    mbox


(* node management *)

let _trace_pids indent pids =
    let indent2 = indent ^ indent in
    Printf.printf
        "%sPID manager:
%screation: %s
%spid_count: %i
%sserial: %i
%spid_to_mbox length: %i
%smbox_to_pid length: %i
"
        indent
        indent2 (match pids.creation with None -> "None" | Some n -> Printf.sprintf "Some %i" n)
        indent2 pids.pid_count
        indent2 pids.serial
        indent2 (Hashtbl.length pids.pid_to_mbox)
        indent2 (Hashtbl.length pids.mbox_to_pid)

let trace indent node =
    Trace.info (lazy (
        Trace.printf
            "Node:
%sname: %s
"
        indent node.name;
        _trace_pids indent node.pids
    ))

let make nodeName =
    Trace.info (lazy (Trace.printf 
        "Making node '%s'\n" 
        nodeName
    ));
    let server = _create_node_server nodeName in
    let epmc = Epmc.make nodeName server.port in
    let pids = _create_pid_manager 0 0 None in
    {
        name = nodeName;
        epmc = epmc;
        server = server;
        pids = pids;
    }

let is_published node =
    match node.pids.creation with
        | None -> false
        | Some _ -> true

let publish node =
    (* TODO should this function fail if publication fails? *)
    let result = Epmc.connect node.epmc in
    node.pids.creation <- result;
    ()
    
let unpublish node =
    Epmc.disconnect node.epmc
    
let create_mbox node =
    let pid = _create_pid node.pids node.name in
    let mbox = _create_mbox node.pids pid in
    mbox

let loop node =
    Thread.join node.server.thread
