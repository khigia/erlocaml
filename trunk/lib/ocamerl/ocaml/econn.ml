type t = {
    addr: Unix.inet_addr;
    port: int;
    thread: Thread.t;
}

type state = {
    nodeName: string;
    cookie: string;
    flags: Int32.t;
    tickTime: float;
}

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
    | Msg_handshake_challenge_rsp of
          string  (* digest *)

type msg_control =
    | Msg_control_tick


let distr_version = 5
let handshake_recv_name = 'n'
let handshake_challenge_reply = 'r'


let generate_challenge () =
    Random.int32 Int32.max_int

let respond_challenge challenge cookie =
    let chal = Tools.implode (
        Tools.chars_of_int32 challenge [] 4
    ) in
    Digest.string (cookie ^ chal)

let check_digest peerDigest cookie challenge =
    match respond_challenge challenge cookie with
        | peerDigest -> true
        | _ -> false

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
            (Digest.to_hex digest)


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
            nodeName
        ) ->
        'n'
        :: (Tools.chars_of_int distr_version [] 2)
        @  (Tools.chars_of_int32 flags [] 4)
        @  (Tools.chars_of_int32 challenge [] 4)
        @  (Tools.explode nodeName)
    | Msg_handshake_challenge_rsp digest ->
        'a'
        :: (Tools.explode digest)


let pack_handshake_msg msg =
    let chars = handshake_message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int len [] 2 in
    let r = Tools.implode (head @ chars) in
    Trace.printf "Packed handshake msg: %s\n"
        (Tools.dump_dec r "<<" ">>");
    r

let control_message_to_chars msg = match msg with
    | Msg_control_tick ->
        []

let pack_control_msg msg =
    let chars = control_message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int len [] 4 in
    let r = Tools.implode (head @ chars) in
    Trace.printf "Packed control msg: %s\n"
        (Tools.dump_dec r "<<" ">>");
    r

let _handshake_recv_name state is oc =
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
            state.nodeName
        )));
    flush oc;
    challenge

let _handshake_recv_challenge_reply state is oc challenge =
    let msg = try
        handshake_message_of_stream is
    with
        Stream.Failure ->
            Stream.dump print_char is;
            failwith "got stream failure"
    in
    Trace.printf
        "Receive handshake message: %s\n"
        (handshake_message_to_string msg);
    match msg with
        | Msg_handshake_challenge_reply (
            peerChallenge,
            peerDigest
        ) ->
        match check_digest peerDigest state.cookie challenge with
            | true ->
                print_endline "handshake digest ok";
                output_string
                    oc
                    (pack_handshake_msg (Msg_handshake_challenge_rsp
                        (respond_challenge peerChallenge state.cookie)
                    ));
    (* TODO
                use tick manager,
                send node msg that peer is ok
                go in tick manager function
    *)
                true
            | false ->
                print_endline "handshake digest NOT ok";
                false
    (* TODO
                debug message (unauthorized node)
                close connection

    *)
    

let _handshake state istream oc =
    let challenge = _handshake_recv_name state istream oc in
    _handshake_recv_challenge_reply state istream oc challenge;
    ()

let rec _ticker time oc =
    Thread.delay time;
    output_string oc (pack_control_msg (Msg_control_tick));
    flush oc;
    _ticker time oc
    (* TODO catch except if oc is closed *)


let _dump state istream oc =
    print_endline "dumping input";
    Stream.iter print_char istream;
    print_endline "dumping end";
    flush_all;
    ()

let _handler state id fd =
    Random.self_init ();
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    _handshake state istream oc;
    (* one thread to tick the peer *)
    let ticker = Thread.create
        (fun () -> _ticker state.tickTime oc)
        ()
    in
    (* TODO current thread receive control message and ticks
    if it closes, it also kill the ticker
    _connected ticker state istream;
    *)
    _dump state istream oc


let create nodeName =
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
                    _handler
                        {
                            flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                            cookie = "crap"; (*TODO cookie as node option! *)
                            nodeName = nodeName;
                            tickTime = 1.0; (*TODO which value? *)
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

