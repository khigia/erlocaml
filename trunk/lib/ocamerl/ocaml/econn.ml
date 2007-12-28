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

(* TODO challenge type is Int32.t *)
type msg_handshake =
    | Msg_handshake_recv_name of
          int     (* distr version *)
        * Int32.t (* peer flags *)
        * string  (* peer name *)
    | Msg_handshake_status_ok
    | Msg_handshake_challenge_req of
          int       (* distr version *)
        * Int32.t   (* node flags *)
        * char list (* challenge: 4 bytes *)
        * string    (* node name *)
    | Msg_handshake_challenge_reply of
          char list (* peer challenge: 4 bytes *)
        * string    (* peer digest *)
    | Msg_handshake_challenge_rsp of
          string  (* digest *)

type msg_control =
    | Msg_control_tick
    | Msg_control_p of
          Eterm.eterm        (* control message *)
        * Eterm.eterm option (* optional parameter *)
    | Msg_control_any of
          char   (* tag? *)
        * string (* raw binary data *)


let distr_version = 5
let handshake_recv_name = 'n'
let handshake_challenge_reply = 'r'


let generate_challenge () =
    [
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
    ]


let respond_challenge challenge cookie =
    let challengeValue = Tools.int32_of_chars challenge in
    let challengeRepr = Printf.sprintf "%lu" challengeValue in
    Digest.string (cookie ^ challengeRepr)

let check_digest peerDigest cookie challenge =
    let expected = respond_challenge challenge cookie in
    match String.compare expected peerDigest with
        | 0 -> true
        | _ -> false

let handshake_message_to_string msg = match msg with
    | Msg_handshake_recv_name (distr, flags, name) ->
        Printf.sprintf
            "Msg_handshake_recv_name(%i, %s, %s)"
            distr
            (Int32.to_string flags)
            name
    | Msg_handshake_challenge_reply ([c0;c1;c2;c3], digest) ->
        Printf.sprintf
            "Msg_handshake_challenge_reply(0x%.2X%.2X%.2X%.2X, 0x%s)"
            (int_of_char c0)
            (int_of_char c1)
            (int_of_char c2)
            (int_of_char c3)
            (Digest.to_hex digest)
    | Msg_handshake_challenge_rsp data ->
        Printf.sprintf
            "Msg_handshake_challenge_rsp(%s)"
            (Digest.to_hex data)

let control_message_to_string msg = match msg with
    | Msg_control_tick ->
        "Msg_control_tick"
    | Msg_control_p (ctrl, None) ->
        Printf.sprintf
            "Msg_control_p(%s)"
            (Eterm.to_string ctrl)
    | Msg_control_p (ctrl, Some arg) ->
        Printf.sprintf
            "Msg_control_p(%s, %s)"
            (Eterm.to_string ctrl)
            (Eterm.to_string arg)
    | Msg_control_any (tag, data) ->
        Printf.sprintf
            "Msg_control_any(%c, %s)"
            tag
            data

let rec control_message_of_stream =
    parser
        | [< len = eint 4; stream >] ->
            Trace.printf "main parse %i\n" len;
            Trace.flush ();
            let p = match len with
                | 0 ->
                    begin
                    parser [< >] ->
                        Msg_control_tick
                    end
                | _ ->
                    begin
                    parser [< 'tag; msg = tag_parse (len - 1) tag >] ->
                        msg
                    end
            in
            p stream

and tag_parse len tag =
    Trace.printf "tag_parse %i %c\n" len tag;
    Trace.flush ();
    match tag with
        | 'p' -> parse_p len
        | _ -> parse_any tag len

and parse_any tag len =
    Trace.printf "parse_any %i %c\n" len tag;
    Trace.flush ();
    parser [< data = string_n len >] ->
        Msg_control_any (tag, data)

and parse_p len =
    Trace.printf "parse_p %i\n" len;
    Trace.flush ();
    parser [< ctrl = Eterm.of_stream; stream >] ->
        parse_p_arg ctrl stream

and parse_p_arg ctrl =
    Trace.printf "parse_p_arg\n";
    Trace.flush ();
    parser
        | [< arg = Eterm.of_stream; s >] ->
            Msg_control_p (ctrl, Some arg)
        | [< s >] ->
            Msg_control_p (ctrl, None)

and string_n n =
  parser [< s = Tools.nnext n [] >] -> Tools.implode s

and eint n =
  parser [< s = Tools.nnext n [] >] -> Tools.int_of_chars s


let rec handshake_message_of_stream =
    parser [< len = eint 2; 'tag; msg = tag_parse (len - 1) tag >] ->
            msg

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
        peerName = string_n (len - 6)
    >] ->
        Msg_handshake_recv_name (
            peerDistrVsn,
            peerFlags,
            peerName
        )

and parse_handshake_challenge_reply len =
    parser [<
        peerChallenge = Tools.nnext 4 [];
        peerDigest = string_n (len - 4)
    >] ->
        Msg_handshake_challenge_reply (
            peerChallenge,
            peerDigest
        )

and string_n n =
  parser [< s = Tools.nnext n [] >] -> Tools.implode s

and eint n =
  parser [< s = Tools.nnext n [] >] -> Tools.int_of_chars s

and eint32 n =
  parser [< s = Tools.nnext n [] >] -> Tools.int32_of_chars s

let handshake_message_to_chars msg = match msg with
    | Msg_handshake_status_ok ->
        Tools.explode "sok"
    | Msg_handshake_challenge_req (
            distr_version,
            flags,
            challenge,
            nodeName
        ) ->
        handshake_recv_name 
        :: (Tools.chars_of_int distr_version [] 2)
        @  (Tools.chars_of_int32 flags [] 4)
        @  challenge
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
        (Tools.dump_hex r "<<" ">>" " ");
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
        (Tools.dump_hex r "<<" ">>" " ");
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
    (* TODO only if node has no ongoing connection! *)
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
                Trace.debug (lazy (Trace.printf
                    "Peer digest OK (cookie=%s); sending node digest\n"
                    state.cookie
                ));
                Trace.flush ();
                let rsp = Msg_handshake_challenge_rsp
                    (respond_challenge
                        peerChallenge
                        state.cookie
                    )
                in
                Trace.debug (lazy (Trace.printf
                    "Computed digest respond: %s\n"
                    (handshake_message_to_string rsp)
                ));
                output_string oc (pack_handshake_msg rsp);
                flush oc;
                Trace.debug (lazy (Trace.printf
                    "Node digest sent back\n"
                ));
                Trace.flush ();
                true
            | false ->
                print_endline "handshake digest NOT ok";
                false
    

let _handshake state istream oc =
    let challenge = _handshake_recv_name state istream oc in
    _handshake_recv_challenge_reply state istream oc challenge

let rec _ticker time oc =
    Thread.delay time;
    output_string oc (pack_control_msg (Msg_control_tick));
    flush oc;
    _ticker time oc
    (* TODO catch except if oc is closed *)

let rec _control state istream oc =
    let msg = try
        control_message_of_stream istream
    with
        Stream.Failure ->
            Trace.flush ();
            Trace.printf
                "rest to parse: %i\n"
                (Stream.count istream);
            Trace.flush ();
            failwith "Control message stream failure"
    in
    Trace.flush ();
    match msg with
        | Msg_control_tick ->
            Trace.debug (lazy (Trace.printf
                "Receive tick control message\n"
            ));
        | Msg_control_p (ctrl, arg) ->
            Trace.debug (lazy (Trace.printf
                "Receive control message: %s\n"
                (control_message_to_string msg)
            ));
        | Msg_control_any (tag, data) ->
            Trace.debug (lazy (Trace.printf
                "Ignoring unknow control message\n"
            ));
    ;
    Trace.flush ();
    _control state istream oc

let _handler state id fd =
    Random.self_init ();
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    match _handshake state istream oc with
        | true ->
            Trace.debug (lazy (Trace.printf
                "Handshake OK\n"
            ));
            Trace.flush ();
            (* one thread to tick the peer *)
            let ticker = Thread.create
                (fun () -> _ticker state.tickTime oc)
                ()
            in
            _control state istream oc;
            Thread.kill ticker;
            Trace.debug (lazy (Trace.printf
                "End of connection\n"
            ));
            Trace.flush ();
        | false ->
            Trace.info (lazy (Trace.printf
                "Close connection comming from unauthorized node\n"
            ));
            Unix.close fd


let create nodeName =
    let sock = Serv.listen 0 in
    let addr, port = Serv.inet_addr sock in
    Trace.debug (lazy (Trace.printf
        "Node server listening on port %i (node '%s')\n"
        port
        nodeName
    ));
    Trace.flush ();
    let handler =
        Serv.handle_in_thread (
            Serv.trace_handler (
                Serv.make_handler (
                    _handler
                        {
                            flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                            cookie = "cookie"; (*TODO cookie as node option! *)
                            nodeName = nodeName;
                            tickTime = 1.0; (*TODO which value? *)
                        }
                )
            )
        )
    in
    let serving = fun () ->
        let _ = try
            Serv.accept_loop 0 sock handler
        with
            exn ->
                Trace.printf "ERROR: exception in server";
                print_newline ()
        in
        print_endline "End of node server!!!"
    in
    let thr = Thread.create serving () in
    {
        addr = addr;
        port = port;
        thread = thr;
    }

