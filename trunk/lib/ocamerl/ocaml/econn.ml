module Handshake = struct

    (* Constants *)

    let tag_status           = 's'
    let tag_recv_name        = 'n'
    let tag_challenge_rsp    = 'r'
    let tag_challenge_digest = 'a'


    (* Challenge *)

    let _create_challenge () =
        Tools.int32_of_chars [
            char_of_int (Random.int 0xFF);
            char_of_int (Random.int 0xFF);
            char_of_int (Random.int 0xFF);
            char_of_int (Random.int 0xFF);
        ]

    let _compute_digest challenge cookie =
        (* do not use Int32.to_string as it assume signed int32 *)
        let challengeRepr = Printf.sprintf "%lu" challenge in
        Digest.string (cookie ^ challengeRepr)

    let _check_digest peerDigest cookie challenge =
        match challenge with
            | Some value ->
                let expected = _compute_digest value cookie in
                (*Trace.debug (lazy (Trace.printf
                    "Check digest\nExpected: %s\nReceived: %s\n"
                    (Digest.to_hex expected)
                    (Digest.to_hex peerDigest)
                ));*)
                expected = peerDigest
            | None ->
                false


    (* Messages *)

    type message =
        | Msg_recv_name of
              int     (* distr version *)
            * Int32.t (* peer flags *)
            * string  (* peer name *)
        | Msg_status of
            string
        | Msg_challenge_req of
              int       (* distr version *)
            * Int32.t   (* node flags *)
            * Int32.t   (* challenge *)
            * string    (* node name *)
        | Msg_challenge_rsp of
              Int32.t   (* peer challenge *)
            * string    (* peer digest *)
        | Msg_challenge_digest of
              string  (* digest *)

    let message_to_string msg = match msg with
        | Msg_recv_name (distr, flags, name) ->
            Printf.sprintf
                "Msg_recv_name(%i, %lu, %s)"
                distr
                flags
                name
        | Msg_challenge_rsp (challenge, digest) ->
            Printf.sprintf
                "Msg_challenge_rsp(%lu, 0x%s)"
                challenge
                (Digest.to_hex digest)
        | Msg_challenge_digest data ->
            Printf.sprintf
                "Msg_challenge_digest(%s)"
                (Digest.to_hex data)

    let rec message_of_stream =
        parser [<
            len = Tools.eint_n 2;
            'tag;
            msg = _parse (len - 1) tag
        >] ->
            msg

    and _parse len tag =
        match tag with
            | n when n = tag_recv_name -> _parse_recv_name len
            | n when n = tag_challenge_rsp -> _parse_challenge_rsp len
            | _ ->
                failwith "handshake message tag not recognized"

    and _parse_recv_name len =
        parser [<
            peerDistrVsn = Tools.eint_n 2;
            peerFlags = Tools.eint32_n 4;
            peerName = Tools.string_n (len - 6)
        >] ->
            Msg_recv_name (
                peerDistrVsn,
                peerFlags,
                peerName
            )

    and _parse_challenge_rsp len =
        parser [<
            challenge = Tools.eint32_n 4;
            digest = Tools.string_n (len - 4)
        >] ->
            Msg_challenge_rsp (
                challenge,
                digest
            )

    let _message_to_chars msg = match msg with
        | Msg_status status ->
            tag_status 
            :: (Tools.explode status)
        | Msg_challenge_req (
                distrVersion,
                flags,
                challenge,
                nodeName
            ) ->
            tag_recv_name 
            :: (Tools.chars_of_int distrVersion 2)
            @  (Tools.chars_of_int32 flags 4)
            @  (Tools.chars_of_int32 challenge 4)
            @  (Tools.explode nodeName)
        | Msg_challenge_digest digest ->
            tag_challenge_digest
            :: (Tools.explode digest)

    let pack msg =
        let chars = _message_to_chars msg in
        let len = List.length chars in
        let head = Tools.chars_of_int len 2 in
        let r = Tools.implode (head @ chars) in
        Trace.debug (lazy (Trace.printf
            "Packed handshake msg: %s\n"
            (Tools.dump_hex r "<<" ">>" " ")
        ));
        r


    (* FSM *)

    type fsmState = {
        version:   int;
        name:      string;
        cookie:    string;
        flags:     Int32.t;
        challenge: Int32.t option;
    }

    let _st_recv_challenge_rsp st msg = match msg with
        | Msg_challenge_rsp (peerChallenge, digest) ->
            match _check_digest digest st.cookie st.challenge with
                | true ->
                    Trace.debug (lazy (Trace.printf
                        "Peer digest OK\n"
                    ));
                    (* reply to peer challenge *)
                    let reply = Msg_challenge_digest
                        (_compute_digest peerChallenge st.cookie)
                    in
                    (* handshake OK! *)
                    (
                        st,
                        None, (* end of FSM *)
                        Some true, (* handshake finished and ok *)
                        [reply;]
                    )
                | false ->
                    Trace.debug (lazy (Trace.printf
                        "Handshake failed: peer digest NOT ok\n"
                    ));
                    (* handshake failing here! *)
                    (st, None, Some false, [])
        | _ ->
            Trace.debug (lazy (Trace.printf
                "Handshake failed: got wrong message: %s\n"
                (message_to_string msg)
            ));
            (* handshake failing here! *)
            (st, None, Some false, [])

    let _st_recv_name st msg =
        (*TODO get info from msg: peerName ... *)
        let challenge = _create_challenge () in
        let msgStatus = Msg_status "ok" in
        let msgChallengeReq = Msg_challenge_req (
            st.version,
            st.flags,
            challenge,
            st.name
        ) in
        (
            {st with challenge = Some challenge;},
            (Fsm.mkstate _st_recv_challenge_rsp),
            None,
            [msgStatus; msgChallengeReq;]
        )

    let create_fsm version name cookie flags =
        Fsm.create
            {
                version   = version;
                name      = name;
                cookie    = cookie;
                flags     = flags;
                challenge = None;
            }
            (Fsm.mkstate _st_recv_name)

end (* module Handshake *)


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


type msg_control =
    | Msg_control_tick
    | Msg_control_p of
          Eterm.eterm        (* control message *)
        * Eterm.eterm option (* optional parameter *)
    | Msg_control_any of
          char   (* tag? *)
        * string (* raw binary data *)


let distr_version = 5



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
        | [< len = Tools.eint_n 4; stream >] ->
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
    parser [< data = Tools.string_n len >] ->
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


let control_message_to_chars msg = match msg with
    | Msg_control_tick ->
        []

let pack_control_msg msg =
    let chars = control_message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int len 4 in
    let r = Tools.implode (head @ chars) in
    Trace.printf "Packed control msg: %s\n"
        (Tools.dump_hex r "<<" ">>" " ");
    r


let _handshake_actions oc actions =
    List.iter
        (fun msg ->
            let bin = Handshake.pack msg in
            output_string oc bin
        )
        actions
    ;
    flush oc

let rec _handshake_loop fsm istream oc =
    let msg = try
        Handshake.message_of_stream istream
    with
    Stream.Failure ->
            failwith "Handshake stream failure"
    in
    Trace.debug (lazy (Trace.printf
        "Received handshake message: %s\n"
        (Handshake.message_to_string msg)
    ));
    match Fsm.send fsm msg with
        | (Some result, actions) ->
            _handshake_actions oc actions;
            Trace.flush ();
            result
        | (None, actions) ->
            _handshake_actions oc actions;
            Trace.flush ();
            _handshake_loop fsm istream oc

let _handshake st istream oc =
    let fsm = Handshake.create_fsm
        distr_version (*TODO value come from where? ... *)
        st.nodeName
        st.cookie
        st.flags
    in
    _handshake_loop fsm istream oc


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
            Thread.kill ticker; (* not implemented ... use a mutex on stop variable: the ticker check this variable and continue only if this variable is true; current thread can then set it to false whenever ... advantage is that it can be used for multiple thread in this connection *)
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

