module Sender = struct

    type t = {
        ochannel: out_channel;
        queue: string Fifo.t;
    }

    let create oc =
        {
            ochannel = oc;
            queue = Fifo.create ();
        }

    let send sender data =
        Fifo.put sender.queue data

    let run sender =
        let rec _loop () =
            let data = Fifo.get sender.queue in
            output_string sender.ochannel data;
            flush sender.ochannel;
            _loop ()
        in
        Thread.create _loop ()

end (* module Sender *)


module Ticker = struct
    
    type t = {
        tickTime: float;
        tickCB: unit -> unit;
        actLock: Mutex.t;
        mutable lastActivity: time;
    }
    and time = float

    let now () = Unix.time ()

    let create tickTime tickCB = {
        tickTime = tickTime;
        tickCB = tickCB;
        actLock = Mutex.create ();
        lastActivity = now ();
    }
    
    let update_activity ticker =
        Mutex.lock ticker.actLock;
        ticker.lastActivity <- now ();
        Mutex.unlock ticker.actLock;
        Trace.printf
            "Ticker: last activity: %f\n"
            ticker.lastActivity

    let run ticker =
        let rec _loop delay =
            Thread.delay delay;
            let since = (now ()) -. ticker.lastActivity in
            let delta = since -. ticker.tickTime in
            match delta >= 0.0 with
                | true ->
                    Trace.printf "Ticker: %f is time to tick\n" (now ());
                    ticker.tickCB ();
                    update_activity ticker;
                    Trace.flush ();
                    _loop ticker.tickTime
                | _ ->
                    _loop delta
        in
        Thread.create _loop ticker.tickTime

end (* module Ticker *)


module Handshake = struct
    (* Only handshake application logic (no network code) *)

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
        localNode: node;
        cookie:    string;
        challenge: Int32.t option;
        peerNode:  node option;
    }
    and node = {
        version:   int;
        name:      string;
        flags:     Int32.t;
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
                    let peerNode = match st.peerNode with
                        | Some node -> node
                    in
                    (
                        st,
                        None, (* end of FSM *)
                        Some (true, Some peerNode.name), (* handshake finished and ok *)
                        [reply;]
                    )
                | false ->
                    Trace.debug (lazy (Trace.printf
                        "Handshake failed: peer digest NOT ok\n"
                    ));
                    (* handshake failing here! *)
                    (st, None, Some (false, None), [])
        | _ ->
            Trace.debug (lazy (Trace.printf
                "Handshake failed: got wrong message: %s\n"
                (message_to_string msg)
            ));
            (* handshake failing here! *)
            (st, None, Some (false, None), [])

    let _st_recv_name st msg =
        let challenge = _create_challenge () in
        let newSt = match msg with
            | Msg_recv_name (
                peerDistVsn,
                peerFlags,
                peerName
            ) ->
                {st with
                    challenge = Some challenge;
                    peerNode = Some {
                        version = peerDistVsn;
                        name    = peerName;
                        flags   = peerFlags;
                    };
                }
        in
        let msgStatus = Msg_status "ok" in
        let msgChallengeReq = Msg_challenge_req (
            st.localNode.version,
            st.localNode.flags,
            challenge,
            st.localNode.name
        ) in
        (
            newSt,
            (Fsm.mkstate _st_recv_challenge_rsp),
            None,
            [msgStatus; msgChallengeReq;]
        )

    let create_fsm version name cookie flags =
        (* TODO this is accepting fsm, need the fsm to handle
        case where node begin the handshake with remote node.
        *)
        Fsm.create
            {
                localNode = {
                    version     = version;
                    name        = name;
                    flags       = flags;
                };
                cookie    = cookie;
                challenge = None;
                peerNode  = None;
            }
            (Fsm.mkstate _st_recv_name)

end (* module Handshake *)


module Control = struct
    
    let tag_control = 'p'

    type msg =
        | Msg_tick
        | Msg_p of
              Eterm.eterm        (* control message *)
            * Eterm.eterm option (* optional parameter *)
        | Msg_any of
              char   (* tag? *)
            * string (* raw binary data *)

    let message_to_string msg = match msg with
        | Msg_tick ->
            "Msg_tick"
        | Msg_p (ctrl, None) ->
            Printf.sprintf
                "Msg_p(%s)"
                (Eterm.to_string ctrl)
        | Msg_p (ctrl, Some arg) ->
            Printf.sprintf
                "Msg_p(%s, %s)"
                (Eterm.to_string ctrl)
                (Eterm.to_string arg)
        | Msg_any (tag, data) ->
            Printf.sprintf
                "Msg_any(%c, %s)"
                tag
                data

    let rec message_of_stream = parser
        | [< len = Tools.eint_n 4; stream >] ->
            let p = match len with
                | 0 ->
                    begin
                    parser [< >] ->
                        Msg_tick
                    end
                | _ ->
                    begin
                    parser [< 'tag; msg = tag_parse (len - 1) tag >] ->
                        msg
                    end
            in
            p stream
    and tag_parse len tag =
        match tag with
            | tag_control -> parse_p len
            | _ -> parse_any tag len
    and parse_p len =
        parser [< data = Tools.string_n len; >] ->
            let s = Stream.of_string data in
            let ctrl = Eterm.of_stream s in
            parse_p_arg ctrl s
    and parse_p_arg ctrl = parser
        | [< arg = Eterm.of_stream; >] ->
            Msg_p (ctrl, Some arg)
        | [< >] ->
            Msg_p (ctrl, None)
    and parse_any tag len =
        parser [< data = Tools.string_n len >] ->
            Msg_any (tag, data)


    let _message_to_chars msg = match msg with
        | Msg_tick ->
            []
        | Msg_p (ctrl, None) ->
            tag_control
            :: (Tools.explode (Eterm.to_binary ctrl)) (* TODO chars list to string to list ... *)
        | Msg_p (ctrl, Some msg) ->
            tag_control
            :: (Tools.explode (Eterm.to_binary ctrl)) (* TODO chars list to string to list ... *)
            @  (Tools.explode (Eterm.to_binary msg)) (* TODO chars list to string to list ... *)

    let pack msg =
        let chars = _message_to_chars msg in
        let len = List.length chars in
        let head = Tools.chars_of_int len 4 in
        let r = Tools.implode (head @ chars) in
        Trace.printf "Packed control msg: %s\n"
            (Tools.dump_hex r "<<" ">>" " ");
        r

end (* module Control *)

(* TODO
    - module Server (node server)
    - module Client (node client)
*)



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
    controlCB: (ctrlmsg -> ctrlmsg list);
}
and ctrlmsg =
    Eterm.eterm
    * Eterm.eterm option

let distr_version = 5


let _handshake st istream sender =
    let _do_actions sender actions = List.iter
        (fun msg ->
            let bin = Handshake.pack msg in
            Sender.send sender bin
        )
        actions
    in
    let rec _loop fsm istream sender =
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
                _do_actions sender actions;
                Trace.flush ();
                result
            | (None, actions) ->
                _do_actions sender actions;
                Trace.flush ();
                _loop fsm istream sender
    in
    let fsm = Handshake.create_fsm
        distr_version (*TODO value come from where? ... *)
        st.nodeName
        st.cookie
        st.flags
    in
    _loop fsm istream sender


let rec _control state istream ticker sender =
    let msg = try
        Control.message_of_stream istream
    with
        Stream.Failure ->
            let data = Stream.npeek 256 istream in
            let buf = Tools.implode data in
            Trace.printf
                "Dump of stream: %s\n"
                (Tools.dump_hex buf "<<" ">>" ",")
            ;
            Trace.flush ();
            failwith "Control message stream failure"
    in
    let _ = match msg with
        | Control.Msg_tick ->
            Trace.debug (lazy (Trace.printf
                "Received tick control message\n"
            ))
            (* TODO check that peer continue to tick
            and else set connection down *)
        | Control.Msg_p (ctrl, arg) ->
            Ticker.update_activity ticker;
            Trace.debug (lazy (Trace.printf
                "Received control message: %s\n"
                (Control.message_to_string msg)
            ));
            let resps = state.controlCB (ctrl, arg) in
            List.iter
                (fun (c, a) ->
                    let bin = Control.pack (Control.Msg_p (c, a)) in
                    Sender.send sender bin
                )
                resps
            ;
        | Control.Msg_any (tag, data) ->
            Trace.debug (lazy (Trace.printf
                "Ignoring unknow control message\n"
            ))
    in
    Trace.flush ();
    _control state istream ticker sender


let _handler state id fd =
    Random.self_init ();
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    (* one thread responsible to send data to peer *)
    let sender = Sender.create oc in
    let senderThread = Sender.run sender in
    (* first: do the handshake *)
    let handshake = _handshake state istream sender in
    match handshake with
        | (true, Some peerName) ->
            Trace.debug (lazy (Trace.printf
                "Handshake OK\n"
            ));
            (*TODO register the connection
            _connection_up state peerName;
            *)
            (* one thread to tick the peer *)
            let tick = Control.pack Control.Msg_tick in
            let tickCB = fun () -> Sender.send sender tick in
            let ticker = Ticker.create state.tickTime tickCB in
            Ticker.run ticker;
            (* current thread receive from peer *)
            let _ = try
                _control state istream ticker sender;
                Trace.printf "(normal?) end of control\n"
            with
                exn ->
                    Trace.printf
                        "ERROR in control loop: %s\n"
                        (Printexc.to_string exn)
            in
            (*TODO join ticker, sender, ... all threads
            Ticker.stop ticker;
            Sender.stop sender;
            *)
            Trace.debug (lazy (Trace.printf
                "End of connection\n"
            ));
            Trace.flush ();
        | _ ->
            Trace.info (lazy (Trace.printf
                "Close connection comming from unauthorized node\n"
            ));
            Unix.close fd


let create nodeName controlCB =
    let sock = Serv.listen 0 in
    let addr, port = Serv.inet_addr sock in
    Trace.debug (lazy (Trace.printf
        "Node server listening on port %i (node '%s')\n"
        port
        nodeName
    ));
    Trace.flush ();
    let handler =
        Serv.handle_in_thread ( Serv.trace_handler ( Serv.make_handler (
            _handler {
                    flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                    cookie = "cookie"; (*TODO cookie as node option! *)
                    nodeName = nodeName;
                    tickTime = 30.0; (*TODO which value? *)
                    controlCB = controlCB;
            }
        )))
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

