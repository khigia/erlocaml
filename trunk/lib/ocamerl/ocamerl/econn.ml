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
        Trace.dbg "Econn"
            "Packed handshake msg: %s\n"
            (Tools.dump_hex r "<<" ">>" " ")
        ;
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
                    Trace.dbg "Econn" "Peer digest OK\n";
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
                    Trace.dbg "Econn" "Handshake failed: peer digest NOT ok\n";
                    (* handshake failing here! *)
                    (st, None, Some (false, None), [])
        | _ ->
            Trace.dbg "Econn"
                "Handshake failed: got wrong message: %s\n"
                (message_to_string msg)
            ;
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
        r

end (* module Control *)


module Ticker = struct
    
    type t = {
        tickTime: float;
        mutable tickCB: (unit -> unit) option;
        actLock: Mutex.t;
        mutable lastActivity: time;
        stopLock: Mutex.t;
        mutable stop: bool;
    }
    and time = float

    let now () = Unix.time ()

    let create tickTime = {
        tickTime = tickTime;
        tickCB = None;
        actLock = Mutex.create ();
        lastActivity = now ();
        stopLock = Mutex.create ();
        stop = false;
    }
    
    let update_activity ticker =
        Mutex.lock ticker.actLock;
        ticker.lastActivity <- now ();
        Mutex.unlock ticker.actLock;
        Trace.dbg "Econn"
            "Ticker: last activity: %f\n"
            ticker.lastActivity

    let set_cb ticker cb =
        ticker.tickCB <- Some cb

    let is_stop ticker =
        Mutex.lock ticker.stopLock;
        let stop = ticker.stop in
        Mutex.unlock ticker.stopLock;
        stop

    let start ticker checkFreq =
        (* checkFreq enable to thread to wakes-up and check
        that it should continue to run or not  ... crap!*)
        let rec _loop delay =
            Thread.delay delay;
            match is_stop ticker with
                | false ->
                    let since = (now ()) -. ticker.lastActivity in
                    let delta = since -. ticker.tickTime in
                    ignore (match delta >= 0.0 with
                        | true ->
                            Trace.dbg "Econn" "Ticker: %f is time to tick\n" (now ());
                            let _ = match ticker.tickCB with
                                | Some f -> f ()
                                | None -> ()
                            in
                            update_activity ticker;
                            Trace.flush ();
                            (*_loop ticker.tickTime *)
                            _loop checkFreq
                        | _ ->
                            (*_loop delta*)
                            _loop checkFreq
                    )
                | _ ->
                    Trace.dbg "Econn" "Ticker is stopping\n";
                    Trace.flush ()
        in
        (*Thread.create _loop ticker.tickTime*)
        Thread.create _loop checkFreq

    let stop ticker =
        Mutex.lock ticker.stopLock;
        ticker.stop <- true;
        Mutex.unlock ticker.stopLock

end (* module Ticker *)


module Sender = struct

    type t = {
        ochannel: out_channel;
        queue: msg Fifo.t;
        mutable thread: Thread.t option;
        mutable ticker: Ticker.t option;
    }
    and msg =
          Data of string
        | Ctrl of int

    let tickData = Data (Control.pack Control.Msg_tick)

    let create oc =
        {
            ochannel = oc;
            queue = Fifo.create ();
            thread = None;
            ticker = None;
        }

    let tick sender =
        Trace.dbg "Econn" "Sender is ticking\n";
        Trace.flush ();
        Fifo.put sender.queue tickData

    let send sender data =
        let _ = Fifo.put sender.queue (Data data) in
        match sender.ticker with
            | Some ticker ->
                Ticker.update_activity ticker
            | _ -> ()

    let start sender =
        let rec _loop () =
            let msg = Fifo.get sender.queue in
            match msg with
                | Data data ->
                    Trace.dbg "Econn" "Sender sent one packet\n";
                    Trace.flush ();
                    output_string sender.ochannel data;
                    flush sender.ochannel;
                    _loop ()
                | Ctrl code ->
                    (* any control message stop the loop *)
                    Trace.dbg "Econn" "Sender is stopping\n"
        in
        let thr = Thread.create _loop () in
        sender.thread <- Some thr;
        ()

    let stop sender =
        let _ = match sender.ticker with
            | Some ticker ->
                Ticker.stop ticker
            | None ->
                ()
        in
        match sender.thread with
            | Some thr ->
                Fifo.put sender.queue (Ctrl 0);
                Thread.join thr
            | None ->
                ()


    let start_ticker sender tickTime =
        let ticker = Ticker.create tickTime in
        let tickCB = fun () -> tick sender in
        let _ = Ticker.set_cb ticker tickCB in
        sender.ticker <- Some ticker;
        Ticker.start ticker 1.0

end (* module Sender *)


module Connection = struct

    type t = {
        sender: Sender.t;
    }

    let create sender = {
        sender = sender;
    }

    let send conn toPid msg =
        let dest = Eterm.ET_tuple [|
            Eterm.ET_int 2l; (* TODO cste *)
            Eterm.ET_atom ""; (* cookie ... *)
            toPid;
        |] in
        let msg = Control.Msg_p (dest, Some msg) in
        Trace.dbg "Econn"
            "Sending control message: %s\n"
            (Control.message_to_string msg);
        Trace.flush ();
        let bin = Control.pack msg in
        Sender.send conn.sender bin

end (* module Connection *)


type t = {
    addr: Unix.inet_addr;
    port: int;
    sock: Unix.file_descr;
    loop: connCB -> inMsgCB -> unit;
    mutable thread: Thread.t option;
}
and handler_state = {
    nodeName: string;
    cookie: string;
    flags: Int32.t;
    tickTime: float;
    connectionUpCB: connCB;
    incomingMessageCB: inMsgCB;
}
and connCB = string -> Connection.t -> unit
and inMsgCB = Eterm.eterm -> Eterm.eterm -> unit

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
        Trace.dbg "Econn"
            "Received handshake message: %s\n"
            (Handshake.message_to_string msg)
        ;
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


let rec _control state istream sender =
    let msg = try
        Control.message_of_stream istream
    with
        Stream.Failure ->
            let data = Stream.npeek 256 istream in
            let buf = Tools.implode data in
            Trace.dbg "Econn"
                "Dump of stream: %s\n"
                (Tools.dump_hex buf "<<" ">>" ",")
            ;
            Trace.flush ();
            failwith "Control message stream failure"
    in
    let _ = match msg with
        | Control.Msg_tick ->
            Trace.dbg "Econn" "Received tick control message\n"
            (* TODO check that peer continue to tick
            and else set connection down *)
        | Control.Msg_p (ectrl, arg) ->
            Trace.dbg "Econn"
                "Received control message: %s\n"
                (Control.message_to_string msg)
            ;
            let c = match ectrl with Eterm.ET_tuple c -> c in
            (match c.(0) with
                | Eterm.ET_int 6l (*TODO cste*) ->
                    let dest = c.(3) in
                    let a = match arg with Some c -> c in
                    try
                        state.incomingMessageCB dest a
                    with
                        exn ->
                            Trace.dbg "Econn"
                                "Incoming message not handled: %s\n"
                                (Printexc.to_string exn)
                (*TODO lot of cases to handle *)
            )
        | Control.Msg_any (tag, data) ->
            Trace.dbg "Econn" "Ignoring unknow control message\n"
    in
    Trace.flush ();
    _control state istream sender


let _handler state id fd =
    Random.self_init ();
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    let istream = Stream.of_channel ic in
    (* sender is responsible of output connection *)
    let sender = Sender.create oc in
    let senderThread = Sender.start sender in
    (* first: do the handshake *)
    match _handshake state istream sender with
        | (true, Some peerName) ->
            Trace.dbg "Econn" "Handshake OK\n";
            (* register the connection in node *)
            state.connectionUpCB
                peerName
                (Connection.create sender)
            ;
            (* thread to tick the peer *)
            Sender.start_ticker sender state.tickTime;
            (* current thread receive from peer *)
            let _ = try
                _control state istream sender;
                Trace.dbg "Econn" "(normal?) end of control\n"
            with
                exn ->
                    Trace.err "Econn"
                        "error in control loop: %s\n"
                        (Printexc.to_string exn)
            in
            (*TODO register connection down in node *)
            Sender.stop sender;
            Trace.dbg "Econn" "End of connection\n";
            Trace.flush ();
        | _ ->
            Trace.inf "Econn" "Close connection comming from unauthorized node\n";
            Unix.close fd


let create nodeName =
    let sock = Serv.listen 0 in
    let addr, port = Serv.inet_addr sock in
    Trace.dbg "Econn"
        "Node server listening on port %i (node '%s')\n"
        port
        nodeName
    ;
    Trace.flush ();
    let handler connUpCB incomingMessageCB =
        Serv.handle_in_thread ( Serv.trace_handler ( Serv.make_handler (
            _handler {
                    flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                    cookie = "cookie"; (*TODO cookie as node option! *)
                    nodeName = nodeName;
                    tickTime = 10.0; (*TODO which value? *)
                    connectionUpCB = connUpCB;
                    incomingMessageCB = incomingMessageCB;
            }
        )))
    in
    let server connUpCB incomingMessageCB =
        try
            Serv.accept_loop 0 sock (handler connUpCB incomingMessageCB)
        with
            exn ->
                Trace.inf "Econn" "Exception in server (may be stopping)\n"
    in
    {
        addr = addr;
        port = port;
        sock = sock;
        loop = server;
        thread = None;
    }

let start server connectionUpCB incomingMessageCB =
    let thr = Thread.create
        (fun () -> server.loop connectionUpCB incomingMessageCB)
        ()
    in
    server.thread <- Some thr

let stop server =
    Trace.dbg "Econn" "Node server is stopping\n";
    Trace.todo "Econn" "connections must be stop/unregistered\n";
    let _ = Unix.shutdown server.sock Unix.SHUTDOWN_ALL in
    let _ = match server.thread with
        | Some thr -> Thread.join thr
        | None -> ()
    in
    Trace.inf "Econn" "Node server stopped\n"
