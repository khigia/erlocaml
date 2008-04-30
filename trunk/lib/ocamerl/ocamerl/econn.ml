module Packing = struct
    
    let tag_pass_through = 'p' (* 112 *)

    type msg =
        | Msg_tick
        | Msg_p of
              Eterm.t        (* control message *)
            * Eterm.t option (* optional parameter *)

    let message_to_string msg = match msg with
        | Msg_tick               -> "Msg_tick"
        | Msg_p (ctrl, None)     -> Printf.sprintf "Msg_p(%s)" (Eterm.to_string ctrl)
        | Msg_p (ctrl, Some arg) -> Printf.sprintf "Msg_p(%s, %s)" (Eterm.to_string ctrl) (Eterm.to_string arg)

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
                    parser [< 'tag; msg = _tag_parse (len - 1) tag >] ->
                        msg
                    end
            in
            p stream
    and _tag_parse len tag =
        match tag with
            | t when t = tag_pass_through -> _parse_p len
            | _ -> failwith "unrecognize control message tag"
    and _parse_p len =
        parser [< data = Tools.string_n len; >] ->
            let s = Stream.of_string data in
            let ctrl = Eterm.of_stream s in
            _parse_p_arg ctrl s
    and _parse_p_arg ctrl = parser
        | [< arg = Eterm.of_stream; >] ->
            Msg_p (ctrl, Some arg)
        | [< >] ->
            Msg_p (ctrl, None)

    let _message_to_chars msg = match msg with
        | Msg_tick ->
            []
        | Msg_p (ctrl, None) ->
            tag_pass_through
            :: (Eterm.to_chars ctrl)
        | Msg_p (ctrl, Some msg) ->
            tag_pass_through
            :: (Eterm.to_chars ctrl)
            @  (Eterm.to_chars msg)

    let pack msg =
        let chars = _message_to_chars msg in
        let len = List.length chars in
        let head = Tools.chars_of_int len 4 in
        let r = Tools.implode (head @ chars) in
        r

end (* module Packing *)


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

    let tickData = Data (Packing.pack Packing.Msg_tick)

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
        | Some ticker -> Ticker.update_activity ticker
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

    let send conn ctrl arg =
        let msg = Packing.Msg_p (ctrl, arg) in
        Trace.dbg "Econn"
            "Sending control message: %s\n"
            (Packing.message_to_string msg);
        Trace.flush ();
        let bin = Packing.pack msg in
        Sender.send conn.sender bin

end (* module Connection *)


module ConnManager = struct

    type t = (string, Connection.t) Hashtbl.t

    let create () =
        Hashtbl.create 10

    let connection_up self peerName conn =
        Hashtbl.add self peerName conn

    let get self peerName =
        Hashtbl.find self peerName

end (* module ConnManager *)


module Server = struct
    (* Incoming connection handler. *)

    type t = {
        addr: Unix.inet_addr;
        port: int;
        sock: Unix.file_descr;
        mutable thread: Thread.t option;
    }

    type handler_state_t = {
        nodeName: string;
        cookie: string;
        flags: Int32.t;
        tickTime: float;
        connectionUpCB: string -> Connection.t -> unit;
        controlCB: Eterm.t -> Eterm.t option -> unit;
    }

    
    let distr_version = 5


    let _handshake st istream sender =
        let _get_message istream =
            let msg = try
                Handshake.message_of_stream istream
            with
                Stream.Failure ->
                    failwith "Handshake stream failure"
            in
            Trace.dbg "Econn"
                "Received handshake message: %s\n"
                (Handshake.message_to_string msg);
            msg
        in
        let _do_actions sender actions = List.iter
            (fun msg ->
                let bin = Handshake.pack msg in
                Sender.send sender bin
            )
            actions
        in
        let rec _loop fsm istream sender =
            let msg = _get_message istream in
            match Fsm.send fsm msg with
            | Fsm.Reply(Some result, actions) ->
                _do_actions sender actions;
                Trace.flush ();
                result
            | Fsm.Reply(None, actions) ->
                _do_actions sender actions;
                Trace.flush ();
                _loop fsm istream sender
            | Fsm.Finish ->
                Trace.flush ();
                (false, None)
        in
        let fsm = Handshake.create_fsm
            distr_version
            st.nodeName
            st.cookie
            st.flags
        in
        _loop fsm istream sender

    let rec _control state istream sender =
        let msg = try
            Packing.message_of_stream istream
        with
            Stream.Failure ->
                failwith "Control stream failure"
        in
        let _ = match msg with
        | Packing.Msg_tick ->
            Trace.dbg "Econn" "Received tick control message\n"
            (* TODO check that peer continue to tick
            and else set connection down *)
        | Packing.Msg_p (ectrl, arg) ->
            Trace.dbg "Econn"
                "Received control message: %s\n"
                (Packing.message_to_string msg)
            ;
            state.controlCB ectrl arg
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
        let _senderThread = Sender.start sender in
        (* first: do the handshake *)
        begin
        match _handshake state istream sender with
        | (true, Some peerName) ->
            Trace.dbg "Econn" "Handshake OK\n";
            (* register the connection *)
            state.connectionUpCB
                peerName
                (Connection.create sender)
            ;
            (* thread to tick the peer *)
            ignore(Sender.start_ticker sender state.tickTime);
            (* current thread receive from peer *)
            let _ = try
                _control state istream sender
            with
                exn ->
                    Trace.err "Econn"
                        "error in control loop: %s\n"
                        (Printexc.to_string exn)
            in
            (*TODO register connection down *)
            Sender.stop sender
        | _ ->
            Trace.inf "Econn" "Close connection comming from unauthorized node\n"
        end;
        Trace.dbg "Econn" "End of connection\n"
        (*Unix.close fd*)

    let port self =
        self.port

    let create () =
        let sock = Serv.listen 0 in
        let addr, port = Serv.inet_addr sock in
        Trace.dbg "Econn"
            "Node server listening on port %i\n"
            port
        ;
        {
            addr = addr;
            port = port;
            sock = sock;
            thread = None;
        }

    let start self name cookie connUpCB controlCB =
        let handler controlCB =
            Serv.handle_in_thread ( Serv.trace_handler ( Serv.make_handler (
                _handler {
                        flags = (Int32.logor 4l 256l); (* TODO set correct flags *)
                        cookie = cookie;
                        nodeName = name;
                        tickTime = 10.0; (*TODO which value? *)
                        connectionUpCB = connUpCB;
                        controlCB = controlCB;
                }
            )))
        in
        let server controlCB =
            try
                Serv.accept_loop 0 self.sock (handler controlCB)
            with
                exn ->
                    Trace.inf "Econn" "Exception in server (may be stopping)\n"
        in
        let thr = Thread.create server controlCB in
        self.thread <- Some thr

    let stop self = 
        let _ = Unix.shutdown self.sock Unix.SHUTDOWN_ALL in
        match self.thread with
        | Some thr ->
            Thread.join thr;
            self.thread <- None
        | None ->
            ()

end (* module Server *)


type t = {
    server: Server.t;
    connections: ConnManager.t;
}


let listen_port self = Server.port self.server

let send self name ctrl arg =
    try
        let conn = ConnManager.get self.connections name in
        Connection.send conn ctrl arg
    with
        Not_found ->
            (* TODO try to establish the connection *)
            Trace.dbg "Econn" "Cannot send message: connection not found";
            failwith "Cannot send message: connection not found"

let create () = {
    server = Server.create ();
    connections = ConnManager.create ();
}

let start self name cookie controlCB =
    let connectionUpCB = ConnManager.connection_up self.connections in
    Server.start
        self.server
        name
        cookie
        connectionUpCB
        controlCB

let stop self =
    Trace.dbg "Econn" "Node connections are stopping\n";
    Server.stop self.server;
    Trace.todo "Econn" "connections must be stop/unregistered\n";
    Trace.inf "Econn" "Node server stopped\n"
