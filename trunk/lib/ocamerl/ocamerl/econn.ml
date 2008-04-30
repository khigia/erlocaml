

module Control = struct
    (* TODO rename Control to ... what? Packing? *)
    
    let tag_pass_through = 'p' (* 112 *)

    type msg =
        | Msg_tick
        | Msg_p of
              Eterm.t        (* control message *)
            * Eterm.t option (* optional parameter *)
        | Msg_any of (*TODO is this really used??? *)
              char   (* tag? *)
            * string (* raw binary data *)

    let message_to_string msg = match msg with
        | Msg_tick               -> "Msg_tick"
        | Msg_p (ctrl, None)     -> Printf.sprintf "Msg_p(%s)" (Eterm.to_string ctrl)
        | Msg_p (ctrl, Some arg) -> Printf.sprintf "Msg_p(%s, %s)" (Eterm.to_string ctrl) (Eterm.to_string arg)
        | Msg_any (tag, data)    -> Printf.sprintf "Msg_any(%c, %s)" tag data

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
            | t when t = tag_pass_through -> parse_p len
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
            tag_pass_through
            :: (Eterm.to_chars ctrl)
        | Msg_p (ctrl, Some msg) ->
            tag_pass_through
            :: (Eterm.to_chars ctrl)
            @  (Eterm.to_chars msg)
        | _ ->
            failwith ("not implemented: cannot encode message: " ^ (message_to_string msg))

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


let tag_link           = 1l  (* {1, FromPid, ToPid}                          *)
let tag_send           = 2l  (* {2, Cookie, ToPid}                 + message *)
let tag_exit           = 3l  (* {3, FromPid, ToPid, Reason}                  *)
let tag_unlink         = 4l  (* {4, FromPid, ToPid}                          *)
let tag_node_link      = 5l  (* {5}                                          *)
let tag_reg_send       = 6l  (* {6, FromPid, Cookie, ToName}       + message *)
let tag_group_leader   = 7l  (* {7, FromPid, ToPid}                          *)
let tag_exit2          = 8l  (* {8, FromPid, ToPid, Reason}                  *)
let tag_send_tt        = 12l (* {12, Cookie, ToPid, TraceToken}    + message *)
let tag_exit_tt        = 13l (* {13, FromPid, ToPid, TraceToken, Reason}     *) 
let tag_reg_send_tt    = 16l (* {16, FromPid, Cookie, ToName, TraceToken}    *)
let tag_exit2_tt       = 18l (* {18, FromPid, ToPid, TraceToken, Reason}     *)
(* shall not be used (only for Erlang node, not hidden node) *)
let tag_monitor_p      = 19l
let tag_demonitor_p    = 20l
let tag_monitor_p_exit = 21l


module Connection = struct

    type t = {
        sender: Sender.t;
    }

    let create sender = {
        sender = sender;
    }

    let send_to_pid conn toPid msg =
        let dest = Eterm.ET_tuple [|
            Eterm.ET_int tag_send;
            Eterm.ET_atom ""; (* TODO cookie ... *)
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


module ConnManager = struct

    type t = (string, Connection.t) Hashtbl.t

    let create () =
        Hashtbl.create 10

    let connection_up self peerName conn =
        Hashtbl.add self peerName conn

    let get self peerName =
        Hashtbl.find self peerName

end (* module ConnManager *)


type t = {
    (* incoming connection (server) *)
    addr: Unix.inet_addr;
    port: int;
    sock: Unix.file_descr;
    loop: connCB -> inMsgCB -> unit; (* TODO remove! *)
    mutable thread: Thread.t option;
    (* alive connections *)
    connections: ConnManager.t;
}
and handler_state = {
    (*TODO most of this is node properties ... *)
    nodeName: string;
    cookie: string;
    flags: Int32.t;
    tickTime: float;
    connectionUpCB: connCB; (*TODO can disapear ... be replace by direct call to the ConnManager *)
    incomingMessageCB: inMsgCB;
}
and connCB = string -> Connection.t -> unit
and inMsgCB = Eterm.t -> Eterm.t -> unit


let distr_version = 5


let listen_port self = self.port

let send_to_pid self toPid msg =
    let peerName = Eterm.et_pid_node_name toPid in
    try
        let conn = ConnManager.get self.connections peerName in
        Connection.send_to_pid conn toPid msg
    with
        Not_found ->
            (* TODO try to establish the connection *)
            failwith "Cannot send message to pid: connection not found"

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
            (match ectrl, arg with
            | (Eterm.ET_tuple [|
                Eterm.ET_int tag_reg_send;
                _;
                _;
                dest;
            |], Some a) ->
                begin
                try
                    state.incomingMessageCB dest a
                with
                    exn ->
                        Trace.dbg "Econn"
                            "Incoming message not handled: %s\n"
                            (Printexc.to_string exn)
                end
            | _ ->
                Trace.dbg "Econn"
                    "not implemented: ignore control message: %s\n"
                    (Control.message_to_string msg)
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
    let _senderThread = Sender.start sender in
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
            (*TODO register connection down in node *)
            Sender.stop sender;
            Trace.dbg "Econn" "End of connection\n";
            Trace.flush ();
        | _ ->
            Trace.inf "Econn" "Close connection comming from unauthorized node\n";
            Unix.close fd


let create nodeName cookie =
    (* TODO server shall be in some sub module *)
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
                    cookie = cookie;
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
        loop = server; (* TODO no need to keep in record! *)
        thread = None;
        connections = ConnManager.create ();
    }

let start self incomingMessageCB =
    let connectionUpCB = ConnManager.connection_up self.connections in
    let thr = Thread.create
        (fun () -> self.loop connectionUpCB incomingMessageCB)
        ()
    in
    self.thread <- Some thr

let stop server =
    Trace.dbg "Econn" "Node server is stopping\n";
    Trace.todo "Econn" "connections must be stop/unregistered\n";
    let _ = Unix.shutdown server.sock Unix.SHUTDOWN_ALL in
    let _ = match server.thread with
        | Some thr -> Thread.join thr
        | None -> ()
    in
    Trace.inf "Econn" "Node server stopped\n"
