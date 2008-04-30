module Mbox = struct

    type element =
        | Data of Eterm.t
        | Ctrl of int

    type t = {
        pid: Eterm.e_pid;
        queue: element Fifo.t;
        mutable name: string option;
        mutable activity: Thread.t option;
    }

    let ctrl_stop = Ctrl 0

    let create pid = {
        name = None;
        pid = pid;
        queue = Fifo.create ();
        activity = None;
    }

    let name mbox =
        mbox.name

    let _set_name self name =
        self.name <- Some name

    let _new_message mbox msg =
        (* no limit on number of message. *)
        Fifo.put mbox.queue (Data msg)

    let _receive mbox =
        Fifo.get mbox.queue

    let create_activity mbox recvCB =
        let rec recv_loop = fun () ->
            let elt = _receive mbox in
            match elt with
            | Data msg ->
                recvCB msg;
                recv_loop ()
            | ctrl_stop ->
                ()
        in
        let thr = Thread.create recv_loop () in
        mbox.activity <- Some thr

    let stop_activity mbox =
        let _ = Fifo.put mbox.queue ctrl_stop in
        Trace.dbg "Enode"
            "Mbox(%s) activity is stopping\n"
            (Eterm.e_pid_to_string mbox.pid)
        ;
        let _ = match mbox.activity with
        (* this may dead-lock ... *)
        | Some thr -> Thread.join thr;
        | None -> ()
        in
        Trace.dbg "Enode" "Mbox activity is stopped\n"

end (* module Mbox *)


module MboxManager : sig
    type t
    val create: unit -> t
    val mboxes: t -> Mbox.t list
    val make_mbox: t -> Eterm.e_pid -> Mbox.t
    val register: t -> Mbox.t -> string -> unit
    val unregister: t -> Mbox.t -> unit
    val find_by_name: t -> string -> Mbox.t
end = struct

    type t = {
        pidMboxMap: (Eterm.e_pid, Mbox.t) Hashtbl.t;
        nameMboxMap: (string, Mbox.t) Hashtbl.t;
    }

    let create () = {
        pidMboxMap = Hashtbl.create 10;
        nameMboxMap = Hashtbl.create 10;
    }

    let mboxes self =
        Hashtbl.fold
            (fun k v acc -> v::acc)
            self.pidMboxMap
            []

    let make_mbox self pid =
        let mbox = Mbox.create pid in
        Hashtbl.add self.pidMboxMap pid mbox;
        mbox

    let register self mbox name =
        let _ = Mbox._set_name mbox name in
        Hashtbl.add self.nameMboxMap name mbox

    let unregister self mbox =
        match Mbox.name mbox with
        | Some name ->
            Hashtbl.remove self.nameMboxMap name
        | _ ->
            ()

    let find_by_name self name =
        Hashtbl.find self.nameMboxMap name

end (* module MboxManager *)


module PidManager : sig
    type t
    val create: string -> t
    val init: t -> int -> bool
    val reset: t -> unit
    val is_initialized: t -> bool
    val make_pid: t -> Eterm.e_pid
end = struct

    type t = {
        node: string;
        mutable creation: int option;
        mutable serial: int;
        mutable count: int;
    }

    let create nodeName = {
        node = nodeName;
        creation = None;
        serial = 0;
        count = 0;
    }
    
    let init self creation =
        self.creation <- Some creation;
        self.serial <- 0;
        self.count <- 0;
        true

    let reset self =
        self.creation <- None
    
    let is_initialized self =
        match self.creation with
        | None -> false
        | Some _ -> true

    let _increment pids =
        pids.count <- pids.count + 1;
        if pids.count > 0x7fff then (
            pids.count <- 0;
            pids.serial <- pids.serial + 1;
            if pids.serial > 0x07 then pids.serial <- 0
        )

    let make_pid self =
        match self.creation with
        | None ->
            failwith "cannot create pid: creation ID not defined"
        | Some n ->
            let pid = (self.node, self.count, self.serial, n) in
            let _ = _increment self in
            pid

end (* module PidManager *)



type t = {
    name: string;
    cookie: string;
    epmc: Epmc.t;
    connections: Econn.t;
    pids: PidManager.t;
    mboxes: MboxManager.t;
}


(* MBoxes. *)

let create_mbox node =
    let pid = PidManager.make_pid node.pids in
    let mbox = MboxManager.make_mbox node.mboxes pid in
    mbox

let register_mbox node mbox name =
    MboxManager.register node.mboxes mbox name


(* Node in/out messages dispatch. *)

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

let _send_to_name node name msg =
    try
        let mbox = MboxManager.find_by_name node.mboxes name in
        Mbox._new_message mbox msg
    with
        Not_found ->
            Trace.dbg "Enode"
                "failed to handle control message: registered name not found: %s\n"
                name

let _receive node ectrl arg =
    match ectrl, arg with
    | (Eterm.ET_tuple [|
        Eterm.ET_int tag_reg_send;
        _;
        _;
        Eterm.ET_atom name;
    |], Some msg) ->
        _send_to_name node name msg
    | _ ->
        Trace.dbg "Enode"
            "not implemented: ignore control message: %s\n"
            (Eterm.to_string ectrl)

let send node dest msg =
    match dest with
    | Eterm.ET_pid _ ->
        let ctrl = Eterm.ET_tuple [|
            Eterm.ET_int tag_send;
            Eterm.ET_atom ""; (* TODO cookie ... *)
            dest;
        |] in
        let arg = Some msg in
        let name = Eterm.et_pid_node_name dest in
        Econn.send node.connections name ctrl arg
    | _ ->
        failwith "Enode.send: dest is not valid"



(* Internal node state. *)

let _create_net_kernel node =
    let mbox = create_mbox node in
    let _ = register_mbox node mbox "net_kernel" in
    let recvCB = fun msg ->
        match msg with
            | Eterm.ET_tuple [|
                Eterm.ET_atom "$gen_call";
                Eterm.ET_tuple [| toPid; ref; |];
                Eterm.ET_tuple [| Eterm.ET_atom "is_auth"; _; |]
            |] ->
                let rsp = Eterm.ET_tuple [|ref; Eterm.ET_atom "yes"|] in
                send node toPid rsp    
            | _ ->
                (* TODO kernel probably do other things *)
                let s = Eterm.to_string msg in
                failwith ("unexpected msg: " ^ s)
    in
    Mbox.create_activity mbox recvCB

let _is_published node =
    PidManager.is_initialized node.pids

let _publish node =
    if _is_published node
    then
        failwith "node already published"
    else
        let port = Econn.listen_port node.connections in
        match Epmc.connect node.epmc node.name port with
        | Some creation ->
            PidManager.init node.pids creation
        | None ->
            failwith "failed to publish node"
    
let _unpublish node =
    let _ = Epmc.disconnect node.epmc in
    PidManager.reset node.pids


(* Construction and connectivity. *)

let create ?(cookie="") nodeName =
    let name =
        if String.contains nodeName '@'
        then nodeName
        else String.concat "@" [nodeName; Unix.gethostname ();]
    in
    Trace.inf "Enode" "Making node '%s'\n" name;
    let epmc = Epmc.create () in
    let connections = Econn.create () in
    let pids = PidManager.create name in
    let mboxes = MboxManager.create () in
    {
        name = name;
        cookie = cookie;
        epmc = epmc;
        connections = connections;
        pids = pids;
        mboxes = mboxes;
    }

let start node =
    let _ = _publish node in (* may fail *)
    let _ = _create_net_kernel node in
    Econn.start node.connections node.name node.cookie (_receive node)

let stop node =
    Trace.dbg "Enode" "Node '%s' is stopping\n" node.name;
    let mboxes = MboxManager.mboxes node.mboxes in
    let _ = List.iter Mbox.stop_activity mboxes in
    let _ = List.iter
        (MboxManager.unregister node.mboxes)
        mboxes
    in
    Econn.stop node.connections;
    _unpublish node;
    Trace.flush ();
    ()
