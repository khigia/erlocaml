module Mbox = struct
    (* may use the object system to have a generic Mbox
    and inherit:
        a threaded Mbox
        a reactive Mbox (on message input ... suck messaging thread!)
    ARGH! all should be event-based!
    *)
    
    type t = {
        pid: Eterm.e_pid;
        queue: queueElement Fifo.t;
        mutable activity: Thread.t option;
    }
    and queueElement =
        | Data of Eterm.eterm
        | Ctrl of int

    let ctrl_stop = Ctrl 0

    let create pid = {
        pid = pid;
        queue = Fifo.create ();
        activity = None;
    }

    let push_message mbox msg =
        (* TODO Fifo need to implement some maximum size?! *)
        Fifo.put mbox.queue (Data msg)

    let receive mbox =
        Fifo.get mbox.queue

    let create_activity mbox recvCB =
        let rec recv_loop = fun () ->
            let elt = receive mbox in
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


module PidManager = struct

    type t = {
        mutable creation: int option;
        mutable serial: int;
        mutable count: int;
    }

    let create = {
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

    let make_pid self nodeName =
        match self.creation with
            | None ->
                failwith "cannot create pid: creation ID not defined"
            | Some n ->
                let pid = Eterm.make_e_pid
                    nodeName
                    self.count
                    self.serial
                    n
                in
                let _ = _increment self in
                pid

end (* module PidManager *)


module MboxManager = struct

    type t = {
        pidMboxMap: (Eterm.e_pid, Mbox.t) Hashtbl.t;
        nameMboxMap: (string, Mbox.t) Hashtbl.t;
    }

    let create = {
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

    let register self name mbox =
        Hashtbl.add self.nameMboxMap name mbox

    let find self name =
        try
            let rsp = Hashtbl.find self.nameMboxMap name in
            Some rsp
        with
            Not_found ->
                None

end (* module MboxManager *)



type node = {
    name: string;
    mutable epmc: Epmc.t; (* TODO mutable??? *)
    server: Econn.t;
    pids: PidManager.t;
    mboxes: MboxManager.t;
    connections: (string, Econn.Connection.t) Hashtbl.t;
}


(* node management *)

let _trace_pids indent pids =
    let indent2 = indent ^ indent in
    Printf.printf
        "%sPID manager:
%screation: %s
%spid_count: %i
%sserial: %i
"
        indent
        indent2 (match pids.PidManager.creation with None -> "None" | Some n -> Printf.sprintf "Some %i" n)
        indent2 pids.PidManager.count
        indent2 pids.PidManager.serial

let _trace_mboxes indent mboxes =
    let indent2 = indent ^ indent in
    Printf.printf
        "%sMBoxes manager:
%spidMboxMap length: %i
%snameMboxMap length: %i
"
        indent
        indent2 (Hashtbl.length mboxes.MboxManager.pidMboxMap)
        indent2 (Hashtbl.length mboxes.MboxManager.nameMboxMap)

let trace indent node =
    Trace.inf "Enode"
        "Node:
%sname: %s
"
        indent node.name;
    _trace_pids indent node.pids;
    _trace_mboxes indent node.mboxes


let create_mbox node =
    let pid = PidManager.make_pid node.pids node.name in
    let mbox = MboxManager.make_mbox node.mboxes pid in
    mbox

let register_mbox node mbox name =
    (*TODO mbox embed its name *)
    MboxManager.register node.mboxes name mbox

let find_mbox node name =
    MboxManager.find node.mboxes name

(* TODO connection manager *)
let _connection_up node peerName conn =
    Hashtbl.add node.connections peerName conn

let _get_connection node peerName =
    try
        let conn = Hashtbl.find node.connections peerName in
        Some conn
    with
        Not_found -> None


let send node toPid msg =
    let peerName = Eterm.et_pid_node_name toPid in
    match _get_connection node peerName with
        | Some conn ->
            Econn.Connection.send conn toPid msg
        | _ ->
            (*TODO clear error management *)
            failwith "Cannot send message to node"

let _incoming_message node dest msg =
    (* dispatch message to the appropriate mbox *)
    match dest with
        | Eterm.ET_atom name ->
            match find_mbox node name with
                | Some mbox ->
                    Mbox.push_message mbox msg
                | _ ->
                    failwith 
                        (Printf.sprintf
                            "dest not found (%s)"
                            name
                        )
        (* TODO probably other like pid ... or at least a clean fail*)

let _create_net_kernel node =
    let mbox = create_mbox node in
    let _ = register_mbox node mbox "net_kernel" in
    let recvCB = fun msg ->
        match msg with
            | Eterm.ET_tuple arg ->
                (* TODO could we have a better interface to match tuple and all eterm? *)
                match arg.(0) with
                    | Eterm.ET_atom "$gen_call" ->
                        let arg1 = match arg.(1) with Eterm.ET_tuple c -> c in
                        let arg2 = match arg.(2) with Eterm.ET_tuple c -> c in
                        (match arg2.(0) with
                            | Eterm.ET_atom "is_auth" ->
                                let toPid = arg1.(0) in
                                let ref = arg1.(1) in
                                let rsp = Eterm.ET_tuple (Eterm.ETuple.make [ref; Eterm.ET_atom "yes"]) in
                                send node toPid rsp
                        )
            (* TODO failing cases ... *)
    in
    Mbox.create_activity mbox recvCB

let is_published node =
    PidManager.is_initialized node.pids

let publish node =
    (* TODO check not already published! *)
    (* TODO should this function fail if publication fails? *)
    match Epmc.connect node.epmc with
        | Some creation ->
            PidManager.init node.pids creation
        | None ->
            false
    
let unpublish node =
    let _ = Epmc.disconnect node.epmc in
    PidManager.reset node.pids
    
let create nodeName =
    Trace.inf "Enode" "Making node '%s'\n" nodeName;
    let server = Econn.create nodeName in
    let epmc = Epmc.create nodeName server.Econn.port in
    let pids = PidManager.create in
    let mboxes = MboxManager.create in
    {
        name = nodeName;
        epmc = epmc;
        server = server;
        pids = pids;
        mboxes = mboxes;
        connections = Hashtbl.create 10;
    }

let local_name base =
    String.concat "@" ["ocaml"; Unix.gethostname ();]

let create_local name =
    create (local_name name)

let start node =
    let _ = publish node in
    let _ = _create_net_kernel node in
    Econn.start
        node.server
        (_connection_up node)
        (_incoming_message node)

let stop node =
    Trace.dbg "Enode" "Node '%s' is stopping\n" node.name;
    trace "    " node;
    let mboxes = MboxManager.mboxes node.mboxes in
    let _ = List.iter Mbox.stop_activity mboxes in
    Trace.todo "Enode" "mboxes must be unregistered\n";
    Econn.stop node.server;
    unpublish node;
    trace "    " node;
    Trace.flush ();
    ()
