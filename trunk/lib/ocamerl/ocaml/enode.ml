module Mbox = struct
    
    type t = {
        pid: Eterm.e_pid;
        mutable recvCB: (Eterm.eterm -> unit) option; (* TODO no callback (it take thread of connection; probably each mbox has its own thread and message are store in fifo *)
    }

    let create pid = {
        pid = pid;
        recvCB = None;
    }

    let set_recv_cb mbox recvCB =
        mbox.recvCB <- recvCB

    let push_message mbox msg =
        (* TODO put message in some queue ...*)
        match mbox.recvCB with
            | Some cb -> cb msg
            | None -> () (* TODO arg! drop msg silently *)

end (* module Mbox *)


type node = {
    name: string;
    mutable epmc: Epmc.t;
    server: Econn.t;
    pids: pids;     (* PID management *)
    name_to_mbox: (string, Mbox.t) Hashtbl.t;
    connections: (string, Econn.Connection.t) Hashtbl.t;
}
and pids = {
    mutable creation: int option;
    mutable pid_count: int;
    mutable serial: int;
    pid_to_mbox: (Eterm.e_pid, Mbox.t) Hashtbl.t;
    mbox_to_pid: (Mbox.t, Eterm.e_pid) Hashtbl.t;
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
    let mbox = Mbox.create pid in
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

let create_mbox node =
    let pid = _create_pid node.pids node.name in
    let mbox = _create_mbox node.pids pid in
    mbox

let register_mbox node mbox name =
    Hashtbl.add node.name_to_mbox name mbox

let find_mbox node name =
    (* may raise Not_found *)
    Hashtbl.find node.name_to_mbox name

let _connection_up node peerName conn =
    Hashtbl.add node.connections peerName conn

let _get_connection node peerName =
    try
        let conn = Hashtbl.find node.connections peerName in
        Some conn
    with
        Not_found -> None

let send node toPid msg =
    let peerName = Eterm.pid_node_name toPid in
    match _get_connection node peerName with
        | Some conn ->
            Econn.Connection.send conn toPid msg
        | _ ->
            (*TODO clear error management *)
            failwith "Cannot send message to node"

let _incoming_message node dest msg =
    match dest with
        | Eterm.ET_atom name ->
            let mbox = find_mbox node name in
            Mbox.push_message mbox msg

let _create_net_kernel node =
    let mbox = create_mbox node in
    let recvCB iarg = (match iarg with
        | Eterm.ET_tuple arg ->
            match arg.(0) with
                | Eterm.ET_atom "$gen_call" ->
                    let arg1 = match arg.(1) with Eterm.ET_tuple c -> c in
                    let arg2 = match arg.(2) with Eterm.ET_tuple c -> c in
                    match arg2.(0) with
                        | Eterm.ET_atom "is_auth" ->
                            let toPid = arg1.(0) in
                            let ref = arg1.(1) in
                            let rsp = Eterm.ET_tuple (Eterm.ETuple.make [ref; Eterm.ET_atom "yes"]) in
                            send node toPid rsp
    ) in
    let _ = Mbox.set_recv_cb mbox (Some recvCB) in
    let _ = register_mbox node mbox "net_kernel" in
    ()

let is_published node =
    match node.pids.creation with
        | None -> false
        | Some _ -> true

let publish node =
    (* TODO check not already published! *)
    (* TODO should this function fail if publication fails? *)
    let result = Epmc.connect node.epmc in
    node.pids.creation <- result;
    ()
    
let unpublish node =
    Epmc.disconnect node.epmc
    
let make nodeName =
    Trace.info (lazy (Trace.printf 
        "Making node '%s'\n" 
        nodeName
    ));
    let server = Econn.create nodeName in
    let epmc = Epmc.make nodeName server.Econn.port in
    let pids = _create_pid_manager 0 0 None in
    let node = {
        name = nodeName;
        epmc = epmc;
        server = server;
        pids = pids;
        name_to_mbox = Hashtbl.create 10;
        connections = Hashtbl.create 10;
    } in
    let _ = publish node in
    let _ = _create_net_kernel node in
    let _ = Econn.run
        server
        (_connection_up node)
        (_incoming_message node)
    in
    node
