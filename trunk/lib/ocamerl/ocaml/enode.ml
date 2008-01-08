module Mbox = struct
    
    type t = {
        pid: Eterm.e_pid;
        queue: Eterm.eterm Fifo.t;
    }

    let create pid = {
        pid = pid;
        queue = Fifo.create ();
    }

    let push_message mbox msg =
        (* TODO Fifo need to implement some maximum size?! *)
        Fifo.put mbox.queue msg

    let receive mbox =
        Fifo.get mbox.queue

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
    try
        let rsp = Hashtbl.find node.name_to_mbox name in
        Some rsp
    with
        Not_found ->
            None

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
    let rec recv_loop = fun () ->
        let msg = Mbox.receive mbox in
        (match msg with
            | Eterm.ET_tuple arg ->
                (* TODO could we have a better interface to match tuple and all eterm? *)
                (match arg.(0) with
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
                )
            (* TODO failing cases ... *)
        );
        recv_loop ()
    in
    Thread.create recv_loop ()

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
    
let create nodeName =
    Trace.info (lazy (Trace.printf 
        "Making node '%s'\n" 
        nodeName
    ));
    let server = Econn.create nodeName in
    let epmc = Epmc.create nodeName server.Econn.port in
    let pids = _create_pid_manager 0 0 None in
    {
        name = nodeName;
        epmc = epmc;
        server = server;
        pids = pids;
        name_to_mbox = Hashtbl.create 10;
        connections = Hashtbl.create 10;
    }

let start node =
    let _ = publish node in
    let _ = _create_net_kernel node in
    Econn.start
        node.server
        (_connection_up node)
        (_incoming_message node)

let stop node =
    (*TODO stop the server, connections, epmc, ... *)
    ()
