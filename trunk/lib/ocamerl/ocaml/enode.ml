type node = {
    name: string;
    mutable epmc: Epmc.t;
    server: Econn.t;
    pids: pids;     (* PID management *)
}

and pids = {
    mutable creation: int option;
    mutable pid_count: int;
    mutable serial: int;
    pid_to_mbox: (Eterm.e_pid, mbox) Hashtbl.t;
    mbox_to_pid: (mbox, Eterm.e_pid) Hashtbl.t;
}

and mbox = {
    pid: Eterm.e_pid;
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
    let mbox =
    {
        pid = pid;
    } in
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

let _control_message (ictrl, iarg) =
    (* TODO this is crapy fun meant to respond only to ping *)
    let ctrl = match ictrl with Eterm.ET_tuple c -> c in
    let arg = match iarg with Some (Eterm.ET_tuple c) -> c in
    match ctrl.(0) with
        | Eterm.ET_int 6l (*TODO cste*) ->
            match ctrl.(3) with
                | Eterm.ET_atom "net_kernel" ->
                    match arg.(0) with
                        | Eterm.ET_atom "$gen_call" ->
                            let arg1 = match arg.(1) with Eterm.ET_tuple c -> c in
                            let arg2 = match arg.(2) with Eterm.ET_tuple c -> c in
                            match arg2.(0) with
                                | Eterm.ET_atom "is_auth" ->
                                    let toPid = arg1.(0) in
                                    let ref = arg1.(1) in
                                    let rsp = Eterm.ET_tuple (Eterm.ETuple.make [ref; Eterm.ET_atom "yes"]) in
                                    let cookie = Eterm.ET_atom "" in
                                    [(
                                        Eterm.ET_tuple (Eterm.ETuple.make [Eterm.ET_int 2l; cookie; toPid;]),
                                        Some rsp;
                                    );]

let make nodeName =
    Trace.info (lazy (Trace.printf 
        "Making node '%s'\n" 
        nodeName
    ));
    let server = Econn.create nodeName _control_message in
    let epmc = Epmc.make nodeName server.Econn.port in
    let pids = _create_pid_manager 0 0 None in
    {
        name = nodeName;
        epmc = epmc;
        server = server;
        pids = pids;
    }

let is_published node =
    match node.pids.creation with
        | None -> false
        | Some _ -> true

let publish node =
    (* TODO should this function fail if publication fails? *)
    let result = Epmc.connect node.epmc in
    node.pids.creation <- result;
    ()
    
let unpublish node =
    Epmc.disconnect node.epmc
    
let create_mbox node =
    let pid = _create_pid node.pids node.name in
    let mbox = _create_mbox node.pids pid in
    mbox
