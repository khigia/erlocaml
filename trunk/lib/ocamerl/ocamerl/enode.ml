module Mbox = struct

    type t = {
        pid: Eterm.e_pid;
        queue: queueElement Fifo.t;
        mutable name: string option;
        mutable activity: Thread.t option;
    }
    and queueElement =
        | Data of Eterm.t
        | Ctrl of int

    let ctrl_stop = Ctrl 0

    let create pid = {
        name = None;
        pid = pid;
        queue = Fifo.create ();
        activity = None;
    }

    let name mbox =
        mbox.name

    let set_name self name =
        self.name = Some name

    let push_message mbox msg =
        (* no limit on number of message. *)
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

    let register self mbox name =
        let _ = Mbox.set_name mbox name in
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



type t = {
    name: string;
    epmc: Epmc.t;
    connections: Econn.t;
    pids: PidManager.t;
    mboxes: MboxManager.t;
}


(* node management *)

let create_mbox node =
    let pid = PidManager.make_pid node.pids in
    let mbox = MboxManager.make_mbox node.mboxes pid in
    mbox

let register_mbox node mbox name =
    MboxManager.register node.mboxes mbox name


let send node dest msg =
    match dest with
    | Eterm.ET_pid _ ->
        Econn.send_to_pid node.connections dest msg
    | _ ->
        failwith "Enode.send: dest is not valid"

let _receive node dest msg =
    (* dispatch message to the appropriate mbox *)
    match dest with
    | Eterm.ET_atom name ->
        begin
        try
            let mbox = MboxManager.find_by_name node.mboxes name in
            Mbox.push_message mbox msg
        with
            Not_found ->
                failwith ("dest not found: " ^ name)
        end
    | _ ->
        (* TODO receive message adressed to direct pid *)
        failwith ("not implemented: cannot receive message where dest is: " ^ (Eterm.to_string dest))

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

let is_published node =
    PidManager.is_initialized node.pids

let _publish node =
    if is_published node
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
    
let create ?(cookie="") nodeName =
    let name =
        if String.contains nodeName '@'
        then nodeName
        else String.concat "@" [nodeName; Unix.gethostname ();]
    in
    Trace.inf "Enode" "Making node '%s'\n" name;
    let epmc = Epmc.create () in
    let connections = Econn.create name cookie in
    let pids = PidManager.create name in
    let mboxes = MboxManager.create in
    {
        name = name;
        epmc = epmc;
        connections = connections;
        pids = pids;
        mboxes = mboxes;
    }

let start node =
    let _ = _publish node in (* may fail *)
    let _ = _create_net_kernel node in
    Econn.start node.connections (_receive node)

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
