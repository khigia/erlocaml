module Packing = struct
    let tag_alive2_req = '\120'
    let tag_alive2_rsp = '\121'

    let node_type_normal = '\077'
    let node_type_hidden = '\072'

    let distr_vsn_range = (5, 5)

    type message =
        | Msg_alive2_req of
              char        (* tag *)
            * int         (* node server port *)
            * char        (* node type: hidden, normal *)
            * int         (* protocol: 0 => tcp/ip-v4 *)
            * (int * int) (* distribution version range *)
            * string      (* node name *)
            * string      (* extra *)
        | Msg_alive2_rsp of
              int         (* result *)
            * int         (* creation *)


    let rec message_of_stream =
        parser [< 'tag; msg = tag_parse tag >] -> msg
    and tag_parse tag =
        match tag with
            | n when n = tag_alive2_rsp -> parse_alive2_rsp
            | _ ->
                failwith "message tag not recognized"
    and parse_alive2_rsp =
        parser [<
            result = Tools.eint_n 1;
            creation = Tools.eint_n 2
        >] ->
            Msg_alive2_rsp (result, creation)

    let message_to_string msg = match msg with
        | Msg_alive2_rsp (result, creation) ->
            Printf.sprintf
                "Alive2Rsp(%s, creation=%i)"
                (match result with
                    | 0 -> "OK"
                    | n -> Printf.sprintf "ERROR(%i)" n
                )
                creation
        | _ -> failwith "message_to_string not implemented for this message"

    let _message_to_chars msg = match msg with
        | Msg_alive2_req (
            tag,
            nodeServerPort,
            nodeType,
            protocol,
            (distrRangeMin, distrRangeMax),
            nodeName,
            extra
            ) ->
               tag
            :: (Tools.chars_of_int (nodeServerPort) 2)
            @  nodeType
            :: char_of_int protocol
            :: (Tools.chars_of_int (distrRangeMin) 2)
            @  (Tools.chars_of_int (distrRangeMax) 2)
            @  (Tools.chars_of_int (String.length nodeName) 2)
            @  (Tools.explode nodeName)
            @  (Tools.chars_of_int (String.length extra) 2)
            @  (Tools.explode extra)
        | _ -> failwith "_message_to_chars not implemented for this message"

    let pack_msg msg =
        let chars = _message_to_chars msg in
        let len = List.length chars in
        let head = Tools.chars_of_int (len) 2 in
        let bin = Tools.implode (head @ chars) in
        bin

end (* module Packing *)


type t = {
    epmdName: string;
    epmdPort: int;
    nodeServerName: string;
    nodeServerPort: int;
    mutable activity: Thread.t option;
}


let make_alive2_req epmc =
    let nodeName =
        try
            let len = String.index epmc.nodeServerName '@' in
            String.sub epmc.nodeServerName 0 len
        with
            Not_found ->
                epmc.nodeServerName
    in
    let extra = "" in
    let req = Packing.Msg_alive2_req (
        Packing.tag_alive2_req,
        epmc.nodeServerPort,
        Packing.node_type_hidden,
        0,
        Packing.distr_vsn_range,
        nodeName,
        extra
    ) in
    req


let create nodeServerName nodeServerPort = {
    epmdName = "localhost";
    epmdPort = 4369;
    nodeServerName = nodeServerName;
    nodeServerPort = nodeServerPort;
    activity = None;
}

let stop_event = Event.new_channel ()

let published_event = Event.new_channel ()

let _receive_message istream =
    try
        Packing.message_of_stream istream
    with
        Stream.Failure ->
            Stream.dump print_char istream;
            failwith "got stream failure"

let _received_message msg =
    Trace.dbg "Epmc"
        "Got message: %s\n"
        (Packing.message_to_string msg)
    ;
    msg

let rec _receive_message_loop istream =
    let msg = _receive_message istream in
    let _ = _received_message msg in
    _receive_message_loop istream

let alive2_connection epmc =
    (* TODO sync on event make difficult to handle failure cases*)
    let ic, oc = (try
        let addr = (Unix.gethostbyname(epmc.epmdName)).Unix.h_addr_list.(0) in
        let sock_addr = Unix.ADDR_INET(addr, epmc.epmdPort) in
        Unix.open_connection sock_addr
    with
        Unix.Unix_error(_, _, _) ->
            Trace.dbg "Epmc" "Sync on sending published_event\n";
            let publishedEvent = Event.send published_event None in
            Event.sync publishedEvent;
            Trace.dbg "Epmc" "Prematured end of EPM connection\n";
            failwith "Unix error cause end of thread"
    ) in
    let istream = Stream.of_channel ic in
    let alive2 = make_alive2_req epmc in
    let bin = Packing.pack_msg alive2 in
    output_string oc bin;
    flush oc;
    let msg = _receive_message istream in
    (match msg with
        | Packing.Msg_alive2_rsp (0, creation) ->
            Trace.dbg "Epmc" "Sync on sending published_event\n";
            let evt = Event.send
                published_event (Some creation)
            in
            Event.sync evt;
            Trace.dbg "Epmc" "Sync on receiving stop_event\n";
            let thr = Thread.create _receive_message_loop istream in
            let stopEvent = Event.receive stop_event in
            ignore (Event.sync stopEvent)
            (* not implemented??? Thread.kill thr *)
        | _ ->
            Trace.dbg "Epmc" "Sync on sending published_event\n";
            let evt = Event.send published_event None in
            Event.sync evt
    );
    Unix.shutdown_connection ic;
    Trace.dbg "Epmc" "End of EPM connection\n"

let connect epmc =
    (* TODO connect only if not yet connected *)
    Trace.inf "Epmc"
        "Epmc connecting to EPMD(%s, %i) for node '%s' listening on port %i\n"
        epmc.epmdName
        epmc.epmdPort
        epmc.nodeServerName
        epmc.nodeServerPort
    ;
    let thr = Thread.create alive2_connection epmc in
    Trace.dbg "Epmc" "Sync on receiving published_event\n";
    let publishedEvent = Event.receive published_event in
    let result = Event.sync publishedEvent in
    (match result with
        | Some creation ->
            ignore(epmc.activity <- Some thr)
        | None ->
            ()
    );
    result

let disconnect epmc =
    match epmc.activity with
        | Some thr ->
            Trace.dbg "Epmc" "Sync on sending of stop_event\n";
            let stopEvent = Event.send stop_event "closing" in
            Event.sync stopEvent;
            Thread.join thr;
            epmc.activity <- None;
            ()
        | None ->
            ()

