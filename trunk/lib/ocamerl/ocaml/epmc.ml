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
    parser [< result = eint 1; creation = eint 2 >] ->
        Msg_alive2_rsp (result, creation)

and eint n =
  parser [< s = Tools.nnext n [] >] -> Tools.int_of_chars s 0


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

let message_to_chars msg = match msg with
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
        :: (Tools.chars_of_int (nodeServerPort) [] 2)
        @  nodeType
        :: char_of_int protocol
        :: (Tools.chars_of_int (distrRangeMin) [] 2)
        @  (Tools.chars_of_int (distrRangeMax) [] 2)
        @  (Tools.chars_of_int (String.length nodeName) [] 2)
        @  (Tools.explode nodeName)
        @  (Tools.chars_of_int (String.length extra) [] 2)
        @  (Tools.explode extra)
    | _ -> failwith "message_to_chars not implemented for this message"

let pack_msg msg =
    let chars = message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int (len) [] 2 in
    let bin = Tools.implode (head @ chars) in
    bin


type t = {
    epmdName: string;
    epmdPort: int;
    nodeServerName: string;
    nodeServerPort: int;
    mutable permanentConnection: Thread.t option;
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
    let req = Msg_alive2_req (
        tag_alive2_req,
        epmc.nodeServerPort,
        node_type_hidden,
        0,
        distr_vsn_range,
        nodeName,
        extra
    ) in
    req


let make nodeServerName nodeServerPort = {
    epmdName = "localhost";
    epmdPort = 4369;
    nodeServerName = nodeServerName;
    nodeServerPort = nodeServerPort;
    permanentConnection = None;
}

let stop_event = Event.new_channel ()
let published_event = Event.new_channel ()

let _receive_message istream =
    try
        message_of_stream istream
    with
        Stream.Failure ->
            Stream.dump print_char istream;
            failwith "got stream failure"

let _received_message msg =
    Trace.debug (lazy (Trace.printf
        "Got message: %s\n"
        (message_to_string msg)
    ));
    msg

let rec _receive_message_loop istream =
    let msg = _receive_message istream in
    let _ = _received_message msg in
    _receive_message_loop istream

let alive2_connection epmc =
    (* TODO sync on event ... difficult to handle failure cases*)
    let ic, oc = (try
        let addr = (Unix.gethostbyname(epmc.epmdName)).Unix.h_addr_list.(0) in
        let sock_addr = Unix.ADDR_INET(addr, epmc.epmdPort) in
        Unix.open_connection sock_addr
    with
        Unix.Unix_error(_, _, _) ->
            Trace.debug (lazy (Trace.printf
                "Sync on sending published_event\n"
            ));
            let publishedEvent = Event.send published_event None in
            Event.sync publishedEvent;
            Trace.debug (lazy (Trace.printf
                "Prematured end of EPM connection\n"
            ));
            failwith "Unix error cause end of thread"
    ) in
    let req = make_alive2_req epmc in
    let pack = pack_msg req in
    Trace.debug (lazy (Trace.printf
        "Generated Alive2Req: %s\n"
        (Tools.dump_dec pack "<<" ">>")
    ));
    output_string oc pack;
    flush oc;
    Trace.debug (lazy (Trace.printf
        "Sent Alive2Req\n"
    ));
    let istream = Stream.of_channel ic in
    let msg = _receive_message istream in
    let _ = _received_message msg in
    (match msg with
        | Msg_alive2_rsp (0, creation) ->
            Trace.debug (lazy (Trace.printf
                "Sync on sending published_event\n"
            ));
            let evt = Event.send
                published_event (Some creation)
            in
            Event.sync evt;
            Trace.debug (lazy (Trace.printf
                "Sync on receiving stop_event\n"
            ));
            let thr = Thread.create _receive_message_loop istream in
            let stopEvent = Event.receive stop_event in
            ignore (Event.sync stopEvent)
            (* not implemented??? Thread.kill thr *)
        | _ ->
            Trace.debug (lazy (Trace.printf 
                "Sync on sending published_event\n"
            ));
            let evt = Event.send published_event None in
            Event.sync evt
    );
    Unix.shutdown_connection ic;
    Trace.debug (lazy (Trace.printf
        "End of EPM connection\n"
    ))

let connect epmc =
    (* TODO connect only if not yet connected *)
    Trace.info (lazy (Trace.printf
        "Epmc connecting to EPMD(%s, %i) for node '%s' listening on port %i\n"
        epmc.epmdName
        epmc.epmdPort
        epmc.nodeServerName
        epmc.nodeServerPort
    ));
    let thr = Thread.create alive2_connection epmc in
    Trace.debug (lazy (Trace.printf
        "Sync on receiving published_event\n"
    ));
    let publishedEvent = Event.receive published_event in
    let result = Event.sync publishedEvent in
    (match result with
        | Some creation ->
            ignore(epmc.permanentConnection <- Some thr)
        | None ->
            ()
    );
    result

let disconnect epmc =
    match epmc.permanentConnection with
        | Some thr ->
            Trace.debug (lazy (Trace.printf "Sync on sending of stop_event\n"));
            let stopEvent = Event.send stop_event "closing" in
            Event.sync stopEvent;
            Thread.join thr;
            epmc.permanentConnection <- None;
            ()
        | None ->
            ()

