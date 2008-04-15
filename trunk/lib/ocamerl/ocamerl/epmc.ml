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
            nodePort,
            nodeType,
            protocol,
            (distrRangeMin, distrRangeMax),
            nodeName,
            extra
            ) ->
               tag
            :: (Tools.chars_of_int (nodePort) 2)
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


type host_t = {
    name: string;
    port: int;
}

type conn_t = {
    input: in_channel;
    output: out_channel;
}

type t = {
    epmd: host_t;
    mutable node: host_t option;
    mutable conn: conn_t option;
}


(* Tools to manage TCP connection *)

let make_host name port =
    {name = name; port = port}

let node_name name =
    try
        let len = String.index name '@' in
        String.sub name 0 len
    with
        Not_found -> name

let _close_connection conn =
    (match conn with
    | Some conn ->
        Unix.shutdown_connection conn.input;
        Trace.dbg "Epmc" "End of connection to EPMD\n"
    | None ->
        ()
    )
    
let _open_connection epmd =
    let ic, oc = (
        try
            let addr = (Unix.gethostbyname(epmd.name)).Unix.h_addr_list.(0) in
            let sock_addr = Unix.ADDR_INET(addr, epmd.port) in
            Unix.open_connection sock_addr
        with
            Unix.Unix_error(_, _, _) ->
                Trace.dbg "Epmc" "Cannot open connection to EPMD\n";
                failwith "Unix error cause end of thread"
    ) in
    {input=ic; output=oc;}


(* Exchanges req/rsp defined in EPMD protocol *)

let _register conn node =
    (* send alive2 req *)
    let req = Packing.Msg_alive2_req (
        Packing.tag_alive2_req,
        node.port,
        Packing.node_type_hidden,
        0,
        Packing.distr_vsn_range,
        node_name node.name,
        "" (* extra *)
    ) in
    let bin = Packing.pack_msg req in
    output_string conn.output bin;
    flush conn.output;
    (* recv alive2 rsp *)
    let istream = Stream.of_channel conn.input in
    let rsp = Packing.message_of_stream istream in (*TODO may raise Stream.failure *)
    match rsp with
        | Packing.Msg_alive2_rsp (0, creation) ->
            Trace.dbg "Epmc" "Connected!\n";
            Some creation
        | _ ->
            Trace.inf "Epmc" "Registration of node failed!\n";
            None


(* Visible API *)

let create ?(epmdName="localhost") ?(epmdPort=4369) () = {
    epmd = make_host epmdName epmdPort;
    conn = None;
    node = None;
}

let connect epmc nodeName nodePort =
    _close_connection epmc.conn;
    let conn = _open_connection epmc.epmd in
    let node = make_host nodeName nodePort in
    epmc.node <- Some node;
    epmc.conn <- Some conn;
    _register conn node

let disconnect epmc =
    _close_connection epmc.conn;
    epmc.node <- None;
    epmc.conn <- None

