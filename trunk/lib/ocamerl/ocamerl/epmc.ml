module Packing = struct

    let tag_alive2_req = '\120'
    let tag_alive2_rsp = '\121'
    let tag_port2_req  = '\122'
    let tag_port2_rsp  = '\119'

    let node_type_normal = '\077'
    let node_type_hidden = '\072'

    let distr_vsn_range = (5, 5)

    type node_desc_t =
          int         (* node server port *)
        * char        (* node type: hidden, normal *)
        * int         (* protocol: 0 => tcp/ip-v4 *)
        * (int * int) (* distribution version range *)
        * string      (* node name *)
        * string      (* extra *)

    type message =
        | Msg_alive2_req of
              node_desc_t
        | Msg_alive2_rsp of
              int         (* result *)
            * int         (* creation *)
        | Msg_port2_req of
              string      (* node name *)
        | Msg_port2_rsp of
              node_desc_t option

    let rec message_of_stream =
        parser [< 'tag; msg = tag_parse tag >] -> msg
    and tag_parse tag =
        match tag with
            | n when n = tag_alive2_rsp -> parse_alive2_rsp
            | n when n = tag_port2_rsp -> parse_port2_rsp
            | _ ->
                failwith "message tag not recognized"
    and parse_alive2_rsp =
        parser [<
            result = Tools.eint_n 1;
            creation = Tools.eint_n 2
        >] ->
            Msg_alive2_rsp (result, creation)
    and parse_port2_rsp =
        parser [<
            result = Tools.eint_n 1;
            stream
        >] ->
            print_endline "";
            let p = match result with
                | 0 -> parse_port2_rsp_info
                | _ -> begin parser [< >] -> Msg_port2_rsp None end
            in
            p stream
    and parse_port2_rsp_info =
        parser [<
            port = Tools.eint_n 2;
            'nodeType;
            prot = Tools.eint_n 1;
            vsn1 = Tools.eint_n 2;
            vsn2 = Tools.eint_n 2;
            nLen = Tools.eint_n 2;
            name = Tools.string_n nLen;
            'lsb; (* if LSB is 0, there is no more data *)
            extr = parse_port2_rsp_info_extra lsb
        >] ->
            Msg_port2_rsp (Some (
                port,
                nodeType,
                prot,
                (vsn1, vsn2),
                name,
                extr
            ))
    and parse_port2_rsp_info_extra lsb =
        match int_of_char lsb with
            | 0 -> begin parser [< >] -> "" end
            | _ -> begin parser [<
                'usb;
                extr = Tools.string_n (Tools.int_of_chars [lsb; usb])
            >] -> extr end

    let message_to_string msg = match msg with
        | Msg_alive2_rsp (result, creation) ->
            Printf.sprintf
                "Alive2Rsp(%s, creation=%i)"
                (match result with
                    | 0 -> "OK"
                    | n -> Printf.sprintf "ERROR(%i)" n
                )
                creation
        | Msg_port2_rsp (Some _) ->
            "Port2Rsp(OK, ...)"
        | Msg_port2_rsp (None) ->
            "Port2Rsp(None)"
        | _ -> failwith "message_to_string not implemented for this message"

    let _message_to_chars msg = match msg with
        | Msg_alive2_req (
            nodePort,
            nodeType,
            protocol,
            (distrRangeMin, distrRangeMax),
            nodeName,
            extra
            ) ->
               tag_alive2_req
            :: (Tools.chars_of_int (nodePort) 2)
            @  nodeType
            :: char_of_int protocol
            :: (Tools.chars_of_int (distrRangeMin) 2)
            @  (Tools.chars_of_int (distrRangeMax) 2)
            @  (Tools.chars_of_int (String.length nodeName) 2)
            @  (Tools.explode nodeName)
            @  (Tools.chars_of_int (String.length extra) 2)
            @  (Tools.explode extra)
        | Msg_port2_req nodeName ->
               tag_port2_req
            :: (Tools.explode nodeName)
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

let send_recv conn req =
    let bin = Packing.pack_msg req in
    output_string conn.output bin;
    flush conn.output;
    let istream = Stream.of_channel conn.input in
    Packing.message_of_stream istream (*TODO may raise Stream.failure *)


(* Exchanges req/rsp defined in EPMD protocol *)

let _register conn node =
    (* send alive2 req *)
    let req = Packing.Msg_alive2_req (
        node.port,
        Packing.node_type_hidden,
        0,
        Packing.distr_vsn_range,
        node_name node.name,
        "" (* extra *)
    ) in
    (* recv alive2 rsp *)
    match send_recv conn req with
        | Packing.Msg_alive2_rsp (0, creation) ->
            Trace.dbg "Epmc" "Registered!";
            Some creation
        | _ ->
            failwith "Epmc: Registration of node failed!"

let _node_info conn name =
    (* send port2 req *)
    let req = Packing.Msg_port2_req name in
    (* recv port2 rsp *)
    match send_recv conn req with
        | Packing.Msg_port2_rsp info ->
            Trace.dbg "Epmc" "Retrieved node info from EPMD\n";
            info
        | _ ->
            failwith "Epmc: Lookup of node failed!"


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

let node_port epmc name =
    let conn = _open_connection epmc.epmd in
    let res = match _node_info conn name with
        | Some (port, _, _, _, _, _) -> Some port
        | _ -> None
    in
    _close_connection (Some conn);
    res
