(* TODO check with documentation in erlang kernel application. *)

(* Constants *)

let _tag_status           = 's'
let _tag_recv_name        = 'n'
let _tag_challenge_rsp    = 'r'
let _tag_challenge_digest = 'a'


(* Challenge *)

let _create_challenge () =
    Tools.int32_of_chars [
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
        char_of_int (Random.int 0xFF);
    ]

let _compute_digest challenge cookie =
    (* do not use Int32.to_string as it assume signed int32 *)
    let challengeRepr = Printf.sprintf "%lu" challenge in
    Digest.string (cookie ^ challengeRepr)

let _check_digest peerDigest cookie challenge =
    match challenge with
        | Some value ->
            let expected = _compute_digest value cookie in
            expected = peerDigest
        | None ->
            false


(* Messages *)

type message =
    | Msg_recv_name of
          int     (* distr version *)
        * Int32.t (* peer flags *)
        * string  (* peer name *)
    | Msg_status of
        string
    | Msg_challenge_req of
          int       (* distr version *)
        * Int32.t   (* node flags *)
        * Int32.t   (* challenge *)
        * string    (* node name *)
    | Msg_challenge_rsp of
          Int32.t   (* peer challenge *)
        * string    (* peer digest *)
    | Msg_challenge_digest of
          string  (* digest *)

let message_to_string msg = match msg with
    | Msg_recv_name (distr, flags, name) ->
        Printf.sprintf
            "Msg_recv_name(%i, %lu, %s)"
            distr
            flags
            name
    | Msg_status s ->
        Printf.sprintf "Msg_status(%s)" s
    | Msg_challenge_req (distr, flags, challenge, node) ->
        Printf.sprintf
            "Msg_challenge_req(%i, %lu, %lu, %s)"
            distr
            flags
            challenge
            node
    | Msg_challenge_rsp (challenge, digest) ->
        Printf.sprintf
            "Msg_challenge_rsp(%lu, 0x%s)"
            challenge
            (Digest.to_hex digest)
    | Msg_challenge_digest data ->
        Printf.sprintf
            "Msg_challenge_digest(%s)"
            (Digest.to_hex data)

let rec message_of_stream =
    parser [<
        len = Tools.eint_n 2;
        'tag;
        msg = _parse (len - 1) tag
    >] ->
        msg
and _parse len tag =
    match tag with
        | n when n = _tag_recv_name -> _parse_recv_name len
        | n when n = _tag_challenge_rsp -> _parse_challenge_rsp len
        | _ ->
            failwith "handshake message tag not recognized"
and _parse_recv_name len =
    parser [<
        peerDistrVsn = Tools.eint_n 2;
        peerFlags = Tools.eint32_n 4;
        peerName = Tools.string_n (len - 6)
    >] ->
        Msg_recv_name (
            peerDistrVsn,
            peerFlags,
            peerName
        )
and _parse_challenge_rsp len =
    parser [<
        challenge = Tools.eint32_n 4;
        digest = Tools.string_n (len - 4)
    >] ->
        Msg_challenge_rsp (
            challenge,
            digest
        )

let _message_to_chars msg = match msg with
    | Msg_status status ->
        _tag_status 
        :: (Tools.explode status)
    | Msg_challenge_req (
            distrVersion,
            flags,
            challenge,
            nodeName
        ) ->
        _tag_recv_name 
        :: (Tools.chars_of_int distrVersion 2)
        @  (Tools.chars_of_int32 flags 4)
        @  (Tools.chars_of_int32 challenge 4)
        @  (Tools.explode nodeName)
    | Msg_challenge_digest digest ->
        _tag_challenge_digest
        :: (Tools.explode digest)
    | _ ->
        failwith ("not implemented: cannot encode message: " ^ (message_to_string msg))

let pack msg =
    let chars = _message_to_chars msg in
    let len = List.length chars in
    let head = Tools.chars_of_int len 2 in
    let r = Tools.implode (head @ chars) in
    Trace.dbg "Econn"
        "Packed handshake msg: %s\n"
        (Tools.dump_hex r "<<" ">>" " ")
    ;
    r


(* FSM *)

type fsmState = {
    localNode: node;
    cookie:    string;
    challenge: Int32.t option;
    peerNode:  node option;
}
and node = {
    version:   int;
    name:      string;
    flags:     Int32.t;
}

let _st_recv_challenge_rsp st msg = match msg with
    | Msg_challenge_rsp (peerChallenge, digest) ->
        (match _check_digest digest st.cookie st.challenge with
            | true ->
                Trace.dbg "Econn" "Peer digest OK\n";
                (* reply to peer challenge *)
                let reply = Msg_challenge_digest
                    (_compute_digest peerChallenge st.cookie)
                in
                (match st.peerNode with
                | Some peerNode ->
                    (* handshake OK! *)
                    (
                        st,
                        Fsm.Stop(Some (true, Some peerNode.name), [reply;])
                    )
                | _ ->
                    (* handshake failing here! *)
                    Trace.dbg "Econn" "Handshake failed: peer node name NOT ok\n";
                    (st, Fsm.Stop(Some (false, None), []))
                )
            | false ->
                Trace.dbg "Econn" "Handshake failed: peer digest NOT ok\n";
                (* handshake failing here! *)
                (st, Fsm.Stop(Some (false, None), []))
        )
    | _ ->
        Trace.dbg "Econn"
            "Handshake failed: got wrong message: %s\n"
            (message_to_string msg)
        ;
        (* handshake failing here! *)
        (st, Fsm.Stop(Some (false, None), []))

let _st_recv_name st msg =
    let challenge = _create_challenge () in
    match msg with
        | Msg_recv_name (
            peerDistVsn,
            peerFlags,
            peerName
        ) ->
            let newSt = {st with
                challenge = Some challenge;
                peerNode = Some {
                    version = peerDistVsn;
                    name    = peerName;
                    flags   = peerFlags;
                };
            } in
            let msgStatus = Msg_status "ok" in
            let msgChallengeReq = Msg_challenge_req (
                st.localNode.version,
                st.localNode.flags,
                challenge,
                st.localNode.name
            ) in
            (
                newSt,
                Fsm.Continue(Fsm.handler _st_recv_challenge_rsp, (None, [msgStatus; msgChallengeReq;]))
            )
        | _ ->
            Trace.dbg "Econn" "Handshake failed: did not receive name\n";
            (* handshake failing here! *)
            (st, Fsm.Stop(Some (false, None), []))

let create_fsm version name cookie flags =
    (* TODO this is accepting fsm, need the fsm to handle
    case where node begin the handshake with remote node.
    *)
    (* TODO return value is so weeeeeeeeiirrrddddd;
    shall use some new type like:
    type result =
        | Processing (* handshake not finish, expecting more event *)
        | Ok of string (* peer name *)
        | Failed of string (* reason *)
    *)
    Fsm.create
        {
            localNode = {
                version     = version;
                name        = name;
                flags       = flags;
            };
            cookie    = cookie;
            challenge = None;
            peerNode  = None;
        }
        (Fsm.handler _st_recv_name)
