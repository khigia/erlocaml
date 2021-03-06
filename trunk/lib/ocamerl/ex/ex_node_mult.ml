open Ocamerl

let create_worker_process node operand =
    let mbox = Enode.create_mbox node in
    let recvCB = fun msg -> match msg with
    | Eterm.ET_tuple [|toPid; ref; Eterm.ET_int i;|] ->
        Enode.send
            node
            toPid
            (Eterm.ET_tuple [| ref; Eterm.ET_int (Int32.mul i operand); |])
    | _ ->
        (* skip unknown message *)
        ()
    in
    Enode.Mbox.create_activity mbox recvCB;
    Enode.Mbox.pid mbox

let create_main_process node name =
    let mbox = Enode.create_mbox node in
    let _ = Enode.register_mbox node mbox name in
    let recvCB = fun msg -> match msg with
    | Eterm.ET_tuple [|toPid; Eterm.ET_int i;|] ->
        let slave = create_worker_process node i in
        Enode.send
            node
            toPid
            slave
    | _ ->
        (* skip unknown message *)
        ()
    in
    Enode.Mbox.create_activity mbox recvCB

let doit () =
    try
        let name = ref "ocaml" in
        let cookie = ref "" in
        Arg.parse
            [
                ("-cookie", Arg.String ((:=) cookie), "erlang node cookie");
                ("-name", Arg.String ((:=) name), "erlang node name");
                ("-debug", Arg.Unit (fun () -> Trace.set_level Trace.lvl_debug), "print debug messages");
            ]
            ignore
            "";
        Trace.inf "Node_double" "Creating node; name: %s; cookie: %s\n" !name !cookie;
        let n = Enode.create !name ~cookie:!cookie in
        let _ = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint] in
        let _ = Enode.start n in
        let _ = create_main_process n "byn" in
        let _ = Thread.wait_signal [Sys.sigint] in
        Enode.stop n
    with
        exn -> Printf.printf "ERROR:%s\n" (Printexc.to_string exn)

let _  = doit ()

