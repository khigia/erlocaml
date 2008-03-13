let create_double_process node name =
    let mbox = Enode.create_mbox node in
    let _ = Enode.register_mbox node mbox name in
    let recvCB = fun msg ->
        match msg with
            | Eterm.ET_tuple arg ->
                (* TODO failing cases ... *)
                let toPid = arg.(0) in
                match arg.(1) with
                    | Eterm.ET_int i ->
                        Enode.send node toPid (Eterm.ET_int (Int32.mul i 2l))
            (* TODO failing cases ... *)
    in
    Enode.Mbox.create_activity mbox recvCB

try
    Trace.inf "Node_double" "Creating node\n";
    let n = Enode.create "ocaml@devhost" in
    let _ = Enode.trace "    " n in
    let _ = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint] in
    let _ = Enode.start n in
    let _ = create_double_process n "bytwo" in
    let _ = Thread.wait_signal [Sys.sigint] in
    Enode.stop n
with
    exn -> Printf.printf "ERROR:%s\n" (Printexc.to_string exn)
