
try
    let _ = Trace.info (lazy (Trace.printf
        "Creating node\n"
    )) in
    let n = Enode.make "ocaml@affle-laptop" in
    let _ = Enode.trace "    " n in

    let _ = Trace.info (lazy (Trace.printf
        "Publishing node\n"
    )) in
    let _ = Enode.publish n in
    let _ = Trace.info (lazy (Trace.printf
        "Node publication result: %B\n"
        (Enode.is_published n)
    )) in
    let _ = Enode.trace "    " n in

    (* TODO move to unit test
    let _ = Trace.info (lazy (Trace.printf
        "Creating a PID\n"
    )) in
    let pid = Enode._create_pid n.pids n.name in
    let _ = Trace.info (lazy ( Trace.printf
        "PID=%s\n"
        (Eterm.to_string (Eterm.ET_pid pid))
    )) in
    let _ = Enode.trace "    " n in
    *)

    let _ = Trace.info (lazy (Trace.printf
        "Creating a mbox\n"
    )) in
    let mbox = Enode.create_mbox n in
    let _ = Enode.trace "    " n in

    let _ = flush_all in
    let _ = Trace.flush in
    let _ = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint] in
    (*
    let _ = Enode.loop n in
    *)
    let _ = Thread.wait_signal [Sys.sigint] in

    let _ = Trace.info (lazy (Trace.printf
        "Un-publishing node\n"
    )) in
    let _ = Enode.unpublish n in
    let _ = Enode.trace "    " n in
    ()
with
    exn -> Printf.printf "ERROR:%s\n" (Printexc.to_string exn)
