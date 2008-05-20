open Ocamerl

let prog = "Ex_node_wc"

let (|>) left right = right left


let words_of_file fn =
    let buf = Scanf.Scanning.from_file fn in
    let scanner () = Scanf.bscanf buf " %s " (fun s -> s) in
    Stream.from (fun _ -> match scanner () with "" -> None | s -> Some s)

let send_ok node self ref ctrlPid =
    Enode.send
        node
        ctrlPid
        (Eterm.ET_tuple [|Eterm.ET_atom "ok"; ref; self;|])

let create_mapper_process node ref ctrlPid =
    let mbox = Enode.create_mbox node in
    let self = Enode.Mbox.pid mbox in
    let recvCB = fun msg -> match msg with
    | Eterm.ET_tuple [| ref; Eterm.ET_atom "from_file"; Eterm.ET_string filename; destPid; ctrlPid;|] ->
        let sender w =
            let item = (Eterm.ET_tuple [| Eterm.ET_string w; Eterm.ET_int 1l;|]) in
            Trace.dbg prog "Pid %s sending item %s\n" (Eterm.to_string self) (Eterm.to_string item);
            Enode.send node destPid item
        in
        words_of_file filename |> Stream.iter sender;
        send_ok node self ref ctrlPid
    | Eterm.ET_atom "stop" ->
        Enode.destroy_mbox node mbox
    | m ->
        (* skip unknown message *)
        Trace.inf prog "Skip unknow message: %s\n" (Eterm.to_string m)
    in
    Enode.Mbox.create_activity mbox recvCB;
    send_ok node self ref ctrlPid

let create_main_process node name =
    let mbox = Enode.create_mbox node in
    let _ = Enode.register_mbox node mbox name in
    let recvCB = fun msg -> match msg with
    | Eterm.ET_tuple [| Eterm.ET_atom "new_mapper"; ref; ctrlPid; |] ->
        create_mapper_process node ref ctrlPid
    | m ->
        (* skip unknown message *)
        Trace.inf prog "Skip unknow message: %s\n" (Eterm.to_string m)
    in
    Enode.Mbox.create_activity mbox recvCB

let doit () =
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
    Trace.inf prog "Creating node; name: %s; cookie: %s\n" !name !cookie;
    let n = Enode.create !name ~cookie:!cookie in
    let _ = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint] in
    let _ = Enode.start n in
    let _ = create_main_process n "wc" in
    let _ = Thread.wait_signal [Sys.sigint] in
    Enode.stop n

let _  =
    try doit ()
    with exn -> Printf.printf "ERROR:%s\n" (Printexc.to_string exn)

