open Ocamerl

let get_port epmc n =
    let port = Epmc.node_port epmc n in
    match port with
    | Some p ->
        Trace.inf "ex_epmc" "node:%s port: %d\n" n p
    | None ->
        Trace.wrn "ex_epmc" "node:%s port: Not_found\n" n

let rec doit epmc args =
    match args with
        | [] ->
            ()
        | "get" :: n :: rest ->
            get_port epmc n;
            doit epmc rest
        | "set" :: n :: p :: rest ->
            ignore(Epmc.connect epmc n (int_of_string p));
            doit epmc rest
        | _ ->
            failwith "unknow command"

let usage exe =
    Trace.inf "ex_epmc" "Usage: %s [set <name> <port>]{1} [get <name>]* [getm]{1}" exe

let _  =
    Trace.inf "ex_epmc" "EPMD inspect\n";
    let args = Array.to_list Sys.argv in
    let exe = List.hd args in
    let rest = List.tl args in
    if List.length rest == 0 then usage exe;
    try
        let epmc = Epmc.create () in
        doit epmc rest
    with
        exn -> usage exe
    

