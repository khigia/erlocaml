open Ocamerl
open Eterm

let _check_string_not_empty s =
    let len = String.length s in
    OUnit.assert_bool "empty string" (len > 0)

let _ = Tests.register "to_string" (fun () ->
    let test et = _check_string_not_empty (to_string et) in
    List.iter
        (fun x -> match x with
            | (DataSet.Exact t, _) -> test t
            | _ -> ()
        )
        DataSet.dataset
)

let _ = Tests.register "to_binary" (fun () ->
    List.iter
        (fun x -> match x with
            | (DataSet.Exact t, e) ->
                let bin = to_binary t in
                begin
                match e with
                    | Some l ->
                        OUnit.assert_equal bin (Tools.implode l)
                    | None ->
                        ()
                end
            | _ -> ()
        )
        DataSet.dataset
)

let _ = Tests.register "of_stream" (fun () ->
    List.iter
        (fun x -> match x with
            | (o, Some l) ->
                let ebin = Tools.implode l in
                let stream = Stream.of_string ebin in
                let eterm = Eterm.of_stream stream in
                (match o with
                    | DataSet.Exact t ->
                        OUnit.assert_bool
                            "left some binary data after parsing"
                            (Stream.peek stream == None);
                        OUnit.assert_equal t eterm
                    | DataSet.Match matcher ->
                        OUnit.assert_bool
                            "parse result don't match expectation"
                            (matcher eterm)
                )
            | _ -> ()
        )
        DataSet.dataset
)

let _ = Tests.register "et_pid_node_name" (fun () ->
    let name = "mypid" in
    let pid = ET_pid (name, 0, 1, 2) in
    OUnit.assert_equal
        (et_pid_node_name pid)
        name
)

let _ = Tests.register "e_pid_to_string" (fun () ->
    let name = "mypid" in
    let pid = (name, 0, 1, 2) in
    _check_string_not_empty (e_pid_to_string pid)
)

let _ = Tests.run "Eterm test suite"
