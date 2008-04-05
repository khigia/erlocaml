open Ocamerl
open Eterm

(*
let terms =
    List.fold_left
        (fun acc x -> match x with
            | (DataSet.Exact t, _) -> t :: acc
            | _ -> acc
        )
        []
        DataSet.dataset
        *)

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

let _ = Tests.register "to_chars" (fun () ->
    List.iter
        (fun x -> match x with
            | (DataSet.Exact t, e) ->
                let chars = to_chars t in
                begin
                match e with
                    | Some (_ :: r) ->
                        OUnit.assert_equal chars r
                    | None ->
                        ()
                    | _ ->
                        OUnit.assert_failure "wrong binary trnasformation"
                end
            | _ -> ()
        )
        DataSet.dataset
)

(* TODO to_binary, of_stream *)

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
