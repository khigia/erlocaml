open OUnit

(** The list of tests registered by the source code. *)
let tests = ref []

(** Register a function as a test. *)
let register name test =
    tests := (name, test) :: !tests

(** Create a suite with all tests. *)
let create_suite name =
    let utests = List.rev_map
        (fun (name, test) -> name >:: test)
        !tests
    in
    name >::: utests

let run name =
    let suite = create_suite name in
    run_test_tt_main suite

