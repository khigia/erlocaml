open Ocamerl

let _ = Tests.register "foo" (fun () ->
    OUnit.assert_equal 12 12
)

let _ = Tests.run "Eterm test suite"
