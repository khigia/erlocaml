open Ocamerl


type test_st = {
    t:   int;
    acc: string list;
}

let test_st2 st evt = (
    {t=2; acc=evt :: st.acc},
    Fsm.Stop (Some true, evt :: [])
)

let test_st1 st evt = (
    {t=1; acc=evt :: st.acc},
    Fsm.Continue (Fsm.handler test_st2, (None, evt :: []))
)

let _ = Tests.register "create" (fun () ->
    let fsm = Fsm.create
        {t=0; acc=[];}
        (Fsm.handler test_st1)
    in
    OUnit.assert_equal (Fsm.is_ready fsm) true
)

let _ = Tests.register "send" (fun () ->
    let fsm = Fsm.create
        {t=0; acc=[];}
        (Fsm.handler test_st1)
    in
    OUnit.assert_equal (Fsm.is_ready fsm) true;
    OUnit.assert_equal (Fsm.send fsm "a") (Fsm.Reply (None, ["a";]));
    OUnit.assert_equal (Fsm.state fsm) {t=1; acc=["a";]};
    OUnit.assert_equal (Fsm.is_ready fsm) true;
    OUnit.assert_equal (Fsm.send fsm "b") (Fsm.Reply (Some true, ["b";]));
    OUnit.assert_equal (Fsm.state fsm) {t=2; acc=["b"; "a";]};
    OUnit.assert_equal (Fsm.is_ready fsm) false;
    OUnit.assert_equal (Fsm.send fsm "c") (Fsm.Finish)
)

let _ = Tests.run "FSM test suite"
