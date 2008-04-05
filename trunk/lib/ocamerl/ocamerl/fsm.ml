(* A state function has 4 roles:
    - update FSM internal state (use case e.g.: remember user login gicen in first state)
    - provide next state function (FSM transitions from this state to others)
    - provide direct output of the FSM (use cases e.g.: (1) signal the end of FSM or (2) give the result of some computation)
    - return a (maybe empty) list of actions wich have a different semantic that main output (use case e.g.: a list of messages received on some connection)
*)
type ('state, 'event, 'answer, 'action) state_f =
    State_f of (
        'state ->
        'event ->
        ('state, 'event, 'answer, 'action) state_f_return
    )
and ('state, 'event, 'answer, 'action) state_f_return =
      'state
    * ('state, 'event, 'answer, 'action) state_f option
    * 'answer option
    * 'action list

type ('state, 'event, 'answer, 'action) t = {
    mutable state: 'state;
    mutable handler: ('state, 'event, 'answer, 'action) state_f option;
}


let create state handler =
    {
        state = state;
        handler = handler;
    }

(* TODO clean name: state is use for fsm global state as well as the state the fsm is in *)
let mkstate f = Some (State_f f)

let send fsm event =
    match fsm.handler with
        | Some (State_f f) ->
            let state, handler, answer, actions = f fsm.state event in
            fsm.state <- state;
            fsm.handler <- handler;
            (answer, actions)
            (* TODO no need for answer+action: if app need it, can do it! *)
        | None ->
            (None, [])
            


(* TODO move in unit-tests *)
(*

type test_st = {
    t:   int;
    acc: string list;
}

let test_st2 st evt =
    ({st with t=2; acc=evt :: st.acc}, None, Some true, evt :: [])

let test_st1 st evt =
    ({st with t=1; acc=evt :: st.acc}, mkstate test_st2, None, evt :: [])

let test () =
    let fsm = create
        {t=0; acc=[];}
        (mkstate test_st1)
    in
    let _ = match send fsm "a" with
        | (None, o :: []) -> print_endline ("OK " ^ o)
        | _ -> failwith "arg!"
    in
    let _ = match send fsm "b" with
        | (Some true, o :: []) -> print_endline ("OK " ^ o)
        | _ -> failwith "arg!"
    in
    let _ = match send fsm "c" with
        | (None, []) -> print_endline "ok"
        | _ -> failwith "arg!"
    in
    List.iter print_endline fsm.state.acc

let _ = test ()
*)
