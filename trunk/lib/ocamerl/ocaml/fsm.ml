type ('state, 'event, 'action) state_f =
    State_f of (
        'state ->
        'event ->
        'state * ('state, 'event, 'action) state_f option * 'action list
    )

type ('state, 'event, 'action) t = {
    mutable state: 'state;
    mutable handler: ('state, 'event, 'action) state_f option;
}

let mkstate f = Some (State_f f)

let create state handler =
    {
        state = state;
        handler = handler;
    }

let send fsm event =
    match fsm.handler with
        | Some (State_f f) ->
            let state, handler, actions = f fsm.state event in
            fsm.state <- state;
            fsm.handler <- handler;
            actions
        | None ->
            []
            


(* TODO move in unit-tests *)

type test_st = {t:int; acc:string list;}

let test_h2 st evt =
    ({st with t=2; acc=evt :: st.acc}, None, evt :: [])

let test_h1 st evt =
    ({st with t=1; acc=evt :: st.acc}, mkstate test_h2, evt :: [])

let test () =
    let fsm = create {t=0; acc=[];} (mkstate test_h1) in
    let _ = match send fsm "a" with
        | o :: [] -> print_endline o
        | _ -> failwith "arg!"
    in
    let _ = match send fsm "b" with
        | o :: [] -> print_endline o
        | _ -> failwith "arg!"
    in
    let _ = match send fsm "c" with
        | [] -> print_endline "ok"
        | _ -> failwith "arg!"
    in
    List.iter print_endline fsm.state.acc

let _ = test ()
