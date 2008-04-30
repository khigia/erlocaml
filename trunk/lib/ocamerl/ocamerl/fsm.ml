type 'r respond =
    | Reply of 'r
    | Finish

type ('state, 'event, 'reply) handler_t =
    Handler_f of (
        'state ->
        'event ->
        'state * ('state, 'event, 'reply) handler_return_t
    )
and ('state, 'event, 'reply) handler_return_t =
    | Continue of ('state, 'event, 'reply) handler_t * 'reply
    | Stop of 'reply

type ('state, 'event, 'reply) t = {
    mutable state: 'state;
    mutable handler: ('state, 'event, 'reply) handler_t option;
}


let create state handler =
    {
        state = state;
        handler = Some handler;
    }

let handler f =
    Handler_f f

let is_ready fsm =
    not(fsm.handler = None)

let state fsm =
    fsm.state

let send fsm event =
    match fsm.handler with
    | Some(Handler_f f) ->
        let state, resp = f fsm.state event in
        fsm.state <- state;
        begin
        match resp with
        | Continue(f, r) ->
            fsm.handler <- Some f;
            Reply r
        | Stop r ->
            fsm.handler <- None;
            Reply r
        end;
    | None ->
        Finish
