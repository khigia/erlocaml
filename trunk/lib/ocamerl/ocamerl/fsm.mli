(** Generic FSM.

Implementation is based on handler function: a state of the
machine is modelled by a function; this function accept the
global state of the FSM and an event, and return a continuation
information [handler_return_t].
*)

(** FSM abstract type. *)
type ('state, 'event, 'reply) t

(** FSM state function. *)
type ('state, 'event, 'reply) handler_t

(** FSM state function return type. *)
type ('state, 'event, 'reply) handler_return_t =
    | Continue of ('state, 'event, 'reply) handler_t * 'reply
    | Stop of 'reply

(** Respond type of send operation. *)
type 'r respond =
    | Reply of 'r
    | Finish

(** [create st f] Create an FSM instance with initial state [st] and initial handler [f]. *)
val create: 'state -> ('state, 'event, 'reply) handler_t -> ('state, 'event, 'reply) t

(** [handler f] Create an FSM handler from function [f]. *)
val handler: ('state -> 'event -> 'state * ('state, 'event, 'reply) handler_return_t) -> ('state, 'event, 'reply) handler_t

(** [is_ready fsm] Return true if FSM [fsm] is not in final state. *)
val is_ready: ('state, 'event, 'reply) t -> bool

(** [state fsm] Return current global state of the FSM. *)
val state: ('state, 'event, 'reply) t -> 'state

(** [send fsm evt] Send event [evt] to FSM [fsm]. *)
val send: ('state, 'event, 'reply) t -> 'event -> 'reply respond
