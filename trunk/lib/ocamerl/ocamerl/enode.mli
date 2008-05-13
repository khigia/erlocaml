(** Erlang node and mbox (equivalent of Erlang process message queue).

See Enode interface for creation of Mbox.
*)

module Mbox : sig
    
    type t

    (** Return name of the mbox; None if mbox is not registered*)
    val name: t -> string option

    (** Return pid term. *)
    val pid: t -> Eterm.t

    (** Start a thread wich call the callback function for each incoming message. *)
    val create_activity: t -> (Eterm.t -> unit) -> unit

    (** Stop the thread created by start, if any. *)
    val stop_activity: t -> unit
end

type t

(** Create a new Mbox (equivalent of erlang process message queue). *)
val create_mbox: t -> Mbox.t

(** Register an mbox. *)
val register_mbox: t -> Mbox.t -> string -> unit

(** [Enode.send n dest msg] Send message [msg] to destination [dest]. *)
val send: t -> Eterm.t -> Eterm.t -> unit

(** [Enode.create cookie name] Create a node named [name] and with cookie [cookie]. *)
val create: ?cookie:string -> string -> t

(** Start the node: register it, start kernel processing, and listen for connections. *)
val start: t -> unit

(** [Enode.run name ~cookie:cookie] Create a node named [name] with cookie [cookie] and start it. *)
val run: ?cookie:string -> string -> t

(** Stop the node. *)
val stop: t -> unit
