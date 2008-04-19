(** EPMD protocol/transport. *)

type t

(** Create a EPMD client. *)
val create : ?epmdName:string -> ?epmdPort:int -> unit -> t

(** Connect client to EPMD server and register node name and port; return PID creation identifier. *)
val connect : t -> string -> int -> int option

(** Disonnect client from EPMD server (thus unregister the node). *)
val disconnect : t -> unit

(** Query EPMD to get port of registered node. *)
val node_port: t -> string -> int option
