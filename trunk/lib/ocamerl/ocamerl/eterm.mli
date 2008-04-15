(** Erlang external term format. *)

(** {5 Main Interface} *)

(** Ocaml type corresponding to Erlang type.*)
type t =
    ET_int of Int32.t
  | ET_float of float
  | ET_atom of string
  | ET_bool of bool
  | ET_tuple of t array
  | ET_string of string
  | ET_list of t list
  | ET_improper_list of t list * t (** Internal use only. *)
  | ET_pid of e_pid
  | ET_ref of e_ref
and e_pid = string * int * int * int
and e_ref = string * Int32.t list * int

(** Return a representation of an Eterm. *)
val to_string : t -> string

(** Return the encoded binary of an Eterm. *)
val to_binary : t -> string

(** Return the encoded binary of an Eterm as byte sequence. *)
val to_chars : t -> char list

(** Parse one Eterm from binary form. *)
val of_stream : char Stream.t -> t


(** {5 Specialized functions} *)

(** {6 PID management} *)

(** Return node name part of a PID Eterm. *)
val et_pid_node_name : t -> string

(** Return representation of internal data of PID Eterm. *)
val e_pid_to_string : string * int * int * int -> string

