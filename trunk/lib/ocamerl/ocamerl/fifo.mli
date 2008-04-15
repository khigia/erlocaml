(** Thread-safe FIFO queue. *)

(** FIFO is a wrapper on standard Queue. *)
type 'a t

(** Create the FIFO. *)
val create : unit -> 'a t

(** Return true only if FIFO is empty. *)
val is_empty : 'a t -> bool

(** Return number of element in the FIFO. *)
val length : 'a t -> int

(** Add an element in the FIFO (no size limit). *)
val put : 'a t -> 'a -> unit

(** Return the next element, blocking until the FIFO is not empty. *)
val get : 'a t -> 'a
