open! Core
(** A simple way of viewing results in a terminal. *)

type 'display_state t

val create : print:('display_state -> unit) -> 'display_state t

val update : 'display_state t -> 'display_state -> unit
(** To register a new value to be printed *)

val compute : _ t -> (unit -> 'a) -> 'a
(** Used to register the computation phase *)
