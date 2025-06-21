open! Core
open Protocol

val stream :
  Random.State.t ->
  Time_float_unix.t ->
  num_hosts:int ->
  pct_initially_active:float ->
  time_scale:Time_float_unix.Span.t ->
  (unit -> Time_float_unix.t * Event.t) Staged.t
(** Create a deterministic, infinite sequence of events *)
