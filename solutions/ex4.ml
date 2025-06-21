(* In this exercise, we'll compute the set of stale checks for each
   host. Specifically, a check is considered stale if it hasn't been
   updated for the last X seconds, for a configured threshold X.

   The all-at-once implementation below should give you a sense of
   what the semantics should be, but implementing this efficiently and
   incrementally is non-trivial.  In particular, if you just use the
   current time in a naive way, then you'll have to do work linear in
   the number of hosts every time you refresh the computation.

   To do this efficiently, you'll want to use Incremental's support
   for Time_float_unix. You'll want to make use of [Incr.advance_clock] and
   [Incr.at].
*)

open! Core
open! Async
open! Import

type result = Time_float_unix.t Check.Name.Map.t Host.Name.Map.t
[@@deriving sexp]

let print_result x = print_s [%sexp (x : result)]

module Simple = struct
  let stale_checks (s : State.t) ~(thresh : Time_float_unix.Span.t) : result =
    Map.filter_map s.hosts ~f:(fun (_, check) ->
        let map =
          Map.filter_map check ~f:(fun (when_registered, _) ->
              if
                Time_float_unix.Span.( < )
                  (Time_float_unix.diff s.time when_registered)
                  thresh
              then None
              else Some when_registered)
        in
        if Map.is_empty map then None else Some map)

  let process_events ~(thresh : Time_float_unix.Span.t)
      (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_result in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
        state := Queue.fold eventq ~init:!state ~f:State.update;
        let stale_checks =
          Viewer.compute viewer (fun () -> stale_checks ~thresh !state)
        in
        Viewer.update viewer stale_checks;
        return ())
end

module Incremental = struct
  let clock = Incremental.Clock.create Incr.State.t ~start:Time_ns.epoch ()

  let stale_checks (s : State.t Incr.t) ~(thresh : Time_float_unix.Span.t) :
      result Incr.t =
    let open Incr.Let_syntax in
    Incr_map.filter_mapi' (s >>| State.hosts) ~f:(fun ~key:_ ~data ->
        let%map map =
          Incr_map.filter_mapi' (data >>| snd) ~f:(fun ~key:_ ~data ->
              let%bind time, _ = data in
              match%map
                Incremental.Clock.at clock
                  (Time_ns.of_time_float_round_nearest
                     (Time_float_unix.add time thresh))
              with
              | Before -> None
              | After -> Some time)
        in
        if Map.is_empty map then None else Some map)

  let process_events ~(thresh : Time_float_unix.Span.t)
      (events : Event.t Pipe.Reader.t) : unit Deferred.t =
    let module Var = Incr.Var in
    let viewer = Viewer.create ~print:print_result in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (stale_checks ~thresh (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(function
      | Initialized x | Changed (_, x) -> Viewer.update viewer x
      | Invalidated -> assert false);
    Pipe.iter' events ~f:(fun eventq ->
        Incr.Var.set state
          (Queue.fold eventq ~init:(Var.value state) ~f:State.update);
        Incremental.Clock.advance_clock clock
          ~to_:(Time_ns.of_time_float_round_nearest (Incr.Var.value state).time);
        Viewer.compute viewer Incr.stabilize;
        return ())
end

let command =
  let time_span_arg : Time_float.Span.t Command.Arg_type.t =
    Command.Arg_type.create Time_float.Span.of_string
  in
  let cmd summary process_events =
    Command.async ~summary
      (let open Command.Let_syntax in
       [%map_open
         let host, port = Command_common.host_and_port_param
         and thresh =
           flag "-thresh"
             (optional_with_default
                (Time_float_unix.Span.of_sec 1.)
                time_span_arg)
             ~doc:"Threshold for determing when a host is stale"
         in
         fun () ->
           Command_common.connect_and_process_events ~host ~port
             ~process_events:(process_events ~thresh)])
  in
  Command.group ~summary:"Exercise 4"
    [
      ("simple", cmd "all-at-once implementation" Simple.process_events);
      ( "incremental",
        cmd "incremental implementation" Incremental.process_events );
    ]
