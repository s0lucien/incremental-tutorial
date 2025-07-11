(*
   Now it's time look at a more real-world example!

   For the rest of this tutorial we are going to work on an
   application that monitors the state and the health of a collection
   of machines, e.g. all boxes in a datacenter.

   To understand the setup, take a look at [shared/protocol.ml] which
   contain the protocol that the client receives from the server.

   You do not need to look at [shared/server.ml] or
   [shared/generator.ml] in any detail.

   As a first simple query, we would like to report the ratio
   [passed/total], where [passed] is the number of notifications of
   checks that have passed, and [total] is the total number of check
   notifications, including both passes and failures.

   As before, we provide an implementation that doesn't use
   [Incremental], which tracks [passed] and [total] using references,
   and computes the ratio directly each Time_float_unix.

   You can run this implementation as follows.

   {v
    ./_build/default/exercises/main.exe server -port 8080 &
    ./_build/default/exercises/main.exe ex2 simple -port 8080
   v}

   The goal of this exercise is to write your own version that uses
   [Incremental]. The idea is to track [passed] and [total] as
   incremental values, and have the ratio be an [Incremental]
   computation on top of those values. Note that this is no faster
   than the original. The goal here is only to see how to set things
   up.

   {v
    ./_build/default/exercises/main.exe ex2 incremental -port 8080
   v}

*)

open! Core
open! Async
open! Import

let print_passed_ratio passed_ratio = printf "passed_ratio: %.2F\n" passed_ratio

module Simple = struct
  let passed_ratio ~total ~passed = passed // total

  let process_events (pipe : Event.t Pipe.Reader.t) =
    let total = ref 0 in
    let passed = ref 0 in
    let viewer = Viewer.create ~print:print_passed_ratio in
    Pipe.iter pipe ~f:(fun event ->
        match event.ev with
        | Host_info _ | Check (Register _) | Check (Unregister _) -> return ()
        | Check (Report { outcome; _ }) ->
            (match outcome with
            | Passed ->
                incr passed;
                incr total
            | Failed _ -> incr total);
            let result = passed_ratio ~total:!total ~passed:!passed in
            Viewer.update viewer result;
            return ())
end

module Incremental = struct
  let passed_ratio ~(total : int Incr.t) ~(passed : int Incr.t) : float Incr.t =
    let open Incr.Let_syntax in
    let%map passed = passed and total = total in
    passed // total

  let process_events (pipe : Event.t Pipe.Reader.t) =
    let total = Incr.Var.create 0 in
    let passed = Incr.Var.create 0 in
    let viewer = Viewer.create ~print:print_passed_ratio in
    let result =
      let ( ! ) = Incr.Var.watch in
      passed_ratio ~total:!total ~passed:!passed |> Incr.observe
    in
    Incr.Observer.on_update_exn result ~f:(function
      | Initialized x | Changed (_, x) -> Viewer.update viewer x
      | Invalidated -> assert false);
    Pipe.iter' pipe ~f:(fun eventq ->
        Queue.iter eventq ~f:(fun event ->
            match event.ev with
            | Host_info _ | Check (Register _) | Check (Unregister _) -> ()
            | Check (Report { outcome; _ }) -> (
                let incr i = Incr.Var.set i (1 + Incr.Var.value i) in
                match outcome with
                | Passed ->
                    incr passed;
                    incr total
                | Failed _ -> incr total));
        Incr.stabilize ();
        return ())
end

(* From here on in is just command-line specification. *)
let build_command ~summary process_events =
  Command.async ~summary
    (let open Command.Let_syntax in
     [%map_open
       let host, port = Command_common.host_and_port_param in
       fun () ->
         Command_common.connect_and_process_events ~process_events ~host ~port])

let simple =
  build_command ~summary:"Simple, all-at-once implementation"
    Simple.process_events

let incremental =
  build_command ~summary:"Incremental implementation" Incremental.process_events

let command =
  Command.group ~summary:"Exercise 2"
    [ ("simple", simple); ("incremental", incremental) ]
