(* Similarly to exercise 3, we want to display information about failed checks.

   This time we'd like to display the descriptions of all tests that
   are currently failing.

   The tricky bit however comes from how we want to represent this:

   You could imagine having a 
   {v 
      string Check.Name.t Host.Name.t Map.t Incr.t
   v}

   to represent this, i.e. the outer map (keyed by hostname)
   contains a map from check name to description.

   But for the purpose of this exercise, let's represent this as a
   /flat/ map of type 

   {v 
      string (Check_name.t * Host.Name.t) Map.t Incr.t
   v}

*)

open! Core
open! Async
open! Import

let print_failure_descriptions c =
  print_s [%sexp (c : (Host.Name.t * Check.Name.t, string) Map.Poly.t)]

module Simple = struct
  let failed_checks (state : State.t) () =
    Map.fold ~init:Map.Poly.empty state.hosts
      ~f:(fun ~key:host_info ~data:(_, checks) acc ->
        Map.fold checks ~init:acc
          ~f:(fun ~key:check_name ~data:(_, outcome) acc ->
            match (outcome : Protocol.Check.Outcome.t option) with
            | None | Some Passed -> acc
            | Some (Failed description) ->
                Map.set acc ~key:(host_info, check_name) ~data:description))

  let process_events (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_failure_descriptions in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
        Queue.iter eventq ~f:(fun event -> state := State.update !state event);
        let update = Viewer.compute viewer (failed_checks !state) in
        Viewer.update viewer update;
        return ())
end

module Incremental = struct
  open! Incr.Let_syntax

  (* First, let's write a helper function that applies [f]
     incrementally to [inc] but keeps track of the input and output of
     the last time [f] ran. You will need to use a [ref] here. *)
  let diff_map (inc : 'a Incr.t) ~(f : old:('a * 'b) option -> 'a -> 'b) :
      'b Incr.t =
    let old = ref None in
    let%map i = inc in
    let r = f ~old:!old i in
    old := Some (i, r);
    r

  (* Next, let's write the function to flatten [State.t Incr.t] into a
     map keyed by [Host.Name.t * Check.Name.t]. (Incr_map has a
     [flatten] function built in, but we want to ignore that and write
     this from first principals.)

     The basic idea is to use [diff_map] to find all the keys that
     were added, removed or changed between the old and current input,
     and apply those changes to the old output to get the new output.

     Check out [Map.symmetric diff] for an efficient way of
     calculating diffs between maps. The goal here is to be
     incremental with respect to both the innter and outer map.  *)
  let flatten_maps (mm : State.t Incr.t) :
      ( Host.Name.t * Check.Name.t,
        Time_float_unix.t * Check.Outcome.t option,
        _ )
      Map.t
      Incr.t =
    diff_map (mm >>| State.hosts) ~f:(fun ~old new_input ->
        match old with
        | None ->
            Map.fold new_input ~init:Map.Poly.empty
              ~f:(fun ~key:host_name ~data:(_, checks) acc ->
                Map.fold checks ~init:acc
                  ~f:(fun ~key:check_name ~data:(time, outcome) acc ->
                    Map.set acc ~key:(host_name, check_name)
                      ~data:(time, outcome)))
        | Some (old_input, old_output) ->
            let changes =
              Map.symmetric_diff ~data_equal:phys_equal old_input new_input
            in
            let to_apply =
              Sequence.map changes ~f:(fun (host_name, data) ->
                  match data with
                  | `Left (_, checks) ->
                      `Remove
                        (Sequence.map (Map.to_sequence checks)
                           ~f:(fun (check_name, _) -> (host_name, check_name)))
                  | `Right (_, checks) ->
                      `Add
                        (Sequence.map (Map.to_sequence checks)
                           ~f:(fun (check_name, value) ->
                             ((host_name, check_name), value)))
                  | `Unequal ((_, old_checks), (_, new_checks)) ->
                      let diff =
                        Map.symmetric_diff
                          ~data_equal:(fun (t1, _) (t2, _) ->
                            Time_float.equal t1 t2)
                          old_checks new_checks
                      in
                      let remove, add, update =
                        Sequence.fold diff
                          ~init:(Sequence.empty, Sequence.empty, Sequence.empty)
                          ~f:(fun (r, a, u) delta ->
                            match delta with
                            | check_name, `Left _ ->
                                ( Sequence.append r
                                    (Sequence.singleton (host_name, check_name)),
                                  a,
                                  u )
                            | check_name, `Right v ->
                                ( r,
                                  Sequence.append a
                                    (Sequence.singleton
                                       ((host_name, check_name), v)),
                                  u )
                            | check_name, `Unequal (_, v) ->
                                ( r,
                                  a,
                                  Sequence.append u
                                    (Sequence.singleton
                                       ((host_name, check_name), v)) ))
                      in
                      `Update (remove, add, update))
            in
            Sequence.fold to_apply ~init:old_output ~f:(fun acc change ->
                match change with
                | `Remove keys_to_remove ->
                    Sequence.fold keys_to_remove ~init:acc ~f:(fun acc k ->
                        Map.remove acc k)
                | `Add key_values_to_add ->
                    Sequence.fold key_values_to_add ~init:acc
                      ~f:(fun acc (key, data) -> Map.set acc ~key ~data)
                | `Update (remove, add, update) ->
                    let acc =
                      Sequence.fold remove ~init:acc ~f:(fun acc k ->
                          Map.remove acc k)
                    in
                    let acc =
                      Sequence.fold add ~init:acc ~f:(fun acc (key, data) ->
                          Map.add_exn acc ~key ~data)
                    in
                    Sequence.fold update ~init:acc ~f:(fun acc (key, data) ->
                        Map.set acc ~key ~data)))

  (* Use [flatten_maps] here to compute the final result. *)
  let failed_checks (s : State.t Incr.t) :
      (Host.Name.t * Check.Name.t, string) Map.Poly.t Incr.t =
    Incr_map.filter_map (flatten_maps s) ~f:(fun (_, check_opt) ->
        match check_opt with
        | None | Some Passed -> None
        | Some (Failed desc) -> Some desc)

  (* The structure of process_events will be fairly similar to the
     corresponding function in exercise 3 *)
  let process_events (events : Event.t Pipe.Reader.t) : unit Deferred.t =
    let viewer = Viewer.create ~print:print_failure_descriptions in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (failed_checks (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(fun update ->
        match update with
        | Initialized x | Changed (_, x) -> Viewer.update viewer x
        | Invalidated -> assert false);
    Pipe.iter events ~f:(fun ev ->
        Incr.Var.set state (State.update (Incr.Var.value state) ev);
        Viewer.compute viewer Incr.stabilize;
        Deferred.return ())
end

(* Command line setup *)

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
  Command.group ~summary:"Exercise 6"
    [ ("simple", simple); ("incremental", incremental) ]
