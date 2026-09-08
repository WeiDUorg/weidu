(* Run against the built Stats module; see README.md in this directory. *)
open Hashtblinit

exception Expected_failure

let check enabled =
  Stats.debug_timings := enabled;
  Hashtbl.clear Stats.record;
  Hashtbl.clear Stats.extra;
  let initial_stack = !Stats.stack in
  let calls = ref 0 in
  let value = Stats.time "coarse" (fun () ->
    Stats.time_detail "detail" (fun value ->
      incr calls;
      Stats.inclusive_time "explicit" (fun () -> value) ()) 42) () in
  assert (value = 42 && !calls = 1);
  assert (!Stats.stack == initial_stack);
  assert (Hashtbl.mem Stats.record "coarse");
  assert (Hashtbl.mem Stats.record "detail" = enabled);
  assert (Hashtbl.mem Stats.extra "explicit");
  (try
    Stats.time "failed coarse" (fun () ->
      Stats.time_detail "failed detail" (fun () ->
        incr calls;
        Stats.time_detail "nested failure" (fun () ->
          raise Expected_failure) ()) ()) ();
    assert false
  with Expected_failure -> ());
  assert (!calls = 2);
  assert (!Stats.stack == initial_stack);
  assert (Hashtbl.mem Stats.record "failed coarse");
  assert (Hashtbl.mem Stats.record "failed detail" = enabled);
  assert (Hashtbl.mem Stats.record "nested failure" = enabled);
  (* A recovered failure must not poison subsequent timing scopes. *)
  assert (Stats.time "after failure" (fun () -> 7) () = 7);
  assert (!Stats.stack == initial_stack)

let () =
  assert (not !Stats.debug_timings);
  check false;
  check true;
  check false;
  print_endline "Stats tests passed"
