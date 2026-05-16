(* Central state and counters for --dry-run.  This module intentionally avoids
   depending on Util so it can be used by low-level filesystem helpers. *)

let enabled = ref false

let copies = ref 0
let moves = ref 0
let deletes = ref 0
let mkdirs = ref 0
let execs = ref 0
let writes = ref 0
let metadata = ref 0
let renames = ref 0
let rmdirs = ref 0
let chmods = ref 0

let active () = !enabled

let record_copy () = incr copies
let record_move () = incr moves
let record_delete () = incr deletes
let record_mkdir () = incr mkdirs
let record_exec () = incr execs
let record_write () = incr writes
let record_metadata () = incr metadata
let record_rename () = incr renames
let record_rmdir () = incr rmdirs
let record_chmod () = incr chmods

let null_device =
  match Sys.os_type with
  | "Win32" -> "NUL"
  | _ -> "/dev/null"

let null_out_channel binary =
  if binary then open_out_bin null_device
  else open_out null_device

let null_out_gen flags perms =
  open_out_gen flags perms null_device

let plural n singular plural =
  if n = 1 then singular else plural

let total () =
  !copies + !moves + !deletes + !mkdirs + !execs + !writes +
  !metadata + !renames + !rmdirs + !chmods

let summary_string () =
  Printf.sprintf
    "DRY-RUN summary: %d cop%s, %d move%s, %d delet%s, %d mkdir%s, %d shell command%s, %d write%s, %d metadata write%s, %d rename%s, %d rmdir%s, %d chmod%s (total: %d operations suppressed)"
    !copies (plural !copies "y" "ies")
    !moves (plural !moves "" "s")
    !deletes (plural !deletes "e" "es")
    !mkdirs (plural !mkdirs "" "s")
    !execs (plural !execs "" "s")
    !writes (plural !writes "" "s")
    !metadata (plural !metadata "" "s")
    !renames (plural !renames "" "s")
    !rmdirs (plural !rmdirs "" "s")
    !chmods (plural !chmods "" "s")
    (total ())
