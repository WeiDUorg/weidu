(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util
open Version

let self_update_message = ref true

let unquote value =
  let length = String.length value in
  if length >= 2 && value.[0] = '"' && value.[length - 1] = '"' then
    String.sub value 1 (length - 2)
  else
    value

let executable_name () =
  let argv_0 = Case_ins.filename_basename (unquote Sys.argv.(0)) in
  let this,ext = try split_resref argv_0 with _ -> argv_0,"" in
  this ^ (if ext = "" then "" else ".exe")

let version_marker_prefix () =
  let marker = Versionmarker.marker in
  let suffix = version ^ "\000" in
  let prefix_length = String.length marker - String.length suffix in
  if prefix_length <= 0 ||
     String.sub marker prefix_length (String.length suffix) <> suffix then
    failwith
      "generated auto-update version marker does not match Version.version" ;
  String.sub marker 0 prefix_length

let read_declared_version filename =
  let prefix = version_marker_prefix () in
  let prefix_length = String.length prefix in
  let max_digits = String.length (string_of_int max_int) in
  let channel = Case_ins.perv_open_in_bin filename in
  let prefix_position = ref 0 in
  let versions = ref [] in
  let read_digits () =
    let buffer = Buffer.create max_digits in
    let valid = ref true in
    let finished = ref false in
    while not !finished do
      let character = input_char channel in
      if character = '\000' then
        finished := true
      else if character >= '0' && character <= '9' then begin
        if Buffer.length buffer < max_digits then Buffer.add_char buffer character
        else valid := false
      end else
        valid := false
    done ;
    if !valid && Buffer.length buffer > 0 then
      try versions := int_of_string (Buffer.contents buffer) :: !versions
      with Failure _ -> ()
  in
  let scan () =
    while true do
      let character = input_char channel in
      if character = prefix.[!prefix_position] then begin
        incr prefix_position ;
        if !prefix_position = prefix_length then begin
          read_digits () ;
          prefix_position := 0
        end
      end else
        prefix_position :=
          if character = prefix.[0] then 1 else 0
    done
  in
  try
    (try scan () with End_of_file -> ()) ;
    close_in channel ;
    match !versions with
    | [] -> None
    | declared :: rest when List.for_all (fun item -> item = declared) rest ->
        Some declared
    | _ -> None
  with e ->
    close_in_noerr channel ;
    raise e

let replace_file source target =
  let directory = Case_ins.filename_dirname target in
  let prefix = "." ^ Case_ins.filename_basename target ^ ".weidu-update-" in
  let temporary, output_channel =
    Filename.open_temp_file ~temp_dir:directory prefix ".tmp" in
  try
    let input_channel = Case_ins.perv_open_in_bin source in
    (try
      let buffer = Bytes.create 65536 in
      let rec copy () =
        let count = input input_channel buffer 0 (Bytes.length buffer) in
        if count > 0 then begin
          output output_channel buffer 0 count ;
          copy ()
        end in
      copy () ;
      close_in input_channel
    with e ->
      close_in_noerr input_channel ;
      raise e) ;
    flush output_channel ;
    Unix.fsync (Unix.descr_of_out_channel output_channel) ;
    close_out output_channel ;
    let permissions = (Case_ins.unix_stat source).Unix.st_perm in
    Case_ins.unix_chmod temporary permissions ;
    Case_ins.unix_rename temporary target
  with e ->
    close_out_noerr output_channel ;
    (try Case_ins.unix_unlink temporary with _ -> ()) ;
    raise e

let get_candidates current =
  let candidates = ref [] in
  (try
    let directory = Case_ins.unix_opendir "." in
    (try
      while true do
        let filename = Unix.readdir directory in
        if filename <> current && Arch.is_weidu_executable filename then
          candidates := filename :: !candidates
      done
    with End_of_file -> ()) ;
    Unix.closedir directory
  with _ -> ()) ;
  List.rev !candidates

let verify_latest _can_spawn =
  let current = executable_name () in
  let current_version = int_of_string version in
  let current_digest = Digest.file current in
  let candidates = get_candidates current in
  List.iter (fun target ->
    try
      let target_digest = Digest.file target in
      if target_digest <> current_digest then begin
        let declared_version = read_declared_version target in
        match declared_version with
        | Some target_version when target_version > current_version ->
            log_and_print
              "Not auto-updating [%s]: it declares WeiDU version %d, newer than the running version %d. Run that setup file directly if the newer version is trusted.\n"
              target target_version current_version
        | _ ->
            log_and_print "Auto-updating [%s] from the running WeiDU version %d: "
              target current_version ;
            replace_file current target ;
            log_and_print "done\n"
      end
    with e ->
      log_and_print "WARNING: could not auto-update [%s]: %s\n"
        target (printexc_to_string e)) candidates

let self () =
  let target = Unix.getenv "weiduautoupdate" in
  if target = "" ||
     String.contains target '/' ||
     String.contains target '\\' ||
     Case_ins.filename_basename target <> target ||
     not (Arch.is_weidu_executable target) then
    failwith
      (Printf.sprintf "refusing invalid legacy auto-update target [%s]" target) ;
  let silent =
    try
      let waus = Unix.getenv "weiduautoupdatesilent" in
      if !debug_ocaml then log_and_print "weiduautoupdatesilent = %s\n"
          waus ;
      if waus = "0" then false else true ;
    with _ -> (
      if !debug_ocaml then log_and_print "weiduautoupdatesilent is not set\n" ;
      false)
  in
  log_and_print "Auto-Updating on behalf of [%s]\n" target ;
  let source = executable_name () in
  log_and_print "\tCopying [%s] -> [%s]: " source target ;
  replace_file source target ;
  log_and_print "\nAuto-Updating on behalf of [%s] (done)\n" target ;

  try
    Sys.argv.(0) <- target ;
    let update_prefix = "weiduautoupdate=" in
    let silent_prefix = "weiduautoupdatesilent=" in
    let has_prefix string prefix =
      String.length string >= String.length prefix &&
      String.sub string 0 (String.length prefix) = prefix in
    let env = Array.of_list (
      List.filter (fun e ->
        not (has_prefix e update_prefix) &&
        not (has_prefix e silent_prefix)
                  )
        (Array.to_list (Unix.environment()))
     ) in
    Unix.execve target Sys.argv env;
  with _ ->
    if not silent then begin
      log_and_print
        "\n\n\t***********************************************************\n\tWeiDU has finished auto-updating all copies of itself\n\tin this directory. Please RE-RUN %s\n\tto actually install the mod.\n\t(sorry, I can't do it for you, Windows won't let me)\n" target ;
      (if not Myarg.good_terminal_p then (try ignore (read_line () ) with _ -> ()))
    end;
    exit (return_value StatusAutoUpdateRetry) ;
