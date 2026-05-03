(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.util.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* generic utilities *)

open BatteriesInit
open Hashtblinit
open Arch

type local_string_entry = {
    lse_male : string ;
    lse_male_sound : string ;
    lse_female : string ;
    lse_female_sound : string ;
  }

type game_type = BGEE | BG2EE | IWDEE | PSTEE | GENERIC

let one_newline_regexp = Str.regexp "[\n]"
let one_newline_or_cr_regexp = Str.regexp "[\r\n]"
let many_newline_or_cr_regexp = Str.regexp "[\r\n]+"
let many_whitespace_regexp = Str.regexp "[ \t]+"
let many_whitespace_or_nl_regexp = Str.regexp "[ \t\r\n]+"
let many_not_whitespace_regexp = Str.regexp "[^ \t\n\r]+"
let many_cr_regexp = Str.regexp "\r\r*"

let log_line_separator = "TB#\"SPACE\""

let dos2unix = Str.global_replace many_cr_regexp "\r"

let errors_this_component = ref false

let value_of_option x = (match x with
| Some(s) -> s
| None -> failwith "value_of_option None")

let log_channel = ref None

let debug_ocaml = ref false

let debug_modder = ref false

let be_silent = ref false

(* Security / audit features *)
let dry_run = ref false
let allow_outside_gamedir = ref false
let audit_log_channel = ref (None : out_channel option)
let game_dir_for_audit = ref ""

(* Dry-run operation counters *)
let dry_run_copies  = ref 0
let dry_run_moves   = ref 0
let dry_run_deletes = ref 0
let dry_run_mkdirs  = ref 0
let dry_run_execs   = ref 0

(* Print a visible console warning for every shell command invoked *)
let warn_shell = ref false

(* Audit log path control *)
let no_audit_log = ref false
let audit_log_path_override = ref (None : string option)

(* NDJSON audit log *)
let audit_log_json = ref false
let audit_log_json_channel = ref (None : out_channel option)

(* Strict hashing policy and security counters *)
let require_sha256 = ref false
let strict_path_risk = ref false
let security_outside_warnings = ref 0
let security_sensitive_warnings = ref 0
let security_high_risk_warnings = ref 0
let security_high_risk_blocked = ref 0
let security_shell_commands = ref 0
let security_md5_fallbacks = ref 0
let security_hash_mismatches = ref 0

(* Current TP2/component context — updated before each component install *)
let audit_component_context = ref ""

let log_or_print fmt =
  let k result =
    (match !log_channel with
    | None -> print_string result ; flush stdout
    | Some(o) -> output_string o result ; flush o) in
  Printf.kprintf k fmt

let log_or_print_modder fmt =
  let k result =
    if !debug_modder then (match !log_channel with
    | None -> print_string result ; flush stdout
    | Some(o) -> output_string o result ; flush o) in
  Printf.kprintf k fmt

let log_only fmt =
  let k result =
    (match !log_channel with
    | None -> ()
    | Some(o) -> output_string o result ; flush o) in
  Printf.kprintf k fmt

let log_only_modder fmt =
  let k result =
    if !debug_modder then (match !log_channel with
    | None -> ()
    | Some(o) -> output_string o result ; flush o) in
  Printf.kprintf k fmt

let read_line_with_bell () =
  print_string "\007";
  flush stdout;
  read_line ()

let log_and_print fmt =
  let k result = begin
    if not !be_silent then (output_string stdout result ; flush stdout ) ;
    (match !log_channel with
    | None -> ()
    | Some(o) -> output_string o result ; flush o)
  end in
  Printf.kprintf k fmt

let log_and_print_modder fmt =
  let k result = begin
    if not !be_silent && !debug_modder then (output_string stdout result ;
                                             flush stdout ) ;
    if !debug_modder then (match !log_channel with
    | None -> ()
    | Some(o) -> output_string o result ; flush o)
  end in
  Printf.kprintf k fmt

let audit_log fmt =
  let k result =
    let t = Unix.localtime (Unix.gettimeofday ()) in
    let ts = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
      (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
      t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec in
    let ctx = !audit_component_context in
    (* Chain value is computed in Hash to keep all digest logic centralized. *)
    let chain = Hash.next_audit_chain ts ctx result in
    (match !audit_log_channel with
    | None -> ()
    | Some o ->
        let ctx_str = if ctx <> "" then Printf.sprintf " {%s}" ctx else "" in
      output_string o (Printf.sprintf "[%s]%s [chain=%s] %s\n" ts ctx_str chain result) ;
        flush o) ;
    (match !audit_log_json_channel with
    | None -> ()
    | Some o ->
        (* minimal JSON escaping for the message field *)
        let json_escape s =
          let buf = Buffer.create (String.length s) in
          String.iter (fun c -> match c with
            | '"'  -> Buffer.add_string buf "\\\""
            | '\\' -> Buffer.add_string buf "\\\\"
            | '\n' -> Buffer.add_string buf "\\n"
            | '\r' -> Buffer.add_string buf "\\r"
            | '\t' -> Buffer.add_string buf "\\t"
            | c    -> Buffer.add_char buf c) s ;
          Buffer.contents buf
        in
        (* extract first token as "op" (everything before first colon or space) *)
        let op = try
          let i = String.index result ':' in
          String.sub result 0 i
        with Not_found -> result in
        let ctx_json = if ctx <> "" then
          Printf.sprintf ",\"ctx\":\"%s\"" (json_escape ctx)
        else "" in
        output_string o
          (Printf.sprintf "{\"ts\":\"%s\",\"chain\":\"%s\",\"op\":\"%s\",\"msg\":\"%s\"%s}\n"
             ts (json_escape chain) (json_escape op) (json_escape result) ctx_json) ;
        flush o)
  in
  Printf.kprintf k fmt

let security_summary_string () =
  Printf.sprintf
    "Security summary: outside-gamedir warnings=%d, sensitive-path warnings=%d, high-risk-path warnings=%d, high-risk blocked=%d, shell commands=%d, md5 fallbacks=%d, tp2 hash mismatches=%d"
    !security_outside_warnings
    !security_sensitive_warnings
    !security_high_risk_warnings
    !security_high_risk_blocked
    !security_shell_commands
    !security_md5_fallbacks
    !security_hash_mismatches

(* Resolve . and .. in a slash-normalised path without filesystem access. *)
let canonicalize_path path =
  let norm p = Str.global_replace (Str.regexp "\\\\") "/" p in
  let full =
    let p = norm path in
    if Filename.is_relative p then
      let base = norm (if !game_dir_for_audit <> "" then !game_dir_for_audit
                       else Sys.getcwd ()) in
      base ^ "/" ^ p
    else p
  in
  let parts = String.split_on_char '/' full in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "" :: rest when acc <> [] -> resolve acc rest
    | "." :: rest -> resolve acc rest
    | ".." :: rest ->
        (match acc with
        | [] | [""] | [_] -> resolve acc rest
        | _ :: prev -> resolve prev rest)
    | p :: rest -> resolve (p :: acc) rest
  in
  String.concat "/" (resolve [] parts)

(* Warn when a path (absolute or relative) resolves outside the game directory. *)
let warn_outside_gamedir path =
  if not !allow_outside_gamedir && !game_dir_for_audit <> "" then begin
    let norm p =
      let p = Str.global_replace (Str.regexp "\\\\") "/" p in
      Str.global_replace (Str.regexp "/+$") "" p
    in
    let canon_path = String.lowercase_ascii (norm (canonicalize_path path)) in
    let canon_gd   = String.lowercase_ascii (norm (canonicalize_path !game_dir_for_audit)) in
    let prefix = canon_gd ^ "/" in
    if canon_path <> canon_gd &&
       not (String.length canon_path >= String.length prefix &&
            String.sub canon_path 0 (String.length prefix) = prefix) then begin
      incr security_outside_warnings ;
      log_or_print
        "WARNING: file operation targets path outside game directory:\n\
        \  Path: [%s]\n\
        \  Game: [%s]\n\
        \  Use --allow-outside-gamedir to suppress this warning.\n"
        path !game_dir_for_audit ;
      audit_log "WARN outside-gamedir: [%s]" path ;
      let env_or_empty name = try Sys.getenv name with _ -> "" in
      let home = String.lowercase_ascii (norm (env_or_empty "HOME")) in
      let userprofile = String.lowercase_ascii (norm (env_or_empty "USERPROFILE")) in
      let weidu_os_raw = String.lowercase_ascii (env_or_empty "WEIDU_OS") in
      let string_contains haystack needle =
        try
          ignore (Str.search_forward (Str.regexp_string needle) haystack 0) ;
          true
        with Not_found -> false
      in
      (* WEIDU_OS is treated as a hint, with runtime fallback when missing/unknown. *)
      let detected_os =
        if weidu_os_raw <> "" then begin
          if string_contains weidu_os_raw "win" then "windows"
          else if string_contains weidu_os_raw "mac" ||
                  string_contains weidu_os_raw "darwin" ||
                  string_contains weidu_os_raw "osx" then "macos"
          else if string_contains weidu_os_raw "linux" ||
                  string_contains weidu_os_raw "unix" then "linux"
          else if Sys.os_type = "Win32" then "windows"
          (* /System/Library exists on macOS and not on Linux; reliable without
             spawning a subprocess and independent of compile-time Arch constants *)
          else if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "macos"
          else "linux"
        end else if Sys.os_type = "Win32" then "windows"
        else if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "macos"
        else "linux"
      in
      let is_prefix base target =
        base <> "" &&
        (target = base ||
         (String.length target > String.length base &&
          String.sub target 0 (String.length base) = base &&
          target.[String.length base] = '/')) in
      let sensitive_reason = ref "" in
      let high_risk_reason = ref "" in
      (match detected_os with
      | "windows" ->
          if is_prefix userprofile canon_path then
            sensitive_reason := "user profile" ;
          if !sensitive_reason = "" &&
             (is_prefix (String.lowercase_ascii (norm (env_or_empty "APPDATA"))) canon_path ||
              is_prefix (String.lowercase_ascii (norm (env_or_empty "LOCALAPPDATA"))) canon_path) then
            sensitive_reason := "application data" ;
          if Str.string_match (Str.regexp "^[a-z]:/*$") canon_path 0 then
            high_risk_reason := "drive root"
          else if Str.string_match (Str.regexp "^[a-z]:/windows\(/.*\)?$") canon_path 0 then
            high_risk_reason := "windows system directory"
          else if Str.string_match (Str.regexp "^[a-z]:/program files\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^[a-z]:/program files (x86)\(/.*\)?$") canon_path 0 then
            high_risk_reason := "program files"
          else if Str.string_match (Str.regexp "^[a-z]:/programdata\(/.*\)?$") canon_path 0 then
            high_risk_reason := "programdata"
      | "macos" ->
          if Str.string_match (Str.regexp "^/users/[^/]+\(/.*\)?$") canon_path 0 ||
             is_prefix home canon_path then
            sensitive_reason := "user home" ;
          if Str.string_match (Str.regexp "^/system\(/.*\)?$") canon_path 0 then
            high_risk_reason := "system directory"
          else if Str.string_match (Str.regexp "^/library\(/.*\)?$") canon_path 0 then
            high_risk_reason := "library directory"
          else if Str.string_match (Str.regexp "^/applications\(/.*\)?$") canon_path 0 then
            high_risk_reason := "applications directory"
          else if Str.string_match (Str.regexp "^/usr\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^/bin\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^/sbin\(/.*\)?$") canon_path 0 then
            high_risk_reason := "os binaries"
          else if canon_path = "/" then
            high_risk_reason := "filesystem root"
      | _ ->
          if Str.string_match (Str.regexp "^/home/[^/]+\(/.*\)?$") canon_path 0 ||
             is_prefix home canon_path || canon_path = "/root" ||
             Str.string_match (Str.regexp "^/root/.*$") canon_path 0 then
            sensitive_reason := "user home/root" ;
          if Str.string_match (Str.regexp "^/etc\(/.*\)?$") canon_path 0 then
            high_risk_reason := "system configuration"
          else if Str.string_match (Str.regexp "^/usr\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^/bin\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^/sbin\(/.*\)?$") canon_path 0 then
            high_risk_reason := "os binaries"
          else if Str.string_match (Str.regexp "^/lib\(/.*\)?$") canon_path 0 ||
                  Str.string_match (Str.regexp "^/lib64\(/.*\)?$") canon_path 0 then
            high_risk_reason := "system libraries"
          else if Str.string_match (Str.regexp "^/boot\(/.*\)?$") canon_path 0 then
            high_risk_reason := "boot files"
          else if canon_path = "/" then
            high_risk_reason := "filesystem root") ;
      if !sensitive_reason <> "" then begin
        incr security_sensitive_warnings ;
        log_or_print
          "WARNING: target path is sensitive (%s, os=%s): [%s]\n"
          !sensitive_reason detected_os path ;
        audit_log "WARN sensitive-path(%s): [%s] reason=%s"
          detected_os path !sensitive_reason
      end ;
      if !high_risk_reason <> "" then begin
        incr security_high_risk_warnings ;
        log_or_print
          "WARNING: target path is HIGH-RISK (%s, os=%s): [%s]\n"
          !high_risk_reason detected_os path ;
        audit_log "WARN high-risk-path(%s): [%s] reason=%s"
          detected_os path !high_risk_reason ;
        if !strict_path_risk then begin
          incr security_high_risk_blocked ;
          exit_status := StatusInstallFailure ;
          log_or_print
            "ERROR: blocked by --strict-path-risk (%s, os=%s): [%s]\n"
            !high_risk_reason detected_os path ;
          audit_log "ERROR high-risk-path-blocked(%s): [%s] reason=%s"
            detected_os path !high_risk_reason ;
          failwith "blocked by --strict-path-risk"
        end
      end
    end
  end

let print_backtrace = ref false

let printexc_to_string e =
  if !print_backtrace then
    Printexc.to_string e ^ "\n" ^
    Printexc.get_backtrace()
  else
    Printexc.to_string e

let set_errors file line =
  if !debug_ocaml then log_and_print "Warning at %s.%d\n" file line ;
  errors_this_component := true

let recursive_mkdir directory mode =
  let dir_split = Str.split (Str.regexp "[/\\]") directory in
  let added_up_dir = ref "" in
  let skip_first_slash = ref false in
  if String.length directory > 0 &&
     (String.get directory 0 = '\\' || String.get directory 0 = '/') then
    skip_first_slash := true ;
  List.iter (fun part ->
    added_up_dir := !added_up_dir ^ (if !skip_first_slash then "/" else "") ^
      part ;
    skip_first_slash := true ;
(*        log_and_print "MKDIR %s\n" !added_up_dir ; *)
    (try
      Case_ins.unix_mkdir !added_up_dir mode ;
    with e -> (match e with
    | Unix.Unix_error(Unix.EEXIST,_,_) -> ()
    | _ -> log_and_print "Problem %s on %s: util.ml\n"
          (printexc_to_string e) !added_up_dir))) dir_split

let inlined_files = Hashtbl.create 15

let clear_inlined () =
  let arr_spec = Hashtbl.copy inlined_files in
  Hashtbl.iter (fun a b -> Hashtbl.remove inlined_files a) arr_spec ;
  Hashtbl.clear inlined_files ;
  List.iter (fun (name,contents) ->
    Hashtbl.add inlined_files name contents) Tph.builtin_inlined_files
;;

List.iter (fun (name,contents) ->
  Hashtbl.add inlined_files name contents) Tph.builtin_inlined_files

let backup_ht = Hashtbl.create 511
let backup_dir = ref None
let backup_list_chn = ref None
let mappings_list_chn = ref None
let move_list_chn = ref None
let other_list_chn = ref None

let set_backup_dir str i =
  let i = Printf.sprintf "%d" i in
  let backup_dir_name = Printf.sprintf "%s/%s" str i in
  recursive_mkdir backup_dir_name 511 ; (* 511 = octal 0777 = a+rwx *)
  backup_dir := Some(backup_dir_name) ;
  (match !backup_list_chn with
  | Some(c) -> close_out c
  | None -> ()) ;
  (match !mappings_list_chn with
  | Some(c) -> close_out c
  | None -> ()) ;
  (match !move_list_chn with
  | Some(c) -> close_out c
  | None -> ()) ;
  (match !other_list_chn with
  | Some(c) -> close_out c
  | None -> ()) ;
  let backup_filename = (backup_dir_name ^ "/UNINSTALL." ^ i) in
  let mappings_filename = (backup_dir_name ^ "/MAPPINGS." ^ i) in
  let move_filename = (backup_dir_name ^ "/MOVE." ^ i) in
  let other_filename = (backup_dir_name ^ "/OTHER." ^ i) in
  Hashtbl.clear backup_ht ;
  (try
    backup_list_chn := Some(Case_ins.perv_open_out_bin backup_filename) ;
    mappings_list_chn := Some(Case_ins.perv_open_out_bin mappings_filename) ;
    move_list_chn := Some(Case_ins.perv_open_out_bin move_filename) ;
    other_list_chn := Some(Case_ins.perv_open_out_bin other_filename) ;
  with e ->
    log_and_print "WARNING: unable to open [%s]: %s
      Will be unable to UNINSTALL later.\n"
       backup_filename (printexc_to_string e))

let log_file = ref ""
let append_to_log = ref false
let log_extern = ref false

let init_log version filename =
  (try
    let out =
      if !append_to_log then
        Case_ins.perv_open_out_gen [ Open_append ; Open_creat ; Open_text ]
          0o777 filename
      else Case_ins.perv_open_out filename in
    log_channel := Some(out) ;
    log_file := filename ;
    log_only "WeiDU v %s Log\n\n" version ;
    Array.iter (fun s -> log_only " %s" s) Sys.argv ;
    log_only "\n"
  with e ->
    Printf.printf "WARNING: unable to open log file [%s]: %s"
      filename (printexc_to_string e) ;
    ())

let int32_of_str_off str off =
  let d = Int32.of_int (Char.code str.[off+0]) in
  let c = Int32.of_int (Char.code str.[off+1]) in
  let b = Int32.of_int (Char.code str.[off+2]) in
  let a = Int32.of_int (Char.code str.[off+3]) in
  Int32.logor
    (Int32.logor (Int32.shift_left a 24)
       (Int32.shift_left b 16))
    (Int32.logor (Int32.shift_left c 8) (d))

(*
  let int_of_str_off str off =
  let d = Char.code str.[off+0] in
  let c = Char.code str.[off+1] in
  let b = Char.code str.[off+2] in
  let a = Char.code str.[off+3] in
  (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
 *)

let int_of_str_off str off = Int32.to_int (int32_of_str_off str off)

let int_of_str str = int_of_str_off str 0

let short_of_str_off str off =
  let d = Char.code str.[off+0] in
  let c = Char.code str.[off+1] in
  (c lsl 8) lor d

let byte_of_str_off str off =
  let d = Char.code str.[off] in
  d

let signed_byte_of d =
  if d > 127 then
    d - 256
  else d

let signed_short_of d =
  if d > 32767 then
    d - 65536
  else
    d

let short_of_str str = short_of_str_off str 0

let str_of_int32 i =
  let d = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let c = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let b = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let a = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let result = Bytes.make 4 (Char.chr a) in
  Bytes.set result 0 (Char.chr d) ;
  Bytes.set result 1 (Char.chr c) ;
  Bytes.set result 2 (Char.chr b) ;
  result

let str_of_int i =
  str_of_int32 (Int32.of_int i)

let str_of_short i =
  let d = i land 255 in
  let i = i lsr 8 in
  let c = i land 255 in
  let i = i lsr 8 in
  let result = Bytes.make 2 (Char.chr d) in
  Bytes.set result 1 (Char.chr c) ;
  result

let str_of_byte i =
  let d = i land 255 in
  let i = i lsr 8 in
  let result = String.make 1 (Char.chr d) in
  result

let str_to_exact_size str size =
  let dest = String.make size '\000' in
  let max =
    if String.length str > size then
      size
    else
      String.length str in
  String.blit str 0 dest 0 max ;
  dest

let write_int buff off value =
  String.blit (str_of_int (value)) 0 buff off 4
let write_int32 buff off value =
  String.blit (str_of_int32 (value)) 0 buff off 4
let write_short buff off value =
  String.blit (str_of_short (value)) 0 buff off 2
let write_byte buff off value =
  if value < 0 then
    Bytes.set buff off (Char.chr (256+value))
  else
    Bytes.set buff off (Char.chr value)
let write_resref buff off str =
  String.blit (str_to_exact_size str 8) 0 buff off 8


let get_string_of_size str off size =
  let almost = String.sub str off size in
  (try
    let null_index = String.index almost '\000' in
    String.sub almost 0 null_index
  with _ -> almost)

let my_write size fd buff name =
  let sofar = ref 0 in
  while !sofar < size do
    let this_chunk = Unix.write fd buff !sofar (size - !sofar) in
    if this_chunk = 0 then begin
      failwith (Printf.sprintf "write %d of %d bytes from [%s]"
                  !sofar size name)
    end else
      sofar := !sofar + this_chunk
  done

let my_read size fd buff name =
  let sofar = ref 0 in
  while !sofar < size do
    let this_chunk = Unix.read fd buff !sofar (size - !sofar) in
    if this_chunk = 0 then begin
      failwith (Printf.sprintf "read %d of %d bytes from [%s]"
                  !sofar size name)
    end else
      sofar := !sofar + this_chunk
  done


let file_size name =
  (try
    let stats = Case_ins.unix_stat name in
    stats.Unix.st_size
  with _ ->  -1)

let file_exists name =
  (try
    let stats = Case_ins.unix_stat64 name in
    stats.Unix.LargeFile.st_size >= Int64.zero
  with _ -> false)

let file_contains_data name =
  (try
    let stats = Case_ins.unix_stat64 name in
    stats.Unix.LargeFile.st_size > Int64.zero
  with _ -> false)

let is_directory name =
  (try
    let stats = Case_ins.unix_stat64 name in
    let res = stats.Unix.LargeFile.st_kind = Unix.S_DIR in
    (* log_only "%s is a directory: %b\n" name res ;  *)
    res
  with _ -> false)

let split name =
  (try
    let base = Case_ins.filename_chop_extension name in
    let ext = String.sub name ((String.length base)+1)
        ((String.length name) - ((String.length base)+1)) in
    base,ext
  with _ -> name,"")

let split_resref name =
  let rec fold acc lst =
    (match lst with
    | ext :: [] -> acc, ext
    | item :: tail -> fold (acc ^ "." ^ item) tail) in
  (try
    let parts = String.split_on_char '.' name in
    (match parts with
    | res :: ext :: [] -> (* foo.bar or .foo *)
        res, ext
    | res :: middle :: tail -> (* foo.bar.baz etc *)
        fold res (middle :: tail)
    | res :: [] -> (* foo (no ext) *)
        name, ""
    | _ -> log_and_print "WARNING: unable to split resref [%s]\n" name ;
        name, "")
  with _ -> log_and_print "WARNING: unable to split resref [%s]\n" name ;
    name, "")


let my_unlink file =
  begin
    try
      Case_ins.unix_unlink file
    with e ->
      log_only "Unable to Unlink [%s]: %s\n"
        file (printexc_to_string e)
  end

let my_rmdir dir =
  begin
    try
      Case_ins.unix_rmdir dir
    with e ->
      log_only "Unable to Rmdir [%s]: %s\n"
        dir (printexc_to_string e)
  end

let handle_readonly filename =
  if file_exists filename then (* if it already exists *)
    begin (* handle read-only files! *)
      try
        Case_ins.unix_chmod filename 511 ; (* 511 = octal 0777 = a+rwx *)
      with e -> ()
          (* log_or_print "WARNING: chmod %s : %s\n" filename
             (printexc_to_string e) *)
    end

let rec backup_if_extant filename =
  if Hashtbl.mem backup_ht
      (String.uppercase (native_separator filename)) then
    ()
  else begin
    if (String.uppercase filename) = "OVERRIDE/SPELL.IDS" ||
    (String.uppercase filename) = "OVERRIDE\\SPELL.IDS" then begin
      if not (file_exists "override/spell.ids.installed") then begin
        backup_if_extant "override/spell.ids.installed" ;
        let out_chn = Case_ins.perv_open_out_bin
            "override/spell.ids.installed" in
        output_string out_chn "spell.ids edits installed\n" ;
        close_out out_chn ;
      end
    end ;
    Hashtbl.add backup_ht
      (String.uppercase (native_separator filename)) true ;
    (match !backup_list_chn with
    | Some(chn) -> output_string chn (filename ^ "\n") ; flush chn
    | None -> ()) ;
    (match !backup_dir with
    | Some(dir) when file_exists filename -> begin
        let name = filename in
        let out = dir ^ "/" ^ (Str.global_replace (Str.regexp "[\\/:]")
                                 "." filename) in
        let out1 = dir ^ "/" ^ Case_ins.filename_basename filename in
        let where = ref "" in
        (try
          if file_exists out1 then
            where := out
          else
            where := out1 ;
          (match !mappings_list_chn with
          | Some(chn) -> output_string chn (filename ^ log_line_separator ^
                                            !where ^ "\n") ; flush chn
          | None -> ()) ;
          copy_large_file name !where "creating a backup"
        with e ->
          log_and_print "ERROR: error copying [%s]\n" name ;
          raise e)
       end
    | _ -> ())
  end

and copy_large_file name out reason =
(*  log_or_print "Copying a large file: %s to %s\n" name out ; *)
  try begin
    Stats.time "loading files" (fun () ->
      let stats = Case_ins.unix_stat64 name in
      let size = stats.Unix.LargeFile.st_size in
      if size = 0L then
        log_or_print_modder "WARNING: [%s] is a 0 byte file\n" name
      else if size < 0L then begin
        log_and_print "ERROR: [%s] has reported size %Ld\n" name size ;
        failwith ("error loading " ^ name)
      end ;
      if file_exists out then my_unlink out ;
      begin
        let in_fd  = Case_ins.unix_openfile name [Unix.O_RDONLY] 0 in
        let out_fd = Case_ins.unix_openfile out
            [Unix.O_WRONLY ; Unix.O_CREAT] 511 in
        let chunk_size = 10240 in
        let chunk = Bytes.create chunk_size in
        let sofar = ref 0L in
        while !sofar < size do
          (* Int64.min was not introduced until OCaml 4.13 *)
          let remaining = (Int64.sub size !sofar) in
          let chunk_size =
            if remaining > (Int64.of_int chunk_size) then
              chunk_size else (Int64.to_int remaining)
          in
          my_read chunk_size in_fd chunk name ;
          my_write chunk_size out_fd chunk out ;
          sofar := (Int64.add !sofar (Int64.of_int chunk_size)) ;
        done ;
        (try
          Unix.close in_fd ;
          Unix.close out_fd ;
        with e ->
          log_and_print "ERROR: copy_large_file failed to close %s or %s\n"
            name out ;
          raise e) ;
        log_only "%s copied to %s, %Ld bytes\n" name out size ;
      end) ()
  end
  with e ->
    log_and_print "ERROR: error copying [%s]\n" name ;
    raise e

let load_file name =
  if Hashtbl.mem inlined_files name then
    String.copy (Hashtbl.find inlined_files (Arch.backslash_to_slash name))
  else
    try begin
      Stats.time "loading files" (fun () ->
        let stats = Case_ins.unix_stat name in
        let size = stats.Unix.st_size in
        if size = 0 then
          log_or_print_modder "WARNING: [%s] is a 0 byte file\n" name
        else if size < 0 then begin
          log_and_print "ERROR: [%s] has reported size %d\n" name size ;
          failwith ("error loading " ^ name)
        end else if size > Sys.max_string_length then begin
          log_and_print "ERROR: [%s] has size %d: TOO BIG FOR WEIDU (max %d)\n"
            name size Sys.max_string_length ;
          failwith ("error loading " ^ name)
        end ;
        let buff = String.make size '\000' in
        let fd = Case_ins.unix_openfile name [Unix.O_RDONLY] 0 in
        my_read size fd buff name ;
        (try
          Unix.close fd ;
        with e ->
          log_and_print "ERROR: load_file failed to close %s\n" name ;
          raise e) ;
        log_only "[%s] loaded, %d bytes\n" name size ;
        buff) () ;
    end
    with e ->
      log_and_print "ERROR: error loading [%s]\n" name ;
      raise e

let list_of_files_in_directory d =
  let result = ref [] in
  (try
    let dh = Case_ins.unix_opendir d in
    begin
      try
        while true do
          result := Unix.readdir dh :: !result
        done
      with _ -> ()
    end ;
    Unix.closedir dh
  with _ -> ()) ; !result

let modder_check_file_exists : (string -> unit) ref = ref (fun s ->
  failwith "Util.modder_check_file_exists not initialized")

let open_for_writing_internal backup filename binary =
  !modder_check_file_exists filename ;
  if (backup) then backup_if_extant filename else
  (match !other_list_chn with
   | Some(chn) -> output_string chn (filename ^ "\n") ; flush chn
   | None -> ()) ;
  let dir = Filename.dirname filename in
  if dir <> "" && not (is_directory dir) then
    recursive_mkdir dir 511 ;
  if file_exists filename then (* if it already exists *)
    begin (* handle read-only files! *)
      try
        Case_ins.unix_chmod filename 511 ; (* 511 = octal 0777 = a+rwx *)
      with e -> ()
          (* log_or_print "WARNING: chmod %s : %s\n" filename
             (printexc_to_string e) *)
    end ;
  let out_chn = (if binary then Case_ins.perv_open_out_bin else
  Case_ins.perv_open_out) filename in
  out_chn

let open_for_writing = open_for_writing_internal true

let record_other_file_op filename =
  match !other_list_chn with
  | Some chn -> output_string chn (filename ^ "\n") ; flush chn
  | None -> ()

(* filter to avoid logging progress bars from external programs *)
type filter_mode = Copy | Strip ;;
let create_filter = function () ->
  let mode = ref Copy in
  let buf = Buffer.create 80 in
  let f = function str ->
    let str = Str.global_replace (Str.regexp "\r\n") "\n" str in
    let strlen = String.length str in
    let pos = ref 0 in
    while !pos < strlen do
      (match !mode with
      | Copy  ->
          let newpos = (try String.index_from str !pos '\r'
          with Not_found -> strlen) in
          Buffer.add_substring buf str !pos (newpos - !pos) ;
          if newpos != strlen then mode := Strip ;
          pos := newpos
      | Strip ->
          let newpos = (try String.index_from str !pos '\n'
          with Not_found -> strlen) in
          if newpos != strlen then mode := Copy ;
          pos := newpos)
    done ;
    let res = Buffer.contents buf in
    Buffer.clear buf ;
    res
  in f

let exec_command cmd exact =
  let cmd = if exact then cmd else Arch.slash_to_backslash cmd in
  incr security_shell_commands ;
  audit_log "EXEC: [%s]" cmd ;
  if !dry_run then begin
    log_or_print "[DRY-RUN] would execute: [%s]\n" cmd ;
    incr dry_run_execs ;
    Unix.WEXITED 0
  end else begin
    if !warn_shell then
      log_or_print "WARNING: mod is executing shell command: [%s]\n" cmd ;
    let ret = if !log_extern then
    begin
      (* copy stdout + stderr to logfile *)
      let proc_stdout = Unix.open_process_in (cmd ^ " 2>&1") in
      let s = Bytes.create 80 in
      let filter = create_filter () in
      begin
        try
          while true do
            let read = input proc_stdout s 0 80 in
            if read = 0 then raise End_of_file ;
            let text = Bytes.sub s 0 read in
            if not !be_silent then begin
              output_string stdout text ;
              flush stdout
            end ;
            log_only "%s" (filter text)
          done
        with _ -> ()
      end ;
      Unix.close_process_in proc_stdout
    end else Unix.system cmd
  in ret
  end

type execute_at_exit_type =
| Command of string * bool
| Fn of (unit) Lazy.t

let execute_at_exit = ref ([] : (execute_at_exit_type) list)

(* for some stupid reason these cannot be in the parser or the lexer *)

type input_context = {
    mutable line : int ;
    mutable col  : int ;
    mutable delta : int ;
    mutable filename : string ;
    mutable lexbuf : Lexing.lexbuf ;
    mutable warn_only : bool ;
  }
let context_stack = ref []
let ignore_context_error = ref false
let push_context filename lexbuf =
  let new_context = { line = 1 ; col = 0 ; delta = 0 ;
                      filename = filename ; lexbuf = lexbuf ;
                      warn_only = false } in
  context_stack := new_context :: !context_stack
let pop_context () = (match !context_stack with
| [] -> if not !ignore_context_error then
    log_and_print "ERROR: no current parsing context to pop!\n" ; ()
| hd::tl ->
    context_stack := List.tl !context_stack)
let the_context () = (match !context_stack with
| hd :: tl -> hd
| [] -> if not !ignore_context_error then
    log_and_print "ERROR: no current parsing context\n" ;
    failwith "no current parsing context")

let lex_init (file: string)
    (inchannel: in_channel) : Lexing.lexbuf =
  let lexbuf = Lexing.from_channel inchannel in
  push_context file lexbuf ;
  lexbuf

let lex_init_from_internal_string (file: string)
    (buff: string) : Lexing.lexbuf =
  let lexbuf = Lexing.from_string buff in
  let ctx = the_context () in
  let new_context = { line = ctx.line ; col = ctx.col - ctx.delta ;
                      delta = 0 ; filename = file ; lexbuf = lexbuf ;
                      warn_only = true } in
  context_stack := new_context :: !context_stack ;
  lexbuf

let lex_init_from_string (file: string)
    (buff: string) : Lexing.lexbuf =
  let lexbuf = Lexing.from_string buff in
  push_context file lexbuf ;
  lexbuf

let newline () =
  let c = the_context () in
  c.line <- c.line + 1 ;
  c.col <- 1 ;
  c.delta <- 0

let tab () =
  let c = the_context () in
  c.col <- c.col + 8 - (c.col mod 8)

(* let adj lb =
   let c = the_context () in
   c.lexbuf <- lb ;
   c.delta <- (Lexing.lexeme_end lb) - (Lexing.lexeme_start lb) ;
   c.col <- c.col + c.delta  *)

let str_adj lb =
  let c = the_context () in
  c.lexbuf <- lb ;
  let st= Lexing.lexeme lb in
  for i = 0 to (String.length st) - 1 do
    if st.[i] = '\n' then newline ()
    else begin
      c.col <- c.col + 1 ; c.delta <- c.delta + 1 ;
    end
  done

let adj = str_adj

let strip str =
  let len = String.length str in
  String.sub str 1 (len - 2)

let rec remove_trailing_space str =
  let len = String.length str in
  if len < 2 then str else
  match str.[0],str.[len - 1] with
  | ' ',' ' -> remove_trailing_space (String.sub str 1 (len - 2))
  | ' ',_   -> remove_trailing_space (String.sub str 1 (len - 1))
  | _  ,' ' -> remove_trailing_space (String.sub str 0 (len - 1))
  | _  ,_   -> str


let error_chn_ht = Hashtbl.create 11
let error_chn_base = ref "errors"
let get_error_chn sort =
  try
    Hashtbl.find error_chn_ht sort
  with Not_found ->
    let oc = Case_ins.perv_open_out (Printf.sprintf "%s/%s"
                                       !error_chn_base sort) in
    Hashtbl.add error_chn_ht sort oc ;
    oc

let error sort fmt =
  let k result =
    let oc = get_error_chn sort in
    output_string oc result ;
    log_and_print "%s" result ;
    flush oc ;
  in
  Printf.kprintf k fmt

let input_error_to_stdout = ref true

let parse_error_verbose = ref true

let input_error sort_msg msg =
  let c = the_context () in
  let near_text = Lexing.lexeme c.lexbuf in
  if !parse_error_verbose then (if !input_error_to_stdout then
    log_and_print
  else
    error "DLG")
      "\n[%s] %s %s at line %d column %d-%d\nNear Text: %s\n\t%s\n"
      c.filename
      sort_msg
      (if c.warn_only then "WARNING" else "ERROR")
      c.line (c.col - c.delta) (c.col-1) near_text msg ;
  raise Parsing.Parse_error

let lex_error msg = input_error "LEXER" msg
let parse_error msg = input_error "PARSE" msg

let my_int_of_string s =
  try int_of_string s
  with e -> parse_error "Not An Integer"

type parse_what =
  | File of string
  | String of string * string

(* big generic parsing function *)
let parse_file verbose what sort_of_file parse_lex_fun =
  let old_parse_error_verbose = !parse_error_verbose in
  parse_error_verbose := verbose ;
  let do_the_work filename lexbuf =
    let result = Stats.time sort_of_file
        (fun () -> parse_lex_fun lexbuf) () in
    pop_context () ;
    log_or_print_modder "[%s] parsed\n" filename ;
    result
  in
  let ans = (match what with
  | File(filename) ->
      if Hashtbl.mem inlined_files filename then begin
        let str = Hashtbl.find inlined_files filename in
        let lexbuf : Lexing.lexbuf = lex_init_from_string filename str in
        (try
          do_the_work filename lexbuf
        with e ->
          if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
              filename (printexc_to_string e) ;
          raise e)
      end else begin
        let inchan = Case_ins.perv_open_in filename in
        (try
          begin
            let lexbuf : Lexing.lexbuf = lex_init filename inchan in
            let res = do_the_work filename lexbuf in
            close_in inchan ;
            res
          end
        with e ->
          (try input_error "" (printexc_to_string e) with _ -> ()) ;
          pop_context () ;
          if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
              filename (printexc_to_string e) ;
          parse_error_verbose := old_parse_error_verbose ;
          close_in inchan ; raise e)
      end
  | String (filename,str) -> begin
      let lexbuf : Lexing.lexbuf = lex_init_from_string filename str in
      (try
        do_the_work filename lexbuf
      with e ->
        (try input_error "" (printexc_to_string e) with _ -> ()) ;
        pop_context () ;
        if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
            filename (printexc_to_string e) ;
        parse_error_verbose := old_parse_error_verbose ;
        raise e)
  end) in
  parse_error_verbose := old_parse_error_verbose ;
  ans

type exit_status_t =
  | StatusSuccess
  | StatusArgumentInvalid
  | StatusInstallFailure
  | StatusInstallWarning
  | StatusParseError
  | StatusAutoUpdateRetry
  | StatusArgumentWarning

let exit_status = ref StatusSuccess

let return_value status =
  (match status with
  | StatusSuccess -> 0
  | StatusArgumentInvalid -> 1
  | StatusInstallFailure -> 2
  | StatusInstallWarning -> 3
  | StatusParseError -> 4
  | StatusAutoUpdateRetry -> 5
  | StatusArgumentWarning -> 6)

let bool_xor a b =
  ((a && not b) || (not a && b))

let eval_pe_warn = ref true

let split_log_line line =
  let pieces = Str.split (Str.regexp log_line_separator) line in
  if List.length pieces = 2 then pieces else
  Str.split (Str.regexp " ") line

let deduplicate list =
  let table = Hashtbl.create (List.length list) in
  List.filter (fun x ->
    if Hashtbl.mem table x then
      false
    else begin
      Hashtbl.add table x true ;
      true
    end) list

let get_user_dir_from_lua file =
  if file_exists file then begin
    let buff = load_file file in
    let regexp = (Str.regexp_case_fold
                    "engine_name[ \t]+=[ \t]+\"\\(.*\\)\"") in
    (try
      ignore (Str.search_forward regexp buff 0) ;
      Some (Str.matched_group 1 buff)
    with Not_found -> None)
  end else
    None

let get_ee_user_dir game_path default =
  let path = get_user_dir_from_lua (Arch.native_separator (game_path ^
                                                           "/engine.lua")) in
  (match path with
  | Some path -> Arch.native_separator ((Arch.get_user_dir game_path) ^
                                        "/" ^ path)
  | None -> Arch.native_separator ((Arch.get_user_dir game_path) ^
                                   "/" ^ default))

let get_user_dir game_path game_type =
  (match game_type with
  | GENERIC -> "."
  | BGEE -> get_ee_user_dir game_path "Baldur's Gate - Enhanced Edition"
  | BG2EE -> get_ee_user_dir game_path "Baldur's Gate II - Enhanced Edition"
  | IWDEE -> get_ee_user_dir game_path "Icewind Dale - Enhanced Edition"
  | PSTEE -> get_ee_user_dir game_path "Planescape Torment - Enhanced Edition")

let all_possible_tp2s filename =
  [(filename ^ "/" ^ filename ^ ".TP2") ;
   (filename ^ "/" ^ "SETUP-" ^ filename ^ ".TP2") ;
   (filename ^ ".TP2") ;
   ("SETUP-" ^ filename ^ ".TP2")]

let tp2_name filename =
  let chunk_list = Str.split (Str.regexp "[-]") filename in
  (match chunk_list with
  | a :: b when (String.uppercase a) = "SETUP" -> (match b with
    | c :: [] -> c
    | c -> (String.concat "-" c))
  | a :: b -> (String.concat "-" (a :: b))
  | _ -> filename)

let tp2_directory tp2_file =
  let parts = List.rev
      (String.split_on_char '/'
         (Str.global_replace
            (Str.regexp "\\\\") "/" tp2_file))
  in
  (match parts with
  | file :: dir :: _ when
      (String.equal
         (String.lowercase
            (Case_ins.filename_chop_extension
               (tp2_name
                  (Case_ins.filename_basename file))))
         (String.lowercase dir)) -> Some dir
  | _ -> None)

let read_file_name tp2_file directory =
  let files = Case_ins.sys_readdir directory in
  Array.fold_left (fun acc item ->
    if (String.equal (String.lowercase_ascii item)
          (String.lowercase_ascii tp2_file)) && not (is_directory item) then
      item else acc) tp2_file files

let read_directory_name dir_name directory =
  let dirs = Case_ins.sys_readdir directory in
  Array.fold_left (fun acc item ->
    if (String.equal (String.lowercase_ascii item)
          (String.lowercase_ascii dir_name)) &&
      is_directory (Arch.native_separator directory ^ "/" ^ item) then
      item else acc) dir_name dirs

let case_exact_tp_file tp_file =
  (match tp2_directory tp_file with
  | None ->
      read_file_name (Case_ins.filename_basename tp_file) "."
  | Some dir ->
      let tp2_name = (read_file_name
                        (Case_ins.filename_basename
                           tp_file) dir) in
      let tp2_dir = (read_directory_name dir ".") in
      Filename.concat tp2_dir tp2_name)

let read_lines file =
  let chan = Case_ins.perv_open_in file in
  let read chan = try Some (input_line chan) with End_of_file -> None in
  let rec loop list =
    match read chan with
    | Some line -> loop (line :: list)
    | None -> close_in chan ; List.rev list in
  loop []

let conf_filename game_path =
  Arch.native_separator (game_path ^ "/weidu.conf")

let load_conf game_path =
  let lines = read_lines (conf_filename game_path) in
  List.fold_left (fun acc line ->
    let parts = List.map String.trim (String.split_on_char '=' line) in
    (match parts with
    | "lang_dir" :: value :: [] -> Hashtbl.replace acc "lang_dir" value ; acc
    | "case_fold" :: value :: [] -> Hashtbl.replace acc "case_fold" value ; acc
    | "lowercase" :: value :: [] -> Hashtbl.replace acc "lowercase" value ; acc
    | _ -> acc)) (Hashtbl.create 5) lines

let save_conf game_path table =
  (try
    if (Hashtbl.length table) > 0 then begin
      let chan = Case_ins.perv_open_out_bin (conf_filename game_path) in
      Hashtbl.iter (fun key value ->
        ignore (output_string chan (Printf.sprintf "%s = %s\n" key value)))
        table ;
      ignore (close_out chan)
    end
  with e ->
    log_and_print "ERROR: unable to save weidu.conf because: %s\n"
      (printexc_to_string e))
