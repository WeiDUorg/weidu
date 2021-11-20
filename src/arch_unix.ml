(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.arch_unix.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* Generic Unix Definitions *)
open BatteriesInit
open Hashtblinit

let registry_paths = ref [ "\\BGII - SoA\\" ]

(* no slash removal: helps make shell scripts work better *)
let slash_to_backslash s = s

let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
                                s

let native_separator string =
  Str.global_replace (Str.regexp "\\\\") "/" string

(* how to view a text (or HTML) file on 90% of linuxes *)
let view_command = "xdg-open ./"

let do_auto_update = true

(* handle AT_EXIT ~VIEW mydir\myfile.txt~ *)
let handle_view_command s skip =
  let result = ref s in
  List.iter (fun view_regexp ->
    if Str.string_match view_regexp !result 0 then begin
      result := Str.replace_first view_regexp view_command !result ;
      result := native_separator !result ;
    end) [Str.regexp_case_fold "^VIEW[ \t]*" ; Str.regexp_case_fold "^NOTEPAD[ \t]*" ;
          Str.regexp_case_fold "^EXPLORER[ \t]*"];
  if skip && (s <> !result) then result := "";
  let s = String.lowercase !result in
  s

let glob str fn = failwith "no globbing support"

let biff_path_separator = "\\\\" (* unix, but BG2 runs on Windows *)

let create_process_env = Unix.create_process_env

let cd_regexp = Str.regexp "^[CH]D[0-9]+.*=\\([^\r\n]*\\)"

let is_weidu_executable f =
  try
    let i = Case_ins.perv_open_in_bin f in
    let buff = Bytes.create 4 in
    let signature = input i buff 0 4 in
    Str.string_match (Str.regexp_case_fold "setup-.*") f 0 && buff = "\x7fELF"
  with _ -> false

let get_version f =
  ignore (Unix.access f [ Unix.X_OK ]) ;
  let newstdin, newstdin' = Unix.pipe () in
  let newstdout, newstdout' = Unix.pipe () in
  let newstderr, newstderr' = Unix.pipe () in
  let pid = create_process_env
      f [| "WeiDU-Backup" ; "--game bar" |] [| |] newstdin newstdout' newstderr'
  in
  if pid < 0 then failwith "invalid pid" ;
  Printf.printf "{%s} Queried (pid = %d)%!" f pid ;
  let ic = Unix.in_channel_of_descr newstdout in
  let line = input_line ic in
  (try Unix.close newstdin with _ -> ()) ;
  (try Unix.close newstdout with _ -> ()) ;
  (try Unix.close newstderr with _ -> ()) ;
  (try Unix.close newstdin' with _ -> ()) ;
  (try Unix.close newstdout' with _ -> ()) ;
  (try Unix.close newstderr' with _ -> ()) ;
  Unix.kill pid 9;
  let version =
    try
      let version_regexp = Str.regexp ".*WeiDU version \\([0-9]+\\).*" in
      let s = Str.global_replace version_regexp "\\1" line in
      int_of_string s
    with _ -> failwith "not weidu"
  in
  version

external get_user_personal_dir : unit -> string = "get_user_home_dir"

let get_user_dir game_path =
  (get_user_personal_dir()) ^ "/.local/share"

let game_path_by_type name =
  failwith "--game-by-path not available on this architecture"
