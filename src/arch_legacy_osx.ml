(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.arch_osx.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* Mac OSX Arch-Specific Definitions *)
open BatteriesInit
open Hashtblinit

let registry_paths = ref [ "\\BGII - SoA\\" ]

(* no slash removal: helps make shell scripts work better *)
let slash_to_backslash s =
  s

let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
                                s

let native_separator string =
  Str.global_replace (Str.regexp "\\\\") "/" string

(* how to view a text file *)
(* let view_command = "open -a /Applications/TextEdit.app/"  *)
let view_command = "open"

let do_auto_update = true

(* handle AT_EXIT ~VIEW mydir\myfile.txt~ *)
let handle_view_command s skip =
  let result = ref s in
  List.iter (fun view_regexp ->
    if Str.string_match view_regexp !result 0 then begin
      result := Str.replace_first view_regexp view_command !result ;
      result := native_separator !result ;
    end) [Str.regexp_case_fold "^VIEW" ; Str.regexp_case_fold "^NOTEPAD" ;
          Str.regexp_case_fold "^EXPLORER"];
  if skip && (s <> !result) then result := "";
  let s = !result in
  s

external glob : string -> (string -> unit) -> unit = "myglob"

let create_process_env = Unix.create_process_env

let biff_path_separator = ":"

(* On OSX, TOB baldur.ini has:
   [Alias]
   CD5:=:CD5:
   HD0:=:
   CD1:=:CD1:
   CD2:=:CD2:
   CD3:=:CD3:
   CD4:=:CD4:
 *)
let cd_regexp = Str.regexp "\\(CD[0-9]\\)"
(* doesn't work:
   let cd_regexp = Str.regexp "^[CH]D[0-9]+.*=\\([^\r\n]*\\)"  *)

let is_weidu_executable f =
  try
    let i = Case_ins.perv_open_in_bin f in
    let buff = Bytes.create 4 in
    let signature = input i buff 0 4 in
    Str.string_match (Str.regexp_case_fold "setup-.*") f 0 && buff = "\xfe\xed\xfa\xce"
  with _ -> false

let get_version f =
  ignore (Unix.access f [ Unix.X_OK ]) ;
  let exec = Printf.sprintf "./%s --exit" f in
  Printf.printf "{%s} queried%!" f;
  let ic,oc,ec = Unix.open_process_full exec (Unix.environment()) in
  let line = input_line ic in
  ignore (Unix.close_process_full (ic,oc,ec));
  let version_regexp = Str.regexp ".*WeiDU version \\([0-9]+\\).*" in
  let s = Str.global_replace version_regexp "\\1" line in
  int_of_string s

external get_user_personal_dir : unit -> string = "get_user_home_dir"

let get_user_dir game_path =
  (get_user_personal_dir ()) ^ "/Documents"

let game_path_by_type name =
  failwith "--game-by-path not available on this architecture"
