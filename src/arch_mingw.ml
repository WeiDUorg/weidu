(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.arch_mingw.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* MinGW Arch-Specific Definitions *)
open BatteriesInit
open Hashtblinit

(* Query the Windows Registry to find out about Infinity Engine games. *)

external registry_path : unit -> string = "get_bg2main_path"
external bg_registry_path : unit -> string = "get_bgmain_path"
external iwd_registry_path : unit -> string = "get_iwdmain_path"
external iwd2_registry_path : unit -> string = "get_iwd2main_path"
external pst_registry_path : unit -> string = "get_pstmain_path"

let registry_paths = ref [
  (registry_path ()) ;
  (bg_registry_path ()) ;
  (iwd_registry_path ()) ;
  (iwd2_registry_path ()) ;
  (pst_registry_path ()) ;
  "C:\\Program Files\\Black Isle\\BGII - SoA\\" ;
    "D:\\Program Files\\Black Isle\\BGII - SoA\\"
]

let slash_to_backslash s =
  Str.global_replace (Str.regexp_string "/") "\\\\" s

let backslash_to_slash (s: string) =
  s

let native_separator string =
  Str.global_replace (Str.regexp "/") "\\\\" string

(* how to view a text file *)
let view_command = "start"

let do_auto_update = true

(* handle AT_EXIT ~VIEW mydir\myfile.txt~ *)
let handle_view_command s skip =
  let result = ref s in
  List.iter (fun view_regexp ->
    if Str.string_match view_regexp !result 0 then begin
      result := Str.replace_first view_regexp view_command !result ;
      result := native_separator !result ;
    end) [Str.regexp_case_fold "^NOTEPAD" ; Str.regexp_case_fold "^EXPLORER"; Str.regexp_case_fold "^VIEW"];
  if skip && (s <> !result) then result := "";
  let s = !result in
  s

external glob : string -> (string -> unit) -> unit = "myglob"

external weidu_win_create_process :
    string -> string -> string option ->
      Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
        int  = "weidu_win_create_process" "weidu_win_create_process_native"

let create_process_env prog args env fd1 fd2 fd3 =
  weidu_win_create_process prog (String.concat " " (Array.to_list args))
    (Some(String.concat "\000" (Array.to_list env) ^ "\000"))
    fd1 fd2 fd3

let biff_path_separator = "\\\\"

let cd_regexp = Str.regexp "^[CH]D[0-9]+.*=\\([^\r\n]*\\)"

let is_weidu_executable f =
  Str.string_match (Str.regexp_case_fold "setup-.*\.exe$") f 0

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
  let version =
    try
      let s = Str.global_replace ( Str.regexp_case_fold ".*version \\([0-9]+\\).*") "\\1" line in
      int_of_string s
    with _ -> -1
  in
  (try Unix.close newstdin with _ -> ()) ;
  (try Unix.close newstdout with _ -> ()) ;
  (try Unix.close newstderr with _ -> ()) ;
  (try Unix.close newstdin' with _ -> ()) ;
  (try Unix.close newstdout' with _ -> ()) ;
  (try Unix.close newstderr' with _ -> ()) ;
  let pid', ps = Unix.waitpid [] pid in
  version

external get_user_personal_dir : unit -> string = "get_user_personal_dir"

let get_user_dir game_path =
  get_user_personal_dir ()

let game_path_by_type name =
  match String.lowercase name with
  | "bg2"  -> registry_path ()
  | "bg1"
  | "bg"   -> bg_registry_path ()
  | "iwd1"
  | "iwd"  -> iwd_registry_path ()
  | "iwd2" -> iwd2_registry_path ()
  | "pst"  -> pst_registry_path ()
  | "bgee" -> failwith (Printf.sprintf "game-by-type does not support BGEE") (* Because you can have up to three concurrent installations *)
  | _ -> failwith (Printf.sprintf "Unknown game: %s" name)
