(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.arch_cygwin.ml.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

(* CygWin Arch-Specific Definitions *)

(* Query the Windows Registry to find out about Infinity Engine games. *)
let unixifydir str = Str.global_replace (Str.regexp (Str.quote "\\")) "/" str

external registry_path : unit -> string = "get_bg2main_path" 
external bg_registry_path : unit -> string = "get_bgmain_path" 
external iwd_registry_path : unit -> string = "get_iwdmain_path" 
external iwd2_registry_path : unit -> string = "get_iwd2main_path" 
external pst_registry_path : unit -> string = "get_pstmain_path" 

let registry_paths = ref [ 
  unixifydir (registry_path ()) ;
  unixifydir (bg_registry_path ()) ;
  unixifydir (iwd_registry_path ()) ;
  unixifydir (iwd2_registry_path ()) ;
  unixifydir (pst_registry_path ()) ; 
  "C:/Program Files/Black Isle/BGII - SoA/" 
] 

(* no slash removal: helps make shell scripts work better *)
let slash_to_backslash s = s 

let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
  s

(* how to view a text file *)
let view_command = "less" 

let do_auto_update = true

(* handle AT_EXIT ~VIEW mydir\myfile.txt~ *)
let handle_view_command s skip =
  let result = ref s in
  List.iter (fun view_regexp ->
    if Str.string_match view_regexp !result 0 then begin
      result := Str.replace_first view_regexp view_command !result ;
      result := Str.global_replace (Str.regexp (Str.quote "\\")) "/" !result;
    end) [Str.regexp_case_fold "^VIEW" ; Str.regexp_case_fold "^NOTEPAD" ;
          Str.regexp_case_fold "^EXPLORER"];
  if skip && (s <> !result) then result := "";
  let s = !result in
  s

let glob str fn = failwith "no globbing support"

let create_process_env = Unix.create_process_env

let biff_path_separator = "\\\\"

let cd_regexp = Str.regexp "^[CH]D[0-9]+.*=\\([^\r\n]*\\)" 



let get_version f =
	let newstdin, newstdin' = Unix.pipe () in
	let newstdout, newstdout' = Unix.pipe () in
	let newstderr, newstderr' = Unix.pipe () in
	let pid = create_process_env
		f [| "WeiDU-Backup" ; "--game bar" |] [| |] newstdin newstdout' newstderr'
	in
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
	Unix.kill pid 9;
	version
;;

external win_check_UAC : unit -> bool = "win_check_UAC" "win_check_UAC_native"

let check_UAC () =
	win_check_UAC ()
;;

let game_path_by_type name =
  match String.lowercase name with
  | "bg2"  -> registry_path ()
  | "bg"   -> bg_registry_path ()
  | "iwd"  -> iwd_registry_path ()
  | "iwd2" -> iwd2_registry_path ()
  | "pst"  -> pst_registry_path ()
  | _ -> failwith "Unknown game: %s" name
