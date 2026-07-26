(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit

let case_fold = ref true
let lowercase = ref false

let case_sensitive_p () = true

external fcase : string -> string = "fcase"

(* fcase returns "./" for "" *)
let case_transform s = if !lowercase then String.lowercase s else
if not !case_fold then s else if String.trim s <> "" then fcase s else ""

let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
				s

let authorize operation s =
  let path = backslash_to_slash s in
  operation path ;
  let path = case_transform path in
  operation path ;
  path

let read_path = authorize File_access.require_read
let write_path = authorize File_access.require_write

let openfile_path s flags =
  let writes = List.exists (function
    | Unix.O_WRONLY
    | Unix.O_RDWR
    | Unix.O_APPEND
    | Unix.O_CREAT
    | Unix.O_TRUNC -> true
    | _ -> false) flags in
  let reads = List.exists (function
    | Unix.O_RDONLY
    | Unix.O_RDWR -> true
    | _ -> false) flags in
  let path = if writes then write_path s else read_path s in
  if reads then File_access.require_read path ;
  path

(* Pervasives FS calls *)
let perv_open_out s = open_out (write_path s) ;;
let perv_open_out_gen m i s = open_out_gen m i (write_path s) ;;
let perv_open_out_bin s = open_out_bin (write_path s) ;;
let perv_open_in s = open_in (read_path s) ;;
let perv_open_in_gen m i s = open_in_gen m i (read_path s) ;;
let perv_open_in_bin s = open_in_bin (read_path s) ;;

let unix_openfile s a b = Unix.openfile (openfile_path s a) a b ;;
let unix_stat s = Unix.stat (read_path s) ;;
let unix_stat64 s = Unix.LargeFile.stat (read_path s) ;;
let unix_chmod s p = Unix.chmod (write_path s) p ;;
let unix_unlink s = Unix.unlink (write_path s) ;;
let unix_mkdir s p = Unix.mkdir (write_path s) p ;;
let unix_opendir s = Unix.opendir (read_path s) ;;
let unix_rename s d = Unix.rename (write_path s) (write_path d);;
let unix_rmdir s = Unix.rmdir (write_path s);;
let unix_access s p =
  let path =
    if List.exists (fun mode -> mode = Unix.W_OK) p then write_path s
    else read_path s in
  Unix.access path p

let sys_readdir s = Sys.readdir (read_path s);;
let sys_remove s = Sys.remove (write_path s)
let sys_file_exists s = Sys.file_exists (read_path s)

let weidu_executable = "weidu" ;;

(* No lowercasing to avoid the bug with ADD_* lowercasing also the variable *)
let filename_basename s = Filename.basename (backslash_to_slash s) ;;
let filename_check_suffix s = Filename.check_suffix (backslash_to_slash s) ;;
let filename_chop_extension s = Filename.chop_extension (backslash_to_slash s) ;;
let filename_chop_suffix s = Filename.chop_suffix (backslash_to_slash s) ;;
let filename_dirname s = Filename.dirname (backslash_to_slash s) ;;
let filename_is_implicit s = Filename.is_implicit (backslash_to_slash s) ;;

let fix_name s = String.lowercase (backslash_to_slash s);;
