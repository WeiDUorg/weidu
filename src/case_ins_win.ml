open BatteriesInit
open Hashtblinit

let case_fold = ref false
let lowercase = ref false

let case_sensitive_p () = false

let read_path s =
  File_access.require_read s ;
  s

let write_path s =
  File_access.require_write s ;
  s

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

let weidu_executable = "weidu.exe" ;;

let filename_basename = Filename.basename;;
let filename_check_suffix = Filename.check_suffix;;
let filename_chop_extension = Filename.chop_extension;;
let filename_chop_suffix = Filename.chop_suffix;;
let filename_dirname = Filename.dirname;;
let filename_is_implicit = Filename.is_implicit;;

let fix_name s = s;;
