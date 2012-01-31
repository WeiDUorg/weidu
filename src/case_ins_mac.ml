open BatteriesInit
let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
				s

(* Pervasives FS calls *)
let perv_open_out s = open_out (backslash_to_slash s) ;;
let perv_open_out_gen m i s = open_out_gen m i (backslash_to_slash s) ;;
let perv_open_out_bin s = open_out_bin (backslash_to_slash s) ;;
let perv_open_in s = open_in (backslash_to_slash s) ;;
let perv_open_in_gen m i s = open_in_gen m i (backslash_to_slash s) ;;
let perv_open_in_bin s = open_in_bin (backslash_to_slash s) ;;

let unix_openfile s a b = Unix.openfile (backslash_to_slash s) a b ;;
let unix_stat s = Unix.stat (backslash_to_slash s) ;;
let unix_chmod s p = Unix.chmod (backslash_to_slash s) p ;;
let unix_unlink s = Unix.unlink (backslash_to_slash s) ;;
let unix_mkdir s p = Unix.mkdir (backslash_to_slash s) p ;;
let unix_opendir s = Unix.opendir (backslash_to_slash s) ;;
let unix_rename s d = Unix.rename (backslash_to_slash s) (backslash_to_slash d);;
let unix_rmdir s = Unix.rmdir (backslash_to_slash s);;

let sys_readdir s = Sys.readdir (backslash_to_slash s);;

let weidu_executable = "WeiDU" ;;

let filename_basename s = Filename.basename (backslash_to_slash s) ;;
let filename_check_suffix s = Filename.check_suffix (backslash_to_slash s) ;;
let filename_chop_extension s = Filename.chop_extension (backslash_to_slash s) ;;
let filename_chop_suffix s = Filename.chop_suffix (backslash_to_slash s) ;;
let filename_dirname s = Filename.dirname (backslash_to_slash s) ;;
let filename_is_implicit s = Filename.is_implicit (backslash_to_slash s) ;;

let fix_name s = backslash_to_slash s;;
