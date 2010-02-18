(* Pervasives FS calls *)
let perv_open_out s = open_out s ;;
let perv_open_out_gen m i s = open_out_gen m i s ;;
let perv_open_out_bin s = open_out_bin s ;;
let perv_open_in s = open_in s ;;
let perv_open_in_gen m i s = open_in_gen m i s ;;
let perv_open_in_bin s = open_in_bin s ;;

let unix_openfile s a b = Unix.openfile s a b ;;
let unix_stat s = Unix.stat s ;;
let unix_chmod s p = Unix.chmod s p ;;
let unix_unlink s = Unix.unlink s ;;
let unix_mkdir s p = Unix.mkdir s p ;;
let unix_opendir s = Unix.opendir s ;;
let unix_rename s d = Unix.rename s d;;
let unix_rmdir s = Unix.rmdir s;;

let sys_readdir s = Sys.readdir s;;

let weidu_executable = "WeiDU.exe" ;;

let filename_basename = Filename.basename;;
let filename_check_suffix = Filename.check_suffix;;
let filename_chop_extension = Filename.chop_extension;;
let filename_chop_suffix = Filename.chop_suffix;;
let filename_dirname = Filename.dirname;;
let filename_is_implicit = Filename.is_implicit;;
