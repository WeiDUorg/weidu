(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit

let case_fold = ref true
let lowercase = ref false

let case_sensitive_p () = true

external fcase : string -> string = "fcase"

(* Directory-listing cache maintenance, implemented in fcase/fcase.c.  These
   keep the C-side per-directory cache coherent as we mutate the file system
   (see the comment there).  [dir] and [name] are the resolved -- i.e. already
   case_transform'd -- directory path and base name. *)
external fcase_cache_add : string -> string -> unit = "fcase_cache_add"
external fcase_cache_remove : string -> string -> unit = "fcase_cache_remove"
external fcase_cache_flush : string -> unit = "fcase_cache_flush"
external fcase_cache_clear : unit -> unit = "fcase_cache_clear"

(* fcase returns "./" for "" *)
let case_transform s = if !lowercase then String.lowercase s else
if not !case_fold then s else if String.trim s <> "" then fcase s else ""

(* The C cache is only populated when case_transform actually resolves through
   fcase (folding, and not lowercasing); in every other mode it stays empty and
   these are cheap no-ops.  [t] is a resolved path as produced by
   case_transform, so its dirname is a cache key and its basename the real
   on-disk name. *)
let caching_active () = !case_fold && not !lowercase

let cache_note_create t =
  if t <> "" && caching_active () then
    fcase_cache_add (Filename.dirname t) (Filename.basename t)

let cache_note_delete t =
  if t <> "" && caching_active () then
    fcase_cache_remove (Filename.dirname t) (Filename.basename t)

let cache_note_flush t =
  if t <> "" && caching_active () then fcase_cache_flush t

(* Pervasives FS calls *)
let backslash_to_slash s =
  let s = Str.global_replace (Str.regexp "\\\\") "/" s in
				s

let perv_open_out s =
  let t = case_transform (backslash_to_slash s) in
  let oc = open_out t in cache_note_create t ; oc ;;
let perv_open_out_gen m i s =
  let t = case_transform (backslash_to_slash s) in
  let oc = open_out_gen m i t in cache_note_create t ; oc ;;
let perv_open_out_bin s =
  let t = case_transform (backslash_to_slash s) in
  let oc = open_out_bin t in cache_note_create t ; oc ;;
let perv_open_in s = open_in (case_transform (backslash_to_slash s)) ;;
let perv_open_in_gen m i s = open_in_gen m i (case_transform (backslash_to_slash s)) ;;
let perv_open_in_bin s = open_in_bin (case_transform (backslash_to_slash s)) ;;

let unix_openfile s a b =
  let t = case_transform (backslash_to_slash s) in
  let fd = Unix.openfile t a b in
  if List.mem Unix.O_CREAT a then cache_note_create t ;
  fd ;;
let unix_stat s = Unix.stat (case_transform (backslash_to_slash s)) ;;
let unix_stat64 s = Unix.LargeFile.stat (case_transform (backslash_to_slash s)) ;;
let unix_chmod s p = Unix.chmod (case_transform (backslash_to_slash s)) p ;;
let unix_unlink s =
  let t = case_transform (backslash_to_slash s) in
  Unix.unlink t ; cache_note_delete t ;;
let unix_mkdir s p =
  let t = case_transform (backslash_to_slash s) in
  Unix.mkdir t p ; cache_note_create t ;;
let unix_opendir s = Unix.opendir (case_transform (backslash_to_slash s)) ;;
let unix_rename s d =
  let ts = case_transform (backslash_to_slash s) in
  let td = case_transform (backslash_to_slash d) in
  Unix.rename ts td ;
  cache_note_delete ts ; cache_note_flush ts ; cache_note_create td ;;
let unix_rmdir s =
  let t = case_transform (backslash_to_slash s) in
  Unix.rmdir t ; cache_note_delete t ; cache_note_flush t ;;
let unix_access s p = Unix.access (case_transform (backslash_to_slash s)) p

let sys_readdir s = Sys.readdir (case_transform (backslash_to_slash s));;
let sys_remove s =
  let t = case_transform (backslash_to_slash s) in
  Sys.remove t ; cache_note_delete t
let sys_file_exists s = Sys.file_exists (case_transform (backslash_to_slash s))

let weidu_executable = "weidu" ;;

(* No lowercasing to avoid the bug with ADD_* lowercasing also the variable *)
let filename_basename s = Filename.basename (backslash_to_slash s) ;;
let filename_check_suffix s = Filename.check_suffix (backslash_to_slash s) ;;
let filename_chop_extension s = Filename.chop_extension (backslash_to_slash s) ;;
let filename_chop_suffix s = Filename.chop_suffix (backslash_to_slash s) ;;
let filename_dirname s = Filename.dirname (backslash_to_slash s) ;;
let filename_is_implicit s = Filename.is_implicit (backslash_to_slash s) ;;

let fix_name s = String.lowercase (backslash_to_slash s);;
