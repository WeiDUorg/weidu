(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.key.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* Infinity Engine [KEY] *)

open BatteriesInit
open Hashtblinit
open Util

type key_biff = {
    length : int ;
    filename : string ;
    locations : int ;
  }

and key_resource = {
    res_name : string ;
    res_type : int ;
    other_index : int ;
    tis_index : int ;
    bif_index : int ;
  }

and key = {
    biff : key_biff array ;
    resource : key_resource array ;
    resfind : ((string*string), key_resource) Hashtbl.t ;
  }

let null_key () = {
  biff = [||] ; resource = [||] ;
  resfind = Hashtbl.create 1
}

let key_ext_ht = Hashtbl.create 101
let ext_key_ht = Hashtbl.create 101

let assoc ext key =
  Hashtbl.add key_ext_ht ext key ;
  Hashtbl.add ext_key_ht key ext

let _ =
  assoc "2DA" 0x3f4 ;
  assoc "ARE" 0x3f2 ;
  assoc "BAH" 0x44c ;
  assoc "BAM" 0x3e8 ;
  assoc "BCS" 0x3ef ;
  assoc "BIO" 0x3fe ;
  assoc "BMP" 0x001 ;
  assoc "BS"  0x3f9 ;
  assoc "CHR" 0x3fa ;
  assoc "CHU" 0x3ea ;
  assoc "CRE" 0x3f1 ;
  assoc "DLG" 0x3f3 ;
  assoc "EFF" 0x3f8 ;
  assoc "FNT" 0x400 ;
  assoc "GAM" 0x3f5 ;
  assoc "GLSL" 0x405 ;
  assoc "GUI" 0x402 ;
  assoc "IDS" 0x3f0 ;
  assoc "INI" 0x802 ;
  assoc "ITM" 0x3ed ;
  assoc "LUA" 0x409 ;
  assoc "MAZE" 0x804 ;
  assoc "MENU" 0x408 ;
  assoc "MOS" 0x3ec ;
  assoc "MVE" 0x002 ;
  assoc "PLT" 0x006 ;
  assoc "PNG" 0x40b ;
  assoc "PRO" 0x3fd ;
  assoc "PVRZ" 0x404 ;
  assoc "SPL" 0x3ee ;
  assoc "SQL" 0x403 ;
  assoc "SRC" 0x803 ;
  assoc "STO" 0x3f6 ;
  assoc "TIS" 0x3eb ;
  assoc "TOH" 0x407 ;
  assoc "TOT" 0x406 ;
  assoc "TTF" 0x40a ;
  assoc "VEF" 0x3fc ;
  assoc "VVC" 0x3fb ;
  assoc "WAV" 0x004 ;
  assoc "WBM" 0x3ff ;
  assoc "WED" 0x3e9 ;
  assoc "WFX" 0x005 ;
  assoc "WMP" 0x3f7 ;
  ()

let ext_of_key key =
  try
    Hashtbl.find ext_key_ht key
  with e ->
    Printf.sprintf "0x%X" key

let key_of_ext warn ext =
  let ext = String.uppercase ext in
  try
    Hashtbl.find key_ext_ht ext
  with e ->
    if warn then log_and_print
        "ERROR: unknown resource extension [%s]\n" ext ;
    raise e

let save_key key outchan =
  let buff = Buffer.create 1024 in
  let biff_name_buff = Buffer.create 1024 in
  let biff_buff = Buffer.create 1024 in
  let res_buff = Buffer.create 512000 in
  Stats.time "marshal KEY" (fun () ->
    Buffer.add_string buff "KEY V1  " ;
    let num_biff = Array.length key.biff in
    let num_resource = Array.length key.resource in
    Buffer.add_string buff (str_of_int (num_biff)) ;
    Buffer.add_string buff (str_of_int (num_resource)) ;
    Buffer.add_string buff (str_of_int (24)) ;

    let base_name_offset = 24 + (12 * num_biff) in

    Array.iter (fun b ->
      let filename = Printf.sprintf "%s%c" b.filename (Char.chr 0) in
      let filename_offset =
        base_name_offset + (Buffer.length biff_name_buff) in
      let filename_length = String.length filename in
      Buffer.add_string biff_name_buff filename ;
      Buffer.add_string biff_buff (str_of_int b.length) ;
      Buffer.add_string biff_buff (str_of_int filename_offset) ;
      Buffer.add_string biff_buff (str_of_short filename_length) ;
      Buffer.add_string biff_buff (str_of_short b.locations)) key.biff ;

    let res_offset = base_name_offset + (Buffer.length biff_name_buff) in
    Buffer.add_string buff (str_of_int (res_offset)) ;

    Array.iter (fun r ->
      let res_name = str_to_exact_size r.res_name 8 in
      Buffer.add_string res_buff res_name ;
      Buffer.add_string res_buff (str_of_short r.res_type) ;
      let bitfield : Int32.t = Int32.logor
          (Int32.logor (Int32.of_int r.other_index)
             (Int32.shift_left (Int32.of_int r.bif_index) 20))
          (Int32.shift_left (Int32.of_int r.tis_index) 14) in
      Buffer.add_string res_buff (str_of_int32 bitfield)) key.resource) () ;
  Stats.time "saving files" (fun () ->
    Buffer.output_buffer outchan buff ;
    Buffer.output_buffer outchan biff_buff ;
    Buffer.output_buffer outchan biff_name_buff ;
    Buffer.output_buffer outchan res_buff ;
    close_out outchan) () ;
  log_or_print "KEY saved (%d biffs, %d resources)\n"
    (Array.length key.biff)
    (Array.length key.resource)

let load_key filename buff =
  Stats.time "unmarshal KEY" (fun () ->
    if String.length buff < 8 || String.sub buff 0 8 <> "KEY V1  " then begin
      failwith "not a valid KEY file (wrong sig)"
    end ;
    let num_bif = int_of_str_off buff 8 in
    let num_resource = int_of_str_off buff 12 in
    let offset_bif = int_of_str_off buff 16 in
    let offset_resource = int_of_str_off buff 20 in
    let resfind = Hashtbl.create (num_resource * 2) in
    let result = {
      resfind = resfind ;
      biff = Array.init num_bif (fun i ->
        let off = offset_bif + (i * 12) in
        let off_file = int_of_str_off buff (off + 4) in
        let len_file = short_of_str_off buff (off + 8) in
        {
         length = int_of_str_off buff off ;
         filename = String.uppercase
           (get_string_of_size buff off_file len_file) ;
         locations = short_of_str_off buff (off + 10) ;
       }) ;
      resource = Array.init num_resource (fun i ->
        let off = offset_resource + (i * 14) in
        let bitfield = int32_of_str_off buff (off + 10) in
        let res = {
          res_name = String.uppercase (get_string_of_size buff off 8) ;
          res_type = short_of_str_off buff (off + 8) ;
          other_index = Int32.to_int
            (Int32.logand bitfield (Int32.of_int 16383)) ;
          bif_index = Int32.to_int (Int32.shift_right bitfield 20) ;
          tis_index = Int32.to_int
            (Int32.logand (Int32.shift_right bitfield 14)
               (Int32.of_int 63)) ;
        } in
        let ext_str = ext_of_key res.res_type in
        Hashtbl.add resfind (res.res_name,ext_str) res ;
        res) ;
    } in
    log_or_print "[%s] %d BIFFs, %d resources\n"
      filename num_bif num_resource ;
    result) ()

let find_resource key name ext =
  try
    Hashtbl.find key.resfind (name,ext)
  with Not_found ->
    begin
      Hashtbl.find key.resfind
        (String.uppercase name,String.uppercase ext)
    end

let resource_exists key name ext =
  try
    ignore (find_resource key name ext) ;
    true ;
  with Not_found ->
    false

let other_path_separators = Str.regexp "[:\\]"

(* returns a path to a BIF and an index within it *)
let bif_of_resource key name ext =
  let res = find_resource key name ext in
  let bif = key.biff.(res.bif_index) in
  let filename =
    Str.global_replace other_path_separators "/" bif.filename in
  (filename,res.other_index,res.tis_index)

let bif_exists_in_key key name =
  let result = ref false in
  Array.iter (fun b ->
    let b = String.uppercase b.filename in
    if String.compare (String.uppercase name) b = 0 then
      result := true) key.biff ;
  !result

let list_biff key o =
  Array.iter (fun b ->
    o (Printf.sprintf "[%s]\t%9d bytes, %3d locations\n"
         b.filename b.length b.locations)) key.biff

let list_biff_contents key o bl =
  Array.iter (fun r ->
    let biff = key.biff.(r.bif_index) in
    let up_name = String.uppercase biff.filename in
    if List.mem up_name bl then
      o (Printf.sprintf "[%s] contains %8s.%3s at index %d\n"
           biff.filename r.res_name ( ext_of_key r.res_type )
           (if ext_of_key r.res_type = "TIS" then
             r.tis_index else r.other_index))) key.resource

let list_key key o =
  Array.iter (fun r ->
    o (Printf.sprintf "%s.%s\n" r.res_name
         (ext_of_key r.res_type))) key.resource

let list_of_key_resources : key -> bool -> string list =
  (fun key use_override ->
    let from_key = List.map String.uppercase
        (Array.to_list (Array.map (fun r ->
          Printf.sprintf "%s.%s" r.res_name
            (ext_of_key r.res_type)) key.resource)) in
    if use_override then begin
      let from_override = List.map String.uppercase
          (Array.to_list (Sys.readdir "override")) in
      List.sort_unique compare (from_key @ from_override)
    end else from_key)

let search_key_resources key use_override search_func =
  List.filter search_func (list_of_key_resources key use_override)

let remove_biff key filename =
  let idx = ref None in
  let filename = String.uppercase filename in
  Array.iteri (fun i b ->
    if (String.uppercase b.filename) = filename then
      idx := Some(i)) key.biff ;
  let i = match !idx with
  | Some(i) -> i
  | None -> failwith "BIFF not found in KEY (try --list-biffs)" in
  let reslist = Array.to_list key.resource in
  let counter = ref 0 in
  let filtered = List.filter (fun r ->
    if r.bif_index = i then (incr counter ; false) else true) reslist in
  let modified = List.map (fun r ->
    if r.bif_index > i then {r with bif_index = r.bif_index - 1}
    else r) filtered in
  let newres = Array.of_list modified in

  let before = Array.sub key.biff 0 i in
  let after = Array.sub key.biff (i+1) (((Array.length key.biff) - i)-1) in
  let newbiff = Array.append before after in
  log_and_print "Removing references to %d resources in [%s]\n" !counter
    filename ;
  {
   key with biff = newbiff ;
   resource = newres ;
 }

let remove_files key file_lst =
  let new_resfind = Hashtbl.copy key.resfind in
  let file_hsh = Hashtbl.create 5 in
  List.iter (fun file ->
    let (name,ext) = split_resref file in
    Hashtbl.remove new_resfind (name,ext) ;
    Hashtbl.add file_hsh (name,ext) true) file_lst ;
  let new_file_count = ref (Array.length key.resource) in
  Array.iter (fun item ->
    if Hashtbl.mem file_hsh (item.res_name, (ext_of_key item.res_type)) then
      begin
        log_only "DISABLE_FROM_KEY [%s.%s]: success\n" item.res_name
          (ext_of_key item.res_type) ;
        ignore (record_other_file_op
                  (String.concat ""
                     ["override/" ; item.res_name ; "." ;
                      (ext_of_key item.res_type)])) ;
        Hashtbl.remove file_hsh (item.res_name, (ext_of_key item.res_type)) ;
        decr new_file_count
      end) key.resource ;
  Hashtbl.iter (fun (a,b) _ ->
    log_only "DISABLE_FROM_KEY [%s.%s]: file does not exist\n" a b) file_hsh ;
  let index = ref 0 in
  let new_resource = Array.init !new_file_count (fun _ ->
    let item = ref (key.resource.(!index)) in
    let memres = Hashtbl.mem new_resfind in
    while not (memres (!item.res_name, (ext_of_key !item.res_type))) do
      incr index ;
      item := key.resource.(!index)
    done ;
    incr index ;
    !item) in
  assert (!index - List.length file_lst +
            Hashtbl.length file_hsh = !new_file_count) ;
  {
   key with
   resfind = new_resfind ;
   resource = new_resource ;
 }

let biff_path_separator eep =
  if eep then "/" else Arch.biff_path_separator
