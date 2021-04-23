(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.biff.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* Infinity Engine [BIF] *)
open BatteriesInit
open Hashtblinit
open Util
open Key
open Cbif

type biff_file = {
    res_loc       : int ;
    res_offset    : int ;
    res_size      : int ;
    res_type      : int ;
  }

and biff_tis = {
    tis_loc                : int ;
    tis_offset             : int ;
    tis_number_of_tiles    : int ;
    tis_size_of_one_tile   : int ;
    tis_type               : int ;
  }

and biff = {
    fd         : Unix.file_descr ;
    files      : biff_file array ;
    tilesets   : biff_tis array ;
    filename   : string ;
    compressed : bool ;
  }

(* create a biff from a bunch of components
 * save the biff to the disk
 * returns an updated KEY _that can only be saved, cannot be used_
 *)
let save_biff key filename keyname components =
  begin
    let all_components = List.flatten (List.map (fun file ->
      let size = file_size file in
      if size > 0 then begin
        let a,b = split (Case_ins.filename_basename file) in
        try
          let tau = key_of_ext false b in
          [ (size,file,String.uppercase a,String.uppercase b,tau) ]
        with _ ->
          log_only "WARNING: Not including [%s]: unknown resource type\n"
            file ; []
      end else []
                                                ) components) in
    let total_size = List.fold_left (fun acc (s,f,a,b,t) -> acc + s) 0
        all_components in
    let tis_key = key_of_ext true "TIS" in
    let files,tiles = List.partition (fun (s,f,a,b,t) -> t <> tis_key)
        all_components in
    let files = Array.of_list files in
    let tiles = Array.of_list tiles in

    log_and_print "[%s] will contain %d resources totalling %d bytes\n"
      filename (List.length all_components) total_size ;

    let num_files = Array.length files in
    let num_tiles = Array.length tiles in

    let header_size = 20 in
    let file_table_size = num_files * 16 in
    let tile_table_size = num_tiles * 20 in

    let offset_files = header_size in
    let offset_tiles = offset_files + file_table_size in
    let offset_data = ref (offset_tiles + tile_table_size) in

    let buff_size = header_size + file_table_size + tile_table_size in
    let buff = Bytes.make buff_size '\000' in

    Bytes.blit "BIFFV1  " 0 buff 0 8 ;
    write_int buff 8  num_files ;
    write_int buff 12 num_tiles ;
    write_int buff 16 offset_files ;

    Array.iteri (fun i (s,f,a,b,t) ->
      let off = offset_files + (i * 16) in
      write_int buff (off+0) i ; (* resource location *)
      write_int buff (off+4) !offset_data ;
      write_int buff (off+8) s ;
      write_short buff (off+12) t ;
      offset_data := !offset_data + s ;
                ) files ;
    Array.iteri (fun i (s,f,a,b,t) ->
      let in_fd = Case_ins.unix_openfile f [Unix.O_RDONLY] 0 in
      let header = Bytes.create 24 in
      my_read 24 in_fd header f ;
      (try
        Unix.close in_fd ;
      with e ->
        log_and_print "ERROR: save_biff failed to close %s during tiles 1\n" f ;
        raise e) ;
      let tisv1 = "TIS V1  " in
      let istis = Bytes.sub header 0 8 in
      let s = (if istis = tisv1 then s - 24 else s) in
      let off = offset_tiles + (i * 20) in
      let tis_loc = (i + 1) lsl 14 in
      write_int buff (off+0) tis_loc ; (* resource location *)
      write_int buff (off+4) !offset_data ;
      let tile_size = (if istis = tisv1 then int_of_str (Bytes.sub header 12 4) else 5120) in
      let num_tiles = (s/tile_size) in
      let tis_type = 1003 in
      write_int buff (off+8) num_tiles ;
      write_int buff (off+12) tile_size ;
      write_short buff (off+16) tis_type ;
      offset_data := !offset_data + s;
                ) tiles ;
    let out_fd = Unix.descr_of_out_channel (open_for_writing filename true) in
    my_write buff_size out_fd buff filename ;

    let chunk_size = 10240 in
    let chunk = Bytes.create chunk_size in

    let copy_over in_fd in_name size =
      let sofar = ref 0 in
      while !sofar < size do
        let chunk_size = min (size - !sofar) chunk_size in
        my_read chunk_size in_fd chunk in_name ;
        my_write chunk_size out_fd chunk filename ;
        sofar := !sofar + chunk_size ;
      done
    in
    Array.iteri (fun i (s,f,a,b,t) ->
      log_only "[%s] incorporating [%s]\n" filename f ;
      let in_fd = Case_ins.unix_openfile f [Unix.O_RDONLY] 0 in
      copy_over in_fd f s ;
      (try
        Unix.close in_fd ;
      with e ->
        log_and_print "ERROR: save_biff failed to close %s during files\n" f ;
        raise e)
                ) files ;
    Array.iteri (fun i (s,f,a,b,t) ->
      log_only "[%s] incorporating [%s]\n" filename f ;
      let in_fd = Case_ins.unix_openfile f [Unix.O_RDONLY] 0 in
      let istis = Bytes.create 8 in
      my_read 8 in_fd istis f ;
      (try
        Unix.close in_fd ;
      with e ->
        log_and_print "ERROR: save_biff failed to close %s during tiles 2\n" f ;
        raise e) ;
      let in_fd = Case_ins.unix_openfile f [Unix.O_RDONLY] 0 in
      if istis = "TIS V1  " then begin
        let istis = Bytes.create 24 in
        (* have it skip the first 24 bytes *)
        my_read 24 in_fd istis f ;
        copy_over in_fd f (s - 24) ;
      end else begin
        copy_over in_fd f s ;
      end ;
      (try
        Unix.close in_fd ;
      with e ->
        log_and_print "ERROR: save_biff failed to close %s during tiles 3\n" f ;
        raise e)
                ) tiles ;
    (try
      Unix.close out_fd ;
    with e ->
      log_and_print "ERROR: save_biff failed to close output file %s\n" filename ;
      raise e) ;
    if true then
      begin
        try
          Case_ins.unix_chmod filename 511 ;
        with e -> ()
      end ;

    let total_size = file_size filename in

    let new_biff_index = Array.length (key.biff) in
    let new_biffs = Array.append key.biff [|
      { Key.length = total_size ;
        Key.filename = keyname ;
        Key.locations = 1;
      } |] in
    let ctr = ref (-1) in
    let new_file_res = Array.map (fun (s,f,a,b,t) -> incr ctr ;
      { Key.res_name = a ;
        Key.res_type = t ;
        Key.other_index = !ctr;
        Key.tis_index = 0;
        Key.bif_index = new_biff_index; }) files in
    let ctr = ref 0 in
    let new_tile_res = Array.map (fun (s,f,a,b,t) -> incr ctr ;
      { Key.res_name = a ;
        Key.res_type = t ;
        Key.other_index = 0;
        Key.tis_index = !ctr;
        Key.bif_index = new_biff_index ; }) tiles in

    let new_a = Array.append key.resource new_file_res in

    (* remove duplicates! *)
    let new_a_list = Array.to_list new_a in
    let dup_ht = Hashtbl.create 65535 in
    let nal_rev = List.rev new_a_list in
    let nal = List.filter (fun elt ->
      if Hashtbl.mem dup_ht (elt.Key.res_name,elt.Key.res_type) then begin
        log_only "KEY: Duplicate: %s.%x\n" elt.Key.res_name
          elt.Key.res_type ;
        false
      end else begin
        Hashtbl.add dup_ht (elt.Key.res_name,elt.Key.res_type) true ;
        true
      end
                          ) nal_rev in
    let new_a = Array.of_list nal in

    let new_b = Array.append new_a new_tile_res in
    {
     biff = new_biffs ;
     resource = new_b ;
     resfind = Hashtbl.create 1
   }
  end

(* reads 'size' bytes that would start at location 'start' in this BIFF
 * if it were not compressed! *)
let read_compressed_biff_internal fd filename start size chunk_fun =
  let cmp_offset = ref 12 in
  let unc_offset = ref 0 in

  (* buffer holds the uncompressed bytes [start_unc,end_unc]  *)
  let (*result*) _ = Buffer.create size in
  let start_unc_offset = ref 0 in
  let end_unc_offset = ref 0 in

  let finished = ref false in
  let found_it = ref false in
  let sizes_buff = Bytes.create 8 in
  while not !finished do
    (* now we're looking at one block *)
    let _ = Unix.lseek fd !cmp_offset Unix.SEEK_SET in
    my_read 8 fd sizes_buff filename ;
    let uncmplen = int_of_str_off sizes_buff 0 in
    let cmplen = int_of_str_off sizes_buff 4 in
    (*
      log_and_print "cmp_off = %d  unc_off = %d :: cmp_len = %d  unc_len = %d\n"
      !cmp_offset !unc_offset cmplen uncmplen ;
     *)

    if not !found_it && !unc_offset + uncmplen >= start then begin
      found_it := true ;
      start_unc_offset := !unc_offset ;
    end ;

    if not !found_it then begin
      () (* skip *)
    end else if !found_it && !unc_offset >= start + size then begin
      (* we're done! *)
      finished := true
    end else begin
      (* read this block *)
      let _ = Unix.lseek fd (!cmp_offset+8) Unix.SEEK_SET in
      let cmp_buff = Bytes.create cmplen in
      my_read cmplen fd cmp_buff filename ;
      let uncmp = Cbif.uncompress cmp_buff 0 cmplen uncmplen in
      (*
        if (String.length uncmp <> uncmplen) then begin
        log_and_print "ERROR: [%s] chunk at offset %d was supposed to have %d bytes of compressed data that expanded to %d, but in reality they expanded to %d"
        filename !cmp_offset cmplen uncmplen (String.length uncmp) ;
        failwith "BIFC decompression error"
        end ;  *)
      end_unc_offset := !unc_offset + uncmplen;

      (* drop unwanted stuff from the beginning ... *)
      let uncmp =
        if !unc_offset < start then
          Str.string_after uncmp (start - !unc_offset)
        else uncmp
      in
      (* ... and/or the end of the uncompressed chunk *)
      let uncmp =
        if !unc_offset + uncmplen > (start + size) then begin
          let too_much = ((!unc_offset + uncmplen) - (start+size)) in
          let res = Str.string_before uncmp ((String.length uncmp) - too_much) in
          res
        end else uncmp
      in
      chunk_fun uncmp
    end ;

    unc_offset := !unc_offset + uncmplen ;
    cmp_offset := !cmp_offset + 8 + cmplen ;

    (if !found_it && !unc_offset >= start + size then finished := true);
  done ;
  ()

let read_compressed_biff fd filename a b =
  let res = Buffer.create (b - a) in
  let chunk_fun str = Buffer.add_string res str in
  read_compressed_biff_internal fd filename a b chunk_fun ;
  Buffer.contents res

let load_compressed_biff filename size fd =
  Stats.time "unmarshal compressed BIFF" (fun () ->
    let header_buff = read_compressed_biff fd filename 0 20 in
    let num_file_entry = int_of_str_off header_buff 8 in
    let num_tileset_entry = int_of_str_off header_buff 12 in
    let offset_file_entry = int_of_str_off header_buff 16 in
    let offset_tileset_entry = offset_file_entry + (num_file_entry * 16) in
    let table_len = offset_file_entry + (num_file_entry * 16) +
        (num_tileset_entry * 20) in
    let table_buff = read_compressed_biff fd filename 0 table_len in
    let result =
      {
       fd = fd ;
       filename = filename ;
       compressed = true ;
       files = Array.init num_file_entry (fun i ->
         let off = offset_file_entry + (i * 16) in
         {
          res_loc = int_of_str_off table_buff (off + 0) ;
          res_offset = int_of_str_off table_buff (off + 4) ;
          res_size = int_of_str_off table_buff (off + 8) ;
          res_type = short_of_str_off table_buff (off + 10) ;
        }
                                         ) ;
       tilesets = Array.init num_tileset_entry (fun i ->
         let off = offset_tileset_entry + (i * 20) in
         {
          tis_loc = int_of_str_off table_buff (off + 0) ;
          tis_offset = int_of_str_off table_buff (off + 4) ;
          tis_number_of_tiles = int_of_str_off table_buff (off + 8);
          tis_size_of_one_tile = int_of_str_off table_buff (off + 12) ;
          tis_type = short_of_str_off table_buff (off + 16) ;
        }
                                               ) ;
     } in
    log_or_print "[%s] %d bytes (compressed), %d files, %d tilesets\n"
      filename size num_file_entry num_tileset_entry ;
    result
                                         ) ()

let load_normal_biff filename size fd buff =
  Stats.time "unmarshal BIFF" (fun () ->
    let num_file_entry = int_of_str_off buff 8 in
    let num_tileset_entry = int_of_str_off buff 12 in
    let offset_file_entry = int_of_str_off buff 16 in
    let offset_tileset_entry = (num_file_entry * 16) in
    let table_len = (num_file_entry * 16) +
        (num_tileset_entry * 20) in
    let buff = Bytes.create table_len in
    let _ = Unix.lseek fd offset_file_entry Unix.SEEK_SET in
    my_read table_len fd buff filename ;
    let result =
      {
       fd = fd ;
       filename = filename ;
       compressed = false ;
       files = Array.init num_file_entry (fun i ->
         let off = (i * 16) in
         {
          res_loc = int_of_str_off buff (off + 0) ;
          res_offset = int_of_str_off buff (off + 4) ;
          res_size = int_of_str_off buff (off + 8) ;
          res_type = short_of_str_off buff (off + 10) ;
        }
                                         ) ;
       tilesets = Array.init num_tileset_entry (fun i ->
         let off = offset_tileset_entry + (i * 20) in
         {
          tis_loc = int_of_str_off buff (off + 0) ;
          tis_offset = int_of_str_off buff (off + 4) ;
          tis_number_of_tiles = int_of_str_off buff (off + 8);
          tis_size_of_one_tile = int_of_str_off buff (off + 12) ;
          tis_type = short_of_str_off buff (off + 16) ;
        }
                                               ) ;
     } in
    log_or_print "[%s] %d bytes, %d files, %d tilesets\n"
      filename size num_file_entry num_tileset_entry ;
    result
                              ) ()

let load_biff filename =
  try
    let stats = Case_ins.unix_stat filename in
    let size = stats.Unix.st_size in
    let fd = Case_ins.unix_openfile filename [Unix.O_RDONLY] 0 in
    let buff = Bytes.create 20 in
    let _ = Unix.read fd buff 0 20 in
    if String.length buff < 8 then begin
      failwith "not a valid BIFF file (wrong sig)"
    end ;
    (
     match Bytes.sub buff 0 8 with
       "BIFFV1  " -> load_normal_biff filename size fd buff
           (* comment out this BIFC line if you don't have zlib *)
     | "BIFCV1.0" -> load_compressed_biff filename size fd
     | s -> failwith ("BIFF file signature unsupported: " ^ s)
    )
  with e ->
    log_and_print "ERROR: BIFF [%s] cannot be loaded: %s\n" filename
      (printexc_to_string e);
    raise e

let check_file biff i ign =
  if i < 0 || i >= Array.length biff.files then begin
    if not ign then log_and_print "ERROR: BIFF [%s] has file entries 0--%d, cannot extract file at entry %d (this BIFF and your KEY file don't match)\n" biff.filename
        ((Array.length biff.files) - 1) i ;
    failwith "invalid biff file entry"
  end

let check_tile biff i ign =
  if i < 0 || i >= Array.length biff.tilesets then begin
    if not ign then log_and_print "ERROR: BIFF [%s] has tileset entries 0--%d, cannot extract tileset at entry %d (this BIFF and your KEY file don't match)\n" biff.filename
        ((Array.length biff.tilesets) - 1) i ;
    failwith "invalid biff tileset entry"
  end

let extract_file biff i ign =
  try
    check_file biff i ign;
    let this = biff.files.(i) in
    let size = this.res_size in
    if (biff.compressed) then begin
      read_compressed_biff biff.fd biff.filename this.res_offset size
    end else begin
      let _ = Unix.lseek biff.fd this.res_offset Unix.SEEK_SET in
      let buff = Bytes.create size in
      my_read size biff.fd buff biff.filename ;
      buff
    end
  with e ->
    if not ign then log_and_print "ERROR: BIFF [%s]: unable to extract file %d\n"
        biff.filename i ;
    raise e

let extract_tis biff i ign =
  try
    check_tile biff i ign;
    let this = biff.tilesets.(i) in
    let size = this.tis_number_of_tiles * this.tis_size_of_one_tile in
    let buff =
      if (biff.compressed) then begin
        read_compressed_biff biff.fd biff.filename this.tis_offset size
      end else begin
        let _ = Unix.lseek biff.fd this.tis_offset Unix.SEEK_SET in
        let buff = Bytes.create size in
        my_read size biff.fd buff biff.filename;
        buff
      end
    in
    let header = Bytes.create 0x18 in
    Bytes.blit "TIS V1  " 0 header 0 8;
    let str = str_of_int this.tis_number_of_tiles in
    Bytes.blit str 0 header 0x08 4 ;
    let str = str_of_int this.tis_size_of_one_tile in
    Bytes.blit str 0 header 0x0c 4 ;
    let str = str_of_int 0x18 in
    Bytes.blit str 0 header 0x10 4 ;
    let str = str_of_int 64 in
    Bytes.blit str 0 header 0x14 4 ;
    header ^ buff
  with e ->
    if not ign then log_and_print "ERROR: BIFF [%s]: unable to extract tileset %d\n"
        biff.filename i ;
    raise e

let copy_file biff i oc is_tis =
  let size,offset = if is_tis then begin
    check_tile biff i false;
    let this = biff.tilesets.(i) in
    let header = Bytes.create 0x18 in
    Bytes.blit "TIS V1  " 0 header 0 8;
    let str = str_of_int this.tis_number_of_tiles in
    Bytes.blit str 0 header 0x08 4 ;
    let str = str_of_int this.tis_size_of_one_tile in
    Bytes.blit str 0 header 0x0c 4 ;
    let str = str_of_int 0x18 in
    Bytes.blit str 0 header 0x10 4 ;
    let str = str_of_int 64 in
    Bytes.blit str 0 header 0x14 4 ;
        output_string oc header;
    (this.tis_number_of_tiles * this.tis_size_of_one_tile) ,
    this.tis_offset
  end else begin
    check_file biff i false;
    let this = biff.files.(i) in
    (this.res_size),
    this.res_offset
  end
  in
  try
    let copy_chunk str = output_string oc str in
    if (biff.compressed) then begin
      read_compressed_biff_internal
        biff.fd biff.filename offset size copy_chunk
    end else begin
      let _ = Unix.lseek biff.fd offset Unix.SEEK_SET in
      let chunk_size = 10240 in
      let chunk = Bytes.create chunk_size in
      let sofar = ref 0 in
      while !sofar < size do
        let chunk_size = min (size - !sofar) chunk_size in
        my_read chunk_size biff.fd chunk biff.filename ;
        output_string oc (Bytes.sub chunk 0 chunk_size) ;
        sofar := !sofar + chunk_size ;
      done
    end
  with e ->
    log_and_print "ERROR: BIFF [%s]: unable to extract-copy file %d\n"
      biff.filename i ;
    raise e

let bifc2biff source dest =
  ignore (handle_readonly dest) ;
  let out = Case_ins.perv_open_out_bin dest in
  let fd = Case_ins.unix_openfile source [Unix.O_RDONLY] 0 in
  let read len =
    let buff = Bytes.create len in
    ignore (my_read len fd buff source) ;
    buff in
  let header = read 12 in
  let uncompressed_size = int_of_str_off header 8 in
  let processed_size = ref 0 in
  while !processed_size < uncompressed_size do
    let block_metadata = read 8 in
    let block_decompressed_size = int_of_str_off block_metadata 0 in
    let block_compressed_size = int_of_str_off block_metadata 4 in
    let block_compressed_data = read block_compressed_size in
    let block_data = Cbif.uncompress block_compressed_data 0
        block_compressed_size block_decompressed_size in
    ignore (output_string out block_data) ;
    processed_size := !processed_size + block_decompressed_size ;
  done ;
  ignore (close_out out) ;
  ignore (Unix.close fd) ;
  !processed_size
