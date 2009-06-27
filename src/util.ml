(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.util.ml.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

(* generic utilities *)

open Arch

type local_string_entry = {
  lse_male : string ;
  lse_male_sound : string ;
  lse_female : string ;
  lse_female_sound : string ;
}

let one_newline_regexp = Str.regexp "[\n]"
let one_newline_or_cr_regexp = Str.regexp "[\r\n]"
let many_newline_or_cr_regexp = Str.regexp "[\r\n]+"
let many_whitespace_regexp = Str.regexp "[ \t]+"
let many_not_whitespace_regexp = Str.regexp "[^ \t\n\r]+"
let many_cr_regexp = Str.regexp "\r\r*"

let dos2unix = Str.global_replace many_cr_regexp "\r"

let errors_this_component = ref false

let value_of_option x = match x with
  | Some(s) -> s
  | None -> failwith "value_of_option None"

let log_channel = ref None

let debug_ocaml = ref false

let debug_modder = ref false

let be_silent = ref false

let log_or_print fmt =
  let k result =
    match !log_channel with
      None -> print_string result ; flush stdout
    | Some(o) -> output_string o result ; flush o
  in
  Printf.kprintf k fmt

let log_or_print_modder fmt =
  let k result =
    if !debug_modder then match !log_channel with
      None -> print_string result ; flush stdout
    | Some(o) -> output_string o result ; flush o
  in
  Printf.kprintf k fmt

let log_only fmt =
  let k result =
    match !log_channel with
      None -> ()
    | Some(o) -> output_string o result ; flush o
  in
  Printf.kprintf k fmt

let log_only_modder fmt =
  let k result =
    if !debug_modder then match !log_channel with
      None -> ()
    | Some(o) -> output_string o result ; flush o
  in
  Printf.kprintf k fmt

let log_and_print fmt =
  let k result = begin
    if not !be_silent then (output_string stdout result ; flush stdout );
    match !log_channel with
      None -> ()
    | Some(o) -> output_string o result ; flush o
  end
  in
  Printf.kprintf k fmt

let log_and_print_modder fmt =
  let k result = begin
    if not !be_silent && !debug_modder then (output_string stdout result ; flush stdout );
    if !debug_modder then match !log_channel with
      None -> ()
    | Some(o) -> output_string o result ; flush o
  end
  in
  Printf.kprintf k fmt

let set_errors file line =
	if !debug_ocaml then log_and_print "Warning at %s.%d\n" file line;
	errors_this_component := true;
;;


let recursive_mkdir directory mode =
  let dir_split = Str.split (Str.regexp "[/\\]") directory in
  let added_up_dir = ref "" in
  let skip_first_slash = ref false in
  if String.get directory 0 = '\\' || String.get directory 0 = '/' then skip_first_slash := true ;
  List.iter ( fun part ->
      added_up_dir := !added_up_dir ^ (if !skip_first_slash then "/" else "") ^ part ;
      skip_first_slash := true ;
(* 	  log_and_print "MKDIR %s\n" !added_up_dir; *)
	  try
	    Case_ins.unix_mkdir !added_up_dir mode;
	  with e -> (
	    match e with
	    | Unix.Unix_error(Unix.EEXIST,_,_) -> ()
	    | _ -> log_and_print "Problem %s on %s: util.ml\n" (Printexc.to_string e) !added_up_dir ;
    )
  ) dir_split

let inlined_files = Hashtbl.create 15
;;

List.iter ( fun (name,contents) ->
    Hashtbl.add inlined_files name contents
  ) Tph.list_of_stuff
;;

let backup_ht = Hashtbl.create 511
let backup_dir = ref None
let backup_list_chn = ref None
let mappings_list_chn = ref None

let set_backup_dir str i =
  let i = Printf.sprintf "%d" i in 
  let backup_dir_name = Printf.sprintf "%s/%s" str i in
  recursive_mkdir backup_dir_name 511; (* 511 = octal 0777 = a+rwx *)
  backup_dir := Some(backup_dir_name) ;
  (match !backup_list_chn with
    Some(c) -> close_out c
  | None -> ()) ; 
  let backup_filename = (backup_dir_name ^ "/UNINSTALL." ^ i) in
  let mappings_filename = (backup_dir_name ^ "/MAPPINGS." ^ i) in
  Hashtbl.clear backup_ht ; 
  (try
    backup_list_chn := Some(Case_ins.perv_open_out_bin backup_filename);
    mappings_list_chn := Some(Case_ins.perv_open_out_bin mappings_filename);
  with e -> 
    log_and_print "WARNING: unable to open [%s]: %s
Will be unable to UNINSTALL later.\n" backup_filename (Printexc.to_string e))

let log_file = ref "" 
let append_to_log = ref false 
let log_extern = ref false

let init_log version filename =
  try
    let out = 
      if !append_to_log then 
        Case_ins.perv_open_out_gen [ Open_append ; Open_creat ; Open_text ] 0o777 filename
      else Case_ins.perv_open_out filename 
    in
    log_channel := Some(out) ;
    log_file := filename ;
    log_only "WeiDU v %s Log\n\n" version ;
    Array.iter (fun s -> log_only " %s" s) Sys.argv ;
    log_only "\n"  
  with e ->
    Printf.printf "WARNING: unable to open log file [%s]: %s"
      filename (Printexc.to_string e) ;
    () 

let int32_of_str_off str off =
  let d = Int32.of_int (Char.code str.[off+0]) in
  let c = Int32.of_int (Char.code str.[off+1]) in
  let b = Int32.of_int (Char.code str.[off+2]) in
  let a = Int32.of_int (Char.code str.[off+3]) in
  Int32.logor
  (Int32.logor (Int32.shift_left a 24)
              (Int32.shift_left b 16))
  (Int32.logor (Int32.shift_left c 8) (d))

(*
let int_of_str_off str off =
  let d = Char.code str.[off+0] in
  let c = Char.code str.[off+1] in
  let b = Char.code str.[off+2] in
  let a = Char.code str.[off+3] in
  (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
  *)

let int_of_str_off str off = Int32.to_int (int32_of_str_off str off)

let int_of_str str = int_of_str_off str 0

let short_of_str_off str off =
  let d = Char.code str.[off+0] in
  let c = Char.code str.[off+1] in
  (c lsl 8) lor d

let byte_of_str_off str off =
  let d = Char.code str.[off] in
  d

let signed_byte_of d = 
  if d > 127 then 
    d - 256 
  else d

let signed_short_of d =
  if d > 32767 then 
    d - 65536
  else
    d 


let short_of_str str = short_of_str_off str 0

let str_of_int32 i =
  let d = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let c = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let b = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let a = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let result = String.make 4 (Char.chr a) in
  result.[0] <- (Char.chr d) ;
  result.[1] <- (Char.chr c) ;
  result.[2] <- (Char.chr b) ;
  result 

let str_of_int i =
  let d = i land 255 in
  let i = i lsr 8 in
  let c = i land 255 in
  let i = i lsr 8 in
  let b = i land 255 in
  let i = i lsr 8 in
  let a = i land 255 in
  let i = i lsr 8 in
  let result = String.make 4 (Char.chr a) in
  result.[0] <- (Char.chr d) ;
  result.[1] <- (Char.chr c) ;
  result.[2] <- (Char.chr b) ;
  result 

let str_of_short i =
  let d = i land 255 in
  let i = i lsr 8 in
  let c = i land 255 in
  let i = i lsr 8 in
  let result = String.make 2 (Char.chr d) in
  result.[1] <- (Char.chr c) ;
  result 

let str_of_byte i =
  let d = i land 255 in
  let i = i lsr 8 in
  let result = String.make 1 (Char.chr d) in
  result

let str_to_exact_size str size =
  let dest = String.make size '\000' in
  let max =
    if String.length str > size then
      size
    else
      String.length str
  in 
  String.blit str 0 dest 0 max ;
  dest

let write_int buff off value =
  String.blit (str_of_int (value)) 0 buff off 4  
let write_int32 buff off value =
  String.blit (str_of_int32 (value)) 0 buff off 4  
let write_short buff off value =
  String.blit (str_of_short (value)) 0 buff off 2  
let write_byte buff off value =
  if value < 0 then 
    buff.[off] <- (Char.chr (256+value))
  else 
    buff.[off] <- (Char.chr value) 
let write_resref buff off str =
  String.blit (str_to_exact_size str 8) 0 buff off 8  


let get_string_of_size str off size =
  let almost = String.sub str off size in
  try
    let null_index = String.index almost '\000' in
    String.sub almost 0 null_index
  with _ -> almost

let my_write size fd buff name = 
  let sofar = ref 0 in
  while !sofar < size do 
    let this_chunk = Unix.write fd buff !sofar (size - !sofar) in
    if this_chunk = 0 then begin
      failwith (Printf.sprintf "write %d of %d bytes from [%s]"
        !sofar size name) 
    end else 
      sofar := !sofar + this_chunk
  done 

let my_read size fd buff name = 
  let sofar = ref 0 in
  while !sofar < size do 
    let this_chunk = Unix.read fd buff !sofar (size - !sofar) in
    if this_chunk = 0 then begin
      failwith (Printf.sprintf "read %d of %d bytes from [%s]"
        !sofar size name) 
    end else 
      sofar := !sofar + this_chunk
  done 


let file_size name =
  try 
    let stats = Case_ins.unix_stat name in
    stats.Unix.st_size 
  with _ ->  -1

let file_exists name = (file_size name >= 0) 

let is_directory name =
  try
    let stats = Case_ins.unix_stat name in
    let res = stats.Unix.st_kind = Unix.S_DIR in
    (* log_only "%s is a directory: %b\n" name res ;  *) 
    res 
  with _ -> false 

let split name =
  try
    let base = Case_ins.filename_chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
    base,ext
  with _ -> name,""

let my_unlink file =
	begin
		try
			Case_ins.unix_unlink file
		with e ->
			log_only "Unable to Unlink [%s]: %s\n"
				file (Printexc.to_string e)
	end

let rec backup_if_extant filename =
  if Hashtbl.mem backup_ht
                 (String.uppercase (slash_to_backslash filename)) then
    ()
  else begin
    Hashtbl.add backup_ht
                (String.uppercase (slash_to_backslash filename)) true ;
    (
    match !backup_list_chn with
    | Some(chn) -> output_string chn (filename ^ "\n") ; flush chn
    | None -> ()
    );
    match !backup_dir with
    | Some(dir) when file_exists filename -> (
      let name = filename in
      let out = dir ^ "/" ^ (Str.global_replace (Str.regexp "[\\/]") "." filename) in
      let out1 = dir ^ "/" ^ Case_ins.filename_basename filename in
      let where = ref "" in
      try
        if file_exists out1 then
					where := out
				else
					where := out1
				;
				(
				match !mappings_list_chn with
					| Some(chn) -> output_string chn (filename ^ " " ^ !where ^ "\n"); flush chn
					| None -> ()
				);
				copy_large_file name !where "creating a backup"
      with e ->
        log_and_print "ERROR: error copying [%s]\n" name ;
        raise e
    )
    | Some(dir) when not (file_exists filename) -> (
				match !mappings_list_chn with
					| Some(chn) -> output_string chn (filename ^ "\n"); flush chn
					| None -> ()
    )
    | _ -> ()
  end

and copy_large_file name out reason =
(*  log_or_print "Copying a large file: %s to %s\n" name out; *)
  try begin
    Stats.time "loading files" (fun () ->
      let stats = Case_ins.unix_stat name in
      let size = stats.Unix.st_size in
      if size = 0 then
        log_or_print_modder "WARNING: [%s] is a 0 byte file\n" name
      else if size < 0 then begin
        log_and_print "ERROR: [%s] has reported size %d\n" name size ;
        failwith ("error loading " ^ name)
      end ;
      if file_exists out then my_unlink out ;
      begin
        let in_fd  = Case_ins.unix_openfile name [Unix.O_RDONLY] 0 in
        let out_fd = Case_ins.unix_openfile out  [Unix.O_WRONLY ; Unix.O_CREAT] 511 in
        let chunk_size = 10240 in
        let chunk = String.create chunk_size in
        let sofar = ref 0 in
        while !sofar < size do
          let chunk_size = min (size - !sofar) chunk_size in
          my_read chunk_size in_fd chunk name ;
          my_write chunk_size out_fd chunk out ;
          sofar := !sofar + chunk_size ;
        done ;
        Unix.close in_fd ;
        Unix.close out_fd ;
        log_only "%s copied to %s, %d bytes\n" name out size ;
      end
    ) ()
  end
  with e ->
    log_and_print "ERROR: error copying [%s]\n" name ;
    raise e

let load_file name =
  if Hashtbl.mem inlined_files name then
    String.copy (Hashtbl.find inlined_files (Arch.backslash_to_slash name))
  else
  try begin
    Stats.time "loading files" (fun () ->
    let stats = Case_ins.unix_stat name in
    let size = stats.Unix.st_size in
    if size = 0 then
      log_or_print_modder "WARNING: [%s] is a 0 byte file\n" name
    else if size < 0 then begin
      log_and_print "ERROR: [%s] has reported size %d\n" name size ;
      failwith ("error loading " ^ name)
    end else if size > Sys.max_string_length then begin
      log_and_print "ERROR: [%s] has size %d: TOO BIG FOR WEIDU (max %d)\n"
          name size Sys.max_string_length;
      failwith ("error loading " ^ name)
    end ;
    let buff = String.make size '\000' in
    let fd = Case_ins.unix_openfile name [Unix.O_RDONLY] 0 in
    my_read size fd buff name ;
    Unix.close fd ;
    log_only "[%s] loaded, %d bytes\n" name size ;
    buff) ();
  end
  with e ->
    log_and_print "ERROR: error loading [%s]\n" name ;
    raise e

let list_of_files_in_directory d =
  let result = ref [] in 
  (try
    let dh = Case_ins.unix_opendir d in
    begin
      try 
        while true do
          result := Unix.readdir dh :: !result 
        done 
      with _ -> () 
    end ;
    Unix.closedir dh 
  with _ -> ()) ; !result

let open_for_writing_internal backup filename binary =
  (if (backup) then backup_if_extant filename) ;
  if file_exists filename then (* if it already exists *)
    begin (* handle read-only files! *)
      try 
        Case_ins.unix_chmod filename 511 ; (* 511 = octal 0777 = a+rwx *)
      with e -> () 
        (* log_or_print "WARNING: chmod %s : %s\n" filename 
          (Printexc.to_string e) *)
    end ;
  let out_chn = (if binary then Case_ins.perv_open_out_bin else Case_ins.perv_open_out) filename in
  out_chn 

let open_for_writing = open_for_writing_internal true


(* filter to avoid logging progress bars from external programs *)
type filter_mode = Copy | Strip ;;
let create_filter = function () ->
  let mode = ref Copy in
  let buf = Buffer.create 80 in
  let f = function str ->
    let str = Str.global_replace (Str.regexp "\r\n") "\n" str in
    let strlen = String.length str in
    let pos = ref 0 in
    while !pos < strlen do
        match !mode with
          Copy  ->
            let newpos = try String.index_from str !pos '\r'
                         with Not_found -> strlen in
            Buffer.add_substring buf str !pos (newpos - !pos) ;
            if newpos != strlen then mode := Strip ;
            pos := newpos
        | Strip ->
            let newpos = try String.index_from str !pos '\n'
                         with Not_found -> strlen in
            if newpos != strlen then mode := Copy ;
            pos := newpos
    done ;
    let res = Buffer.contents buf in
    Buffer.clear buf ;
    res
  in f

let exec_command cmd exact =
  let cmd = if exact then cmd else Arch.slash_to_backslash cmd in
  let ret = if !log_extern then
    begin
      (* copy stdout + stderr to logfile *)
      let proc_stdout = Unix.open_process_in (cmd ^ " 2>&1") in
      let s = String.create 80 in
      let filter = create_filter () in
      begin
        try
          while true do
            let read = input proc_stdout s 0 80 in
            if read = 0 then raise End_of_file ;
            let text = String.sub s 0 read in
            if not !be_silent then begin output_string stdout text ; flush stdout end ;
            log_only "%s" (filter text)
          done
        with _ -> ()
      end ;
      Unix.close_process_in proc_stdout
    end else Unix.system cmd
  in ret

let execute_at_exit = ref ([] : (string*bool) list)

let weidu_version = ref ""

(* for some stupid reason these cannot be in the parser or the lexer *)

type input_context = {
  mutable line : int ;
  mutable col  : int ;
  mutable delta : int ;
  mutable filename : string ;
  mutable lexbuf : Lexing.lexbuf ;
  mutable warn_only : bool ;
}
let context_stack = ref []
let push_context filename lexbuf =
  let new_context = { line = 1; col = 0; delta = 0;
    filename = filename ; lexbuf = lexbuf ; warn_only = false } in
  context_stack := new_context :: !context_stack
let pop_context () = match !context_stack with
  [] -> log_and_print "ERROR: no current parsing context to pop!\n" ; ()
| hd::tl ->
    context_stack := List.tl !context_stack
let the_context () = match !context_stack with
  hd :: tl -> hd
| [] -> log_and_print "ERROR: no current parsing context\n" ;
        failwith "no current parsing context"

let lex_init (file: string)
         (inchannel: in_channel) : Lexing.lexbuf =
  let lexbuf = Lexing.from_channel inchannel in
  push_context file lexbuf ;
  lexbuf

let lex_init_from_internal_string (file: string)
         (buff: string) : Lexing.lexbuf =
  let lexbuf = Lexing.from_string buff in
  let ctx = the_context () in
  let new_context = { line = ctx.line; col = ctx.col - ctx.delta;
    delta = 0; filename = file ; lexbuf = lexbuf ; warn_only = true } in
  context_stack := new_context :: !context_stack ;
  lexbuf

let lex_init_from_string (file: string)
         (buff: string) : Lexing.lexbuf =
  let lexbuf = Lexing.from_string buff in
  push_context file lexbuf ;
  lexbuf 

let newline () = 
  let c = the_context () in 
  c.line <- c.line + 1;
  c.col <- 1;
  c.delta <- 0

let tab () = 
  let c = the_context () in
  c.col <- c.col + 8 - (c.col mod 8)

(* let adj lb =
  let c = the_context () in
  c.lexbuf <- lb ; 
  c.delta <- (Lexing.lexeme_end lb) - (Lexing.lexeme_start lb) ;
  c.col <- c.col + c.delta  *)

let str_adj lb =
  let c = the_context () in
  c.lexbuf <- lb ;
  let st= Lexing.lexeme lb in
  for i = 0 to (String.length st) - 1 do
    if st.[i] = '\n' then newline ()
    else begin
      c.col <- c.col + 1; c.delta <- c.delta + 1 ;
    end
  done

let adj = str_adj

let strip str =
  let len = String.length str in
  String.sub str 1 (len - 2)

let error_chn_ht = Hashtbl.create 11
let error_chn_base = ref "iwg2/errors"
let get_error_chn sort =
  try
    Hashtbl.find error_chn_ht sort
  with Not_found ->
    let oc = Case_ins.perv_open_out (Printf.sprintf "%s/%s" !error_chn_base sort) in
    Hashtbl.add error_chn_ht sort oc ;
    oc

let error sort fmt =
  let k result =
    let oc = get_error_chn sort in
    output_string oc result ;
    log_and_print "%s" result ;
    flush oc ;
  in
  Printf.kprintf k fmt

let input_error_to_stdout = ref true

let parse_error_verbose = ref true

let input_error sort_msg msg =
  let c = the_context () in
  let near_text = Lexing.lexeme c.lexbuf in
  if !parse_error_verbose then (if !input_error_to_stdout then
  log_and_print
  else
  error "DLG")
  "\n[%s] %s %s at line %d column %d-%d\nNear Text: %s\n\t%s\n"
    c.filename
    sort_msg
    (if c.warn_only then "WARNING" else "ERROR")
    c.line (c.col - c.delta) (c.col-1) near_text msg ;
  raise Parsing.Parse_error

let lex_error msg = input_error "LEXER" msg
let parse_error msg = input_error "PARSE" msg

let my_int_of_string s = 
  try int_of_string s
  with e -> parse_error "Not An Integer"

type parse_what	=
	| File of string
	| String of string * string

(* big generic parsing function *) 
let parse_file verbose what sort_of_file parse_lex_fun =
	parse_error_verbose := verbose;
  let do_the_work filename lexbuf =
    try
      let result = Stats.time sort_of_file
        (fun () -> parse_lex_fun lexbuf) () in
      pop_context () ;
      log_or_print_modder "[%s] parsed\n" filename ;
      result
    with e ->
      (try input_error "" (Printexc.to_string e) with _ -> () ) ;
      pop_context () ;
      raise e
  in
  match what with
  | File(filename) ->
	  if Hashtbl.mem inlined_files filename then begin
	    let str = Hashtbl.find inlined_files filename in
	    let lexbuf : Lexing.lexbuf = lex_init_from_string filename str in
	    try
	      do_the_work filename lexbuf
	    with e ->
	      if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
	        filename (Printexc.to_string e) ;
	      raise e
	  end else begin
	    let inchan = Case_ins.perv_open_in filename in
	    try
	      begin
	      let lexbuf : Lexing.lexbuf = lex_init filename inchan in
	      let res = do_the_work filename lexbuf in
	      close_in inchan ;
	      res
	      end
	    with e ->
	      if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
	        filename (Printexc.to_string e) ;
	      close_in inchan ; raise e
	  end
	| String (filename,str) -> begin
		    let lexbuf : Lexing.lexbuf = lex_init_from_string filename str in
	    try
	      do_the_work filename lexbuf
	    with e ->
	      if verbose then log_and_print "ERROR: parsing [%s]: %s\n"
	        filename (Printexc.to_string e) ;
	      raise e
		end

let return_value_success = 0
let return_value_error_tp2_component_install = 1
let return_value_error_autoupdate = 2
let return_value_retry_autoupdate = 3
let return_value_error_argument = 4

let return_value = ref return_value_success

let bool_xor a b =
  ((a && not b) || (not a && b))
