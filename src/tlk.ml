(* Infinity Engine [TLK] Format *)
open Util

(* let flags_parse = 7  *)

let is_bg2 = ref false

type tlk_string = {
  mutable flags : int; 
  mutable sound_name : string ;
  mutable volume : int ;
  mutable pitch : int ;
  mutable text : string ; 
} 

let blank_tlk_string = {
  flags = 0;
  sound_name = "";
  volume = 0;
  pitch = 0;
  text = "";
} 
  
type tlk = tlk_string array 

exception FoundString of int 

let find_string_fast lse male female search =
  let could_be = Hashtbl.find_all search lse.lse_male in
  let rec is_it lst = match lst with
    hd :: tl -> 
      let s = male.(hd) in 
      if lse.lse_male = s.text && 
         lse.lse_male_sound = s.sound_name &&
         (match female with
           None -> true
         | Some(f) -> lse.lse_female = f.(hd).text &&
                      lse.lse_female_sound = f.(hd).sound_name ) then
        hd
      else
        is_it tl 
  | [] -> raise Not_found
  in
  is_it could_be

let find_string lse male female =
  try 
    Array.iteri (fun i s ->
      if lse.lse_male = s.text && 
         lse.lse_male_sound = s.sound_name &&
         (match female with
           None -> true
         | Some(f) -> lse.lse_female = f.(i).text &&
                      lse.lse_female_sound = f.(i).sound_name ) then
        raise (FoundString(i))
    ) male ;
    raise Not_found
  with FoundString(i) -> i 

let five_tilde_regexp = Str.regexp (Str.quote "~~~~~")

let weidu_quotify s =
  let quotes = 
    if not (String.contains s '~') then "~" 
    else if not (String.contains s '"') then "\""
    else if not (String.contains s '%') then "%" 
    else if try let _ = Str.search_forward five_tilde_regexp s 0 in false
            with Not_found -> true then "~~~~~"
    else failwith ("cannot put WeiDU quotes around " ^ s)
  in
  quotes ^ s ^ quotes

let pretty_print_q tlk i quot sound =
  if i >= 0 && i < Array.length tlk then begin
    let e = tlk.(i) in
    let s1 = if quot then weidu_quotify e.text else e.text in
    if sound then
    	e.sound_name
		else begin
			if (e.sound_name <> "") && quot then
	      s1 ^ " [" ^ e.sound_name ^ "]"
	    else
	      s1
    end
  end else
    Printf.sprintf "<Invalid Strref %d>" i

let pretty_print tlk i = pretty_print_q tlk i true false

let short_print tlk_str len =
  let e = tlk_str.text in
  let e = if String.length e > len then (String.sub e 0 (len - 3)) ^ "..." else e in
  weidu_quotify e

let cr_lf = (Str.regexp "[\r\n]+")

let one_line_pretty_print tlk i =
  let res = pretty_print tlk i in
  Str.global_replace cr_lf "" res

let pretty_print_opt tlk_opt i =
  match tlk_opt with
    Some(t) -> pretty_print t i
  | None -> ""

let null_tlk () : tlk = [| |]

let load_big_slow_tlk filename = begin
  Stats.time "unmarshal TLK" (fun () -> 
  let fd = Case_ins.unix_openfile filename [Unix.O_RDONLY] 0 in 
  let buff = String.create 8 in 
  my_read 8 fd buff filename ; 
  if buff <> "TLK V1  " then begin
    failwith "not a valid TLK file (wrong sig)"
  end ; 
  my_read 2 fd buff filename ; 
  let get_int () = my_read 4 fd buff filename ; int_of_str buff in 
  let get_short () = my_read 2 fd buff filename ; short_of_str buff in 
  try 
    let size = get_int () in
    let offset_strdata = get_int () in 
    let (result:tlk) = Array.init size (fun i ->
      let entry_offset = 18 + (i * 26) in
      ignore( Unix.lseek fd entry_offset Unix.SEEK_SET ); 
      let flags = get_short () in
      my_read 8 fd buff filename ; 
      let sound_name = get_string_of_size buff 0 8 in 
      let volume = get_int () in
      let pitch = get_int () in 
      let text_offset = get_int () in
      let text_length = get_int () in 
      let text = if text_length = 0 then "" else
        begin
          let string_i_text = String.create text_length in 
          ignore(Unix.lseek fd (offset_strdata + text_offset) Unix.SEEK_SET) ;
          my_read text_length fd string_i_text filename ; 
          string_i_text
        end
      in 
      {
        flags = flags ; 
        sound_name = sound_name ;
        volume = volume ;
        pitch = pitch ; 
        text = text ; 
      } 
    ) in 
  (*
    let size = int_of_str (String.sub buff 10 4) in
    let offset_strdata = int_of_str (String.sub buff 14 4) in

    let (result : tlk ) = Array.init size (fun i -> 
      let entry_offset = 18 + (i * 26) in
      let text_offset = int_of_str_off buff (entry_offset + 18) in
      let text_length = int_of_str_off buff (entry_offset + 22) in
      {
        flags       = short_of_str_off buff entry_offset ;
        sound_name  = get_string_of_size buff (entry_offset + 2) 8 ;
        volume      = int_of_str_off buff (entry_offset + 10) ; 
        pitch       = int_of_str_off buff (entry_offset + 14); 
        text        = 
          if text_length = 0 then "" 
          else try 
          String.sub buff (offset_strdata + text_offset) text_length ;
                      with _ -> 
          log_or_print "[%s] error with string entry %d: %d + %d %d %d\n" 
            filename i offset_strdata text_offset text_length 
            (String.length buff) ; ""
      } 
    ) in
      *)
    log_or_print "[%s] %d string entries\n" filename size ; 
    if (size = 0) then begin
      log_or_print "[%s] creating default entry #0\n" filename ; 
      Array.init 1 (fun _ ->
      {
        flags = 0;
        sound_name = "" ;
        volume = 0;
        pitch = 0;
        text = "<NO TEXT>" ;
      }) 
    end else result 
  with e ->
    log_and_print "ERROR unmarshaling TLK file [%s]" filename ;
    raise e
  ) () 
end

let load_small_fast_tlk filename = begin 
  let buff = Stats.time "load TLK" (fun () -> load_file filename) () in 
  Stats.time "unmarshal TLK" (fun () -> 
  if String.sub buff 0 8 <> "TLK V1  " then begin
    failwith "not a valid TLK file (wrong sig)"
  end ; 
  try 
    let size = int_of_str (String.sub buff 10 4) in
    let offset_strdata = int_of_str (String.sub buff 14 4) in

    let (result : tlk ) = Array.init size (fun i -> 
      let entry_offset = 18 + (i * 26) in
      let text_offset = int_of_str_off buff (entry_offset + 18) in
      let text_length = int_of_str_off buff (entry_offset + 22) in
      {
        flags       = short_of_str_off buff entry_offset ;
        sound_name  = get_string_of_size buff (entry_offset + 2) 8 ;
        volume      = int_of_str_off buff (entry_offset + 10) ; 
        pitch       = int_of_str_off buff (entry_offset + 14); 
        text        = 
          if text_length = 0 then "" 
          else try 
          String.sub buff (offset_strdata + text_offset) text_length ;
                      with _ -> 
          log_or_print "[%s] error with string entry %d: %d + %d %d %d\n" 
            filename i offset_strdata text_offset text_length 
            (String.length buff) ; ""
      } 
    ) in
    log_or_print "[%s] %d string entries\n" filename size ; 
    if (size = 0) then begin
      log_or_print "[%s] creating default entry #0\n" filename ; 
      Array.init 1 (fun _ ->
      {
        flags = 0;
        sound_name = "" ;
        volume = 0;
        pitch = 0;
        text = "<NO TEXT>" ;
      }) 
    end else result 
  with e ->
    log_and_print "ERROR unmarshaling TLK file [%s]" filename ;
    raise e
  ) () 
end

let load_tlk filename = 
  let stats = Case_ins.unix_stat filename in
  let size = stats.Unix.st_size in 
  if (size < 8) then
    failwith "not a valid TLK file (too small)"
  else if (size > Sys.max_string_length) then 
    load_big_slow_tlk filename
  else
    load_small_fast_tlk filename 

(* ww: Wed Jan  7 10:55:42 PST 2004
 * now we don't use buffers or strings to save parts of the TLK file, so it
 * can be pretty darn big ... *) 
let save_tlk filename tlk outchan = begin
  let num = Array.length tlk in 
  Stats.time "marshal and save TLK" (fun () -> 
    output_string outchan "TLK V1  " ;
    output_char outchan '\000' ;
    output_char outchan '\000' ;
    output_string outchan (str_of_int num) ;

  let offset_strdata = 18 + (num * 26) in 

  output_string outchan (str_of_int offset_strdata) ; 

  let string_buff_length = ref 0 in 

  for i = 0 to num - 1 do
    output_string outchan (str_of_short tlk.(i).flags) ; 
    output_string outchan (str_to_exact_size tlk.(i).sound_name 8) ; 
    output_string outchan (str_of_int tlk.(i).volume) ; 
    output_string outchan (str_of_int tlk.(i).pitch) ; 
    output_string outchan (str_of_int !string_buff_length ) ; 
    let len = String.length tlk.(i).text in 
    output_string outchan (str_of_int len) ;
    string_buff_length := !string_buff_length + len
  done ;
  for i = 0 to num - 1 do
    output_string outchan tlk.(i).text 
  done ; 
  close_out outchan ;
  ) () ;
  log_and_print "[%s] created, %d string entries\n" filename num ; 
  () 
end

let lse_to_tlk_string lse =
  let male = {
    flags = 0 ;
    sound_name = lse.lse_male_sound ;
    volume = 0;
    pitch = 0;
    text = lse.lse_male;
  } in
  let female = {
    flags = 0 ;
    sound_name = lse.lse_female_sound ;
    volume = 0;
    pitch = 0;
    text = lse.lse_female;
  } in
  let apply_flags x =
	  {x with flags = match x.sound_name, x.text, !is_bg2 with
			| "", "", _ -> 0
			| _, "", _ -> 2
			| _, _, false -> 3
			| _, _, true -> (try ignore (String.index x.text '<'); 7 with _ -> 3)
	}  in
	let apply_flags x = {x with flags = 7 } in
	apply_flags male, apply_flags female
