open Util

exception Cre22
;;

type cre = {
    (* the buffer *)
    main_body : string;
    
    (* name, level, type *)
    known_spells : (string * int * int) list;
    
    (* level, count, count2, type, (name * in memory?) *)
    memorized_info : (int * int * int * int * (string * bool) list) list;

    (* the buffers, effV1? *)
    effects : (string list * bool);
    
    (* name, unknown, quantity1, quantity2, quantity3, flags, slots *)
    items : (string * (int * int * int * int * int * int list)) list;
    
    equipped: int;
    
    ending_unknown: int;
  }
;;

let cre_of_string buff =
  let head_length = match String.sub buff 0 8 with
  | "CRE V1.0" -> 0x2d4
  | "CRE V1.2" -> 0x378
  | "CRE V2.2" -> raise Cre22
  | "CRE V9.0" -> 0x33c
  | _ -> failwith "not a vaild CRE file"
  in

  let main_body = String.sub buff 0 head_length in
  
  let is_eff_v1 = (byte_of_str_off buff 0x33 = 0) in

  let known_off_off = head_length - 0x34 in
  let known_cnt_off = head_length - 0x30 in
  let minfo_off_off = head_length - 0x2c in
  let minfo_cnt_off = head_length - 0x28 in
  let mlist_off_off = head_length - 0x24 in
  let mlist_cnt_off = head_length - 0x20 in
  let islot_off_off = head_length - 0x1c in
  let items_off_off = head_length - 0x18 in
  let items_cnt_off = head_length - 0x14 in
  let effec_off_off = head_length - 0x10 in
  let effec_cnt_off = head_length - 0x0c in

  let known_off = int_of_str_off buff known_off_off in
  let known_cnt = int_of_str_off buff known_cnt_off in
  let minfo_off = int_of_str_off buff minfo_off_off in
  let minfo_cnt = int_of_str_off buff minfo_cnt_off in
  let mlist_off = int_of_str_off buff mlist_off_off in
  let mlist_cnt = int_of_str_off buff mlist_cnt_off in
  let islot_off = int_of_str_off buff islot_off_off in
  let items_off = int_of_str_off buff items_off_off in
  let items_cnt = int_of_str_off buff items_cnt_off in
  let effec_off = int_of_str_off buff effec_off_off in
  let effec_cnt = int_of_str_off buff effec_cnt_off in
  
  if !debug_ocaml then log_and_print "Read static stuff\n";

  let known_spells = ref [] in
  for i = 0 to known_cnt - 1 do
    let this_buff = String.sub buff (known_off + i * 0xc) 0xc in
    let name = String.uppercase (get_string_of_size this_buff 0 8) in
    let level = short_of_str_off this_buff 8 in
    let spell_type = short_of_str_off this_buff 0xa in
    known_spells := (name,level,spell_type) :: !known_spells;
  done;
  if !debug_ocaml then log_and_print "Read Known Spells\n";

  let minfo = ref [] in
  let mlist_i = ref 0 in
  for i = 0 to minfo_cnt - 1 do
    let this_buff = String.sub buff (minfo_off + i * 0x10) 0x10 in
    let level = short_of_str_off this_buff 0 in
    let count = short_of_str_off this_buff 2 in
    let count2 = short_of_str_off this_buff 4 in
    let spell_type = short_of_str_off this_buff 6 in
    let mlist_idx = int_of_str_off this_buff 8 in
    let mlist_cnt = int_of_str_off this_buff 0xc in
    let mlist = ref [] in
    while !mlist_i < mlist_idx + mlist_cnt do
      let this_buff = String.sub buff (mlist_off + !mlist_i * 0xc) 0xc in
      let name = String.uppercase (get_string_of_size this_buff 0 8) in
      let memorized = 0 <> (int_of_str_off this_buff 8) in
      incr mlist_i;
      mlist := (name, memorized) :: !mlist
    done;
    minfo := (level, count, count2, spell_type, List.rev !mlist) :: !minfo
  done;
  if !debug_ocaml then log_and_print "Read MemInf\n";

  let effects = ref [] in
  let eff_l = if is_eff_v1 then 0x30 else 0x108 in
  for i = 0 to effec_cnt - 1 do
    let this_buff = String.sub buff (effec_off + i * eff_l) eff_l in
    effects := this_buff :: !effects
  done;
  if !debug_ocaml then log_and_print "Read Effects\n";

  let items = ref [] in
  for i = 0 to items_cnt - 1 do
    let slots = ref [] in
    let this_buff = String.sub buff (items_off +i * 0x14) 0x14 in
    let name = String.uppercase (get_string_of_size this_buff 0 8) in
    let unknown = short_of_str_off this_buff 0x8 in
    let q1 = short_of_str_off this_buff 0xa in
    let q2 = short_of_str_off this_buff 0xc in
    let q3 = short_of_str_off this_buff 0xe in
    let flags = int_of_str_off this_buff 0x10 in
    for j = 0 to 37 do
      let which_one = short_of_str_off buff (islot_off + j * 2) in
      if which_one = i then slots := j :: !slots
    done;
    items := (name,(unknown,q1,q2,q3,flags,!slots)) :: !items
  done;
  if !debug_ocaml then log_and_print "Read items\n";

  let equiped = short_of_str_off buff (islot_off + 76) in
  let ending_unknown = short_of_str_off buff (islot_off + 78) in
  if !debug_ocaml then log_and_print "Read last stuff\n";
  {
   main_body = main_body;
   known_spells = List.rev !known_spells;
   memorized_info = List.rev !minfo;
   effects = (List.rev !effects, is_eff_v1);
   items = List.rev !items;
   equipped = equiped;
   ending_unknown = ending_unknown;
 }
;;

let string_of_cre cre =
  let buff = Buffer.create 1000 in

  let main_body = cre.main_body in
  let known_spells = cre.known_spells in
  let memorized_info = cre.memorized_info in
  let (effects, is_eff_v1) = cre.effects in
  let items = cre.items in
  let equipped = cre.equipped in
  let ending_unknown = cre.ending_unknown in
  
  let head_length = match String.sub main_body 0 8 with
  | "CRE V1.0" -> 0x2d4
  | "CRE V1.2" -> 0x378
  | "CRE V2.2" -> raise Cre22
  | "CRE V9.0" -> 0x33c
  | _ -> failwith "not a vaild CRE file"
  in
  let known_off_off = head_length - 0x34 in
  let known_cnt_off = head_length - 0x30 in
  let minfo_off_off = head_length - 0x2c in
  let minfo_cnt_off = head_length - 0x28 in
  let mlist_off_off = head_length - 0x24 in
  let mlist_cnt_off = head_length - 0x20 in
  let islot_off_off = head_length - 0x1c in
  let items_off_off = head_length - 0x18 in
  let items_cnt_off = head_length - 0x14 in
  let effec_off_off = head_length - 0x10 in
  let effec_cnt_off = head_length - 0x0c in
  
  let known_off = String.length main_body in
  let known_cnt = List.length known_spells in
  let known_buff = Buffer.create 100 in
  List.iter (fun (name,level,spell_type) ->
    let buff = String.make 0xc '\000' in
    write_resref buff 0 name;
    write_short buff 0x8 level;
    write_short buff 0xa spell_type;
    Buffer.add_string known_buff buff
	    ) known_spells;
  let known_buff = Buffer.contents known_buff in
  
  let minfo_off = known_off + String.length known_buff in
  let minfo_cnt = List.length memorized_info in
  let minfo_buff = Buffer.create 100 in
  let mlist_buff = Buffer.create 100 in
  let mlist_i = ref 0 in
  List.iter (fun (level, count, count2, spell_type, mlist) ->
    let buff = String.make 0x10 '\000' in
    write_short buff 0 level;
    write_short buff 2 count;
    write_short buff 4 count2;
    write_short buff 6 spell_type;
    write_int buff 8 !mlist_i;
    write_int buff 0xc (List.length mlist);
    List.iter (fun (name, memor) ->
      let buff = String.make 0xc '\000' in
      write_resref buff 0 name;
      write_int buff 8 (if memor then 1 else 0);
      incr mlist_i;
      Buffer.add_string mlist_buff buff
	      ) mlist;
    Buffer.add_string minfo_buff buff;
    ()
	    ) memorized_info;
  let minfo_buff = Buffer.contents minfo_buff in
  let mlist_buff = Buffer.contents mlist_buff in
  let mlist_off = minfo_off + String.length minfo_buff in
  let mlist_cnt = !mlist_i in
  
  let effec_off = mlist_off + String.length mlist_buff in
  let effec_cnt = List.length effects in
  let effec_buff = Buffer.create 100 in
  List.iter (Buffer.add_string effec_buff) effects;
  let effec_buff = Buffer.contents effec_buff in
  
  let items_off = effec_off + String.length effec_buff in
  let items_cnt = List.length items in
  let islot_off = items_off + 0x14 * items_cnt in
  let items_buff = Buffer.create 100 in
  let islot_buff = String.make 0x50 '\000' in
  let items_i = ref 0 in
  for i = 0 to 0x26 do
    write_short islot_buff (i * 2) (-1);
  done;
  List.iter (fun (name, (unknown, q1, q2, q3, flags, slots)) ->
    let buff = String.make 0x14 '\000' in
    write_resref buff 0x00 name;
    write_short  buff 0x08 unknown;
    write_short  buff 0x0a q1;
    write_short  buff 0x0c q2;
    write_short  buff 0x0e q3;
    write_int    buff 0x10 flags;
    List.iter (fun slot ->
      write_short  islot_buff (slot * 2) !items_i;
	      ) slots;
    Buffer.add_string items_buff buff;
    incr items_i
	    ) items;
  let items_buff = Buffer.contents items_buff in
  write_short islot_buff 76 equipped;
  write_short islot_buff 78 ending_unknown;
  
  write_int main_body known_off_off known_off;
  write_int main_body known_cnt_off known_cnt;
  write_int main_body minfo_off_off minfo_off;
  write_int main_body minfo_cnt_off minfo_cnt;
  write_int main_body mlist_off_off mlist_off;
  write_int main_body mlist_cnt_off mlist_cnt;
  write_int main_body islot_off_off islot_off;
  write_int main_body items_off_off items_off;
  write_int main_body items_cnt_off items_cnt;
  write_int main_body effec_off_off effec_off;
  write_int main_body effec_cnt_off effec_cnt;
  
  Buffer.add_string buff main_body;
  Buffer.add_string buff known_buff;
  Buffer.add_string buff minfo_buff;
  Buffer.add_string buff mlist_buff;
  Buffer.add_string buff effec_buff;
  Buffer.add_string buff items_buff;
  Buffer.add_string buff islot_buff;
  
  Buffer.contents buff
;;
