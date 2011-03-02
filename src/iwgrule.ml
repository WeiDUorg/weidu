(*
 * Weimer's BG2 -> IWD2 automatic converter
 *)
open Util
open Bcs
open Ids
open Dlg 
open Iwgconf

type rule =
    Replace of string * (string list)
  | ReplaceWithConverted of string * (string list) 

let convert_at_end = ref ([] : ((string * string) list))

(***********************************************************************
 *
 * Game Data
 *
 ***********************************************************************)
let copy_one_file src dst = 
  let buff = load_file src in
  let oc = Case_ins.perv_open_out_bin dst in
  output_string oc buff ;
  close_out oc 

let override = ref "override" 

let save_in_override_truly_generic n e args fn =
  let path = Printf.sprintf "%s/%s/%s.%s" 
      (config.target).Load.game_path
      !override
      n e 
  in 
  let oc = Case_ins.perv_open_out_gen args 0o666 path in
  fn oc ; 
  close_out oc ;
  log_and_print "[%s] saved\n" path 

let save_in_override n e buff =
  save_in_override_truly_generic n e 
    [Open_wronly; Open_creat; Open_trunc; Open_binary] 
    (fun oc -> output_string oc buff)

let save_in_override_generic n e fn =
  save_in_override_truly_generic n e 
    [Open_wronly; Open_creat; Open_trunc; Open_binary] 
    fn 

let load_target_res r e =
  Stats.time "LOAD" (fun () -> 
    try 
      Load.skip_next_load_error := true ; 
      let buff,_ = Load.load_resource "weimorph" config.target true r e in
      buff
    with exc -> 
      error e "%8s: resource not found in target: %s\n" 
	(printexc_to_string exc) r ; raise exc 
		    ) () 

let load_source_res r e =
  Stats.time "LOAD" (fun () -> 
    try 
      Load.skip_next_load_error := true ; 
      let buff,_ = Load.load_resource "weimorph" config.source true r e in
      buff
    with exc -> 
      error e "%8s: resource not found in source: %s\n" 
	(printexc_to_string exc) r ; raise exc 
		    ) () 

type convert_eff_args = { 
    ce_is_shield : bool ;
  } 

type convert_bcs_args = {
    cb_is_cutscene : bool ;
    cb_is_infopoint : bool ; 
  } 
let default_convert_bcs_args = {
  cb_is_cutscene = false ;
  cb_is_infopoint = false ;
} 


type spl = { 
    mutable s_gname : int ; 
    mutable s_iname : int ; 
    mutable s_sound : string ;
    mutable s_loc : int ;
    mutable s_type : int ; (* inventory item "S1" *)
    mutable s_wiz : int ;
    mutable s_pri : int ;
    mutable s_school1 : int ;
    mutable s_school2 : int ;
    mutable s_cat : int ;
    mutable s_lev : int ;
    mutable s_icon : string ; (* BG2 prof required *) 
    mutable s_desc : int;
    mutable s_global : eff array ;
    mutable s_abil: abil array ;
  } 
and itm = { 
    mutable i_gname : int ; 
    mutable i_iname : int ; 
    mutable i_flags : int ;
    mutable i_cat : int ;
    mutable i_inv : string ; (* inventory item "S1" *)
    mutable i_u1 : Int32.t ;
    mutable i_u2 : int ;
    mutable i_u3 : int ;
    mutable i_u4 : int ;
    mutable i_u5 : int ;
    mutable i_prof : int ; (* BG2 prof required *) 
    mutable i_price : int;
    mutable i_stack: int;
    mutable i_icon : string ;
    mutable i_gicon : string ;
    mutable i_lore : int;
    mutable i_weight : int ;
    mutable i_gdesc : int;
    mutable i_idesc : int;
    mutable i_cicon : string ; 
    mutable i_enchant : int;
    mutable i_equipped : eff array ;
    mutable i_abil: abil array ;
  }
and abil = {
    mutable a_type : int;
    mutable a_iloc : int;
    mutable a_icon : string ;
    mutable a_targ : int;
    mutable a_range : int;
    mutable a_launch : int;
    mutable a_speed : int;
    mutable a_tohit : int;
    mutable a_dicesize : int;
    mutable a_numdice : int;
    mutable a_dmgbonus : int;
    mutable a_dmgtype : int;
    mutable a_charges : int;
    mutable a_whendrained : int;
    mutable a_allowstr : int ;
    mutable a_recharge : int;
    mutable a_attacktype : int;
    mutable a_proj : int;
    mutable a_anim1 : int;
    mutable a_anim2 : int;
    mutable a_anim3 : int;
    mutable a_is1 : int;
    mutable a_is2 : int;
    mutable a_is3 : int;
    mutable a_eff : eff array ;
  } 
and eff = {
    mutable e_opcode : int;
    mutable e_target : int;
    mutable e_power : int;
    mutable e_arg1 : int;
    mutable e_arg2 : int;
    mutable e_duration : int;
    mutable e_disres : int;
    mutable e_time : int;
    mutable e_prob1: int;
    mutable e_prob2: int;
    mutable e_resref : string;
    mutable e_numdice : int;
    mutable e_dicesize : int;
    mutable e_savetype : int;
    mutable e_savebonus : int;
  } 

let empty_effect () = {
  e_opcode = 0;
  e_target = 0;
  e_power = 0;
  e_arg1 = 0;
  e_arg2 = 0;
  e_duration = 0;
  e_disres = 0;
  e_time = 0;
  e_prob1= 0;
  e_prob2= 0;
  e_resref = ""; 
  e_numdice = 0;
  e_dicesize = 0;
  e_savetype = 0;
  e_savebonus = 0;
} 


type cre = {
    mutable  c_name : int ;
    mutable  c_xpv : int ;
    mutable  c_status : int ;
    mutable  c_chp : int ; 
    mutable  c_mhp : int ; 
    mutable  c_anim : int; 
    mutable  c_color : string ; (* seven chars *)
    mutable  c_sport : string ;
    mutable  c_lport : string ;
    mutable  c_ac : int;
    mutable  c_s_f: int; (* save fort *)
    mutable  c_s_r: int; (* ref *)
    mutable  c_s_w: int; (* will *)
    mutable  c_res : int array ;
    mutable  c_mdam_res : int ; 
    mutable  c_orig_level : int array ; 
    mutable  c_lev: (c_class * int) list ;
    mutable  c_sound : int array ;
    mutable  c_skill : (int * int) list ;  (* use SKILLS.IDS values *)
    mutable  c_feat  : (Int32.t * int) list ;  (* use FEATS.IDS values *)
    mutable  c_featweapon : int ;          (* weapon feat level *) 
    mutable  c_subrace : int ;
    mutable  c_attr : int array ; 
    mutable  c_orig_kit : int;
    mutable  c_script : string list ; 
    mutable  c_ea : int;
    mutable  c_general : int;
    mutable  c_race : int;
    mutable  c_orig_class : int; 
    mutable  c_class : int;
    mutable  c_specific : int;
    mutable  c_gender : int;
    mutable  c_align : int ;
    mutable  c_variable : string;
    mutable  c_items : c_item list ; 
    mutable  c_dialog : string ; 
    mutable  c_spells : (int * string) array ; 
    (* effects *) 
    (* spells *) 
  } 
and c_item = {
    mutable ci_name : string ;
    ci_q1 : int ;
    ci_q2 : int ;
    ci_q3 : int ;
    mutable ci_flags : int ; 
    ci_slot : int ; 
  } 
and c_class =
    CC_Barb | CC_Bard | CC_Cler | CC_Drui | CC_Figh | CC_Monk | CC_Pala |
    CC_Rang | CC_Rogu | CC_Sorc | CC_Wiz

let class_to_short_str c = match c with 
| CC_Barb -> "b"
| CC_Bard -> "B"
| CC_Cler -> "c"
| CC_Drui -> "d"
| CC_Figh -> "f"
| CC_Monk -> "m"
| CC_Pala -> "p"
| CC_Rang -> "R"
| CC_Rogu -> "r"
| CC_Sorc -> "s"
| CC_Wiz ->  "w"

let rec lev_to_short_str lst = match lst with
| [] -> ""
| (c,i) :: tl -> Printf.sprintf "%s%d%s" 
      (class_to_short_str c) i (lev_to_short_str tl)

let mask_of_class c = match c with 
  CC_Barb -> 1
| CC_Bard -> 2
| CC_Cler -> 4
| CC_Drui -> 8
| CC_Figh -> 16
| CC_Monk -> 32
| CC_Pala -> 64
| CC_Rang -> 128
| CC_Rogu -> 256
| CC_Sorc -> 512
| CC_Wiz ->  1024
let name_of_class c = match c with 
  CC_Barb -> "Barb"
| CC_Bard -> "Bard"
| CC_Cler -> "Cler"
| CC_Drui -> "Drui"
| CC_Figh -> "Figh"
| CC_Monk -> "Monk"
| CC_Pala -> "Pala"
| CC_Rang -> "Rang"
| CC_Rogu -> "Rogu"
| CC_Sorc -> "Sorc"
| CC_Wiz ->  "Wiz"

let cre_can_use_weapons c =
  try 
    if c.c_general = 4 then false else 
    match (Bcs.ids_of_int (config.target) "ANIMATE" (Int32.of_int c.c_anim)).i_name with
    | "Abishai, White" | "BOY" | "Bat, Inside" | "Bear, Black" | "Bear, Brown" 
    | "Bear, Cave" | "Bear, Polar" | "Beetle" | "Beetle, Bombadier" |
      "Beetle, Boring" | "Beetle, Fire" | "Beetle, Rhinocerous" | "Beholder" |
      "Boar, Arctic" | "Carrion Crawler" | "Cat" | "Cat Great, Leopard" | 
      "Cat Great, Panther" | "Chicken" | "Chimera" | "Cornugon" | "Cow" | 
      "Creeping Doom" | "Deer" | "Dog, Wild" | "Doom Guard" | 
      "Dragon, Black (BG2)" | "Dragon, White Young" | "Drider, Female" | 
      "Drider, Male" | "Drowned Dead" | "Elemental, Air (BG2)" | 
      "Elemental, Air Small (BG2)" | "Elemental, Earth" | "Elemental, Fire" | 
      "Elemental, Fire (Small)(BG2)" | "Elemental, Shambling Mound" | 
      "Elemental, Water" | "Elemental, Water Small" | "Ettercap" | "FAT_MAN" 
    | "FAT_WOMAN" | "GIRL" | "Glabrezu" | "Golem, Clay (BG2)" | "Golem, Ice" 
    | "Golem, Iron" | "HARLOT_WOMAN" | "Harpy" | "Hook Horror" 
    | "Invisible - Giant" | "Invisible - Huge" | "Invisible - Medium" 
    | "Invisible - Small" | "Invisible - Tiny" | "Keg 1" | "Keg 2" | "Keg 3" 
    | "Myconid, Blue" | "Myconid, Red" | "NOBLE_MAN" | "NOBLE_WOMAN" 
    | "Otyugh" | "PEASANT_MAN" | "PEASANT_WOMAN" | "Rat" | "Raver" 
    | "Remorhaz" | "SLAVE" | "SLEEPING_DWARF" | "SLEEPING_MAN_HALFLING" 
    | "SLEEPING_MAN_HUMAN" | "STATIC_BOY" | "STATIC_GIRL" 
    | "STATIC_NOBLE_MAN_CHAIR" | "Slime, Green" | "Slime, Mustard" 
    | "Slime, Ochre" | "Slime, Olive" | "Snake" | "Spider, Giant" 
    | "Spider, Huge" | "Spider, Phase" | "Spider, Small" | "Spider, Sword" 
    | "Spider, Wraith" | "Umber Hulk" | "Water Weird" | "Werewolf" 
    | "Werewolf, Greater" | "Will O' Wisp" | "Wolf" | "Wolf, Winter" | "Worg" 
    | "Wyvern" | "Wyvern, White (Big)" | "Yuan Ti" | "Yuan Ti, Elite" 
    | "Yuan Ti, Halfbreed" | "Yuan Ti, Priest" | "Zombie" | "Zombie 2" 
    | "Imp" | "Mindflayer" | "Slayer" 
    | "Troll" | "Troll, Ice"| "Troll, Scrag" | "Troll, Snow"
    | "Djinni" | "Djinni w/legs" 
    | "Dog, War" 
      -> false 
    | _ -> true 
  with _ -> false 

(* IWD2 doesn't do <CHARNAME> and friends the way we are used to *) 
let charname_regexp = 
  [ 
    ((Str.regexp_string_case_fold "CHARNAME"),"PLAYER1") ;
    ((Str.regexp_string_case_fold "<PRO"),"<PROTAGONIST") ;
  ] 

let stat_regexp = Str.regexp_string_case_fold "STATISTICS" 
let sprite_is_dead_regexp = Str.regexp_string_case_fold "Sprite_Is_Dead" 
let newline_regexp = Str.regexp "[\r\n]+" 
let blank_regexp = Str.regexp "[\t ]+"  

(* how much should we scale down something like "fire resistance" *)
let eff_scale i = if i >= 100 then 100 else (i / 5) 
let eff_scale_int32 i = Int32.of_int (eff_scale (Int32.to_int i))

let target_action a = 
  try (Bcs.ids_of_sym (config.target) "ACTION" a).i_num 
  with Not_found -> 
    log_and_print "ERROR: PROGRAMMER: %s is not a Target action" a ; exit 1
let target_trigger a = 
  try (Bcs.ids_of_sym (config.target) "TRIGGER" a).i_num 
  with Not_found -> 
    log_and_print "ERROR: PROGRAMMER: %s is not a Target trigger" a ; exit 1
let target_opcode a = 
  try Int32.to_int ((Bcs.ids_of_sym (config.target) "TARGOP" a).i_num )
  with Not_found -> 
    log_and_print "ERROR: PROGRAMMER: %s is not a Target opcode" a ; exit 1
let empty_a = empty_action () 
let empty_t = empty_trigger () 
let empty_op = empty_object_param () 
let empty_eff = empty_effect () 
let myself = { empty_op with o_identifiers = Int32.one } (* myself *)
let player1 = { empty_op with o_identifiers = Int32.of_int 21 } (* player1 *)
let player2 = { empty_op with o_identifiers = Int32.of_int 22 } 
let player3 = { empty_op with o_identifiers = Int32.of_int 23 } 
let player4 = { empty_op with o_identifiers = Int32.of_int 24 } 
let player5 = { empty_op with o_identifiers = Int32.of_int 25 } 
let player6 = { empty_op with o_identifiers = Int32.of_int 26 } 
let protagonist = { empty_op with o_identifiers = Int32.of_int 27 } 

let ids_pair_of_opcode_arg i = match i with
| 2 -> "OBJECT","OBJECT"
| 3 -> "GENERAL","GENERAL"
| 4 -> "RACE","RACE"
| 5 -> "CLASS","CLASS"
| 6 -> "SPECIFIC","SPECIFIC" 
| 7 -> "GENDER","GENDER"
| 8 -> "ALIGN","ALIGNMNT" 
| _ -> "",""


(***********************************************************************
 * ITM Files (low-level I/O) 
 ***********************************************************************)
let read_eff buff off = { 
  e_opcode = short_of_str_off buff (off + 0) ;
  e_target = byte_of_str_off buff (off + 2) ;
  e_power = byte_of_str_off buff (off + 3) ;
  e_arg1 = int_of_str_off buff (off + 4) ;
  e_arg2 = int_of_str_off buff (off + 8) ;
  e_duration = byte_of_str_off buff (off + 12) ;
  e_disres = byte_of_str_off buff (off + 13) ;
  e_time = int_of_str_off buff (off + 14) ;
  e_prob1= byte_of_str_off buff (off + 18) ;
  e_prob2= byte_of_str_off buff (off + 19) ;
  e_resref = get_string_of_size buff (off+20) 8 ; 
  e_numdice = int_of_str_off buff (off + 28) ;
  e_dicesize = int_of_str_off buff (off + 32) ;
  e_savetype = int_of_str_off buff (off + 36) ;
  e_savebonus = int_of_str_off buff (off + 40) ;
} 
let write_eff buff off e = 
  write_short buff (off + 0) e.e_opcode ;
  write_byte buff (off + 2) e.e_target ;
  write_byte buff (off + 3) e.e_power ;
  write_int buff (off + 4) e.e_arg1 ;
  write_int buff (off + 8) e.e_arg2 ;
  write_byte buff (off + 12) e.e_duration ;
  write_byte buff (off + 13) e.e_disres ;
  write_int buff (off + 14) e.e_time ;
  write_byte buff (off + 18) e.e_prob1;
  write_byte buff (off + 19) e.e_prob2;
  write_resref buff (off + 20) e.e_resref ;
  write_int buff (off + 28) e.e_numdice ;
  write_int buff (off + 32) e.e_dicesize ;
  write_int buff (off + 36) e.e_savetype ;
  write_int buff (off + 40) e.e_savebonus ;
  () 

let read_spl_abil buff off = { 
  a_type = byte_of_str_off buff (off + 0) ;
  a_iloc = short_of_str_off buff (off + 2) ;
  a_icon = get_string_of_size buff (off+4) 8 ; 
  a_targ = short_of_str_off buff (off + 12) ;
  a_range = short_of_str_off buff (off + 14) ;
  a_launch = short_of_str_off buff (off + 16) ;
  a_speed = short_of_str_off buff (off + 18) ;
  a_tohit = short_of_str_off buff (off + 20) ;
  a_dicesize = short_of_str_off buff (off + 22) ;
  a_numdice = short_of_str_off buff (off + 24) ;
  a_dmgbonus = short_of_str_off buff (off + 26) ;
  a_dmgtype = short_of_str_off buff (off + 28) ;
  (* effect, eff index *) 
  a_charges = short_of_str_off buff (off + 34) ;
  a_proj = short_of_str_off buff (off + 38) ;

  a_whendrained = 0 ;
  a_allowstr = 0 ;
  a_recharge = 0 ;
  a_attacktype = 0 ;
  a_anim1 = 0 ;
  a_anim2 = 0 ;
  a_anim3 = 0 ;
  a_is1 = 0 ;
  a_is2 = 0 ;
  a_is3 = 0 ;
  a_eff = [||] ; 
},(short_of_str_off buff (off + 30)),(short_of_str_off buff (off +32))

let write_spl_abil buff off a eff_index = 
  write_byte buff (off + 0)  a.a_type ;
  write_short buff (off + 2)  a.a_iloc ;
  write_resref buff (off+4)  a.a_icon  ;
  write_short buff (off + 12)  a.a_targ ;
  write_short buff (off + 14)  a.a_range ;
  write_short buff (off + 16)  a.a_launch ;
  write_short buff (off + 18)  a.a_speed ;
  write_short buff (off + 20)  a.a_tohit ;
  write_short buff (off + 22)  a.a_dicesize ;
  write_short buff (off + 24)  a.a_numdice ;
  write_short buff (off + 26)  a.a_dmgbonus ;
  write_short buff (off + 28)  a.a_dmgtype ;
  write_short buff (off + 30) (Array.length a.a_eff) ; 
  write_short buff (off + 32) eff_index ; 
  write_short buff (off + 34)  a.a_charges ;
  write_short buff (off + 38)  a.a_proj ;
  () 

let read_abil buff off = { 
  a_type = byte_of_str_off buff (off + 0) ;
  a_iloc = short_of_str_off buff (off + 2) ;
  a_icon = get_string_of_size buff (off+4) 8 ; 
  a_targ = short_of_str_off buff (off + 12) ;
  a_range = short_of_str_off buff (off + 14) ;
  a_launch = short_of_str_off buff (off + 16) ;
  a_speed = short_of_str_off buff (off + 18) ;
  a_tohit = short_of_str_off buff (off + 20) ;
  a_dicesize = short_of_str_off buff (off + 22) ;
  a_numdice = short_of_str_off buff (off + 24) ;
  a_dmgbonus = short_of_str_off buff (off + 26) ;
  a_dmgtype = short_of_str_off buff (off + 28) ;
  (* effect, eff index *) 
  a_charges = short_of_str_off buff (off + 34) ;
  a_whendrained = byte_of_str_off buff (off + 36) ;
  (* unknown byte *) 
  a_allowstr = byte_of_str_off buff (off + 38) ;
  a_recharge = byte_of_str_off buff (off + 39) ;
  a_attacktype = 0 ; 
  (* unknown short *) 
  a_proj = short_of_str_off buff (off + 42) ;
  a_anim1 = short_of_str_off buff (off + 44) ;
  a_anim2 = short_of_str_off buff (off + 46) ;
  a_anim3 = short_of_str_off buff (off + 48) ;
  a_is1 = short_of_str_off buff (off + 50) ;
  a_is2 = short_of_str_off buff (off + 52) ;
  a_is3 = short_of_str_off buff (off + 54) ;
  a_eff = [||] ; 
},(short_of_str_off buff (off + 30)),(short_of_str_off buff (off +32))

let write_abil buff off a eff_index = 
  write_byte buff (off + 0)  a.a_type ;
  write_short buff (off + 2)  a.a_iloc ;
  write_resref buff (off+4)  a.a_icon  ;
  write_short buff (off + 12)  a.a_targ ;
  write_short buff (off + 14)  a.a_range ;
  write_short buff (off + 16)  a.a_launch ;
  write_short buff (off + 18)  a.a_speed ;
  write_short buff (off + 20)  a.a_tohit ;
  write_short buff (off + 22)  a.a_dicesize ;
  write_short buff (off + 24)  a.a_numdice ;
  write_short buff (off + 26)  a.a_dmgbonus ;
  write_short buff (off + 28)  a.a_dmgtype ;
  write_short buff (off + 30) (Array.length a.a_eff) ; 
  write_short buff (off + 32) eff_index ; 
  write_short buff (off + 34)  a.a_charges ;
  write_byte buff (off + 36)  a.a_whendrained ;
  write_byte buff (off + 38)  a.a_allowstr ;
  write_byte buff (off + 39)  a.a_recharge ;
  write_byte buff (off + 39)  a.a_recharge ;
  write_short buff (off + 40)  a.a_attacktype ;
  write_short buff (off + 42)  a.a_proj ;
  write_short buff (off + 44)  a.a_anim1 ;
  write_short buff (off + 46)  a.a_anim2 ;
  write_short buff (off + 48)  a.a_anim3 ;
  write_short buff (off + 50)  a.a_is1 ;
  write_short buff (off + 52)  a.a_is2 ;
  write_short buff (off + 54)  a.a_is3 ;
  () 

let read_itm buff = 
  (if (String.length buff < 0x72) then failwith "ITM is too small") ;
  let i = { 
    i_gname = int_of_str_off buff 8;
    i_iname = int_of_str_off buff 0xc;
    i_flags = short_of_str_off buff 0x18;
    i_cat = short_of_str_off buff 0x1c;
    i_inv = String.sub buff 0x22 2 ; 
    i_u1 = int32_of_str_off buff 0x1e;
    i_u2 = byte_of_str_off buff 0x29;
    i_u3 = byte_of_str_off buff 0x2b;
    i_u4 = byte_of_str_off buff 0x2d;
    i_u5 = byte_of_str_off buff 0x2f;
    i_prof = byte_of_str_off buff 0x31; 
    i_price = int_of_str_off buff 0x34;
    i_stack= short_of_str_off buff 0x38;
    i_icon = get_string_of_size buff 0x3a 8; 
    i_gicon = get_string_of_size buff 0x44 8;
    i_lore = short_of_str_off buff 0x42;
    i_weight = int_of_str_off buff 0x4c;
    i_gdesc = int_of_str_off buff 0x50;
    i_idesc = int_of_str_off buff 0x54;
    i_cicon = get_string_of_size buff 0x58 8;
    i_enchant = int_of_str_off buff 0x60;
    i_equipped = [| |] ; 
    i_abil = [| |] ; 
  } in 
  let abil_off = int_of_str_off buff 0x64 in
  let num_abil = short_of_str_off buff 0x68 in 
  let eff_off = int_of_str_off buff 0x6a in
  let num_equip = short_of_str_off buff 0x70 in 
  let num_effs = ref num_equip in 
  (* log_and_print "... num_equip = %d, num_effs = %d\n" num_equip !num_effs ; *)
  let abils = Array.init num_abil (fun n ->
    (* log_and_print "... reading abil %d\n" n ;  *)
    let abil, num_eff, eff_index = read_abil buff (abil_off + (n * 0x38)) in
    num_effs := num_eff + !num_effs ;
    (abil,num_eff,eff_index) 
				  ) in 
  let effs = Array.init !num_effs (fun n ->
    (* log_and_print "... reading eff %d\n" n ;  *)
    read_eff buff (eff_off + (n * 48))
				  ) in 

  (if num_equip <> 0 then i.i_equipped <- Array.sub effs 0 num_equip );
  (if num_abil <> 0 then 
    i.i_abil <- Array.map (fun (a,num_eff,eff_index) ->
      if num_eff > 0 then 
	{ a with a_eff = Array.sub effs eff_index num_eff ; } 
      else a 
			  ) abils ); 
  i 

let serialize_itm i =
  let num_abil = Array.length i.i_abil in
  let num_eff = Array.fold_left (fun acc elt ->
    acc + (Array.length elt.a_eff)) (Array.length i.i_equipped) i.i_abil in 
  let header_size = 
    if config.target_variant = IWD2 then 0x72 + 16 else 0x72 
  in
  let eff_size = num_eff * 48 in
  let abil_size = num_abil * 0x38 in 
  let buff = String.make (header_size + eff_size + abil_size) (Char.chr 0) in 

  (match config.target_variant with
  | IWD2 -> String.blit "ITM V2.0" 0 buff 0 8 ; 
  | _ -> String.blit "ITM V1  " 0 buff 0 8 ; 
  ) ;

  write_int buff 8 i.i_gname ;
  write_int buff 0xc i.i_iname ;
  write_short buff 0x18 i.i_flags ;
  write_short buff 0x1c i.i_cat ;
  String.blit i.i_inv 0 buff 0x22 2 ;
  write_int32 buff 0x1e i.i_u1 ;
  write_byte buff 0x29 i.i_u2 ;
  write_byte buff 0x2b i.i_u3 ;
  write_byte buff 0x2d i.i_u4 ;
  write_byte buff 0x2f i.i_u5 ;
  (if config.target_variant <> IWD2 then write_byte buff 0x31 i.i_prof) ; 
  write_int buff 0x34 i.i_price ;
  write_short buff 0x38 i.i_stack;
  write_resref buff 0x3a i.i_icon  ;
  write_resref buff 0x44 i.i_gicon ;
  write_short buff 0x42 i.i_lore ;
  write_int buff 0x4c i.i_weight ;
  write_int buff 0x50 i.i_gdesc ;
  write_int buff 0x54 i.i_idesc ;
  write_resref buff 0x58 i.i_cicon ;
  write_int buff 0x60 i.i_enchant ;

  let abil_offset = 
    if config.target_variant = IWD2 then 0x82 else 0x82 - 16 
  in 
  write_int buff 0x64 abil_offset ;
  write_short buff 0x68 num_abil ; 

  let eff_offset = abil_offset + abil_size in 
  write_int buff 0x6a eff_offset ; 
  write_short buff 0x70 (Array.length i.i_equipped) ; 

  (* write out the abilities *)
  let eff_index = ref (Array.length i.i_equipped) in 
  Array.iteri (fun n a ->
    let off = abil_offset + (n * 0x38) in 
    write_abil buff off a !eff_index ;
    eff_index := !eff_index + (Array.length a.a_eff) 
	      ) i.i_abil ;

  (* gather all of the effects *) 
  let all_effs = Array.fold_left (fun acc elt ->
    Array.append acc (elt.a_eff) 
				 ) i.i_equipped i.i_abil in

  Array.iteri (fun n e ->
    let off = eff_offset + (n * 48) in 
    write_eff buff off e
	      ) all_effs ;
  buff 

(***********************************************************************
 * SPL Files (low-level I/O) 
 ***********************************************************************)
let read_spl buff r = 
  (if (String.length buff < 0x72) then failwith "SPL is too small") ;
  let s = { 
    s_gname = int_of_str_off buff 8 ;
    s_iname = int_of_str_off buff 0xc ;
    s_sound = get_string_of_size buff 0x10 8 ;
    s_loc = short_of_str_off buff 0x18 ;
    s_type = short_of_str_off buff 0x1c ;
    s_wiz = short_of_str_off buff 0x1e ;
    s_pri = short_of_str_off buff 0x20 ;
    s_school1 = short_of_str_off buff 0x22; 
    s_school2 = short_of_str_off buff 0x25; 
    s_cat = byte_of_str_off buff 0x27 ; 
    s_lev = int_of_str_off buff 0x34 ;
    s_icon = get_string_of_size buff 0x3a 8 ; 
    s_desc = int_of_str_off buff 0x50 ; 
    s_global = [| |] ;
    s_abil= [| |] ;
  } in 
  let abil_off = int_of_str_off buff 0x64 in
  let num_abil = short_of_str_off buff 0x68 in 
  let eff_off = int_of_str_off buff 0x6a in
  let num_equip = short_of_str_off buff 0x70 in 
  let num_effs = ref num_equip in 
  let abils = Array.init num_abil (fun n ->
    let abil, num_eff, eff_index = read_spl_abil buff (abil_off + (n * 40)) in
    num_effs := num_eff + !num_effs ;
    (abil,num_eff,eff_index) 
				  ) in 
  let effs = Array.init !num_effs (fun n ->
    read_eff buff (eff_off + (n * 48))
				  ) in 
  (if num_equip <> 0 then s.s_global <- Array.sub effs 0 num_equip );
  (if num_abil <> 0 then 
    s.s_abil <- Array.map (fun (a,num_eff,eff_index) ->
      if num_eff > 0 then 
	{ a with a_eff = Array.sub effs eff_index num_eff ; } 
      else a 
			  ) abils ); 
  s 

let serialize_spl s r =
  let num_abil = Array.length s.s_abil in
  let num_eff = Array.fold_left (fun acc elt ->
    acc + (Array.length elt.a_eff)) (Array.length s.s_global) s.s_abil in 
  let header_size = 0x72 + 16 in
  let eff_size = num_eff * 48 in
  let abil_size = num_abil * 40 in 
  let total_size = header_size + eff_size + abil_size in
  (if (total_size < header_size) then 
    failwith "total size less than header size" );
  let buff = String.make total_size (Char.chr 0) in 

  String.blit "SPL V2.0" 0 buff 0 8 ; 

  (try 
    write_int buff 8 s.s_gname ;
    write_int buff 0xc s.s_iname ;
    write_resref buff 0x10 s.s_sound ; 
    write_short buff 0x18 s.s_loc ;
    write_short buff 0x1c s.s_type ;
    write_short buff 0x1e s.s_wiz ;
    write_short buff 0x20 s.s_pri ;
    write_short buff 0x22 s.s_school1 ;
    write_short buff 0x25 s.s_school2 ;
    write_byte buff 0x27 s.s_cat ;
    write_int buff 0x34 s.s_lev ;
    write_resref buff 0x3a s.s_icon ; 
    write_int buff 0x50 s.s_desc ;
  with e -> error "SPL" "%s: error serializing header\n" r ; raise e) ;

  let abil_offset = 0x82 in 
  write_int buff 0x64 abil_offset ;
  write_short buff 0x68 num_abil ; 

  let eff_offset = abil_offset + abil_size in 
  write_int buff 0x6a eff_offset ; 
  write_short buff 0x70 (Array.length s.s_global) ; 

  (* write out the abilities *)
  let eff_index = ref (Array.length s.s_global) in 
  (try 
    Array.iteri (fun n a ->
      let off = abil_offset + (n * 40) in 
      write_spl_abil buff off a !eff_index ;
      eff_index := !eff_index + (Array.length a.a_eff) 
		) s.s_abil ;
  with e -> error "SPL" "%s: error serializing abilities\n" r ; raise e) ;

  (* gather all of the effects *) 
  let all_effs = try Array.fold_left (fun acc elt ->
    Array.append acc (elt.a_eff) ) s.s_global s.s_abil 
  with e -> error "SPL" "%s: error gathering effects\n" r ; raise e ; 
  in

  ( try Array.iteri (fun n e ->
    let off = eff_offset + (n * 48) in 
    write_eff buff off e
		    ) all_effs with e -> 
		      error "SPL" "%s: error serializing effects\n" r ; raise e ) ;
  buff 


(***********************************************************************
 * To avoid interfering with IWD2's naming, we'll use our own naming
 * convention for converted files. Basically, FOO goes to @FOO with a 
 * few exceptions. 
 ***********************************************************************)
let rename_ht = Hashtbl.create 65535 
let mapping = Hashtbl.create 65535  
let rename_counter = ref 0 
let area_counter = ref 0 
let rename r e =   
  let r = String.uppercase r in 
  let e = if e = "WED" then "ARE" else e in 
  try 
    Hashtbl.find rename_ht (r,e)
  with Not_found -> begin
    let r' = 
      if e = "BMP" || e = "MOS" || (String.length r = 8) then begin
        let str2 = String.copy r in
        str2.[0] <- '_' ;
        str2
      end else if e = "ARE" || e = "WED" then 
        let str = 
          match config.source_variant, config.target_variant with
          | BG1, BG2 -> 
              let tmp = String.copy r in
              tmp.[0] <- 'F' ; tmp.[1] <- 'W' ;
              tmp 
          | _ -> Printf.sprintf "AR%04d" !area_counter 
        in 
        error "ARE" "Source %s becomes %s\n" r str ;
        incr area_counter ; 
        str 
      else begin
        "_" ^ r 
      end 
    in 
    if (Hashtbl.mem mapping (r',e)) then begin (* collide *) 
      let r'' = Printf.sprintf "_%X_%s" (!rename_counter) r in
      incr rename_counter ; 
      let r'' = str_to_exact_size r'' 8 in 
      (if Hashtbl.mem mapping (r'',e) then failwith "collide!") ; 
      Hashtbl.add mapping (r'',e) r ;
      Hashtbl.add rename_ht (r,e) r'' ;
      r''
    end else begin
      Hashtbl.add mapping (r',e) r ;
      Hashtbl.add rename_ht (r,e) r' ;
      r'
    end
  end 

(***********************************************************************
 * To save space, we hack up IWD2's CHITIN.KEY so that it refers directly
 * to the BG2 biffs. If you have a full install of BG2, most of the big
 * files (BAM, TIS, etc.) need not be touched by this conversion at all.  
 ***********************************************************************)

(* a list of resources in BG2 BIFFs that we want to add to IWD2's KEY
 * file *) 
let add_to_key_list = Hashtbl.create 32767 

(* old.ext from BG2's biffs will be listed as new.ext in IWD2's KEY *)
let add_to_key new_name old_name ext =
  try
    let targ_res = Key.find_resource config.target.Load.key new_name ext in
    () ; (* do nothing, already present in target! *)
  with Not_found -> 
    begin 
      let ht_key = (new_name,old_name,ext) in
      if Hashtbl.mem add_to_key_list ht_key then 
        () (* do nothing, already slated to be added! *)
      else begin
        try 
          let src_res = Key.find_resource config.source.Load.key old_name ext in
          Hashtbl.add add_to_key_list ht_key true 
        with Not_found -> 
          (* it's not in the source BIFFs, so it better be in the source
             override! *) 
          begin 
            try 
              let buff = load_source_res old_name ext in
              save_in_override new_name ext buff
            with _ -> () 
          end 
      end 
    end 

let find_exact_path_of_bif game bif_file =
  let biff_path = 
    let rec trial lst =
      match lst with
        [] -> game.Load.game_path ^ "/" ^ bif_file 
      | hd :: tl ->
          let perhaps = hd ^ "/" ^ bif_file in 
          log_only "BIFF may be in hard-drive CD-path [%s]\n" perhaps ; 
          if file_exists perhaps then 
            perhaps
          else trial tl 
    in
    trial (game.Load.cd_path_list)
  in biff_path 

let process_add_to_key () = 

  let skip3 str = String.sub str 3 ((String.length str) - 3) in 
  let slash = Str.regexp "[:/\\]+" in
  let slash_to_backslash str = Str.global_replace slash "\\\\" str in 
let non_backslash = Str.regexp "[^\\]+" in
let bs_to_elipsis str = Str.global_replace non_backslash ".." str in 
let elipsis_path = skip3 ((bs_to_elipsis (config.target).Load.game_path)) in 

let next_bif_index = ref ( Array.length (config.target).Load.key.Key.biff ) in

let index_of_bif_ht = Hashtbl.create 51 in 
let added_res_ht = Hashtbl.create 511 in 
let added_old_res_ht = Hashtbl.create 511 in 

let new_bif_list = ref [] in 
let new_resource_list = ref [] in 

let handle_new_bif b = 
  try
    Hashtbl.find index_of_bif_ht b.Key.filename 
  with Not_found -> begin
    let i = !next_bif_index in
    Hashtbl.add index_of_bif_ht b.Key.filename i ;
    incr next_bif_index ;
    let exact_path = (find_exact_path_of_bif config.source b.Key.filename) in 
    let hacked_path = slash_to_backslash 
        (elipsis_path ^ "\\" ^ (skip3 exact_path)) in
			  new_bif_list := 
			    { b with Key.filename = hacked_path ;
			      Key.locations = 1
			    } ::
			    !new_bif_list ;
	 i 
  end 
in 

let handle_new_res old_r new_name new_bif_index =
  if Hashtbl.mem added_res_ht new_name then
    ()
  else begin
    let new_r = { old_r with
		  Key.res_name = new_name ;
		  Key.bif_index = new_bif_index ; } in
    Hashtbl.add added_res_ht new_name true ;
    (if (Hashtbl.mem added_old_res_ht old_r) then begin
      let third_name = Hashtbl.find added_old_res_ht old_r in 
      error "KEY" "Double-linking Source's %8s.%s (links to %8s, %8s)\n"
        old_r.Key.res_name 
        (Key.ext_of_key old_r.Key.res_type) new_name 
        third_name 
    end ); 
    Hashtbl.add added_old_res_ht old_r new_name ; 
    new_resource_list := new_r :: !new_resource_list 
  end
in 

Hashtbl.iter (fun (new_name, old_name, ext) _ -> 
  try 
    let res = Key.find_resource (config.source).Load.key old_name ext in
    let old_bif = (config.source).Load.key.Key.biff.(res.Key.bif_index) in
    log_only "%s.%s is in %s\\%s\n" old_name ext 
      (config.source).Load.game_path old_bif.Key.filename ;

    let new_biff_index = handle_new_bif old_bif in 
    handle_new_res res new_name new_biff_index
  with e ->
    error "KEY" "Cannot add %8s.%s to KEY: %s (in Source)\n" 
      old_name ext (printexc_to_string e) 
	     ) add_to_key_list ;
{ 
  Key.biff = Array.append (config.target).Load.key.Key.biff 
    (Array.of_list (List.rev !new_bif_list)) ; 
  Key.resource = Array.append 
    (config.target).Load.key.Key.resource 
    (Array.of_list !new_resource_list) ;
  Key.resfind = (config.target).Load.key.Key.resfind 
}

type twoda = (string , (string array)) Hashtbl.t 

let load_2da game r = 
  let buff,_ = Load.load_resource "weimorph" game true r "2DA" in
  let res = Hashtbl.create 63 in
  let lines = Str.split newline_regexp buff in 
  let lines = match lines with
    a :: b :: c :: rest -> a :: (b ^" " ^ c) :: rest
  | _ -> lines 
  in 
  List.iter (fun line ->
    let words = Str.split_delim blank_regexp line in 
    let key = List.hd words in 
    let arr = Array.of_list (List.tl words) in 
    Hashtbl.add res key arr 
	    ) lines ;
  res 

type int_2da = (int,(int array)) Hashtbl.t 

let conv_to_int a =
  let res = Hashtbl.create 63 in
  Hashtbl.iter (fun k e ->
    let k' = try int_of_string k with _ -> 0 in
    let e' = Array.map (fun elt -> try int_of_string elt with _ -> 0) e in
    Hashtbl.add res k' e' 
	       ) a ; res

type spell_max_data = 
    { s_stat : int_2da ;
      s_brd : int_2da ;
      s_clr : int_2da ;
      s_drd : int_2da ;
      s_pal : int_2da ;
      s_rgr : int_2da ;
      s_sor : int_2da ;
      s_wiz : int_2da ; } 

let spell_max = ref None 

let setup_max_spell () = 
  let sm = 
    { s_stat = conv_to_int (load_2da config.target "MXSPLBON") ;
      s_brd = conv_to_int (load_2da config.target "MXSPLBRD") ;
      s_clr = conv_to_int (load_2da config.target "MXSPLCLR") ;
      s_drd = conv_to_int (load_2da config.target "MXSPLDRD") ;
      s_pal = conv_to_int (load_2da config.target "MXSPLPAL") ;
      s_rgr = conv_to_int (load_2da config.target "MXSPLRGR") ;
      s_sor = conv_to_int (load_2da config.target "MXSPLSOR") ;
      s_wiz = conv_to_int (load_2da config.target "MXSPLWIZ") ; } in
  spell_max := Some(sm) ; sm 


let max_spell_of c = 
  let s = match !spell_max with 
    Some(s) -> s
  | None -> setup_max_spell () 
  in 
  let res = Array.make 9 0 in 
  let add_in class_arr lvl stat fake_stat_bonus =
    try begin
      let new_arr = Hashtbl.find class_arr lvl in
      try
        let sb = Hashtbl.find s.s_stat stat in
        Array.iteri (fun i v -> 
          res.(i) <- res.(i) + v ;
          if v <> 0 then res.(i) <- res.(i) + sb.(i) + fake_stat_bonus;
		    ) new_arr 
      with _ -> (* no stat bonus *) 
        Array.iteri (fun i v -> res.(i) <- res.(i) + v) new_arr 
    end with _ -> () 
  in 
  List.iter (fun (cl,i) -> 
    match cl with
    | CC_Bard -> add_in s.s_brd i 5 0
    | CC_Cler -> add_in s.s_clr i 2 1 (* we don't do domain spells here,
					 so we just give all clerics one extra spell/level *) 
    | CC_Drui -> add_in s.s_drd i 2 0
    | CC_Pala -> add_in s.s_pal i 2 0
    | CC_Rang -> add_in s.s_rgr i 2 0
    | CC_Sorc -> add_in s.s_sor i 5 0
    | CC_Wiz ->  add_in s.s_wiz i 1 0
    | _ -> () 
	    ) c.c_lev ;
  res 

let lvl_of_spl_ht = Hashtbl.create 1023 

let lvl_of_spl r =
  try
    Hashtbl.find lvl_of_spl_ht r 
  with Not_found ->
    begin
      let buff,_ = Load.load_resource "weimorph" config.target true r "SPL" in
      let lvl = int_of_str_off buff 0x34 in
      Hashtbl.add lvl_of_spl_ht r lvl ;
      lvl 
    end

let random_memorize : (string array) array = Array.make 10 [| |]
let random_melee : (string array) array = Array.make 3 [| |]

let random_sling : (string array) array = Array.make 3 [| |]
let random_bullet : (string array) array = Array.make 3 [| |]
let random_xbow : (string array) array = Array.make 3 [| |]
let random_bolt : (string array) array = Array.make 3 [| |]
let random_bow : (string array) array = Array.make 3 [| |]
let random_arrow : (string array) array = Array.make 3 [| |]

type wmp = w_entry array 
and w_entry = {
    w_background : string ;
    w_width : int ;
    w_height : int ;
    w_unknown1 : int ;
    w_name : int ;
    w_unknown2 : int ;
    w_unknown3 : int ;
    w_mapicons : string;
    mutable w_area_entry : area_entry array ;
    (* mutable w_area_link : area_link array ; *) 
  } and area_entry = { 
    wa_name : string;
    wa_area : string ;
    mutable wa_longname : string ;
    mutable wa_status : int;
    wa_bamseq : int;
    wa_x : int;
    wa_y : int;
    wa_name2 : int; 
    wa_name3 : int; 
    wa_loading : string; 
    mutable wa_north : area_link array ; 
    mutable wa_west : area_link array ; 
    mutable wa_south : area_link array ; 
    mutable wa_east : area_link array ; 
  } and area_link = { 
    wl_area_index : int ;
    wl_entry : string ;
    wl_time : int;
    wl_unknown : int; 
    wl_rand1 : string ;
    wl_rand2 : string ;
    wl_rand3 : string ;
    wl_rand4 : string ;
    wl_rand5 : string ;
    wl_rand_prob : int ;
  } 


(***********************************************************************
 * WMP 
 ***********************************************************************)
let serialize_wmp w =
  let num_w_entry = Array.length w in 
  let num_area_entry = Array.fold_left (fun acc elt ->
    acc + (Array.length elt.w_area_entry)) 0 w in
  let num_area_link = Array.fold_left (fun acc elt ->
    acc + 
      (Array.fold_left (fun acc elt -> acc + 
          (Array.length elt.wa_north) +
          (Array.length elt.wa_west) +
          (Array.length elt.wa_south) +
          (Array.length elt.wa_east) 
		       ) 0 elt.w_area_entry)
				      ) 0 w in
  let header_size = 16 in
  let w_entry_size = num_w_entry * 184 in
  let w_entry_offset = 16 in 
  let area_entry_size = num_area_entry * 240 in 
  let area_entry_offset = w_entry_offset + w_entry_size in
  let area_link_size = num_area_link * 216 in 
  let area_link_offset = area_entry_offset + area_entry_size in 
  let total_size = header_size + w_entry_size + area_entry_size +
      area_link_size in 
  let buff = String.make total_size '\000' in 
  String.blit "WMAPV1.0" 0 buff 0 8 ; 
  write_int buff 0x8 num_w_entry ;
  write_int buff 0xc w_entry_offset ;
  let ae_sofar = ref 0 in
  let al_sofar = ref 0 in
  Array.iteri (fun i a ->
    let off = w_entry_offset + (i * 184) in
    write_resref buff off a.w_background ;
    write_int buff (off+0x8) a.w_width ;
    write_int buff (off+0xc) a.w_height ;
    write_int buff (off+0x10) a.w_unknown1 ;
    write_int buff (off+0x14) a.w_name ;
    write_int buff (off+0x18) a.w_unknown2 ;
    write_int buff (off+0x1c) a.w_unknown3 ;
    write_resref buff (off+0x30) a.w_mapicons ;

    write_int buff (off+0x20) (Array.length a.w_area_entry) ; 
    let my_ae_offset = (area_entry_offset + (!ae_sofar * 240)) in 
    write_int buff (off+0x24) my_ae_offset ; 

    let my_al_offset = (area_link_offset + (!al_sofar * 216)) in 
    write_int buff (off+0x28) my_al_offset ;
    write_int buff (off+0x2c) num_area_link ; 

    let area_link_j = ref 0 in 
    let write_area_link al = begin
      let j = !area_link_j in 
      incr area_link_j ;
      let off = my_al_offset + (j * 216) in 
      write_int buff (off+ 0x0) al.wl_area_index ;
      String.blit (str_to_exact_size al.wl_entry 32) 0 buff (off+0x4) 32 ;
      write_int buff (off+0x24) al.wl_time ;
      write_int buff (off+0x28) al.wl_unknown ;
      write_resref buff (off+0x2c) al.wl_rand1;
      write_resref buff (off+0x34) al.wl_rand2;
      write_resref buff (off+0x3c) al.wl_rand3;
      write_resref buff (off+0x44) al.wl_rand4;
      write_resref buff (off+0x4c) al.wl_rand5;
      write_int buff (off+0x50) al.wl_rand_prob ;
    end in 

    Array.iteri (fun j ae ->
      let off = my_ae_offset + (j * 240) in 
      write_resref buff off ae.wa_name ;
      write_resref buff (off+0x8) ae.wa_area ;
      String.blit (str_to_exact_size ae.wa_longname 32) 0 buff (off+0x10) 32 ;
      write_int buff (off+0x30) ae.wa_status ;
      write_int buff (off+0x34) ae.wa_bamseq ;
      write_int buff (off+0x38) ae.wa_x ;
      write_int buff (off+0x3c) ae.wa_y ;
      write_int buff (off+0x40) ae.wa_name2 ;
      write_int buff (off+0x44) ae.wa_name3 ;
      write_resref buff (off+0x48) ae.wa_loading ; 
      let write_pair o (a,b) = begin 
        write_int buff (off+o) a ;
        write_int buff (off+o+4) b ;
      end in 
      (* first, number *) 

      write_int buff (off+0x50) !area_link_j ; 
      write_int buff (off+0x54) (Array.length ae.wa_north); 
      Array.iter (fun al -> write_area_link al) ae.wa_north; 

      write_int buff (off+0x58) !area_link_j ; 
      write_int buff (off+0x5c) (Array.length ae.wa_west); 
      Array.iter (fun al -> write_area_link al) ae.wa_west; 

      write_int buff (off+0x60) !area_link_j ; 
      write_int buff (off+0x64) (Array.length ae.wa_south); 
      Array.iter (fun al -> write_area_link al) ae.wa_south; 

      write_int buff (off+0x68) !area_link_j ; 
      write_int buff (off+0x6c) (Array.length ae.wa_east); 
      Array.iter (fun al -> write_area_link al) ae.wa_east; 
		) a.w_area_entry ; 

	      ) w ;
  buff

(***********************************************************************
 * EFF 
 ***********************************************************************)
let read_bg2_eff_file buff = 
  let off = 0 in 
  { 
    e_opcode = int_of_str_off buff (off + 0x10) ;
    e_target = int_of_str_off buff (off + 0x14) ;
    e_power = int_of_str_off buff (off + 0x18) ;
    e_arg1 = int_of_str_off buff (off + 0x1c) ;
    e_arg2 = int_of_str_off buff (off + 0x20) ;
    e_duration = short_of_str_off buff (off + 0x24) ;
    e_time = short_of_str_off buff (off + 0x28) ;
    e_prob1= short_of_str_off buff (off + 0x2c) ;
    e_prob2= short_of_str_off buff (off + 0x2e) ;
    e_resref = get_string_of_size buff (off+0x30) 8 ; 
    e_savetype = int_of_str_off buff (off + 0x38) ;
    e_savebonus = int_of_str_off buff (off + 0x3c) ;
    e_numdice = int_of_str_off buff (off + 0x40) ;
    e_dicesize = int_of_str_off buff (off + 0x44) ;
    e_disres = 0; 
  }

(***********************************************************************
 * XP 
 *
 * A 40th level char in BG2 has 9 Million XP.
 * A 30th level char in IWD2 has 450 Thousand XP. 
 ***********************************************************************)

let xplevel = Array.make 35 (Int32.max_int) 
let xplevel_set = ref false 
let get_xplevel () =
  if !xplevel_set then () 
  else begin
    let xpl = load_2da config.target "XPLEVEL" in 
    let arr = Hashtbl.find xpl "FIGHTER" in 
    for i = 0 to (Array.length arr) - 1 do
      let amt = Int32.of_string arr.(i) in
      xplevel.(i) <- amt ;
    done 
  end 

let convert_xp_award i =
  Int32.div i (Int32.of_int 20) 
let convert_challenge_rating i =
  max 1 ((i * 2) / 3)
let convert_xp_award_int i = 
  Int32.to_int (convert_xp_award (Int32.of_int i))

let convert_xp_to_level_int32 bg2_xp =
  get_xplevel () ;
  let xp = convert_xp_award bg2_xp in 
  (* find the first level such that its associated xp is greater than XP *)
  let i = ref 0 in
  while xplevel.(!i) < xp do
    incr i
  done ;
  (* then return one less than that *) 
  Int32.of_int (!i - 1)

let number_of_classes_in_bg2_class c = match c with 
| 7 | 8 | 9 | 13 | 14 | 15 | 16 | 18 
  -> 2
| 10 | 17 
  -> 3
| _ 
  -> 1

let random_hair = ref (Array.make 0 0)
let random_skin = ref (Array.make 0 0)

let left_quote_regexp = Str.regexp_case_fold "\"\\([A-Z0-9_]+\\)\\([^\"A-Z0-9_]\\)"
let right_quote_regexp = Str.regexp_case_fold "\\([^\"A-Z0-9_]\\)\\([A-Z0-9_]+\\)\""

let balance_quotes fn s =
  let num_quotes = ref 0 in
  for i = 0 to String.length s do
    if s.[i] = '"' then incr num_quotes
    else if s.[i] = '\'' then (s.[i] <- '"'; incr num_quotes) 
  done ;
  if !num_quotes mod 2 <> 0 then begin
    error "DLG" "[%s] has %d quotes in ~%s~: balancing.\n" fn !num_quotes s;
    let s = Str.global_replace left_quote_regexp "\"\\1\"\\2" s in
    let s = Str.global_replace right_quote_regexp "\\1\"\\2\"" s in
    s
  end else s 

(***********************************************************************
 * 2DA Files - SPAWNGRP.2DA
 ***********************************************************************)
let already_converted = Hashtbl.create 65535
type possibly_converted =
    Converted of string
  | Unconverted of string
let random_spawn_ht = Hashtbl.create 31 
let convert_spawngrp () = begin
  let s = load_2da (config.source) "SPAWNGRP" in 
  let top = Hashtbl.find s "0" in
  for i = 0 to (Array.length top) - 1 do

    let name = String.uppercase top.(i) in 
    log_and_print "convert_spawngrp: %s\n" name ; 
    let contents = List.map (fun str -> 
      try let a = Hashtbl.find s str in Unconverted(a.(i))
      with _ -> Converted("None")
			    ) [ "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ] in

    Hashtbl.add random_spawn_ht name (contents)
  done ;
end 

(***********************************************************************
 * 2DA Files - INTERDIA.2DA
 ***********************************************************************)
let convert_interdia () = begin
  let s = load_2da (config.source) "INTERDIA" in 
  Hashtbl.iter (fun lhs arr ->
    if Array.length arr > 0 then begin
      let lhs = if String.length lhs > 8 then String.sub lhs 0 8 else lhs in
      Hashtbl.add already_converted (lhs,"interdia") arr.(0) ;
      Hashtbl.add already_converted ((rename lhs "CRE"),"interdia") arr.(0)
    end 
	       ) s
end 

let join_dlg_ht = Hashtbl.create 31 

let convert_pdialog () = begin
  let s = load_2da (config.source) "PDIALOG" in 
  let oc = Case_ins.perv_open_out "override/pdialog.2da" in 
  Printf.fprintf oc " 2DA V1.0\n multig\n\t\tPOST_DIALOG_FILE\tJOIN_DIALOG_FILE\tDREAM_SCRIPT_FILE\n"; 
  Hashtbl.iter (fun lhs arr ->
    if Array.length arr > 3 && lhs <> "" then begin
      Hashtbl.add already_converted (lhs,"join_dialog") arr.(1);
      let lhs = if String.length lhs > 8 then String.sub lhs 0 8 else lhs in
      Hashtbl.add already_converted (lhs,"join_dialog") arr.(1);
      Printf.fprintf oc "%-12s%20s%20s%20s\n" lhs 
        (rename arr.(0) "DLG") (rename arr.(1) "DLG") 
        (rename arr.(2) "BCS");
      Hashtbl.add already_converted ((String.uppercase arr.(2)),"is_dream_script") lhs ; 
      Hashtbl.add already_converted 
        ((String.uppercase lhs),"dream_script_of_var") (arr.(2)) ;

      Hashtbl.add join_dlg_ht arr.(1) lhs ;
      Hashtbl.add join_dlg_ht (rename arr.(1) "DLG") lhs ;
    end 
	       ) s ;
  close_out oc
end

let parse_tra filename = 
  let lst : (int * Dlg.tlk_string) list = 
    parse_file filename "parsing .tra files" 
      (Dparser.tra_file Dlexer.initial) in 
  log_or_print "[%s] has %d translation strings\n" filename 
    (List.length lst); 
  lst

let is_summon_spell a =
  try 
    begin 
      match Int32.to_int a.a_4 with
      | 1150
      | 1250
      | 1350
      | 1402
      | 1410
      | 1501
      | 1602
      | 1605
      | 1615
      | 1702
      | 1704
      | 1723
      | 1750
      | 1850
      | 1901
      | 1950
      | 2124
      | 2230
      | 2309
      | 2407
      | 2418
      | 2428
      | 2501
      | 2505
      | 2512
      | 2513
      | 2514
      | 2515
      | 2516
      | 2520
      | 2598
      | 2599
      | 2611
      | 2621
      | 2622
      | 2626
      | 2627
      | 2628
      | 2629
      | 2703
      | 2713
      | 2715
      | 2716
      | 2803
      | 2809
      | 2899
      | 2902
      | 2905
      | 3177
      | 3183
      | 3227
      | 3228
      | 3282
      | 3283
      | 3284
      | 3968
	-> true
      | _ -> false 
    end
  with _ -> false 


let iwg2_name_of_itm res =
  try 
    let buff = load_target_res res "ITM" in 
    let idx = int_of_str_off buff 0xc in 
    let tlk = config.target.Load.dialog in
    if idx >= 0 && idx < Array.length tlk then
      tlk.(idx).Tlk.text 
    else
      "Special"
  with _ -> "Special"

let iwg2_name_of_spl res =
  try 
    let buff = load_target_res res "SPL" in 
    let idx = int_of_str_off buff 0x8 in 
    let tlk = config.target.Load.dialog in
    if idx >= 0 && idx < Array.length tlk then
      tlk.(idx).Tlk.text 
    else
      "Special"
  with _ -> "Special"

let iwg2_name_of_cre res =
  try 
    let buff = load_target_res res "CRE" in 
    let idx = int_of_str_off buff 0xc in 
    let tlk = config.target.Load.dialog in
    if idx >= 0 && idx < Array.length tlk then
      tlk.(idx).Tlk.text 
    else
      "Special"
  with _ -> "Special"
