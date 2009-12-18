(*
 * Weimer's BG2 -> IWD2 automatic converter
 *)

open Util
open Version 
open Bcs
open Ids
open Dlg 
open Iwgrule
open Key
open Iwgconf

let option_bcs_debug = ref false
let option_bcs_none = ref false 
let option_cre_keg = ref false 
let option_animate_keg = ref false 
let option_cre_fighter = ref false 
let option_cre_no_feats = ref false 
let option_cre_no_spells = ref false 
let option_cre_no_items = ref false 
let option_cre_in_name = ref true 
let option_damage_reduction = ref false 
let res_to_process = ref [] 

let iwg2_version = 35  

(***********************************************************************
 *
 * Conversion Tables 
 *
 ***********************************************************************)
let convert_count = Hashtbl.create 22 

(* special purpose hash tables *)
let infopoint_bcs_ht = Hashtbl.create 255 
let area_npc_ht = Hashtbl.create 31 
let random_spawn_list_ht = Hashtbl.create 31 
let animate_ht = Hashtbl.create 31 
let convert_bringer_ht = Hashtbl.create 31
let convert_overrider_ht = Hashtbl.create 31
(* let convert_banter_ht = Hashtbl.create 31 *) 
let cre_script_tweak_ht = Hashtbl.create 31 
let hp_of_converted_cre = Hashtbl.create 4095
let bg1_wed_overlay = Hashtbl.create 255 

let treat_as_robe_ht = Hashtbl.create 31
let treat_as_elvenchain_ht = Hashtbl.create 31

let innate_ability_ht = Hashtbl.create 255

(* data structures for keeping NPC banter information --
 * in IWD2 you cannot have multiple DLG files associated with one CRE *) 
type npc_banter = {
    mutable npc_variable : string ;
    mutable npc_cre_dlg  : string ;
    mutable npc_pdialog  : string ;
    mutable npc_interdia : string ;
  } 

let npc_banter_ht = Hashtbl.create 31 
let merged_dlg_ht = Hashtbl.create 63 

let opcode_CutSceneID = Int32.of_int 127 
let opcode_GlobalTimerExpired = Int32.of_int 0x4040 
let opcode_Wait = Int32.of_int 63 
let opcode_SmallWait = Int32.of_int 83 
let opcode_FadeToColor = Int32.of_int 202 
let opcode_SetGlobal = Int32.of_int 30 
let opcode_SetGlobalTimer = Int32.of_int 115
let opcode_IncrementGlobal = Int32.of_int 109
let opcode_LeaveParty = Int32.of_int 21
let opcode_TransferInventory = Int32.of_int 295 
let opcode_CreateCreature = Int32.of_int 7 
let opcode_CreateCreatureObject = Int32.of_int 227 
let opcode_CreateCreatureImpassable = Int32.of_int 228
let opcode_OpenDoor = Int32.of_int 143
let opcode_Unlock = Int32.of_int 196
let targcode_InCutsceneMode = Int32.of_int 0x40ef 
let warn_in_p1_cs = [ opcode_Wait ] 
let not_in_p1_cs = [ opcode_FadeToColor ] 

let charbase_buff = ref ""

(* 
 * How long did this take? 
 * about five hours on Thursday: 13941 chars, KEY, ARE, WED, (BAM/TIS/...)
 * thursday: 5pm -> 5:50
 * Fri Feb  7 08:28:55  2003
 * handling of renaming, rewrite weidu's compressed biff handling 
 * Fri Feb  7 12:08:34  2003
 *
 * Fri Feb  7 13:47:42  2003
 * -- working demo, CRE, BCS, cut-scenes, etc. 
 * Fri Feb  7 23:10:21  2003
 *
 * Sat Feb  8 08:36:38  2003
 * DLG handling
 * Sat Feb  8 11:02:55  2003
 *
 * Sat Feb  8 11:32:55  2003
 * dlg handling, global variable namespaces, more baf/bcs gibberish
 * Sat Feb  8 13:27:30  2003
 *
 * Sun Feb  9 09:44:07  2003
 * basic ITM handling, no EFF handling, redo renaming (_ over @) 
 * Sun Feb  9 13:01:37  2003
 *
 * Sun Feb  9 17:02:59  2003
 * CHARNAME -> PLAYER1, discover MoveToArea() in IWD2, etc. 
 * Sun Feb  9 20:02:10  2003
 *
 * Sun Feb  9 21:10:44  2003
 * sprite_is_dead, basic CRE ITM handling, 
 * Sun Feb  9 23:20:13  2003
 *
 * Mon Feb 10 10:05:52  2003
 * transition problems, CRE ITM slot ordering, 
 * ITM conversion and auto-description, STO 
 * rudimentary SPL
 * saying "CLERIC_FOO.spell <- YADA.spell" should also convert SPPRxyz.spl
 * Mon Feb 10 19:39:41  2003
 * 
 * Wed Feb 12 08:48:43  2003
 * better spell and ids handling, many scroll conversions
 * Wed Feb 12 10:32:46  2003
 *
 * Fri Feb 14 10:03:13  2003
 * cutscene problems, CRE spell memorization, ... Imoen!
 * reverse-engineered the "class mask" thiny in CRE files
 * Fri Feb 14 15:58:54  2003
 * 
 * Fri Feb 14 17:48:53  2003
 * Fri Feb 14 24:48:53  2003
 *
 * Sat Feb 15 09:02:06  2003
 * monster CR, numtimestalkedto, demo game screenshots
 * monster feats, monster inventory tweaking, ARE yada 
 * Sat Feb 15 16:41:19  2003
 *
 * Sat Feb 15 19:52:43  2003
 * merge duplicate effects, stacking, ...
 * Sat Feb 15 20:51:43  2003
 *
 * Sun Feb 16 10:21:28  2003
 * ARE/MOS/WED naming deal, better KEY handling, stores purchase
 * descriptions
 * Sun Feb 16 13:36:09  2003
 *
 * ... unclear 
 * massive mage script
 * opcode fixups
 * Sun Feb 16 19:11:24  2003
 *
 * Mon Feb 17 20:42:44  2003
 * exported_allow ranger crash
 * hold_creature_2 opcode crash
 * throwing daggers and other ranged melee weapons should all get str bon
 *
 * Wed Feb 19 17:02:27  2003
 * wmp problems apparently fixed!
 * Wed Feb 19 18:02:35  2003
 *
 * Wed Feb 19 18:30:37  2003
 * pdialog.2da, baldur.gam autoconvert
 * v1 release ...
 *
 * Thu Feb 20 10:46:48  2003
 * fixed WED "excessive poly count"
 * fixed WED "ugly green overlay" 
 * fixed Immunity_to_weapons opcode
 * Thu Feb 20 12:04:25  2003
 *
 * Fri Feb 21 10:33:52  2003
 * winter wolf pelts -> misc, not short swords, etc. 
 *
 * TODO: 
 * the convert failure report should be checked
 *
 * check out windspear hills
 *
 * ar0112 ambients
 *
 *
 * close transitions while players see enemies
 *
 * WTASIGHT
 * EFF (in CRE, SPL) 
 *) 
    
(***********************************************************************
 *
 * Conversion Rules 
 *
 ***********************************************************************)

let associate_variable_with_original_cre variable cre =
  if Hashtbl.mem already_converted (String.uppercase cre,"variable_of_cre") 
  then begin
    let orig = Hashtbl.find already_converted (String.uppercase cre,"variable_of_cre") in 
    log_and_print "ERROR: VARIABLE_OF_CRE: trying to associate two variables with %s.CRE: [%s] [%s]\n" cre variable orig ;
    exit 1
  end ;
  (* log_and_print "WARNING: associating %s with %s.variable_of_cre\n" 
     variable (String.uppercase cre ); *)
  let variable = String.lowercase variable in 
  Hashtbl.add already_converted (String.uppercase cre,"variable_of_cre") 
    (variable) 

let spell_num_to_res i = 
  let i = Int32.to_int i in
  let big = i / 1000 in
  let small = i mod 1000 in
  match big with
    1 -> Printf.sprintf "SPPR%03d" small
  | 2 -> Printf.sprintf "SPWI%03d" small
  | 3 -> Printf.sprintf "SPIN%03d" small
  | 4 -> Printf.sprintf "SPCL%03d" small
  | _   -> failwith (Printf.sprintf "Spell_to_num_res %d" i)

let spell_res_to_num s =
  let big = match String.sub s 0 4 with
  | "SPPR" -> 1
  | "SPWI" -> 2
  | "SPIN" -> 3
  | _ -> failwith ("spell_res_to_num " ^ s)
  in 
  let small = try int_of_string (String.sub s 4 3) with _ -> 
    failwith ("spell_res_to_num " ^ s) in
  (big * 1000) + small 


let process_rules conv_rules = 
  List.iter (fun r -> match r with

  | Iwgrule.ReplaceWithConverted(f,[]) ->
      log_and_print "ERROR: PROGRAMMER: %s has no converted target\n" f;
      exit 1 

  | Iwgrule.ReplaceWithConverted(f,lst) ->
      let (r,e) = split f in
      let (r',e') = split (List.hd lst) in 
      let r'' = rename r' e' in 
      convert_at_end := (r',e') :: !convert_at_end ;
      Hashtbl.add already_converted (String.uppercase r,e) (r'')

  | Iwgrule.Replace(f,lst) -> 
      begin 
	let (r,e) = split f in
	let (r',e') = if lst = [] then (r,e) else split (List.hd lst) in 
	let add_to_already_converted = 
	  match e with
	  | "spell" -> 
              let i = try (Bcs.ids_of_sym (config.source) "SPELL" r).Ids.i_num 
              with e -> 
		log_and_print "ERROR: PROGRAMMER: %s not in Source SPELL.IDS\n" r;exit 1 
              in 
              let i' = try (Bcs.ids_of_sym (config.target) "SPELL" r').Ids.i_num 
              with e -> 
		log_and_print "ERROR: PROGRAMMER: %s not in Target SPELL.IDS\n" r';
		exit 1 
              in 
              let iname = spell_num_to_res i in
              let iname' = spell_num_to_res i' in 
              Hashtbl.add already_converted (iname,"SPL") (iname') ;
              false 

	  | "random_memorize" -> 
              let idx = int_of_string r in
              let choices = List.map (fun s ->
		let i = (Bcs.ids_of_sym (config.target) "SPELL" s).Ids.i_num in
		spell_num_to_res i
				     ) lst in 
              random_memorize.(idx) <- Array.of_list choices ;
              false 

	  | "random_melee"
	  | "random_bow" 
	  | "random_sling" 
	  | "random_xbow" 
	  | "random_bullet"
	  | "random_bolt" 
	  | "random_arrow" -> 
              let a = match e with 
              | "random_melee" -> (random_melee)
              | "random_bow" -> (random_bow)
              | "random_sling" -> (random_sling)
              | "random_xbow" -> (random_xbow)
              | "random_bullet" -> (random_bullet)
              | "random_bolt" -> (random_bolt)
              | "random_arrow" -> (random_arrow)
              | _ -> log_and_print "ERROR: PROGRAMMER: random_foo?\n" ; exit 1
              in 
              let idx = int_of_string r in
              a.(idx) <- Array.of_list lst ;
              false 

	  | "random_hair" -> 
              let choices = List.map (fun s -> int_of_string s) lst in 
              random_hair := Array.of_list choices ;
              false 

	  | "random_skin" -> 
              let choices = List.map (fun s -> int_of_string s) lst in 
              random_skin := Array.of_list choices ;
              false 

	  | "convert_animation" -> 
              Hashtbl.add animate_ht r (e', [(r',r')]) ;
              false 

	  | "rename_animation" -> 
              let delim = String.index r '>' in
              let my_r = Str.string_before r delim in
              let my_r' = Str.string_after r (delim+1) in
              let bg2_iwd2_list = List.map split lst in 
              Hashtbl.add animate_ht my_r (my_r', bg2_iwd2_list) ;
              false 

	  | "cre_script_tweak" -> 
              let script_tweak_ht = 
		if (Hashtbl.mem cre_script_tweak_ht r) then 
		  Hashtbl.find cre_script_tweak_ht r 
		else Hashtbl.create 31 
              in 
              Hashtbl.add script_tweak_ht r' e' ;
              Hashtbl.replace cre_script_tweak_ht r script_tweak_ht ;
              false 

	  | "wed_overlay" -> 
              let choices = List.map (fun s -> int_of_string s) lst in 
              Hashtbl.add bg1_wed_overlay r choices ;
              false 

	  | "treat_as_robe" -> 
              List.iter (fun s ->
		log_and_print "treat_as_robe: %s yes!\n" s ; 
		Hashtbl.add treat_as_robe_ht s true
			) (lst ) ;
              false 

	  | "treat_as_elvenchain" -> 
              List.iter (fun s ->
		log_and_print "treat_as_elvenchain: %s yes!\n" s ; 
		Hashtbl.add treat_as_elvenchain_ht s true
			) (lst ) ;
              false

	  | _ -> true 
	in 
	if add_to_already_converted then 
          Hashtbl.add already_converted (String.uppercase r,e) (r')
      end 
	    ) conv_rules ;
  () 

let process_automatic_rules () = 
  let bg2_spell_ids = Bcs.get_ids_map (config.source) "SPELL" in 
  Hashtbl.iter (fun sym bg2_ids ->
    try
      let iwd2_ids = ids_of_sym (config.target) "SPELL" sym in
      let bname = spell_num_to_res (bg2_ids).i_num in 
      let iname = spell_num_to_res (iwd2_ids).i_num in
      (* log_and_print "AUTOMATIC: %s.spell (%s -> %s)\n"
         sym bname iname ;  *)
      Hashtbl.add already_converted (bname,"SPL") (iname) ;
    with Not_found -> () 
	       ) bg2_spell_ids.Bcs.from_uppercase_sym ;

  (* now handle listspll.2da *)
  if config.target_variant = IWD2 then begin 
    let buff,path = Load.load_resource "weimorph" config.target true "LISTSPLL" "2DA" in 
    let reg = Str.regexp "^\\([0-9]+\\)[^S]*\\(SP.....\\)" in 
    (try let i = ref 0 in 
    while true do
      i := (Str.search_forward reg buff !i) + 1 ; 
      let num = Str.matched_group 1 buff in
      let res = Str.matched_group 2 buff in
      (* log_and_print "AUTOMATIC: LISTSPLL.2DA: %s is %s\n" res num ;  *)
      Hashtbl.add already_converted (res,"listspll") (num) ;
    done
    with _ -> () ) ;
  end ; 
  () 

(***********************************************************************
 *
 * The Big Conversion
 *
 * Take a resource, convert it to IWD2, and then return the new resource
 * reference that should be used. 
 ***********************************************************************)

let c_count = ref 13 

let do_count e = 
  if e = String.uppercase e then begin
    let so_far = try Hashtbl.find convert_count e with Not_found -> 0 in
    Hashtbl.replace convert_count e (so_far + 1) 
  end 

let rec convert r e = Stats.time e (fun () ->
  let upper_r = (String.uppercase r) in 
  if upper_r <> (String.escaped upper_r) then begin
    "None" 
  end else if Hashtbl.mem already_converted (upper_r,e) then
    Hashtbl.find already_converted (upper_r,e)
  else if upper_r = "NONE" || upper_r = "*" then
    "None"
  else if r = "" then 
    ""
  else try begin
    log_and_print "convert: %s.%s\n" r e ; 
    let new_r = rename r e in 
    Hashtbl.replace already_converted (upper_r,e) new_r ; 

    do_count e ; 

    let new_r = match e with

    | "is_npc_script" 
    | "is_dream_script" 
    | "is_melee_weapon" 
    | "listspll" 
      -> "" 

    | "is_droppable" -> "yes" 
    | "interdia" -> "None" 

    | "variable" -> String.lowercase r 

    | "targslot" | "srcslot" 
    | "targicon" | "srcicon"
    | "targop"   | "srcop"
    | "ea" 
    | "general" 
    | "race" 
    | "gender" 
    | "align" 
    | "object" 
    | "spell" 
    | "stats" 
    | "time" 
    | "timeoday" 
    | "kit" 
    | "state" 
    | "projectl" 
    | "not_found" 
      -> r


	  (*| "random_spawn" -> convert_random_spawn new_r r ; new_r *)

    | "animate"
    | "ANIMATE" -> 
        if !option_animate_keg then "Keg 1"
        else (convert_animate r)

    | "specific" -> begin try 
        let _ = (Bcs.ids_of_sym (config.target) "SPECIFIC" r) in r 
    with Not_found -> 
      error "SPECIFIC" "Specific [%s] not found in Target\n" r ;
      r
    end 

    | "class" -> begin try 
        let _ = (Bcs.ids_of_sym (config.target) "CLASS" r) in r 
    with Not_found -> "FIGHTER" 
    end 


    | "CRE" -> 
        if !option_cre_keg then begin 
          associate_variable_with_original_cre r r ;
          "10KEG"
        end else begin
          if Hashtbl.mem random_spawn_ht r then begin
            (Stats.time "random_spawn" (convert_random_spawn new_r) r ; new_r)
          end else begin
            (convert_cre new_r r ; new_r)
          end
        end 


    | "BCS" -> 
	if !option_bcs_none then begin
          error "BCS" "~%s.BCS~ [~None.bcs~]\n" r ; "None" 
	end else
          let args = {
            cb_is_cutscene = false ;
            cb_is_infopoint = Hashtbl.mem infopoint_bcs_ht r
          } in 
          (try convert_bcs new_r r args [] ; new_r with e -> "None")

    | "DLG" -> (try convert_dlg new_r r ; new_r with _ -> "None") 

    | "ITM" -> convert_itm new_r r ; new_r

    | "STO" -> convert_sto new_r r ; new_r

    | "SPL" -> convert_spl new_r r ; new_r

    | "BAM" 
    | "BMP"   
    | "MOS"   
    | "WAV" 
    | "TIS" ->
        add_to_key new_r r e ; new_r 

    | "ARE" ->
        if !res_to_process <> [] &&
          not (List.mem (r,e) !res_to_process) then 
          "None"
        else begin
          convert_are new_r r ; new_r
        end 

    | "WED" -> convert_wed new_r r ; new_r
    | "WMP" -> convert_wmp new_r r ; new_r
    | "GAM" -> convert_gam new_r r ; new_r 

    | "2DA"   
    | "CHU"   
    | "IDS"   
    | "MVE"   
    | "PLT"   
    | "EFF"   
    | "PRO" -> 
        error "CONVERT" "%s: not expecting to convert (%s.%s)\n" e r e ;
        failwith "cannot convert: unexpected" 

    | _ -> 
        error "CONVERT" "%s: unknown resource TYPE (%s.%s)\n" e r e ;
        failwith ("cannot convert: unknown resource type: " ^ e)
    in 
    Hashtbl.replace already_converted (upper_r,e) new_r ; 
    new_r
  end with exc -> begin
    error "CONVERT" "%8s.%s : %s\n" r e (Printexc.to_string exc) ;
    Hashtbl.remove already_converted (upper_r,e) ;
    raise exc end
				   ) () 

(***********************************************************************
 * Generic Patching: Resource References
 *
 * Given a buffer, an offset, and the type of resource reference stored
 * there ... fix it up!
 ***********************************************************************)
and patch_resref buff off e =
  let r = get_string_of_size buff off 8 in 
  let new_r = str_to_exact_size (try convert r e with _ -> 
    if e = "CRE" then "10KEG" else "None") 8 in
  String.blit new_r 0 buff off 8  

and convert_resref buff off e =
  let r = get_string_of_size buff off 8 in 
  let new_r = (convert r e) in
  new_r

and patch_diff_short buff off =
  let cur = short_of_str_off buff off in
  let cur = eff_scale cur in
  write_short buff off cur 

and patch_diff_int buff off =
  let cur = int_of_str_off buff off in
  let cur = eff_scale cur in
  write_int buff off cur 

(***********************************************************************
 * Animations
 ***********************************************************************)
and convert_animate r =
  try 
    (* we must bring over the files for this animation *) 
    if Hashtbl.mem animate_ht r then begin
      let (r',lst) = Hashtbl.find animate_ht r in
      let bam_ext = Key.key_of_ext true "BAM" in 
      error "ANIMATE" "For Source [%s] we use Source BAMs to make [%s]\n" r r'; 
      List.iter (fun (bg2_prefix, iwd2_prefix) -> 
        Array.iter (fun res ->
          if res.Key.res_type = bam_ext && 
            String.length res.Key.res_name >= (String.length bg2_prefix) &&
            String.sub res.Key.res_name 0 (String.length bg2_prefix) = bg2_prefix then begin
              let new_name = Str.string_after res.Key.res_name 
		  (String.length bg2_prefix) in
              let new_name = iwd2_prefix ^ new_name in 
              add_to_key new_name res.Key.res_name "BAM" ; 
              (* error "ANIMATE" "For BG2 [%s] we use BG2's [%s.BAM] as [%s.BAM]\n" r res.Key.res_name new_name *)
            end 
		   ) (config.source).Load.key.Key.resource 
		) lst ; 
      r'
    end else r
  with Not_found -> 
    error "ANIMATE" "Convert Animate: [%s] is not in Target's ANIMATE.IDS\n" r ; 
    "Keg 1"


(***********************************************************************
 * Generic Patching: String References
 *
 * Given a buffer, and offset, fix up a strref stored there. 
 ***********************************************************************)
and convert_lse lse =
  match Dc.resolve_tlk_string (config.target) (Dlg.Local_String(lse)) with
    Dlg.TLK_Index(i) -> i
  | _ -> (error "CONVERT" "Cannot make string ~%s~" lse.lse_male) ; 
      failwith "cannot resolve strref" 

and debug_string str = 
  let lse = { lse_male = str ; lse_male_sound = "" ; lse_female = "" ;
              lse_female_sound = "" ; } in
  convert_lse lse 

and convert_strref idx = 
  if (idx <= 0) || idx > (Array.length (config.source).Load.dialog) then 
    -1 
  else begin
    let bg2_tlk = (config.source).Load.dialog.(idx) in 
    let new_text = 
      List.fold_left (fun acc (reg,rep) -> Str.global_replace reg rep acc)
	bg2_tlk.Tlk.text charname_regexp in 
    let lse = { lse_male = new_text ; 
                lse_male_sound = (convert bg2_tlk.Tlk.sound_name "WAV"); 
                lse_female = "" ;
                lse_female_sound = "" ; } in
    match Dc.resolve_tlk_string (config.target) (Dlg.Local_String(lse)) with
    | Dlg.TLK_Index(i) -> i
    | _ -> (error "CONVERT" "Cannot resolve Source strref %d inside Target\n" idx) ;
        failwith "cannot resolve strref" 
  end  

and convert_tlk_string_to_idx ts = match ts with
| TLK_Index(i) -> convert_strref i 
| Local_String(lse) -> convert_lse lse
| Trans_String(i) -> 
    ((error "CONVERT" "No translation provided for @%d" i) ;
     failwith "cannot resolv trans str" )

and convert_strref_of buff off =
  let idx = int_of_str_off buff off in
  convert_strref idx

and patch_strref buff off =
  let idx = int_of_str_off buff off in
  let iwd2_idx = convert_strref idx in 
  (* log_and_print " ~%s~ is [%d]\n" lse.lse_male iwd2_idx ;  *)
  String.blit (str_of_int iwd2_idx) 0 buff off 4  

and convert_ids_internal input ids_file iwd2_file yell = 
  if input = Int32.zero then Int32.zero
  else begin
    let convert_name = String.lowercase ids_file in 
    let str = 
      try (Bcs.ids_of_int (config.source) ids_file input).Ids.i_name 
      with Not_found -> Printf.sprintf "%ld" input  
    in
    try 
      let str' = convert str convert_name in 
      (Bcs.ids_of_sym (config.target) iwd2_file str').Ids.i_num 
    with 
      Not_found -> 
	if ids_file = "TIME" || ids_file = "SPECIFIC" then Int32.of_string str 
	else if yell then ( error "IDS" "[%s.IDS] cannot find Source's [%s] in Target (from Source %ld)\n" iwd2_file str input; Int32.zero )
	else raise Not_found
    | _ -> Int32.zero 
  end 

and convert_ids input ids_file iwd2_file =
  convert_ids_internal input ids_file iwd2_file true 

and convert_ids_check input ids_file iwd2_file =
  convert_ids_internal input ids_file iwd2_file false 

and convert_ids_int input ids_file iwd2_file =
  let id = Int32.of_int input in
  Int32.to_int (convert_ids id ids_file iwd2_file) 

and convert_ids_check_int input ids_file iwd2_file =
  let id = Int32.of_int input in
  Int32.to_int (convert_ids_check id ids_file iwd2_file) 

(***********************************************************************
 * EFF 
 *            --> convert a BG2 effect to IWD2
 ***********************************************************************)
and convert_eff new_r r ce e =
  try 
    begin 
      let op32 = Int32.of_int e.e_opcode in 
      (match e.e_savetype with
      | 2    (* breath *)
      | 8 -> (* wands *)
	  e.e_savetype <- 8 (* REFLEX *) 

      | 4    (* poison *)
      | 16 -> (* pet/poly *)
	  e.e_savetype <- 4 (* FORT *) 

      | 1 -> (* spells *) 
	  e.e_savetype <- 16 (* WILL *) 
      | _ -> () 
      ) ;
      e.e_savebonus <- 0 - e.e_savebonus ; 
      let bg2_ids = try (Bcs.ids_of_int (config.source) "SRCOP" op32) with Not_found -> 
	log_and_print "ERROR: OPCODE UNKNOWN %ld (convert_eff %s)\n" op32 r ; exit 1
      in
      let name = convert bg2_ids.Ids.i_name "srcop" in
      try 
	let iwd2_ids = Bcs.ids_of_sym (config.target) "TARGOP" name in 
	e.e_opcode <- Int32.to_int iwd2_ids.i_num ;  
	let name = iwd2_ids.i_name in 
	begin
	  match name with 
	  | "Set_item_color" 
	  | "Set_colorglow_pulse" 
	  | "Set_colorglow_solid" 
	  | "Character_color_1" 
	  | "Character_color_2"
	  | "Blindness"
	  | "Blur" 
	  | "Cure_disease" 
	  | "Cure_poison" 
	  | "Damage" 
	  | "Draw_upon_holy_might__non-cumulative"
	  | "Fatigue_bonus" 
	  | "Intoxication_bonus" 
	  | "Mirror_image"
	  | "Stoneskin_effect" 
	  | "Teleport" 
	  | "Translucent" 
	  | "Current_HP_bonus" 
	  | "Reputation" 
	    -> [ e ] 

	  | "Minimum_HP"
	    -> [ e ;
		 (* IWG2 hackery -- if you become panicked, you may never take
		    off MINHP1, thus leading to frustrated players ... *) 
		 { e with e_opcode = target_opcode "Immunity_to_effect" ;
                   e_arg1 = 0 ;
                   e_arg2 = target_opcode "Panic" ; } ] 

	  | "Set_AI_script" 
	    -> [ { e with e_resref = convert e.e_resref "BCS" } ] 

	  | "Hold_creature_2" 
	    -> [ { e with e_opcode = target_opcode "Hold_creature" ; 
                   e_arg1 = 0; e_arg2 = 0; } ] 

	  | "Attacks_per_round_bonus"
	    -> if r = "BOW14" || r = "XBOW06" then
              [ { e with e_arg2 = 0 ; e_arg1 = 1 } ] 
            else if e.e_arg2 = 1 (* set *) && e.e_arg1 < 5 then 
              [] 
            else 
              [ e ] 

	  | "Disable_spellcasting" 
	  | "Disable_button" 
	    -> [ ] 

	  | "Protection_from_evil" 
	    -> [ e ; { e with e_opcode = target_opcode "Set_state" ;
                       e_arg1 = 0; e_arg2 = 1; } ] 

	  | "Bonus_wizard_spells" 
	    -> [ { e with e_opcode = target_opcode "Intelligence_bonus" ;
		   e_arg2 = 0; e_arg1 = 4 } ] 
	  | "Bonus_priest_spells" 
	    -> [ { e with e_opcode = target_opcode "Wisdom_bonus" ;
		   e_arg2 = 0; e_arg1 = 4 } ] 
	  | "Immunity_to_spell_level" 
	    -> [ { e with e_opcode = target_opcode "Magic_resistance_bonus" ;
		   e_arg2 = 0; e_arg1 = e.e_arg1 * 6 } ] 

	  | "Bonus_to_AC" -> 
              if (e.e_arg2 = 16 || e.e_arg2 = 0) && ce.ce_is_shield then 
		[ { e with e_arg2 = 3 } ] 
              else if e.e_arg2 = 16 && !option_damage_reduction then begin
		e.e_arg1 <- 10 - e.e_arg1 ; 
		e.e_arg2 <- 0 ; 
		[  
		   { e with e_opcode = target_opcode "Slashing_resistance_bonus" };
		   { e with e_opcode = target_opcode "Crushing_resistance_bonus" };
		   { e with e_opcode = target_opcode "Piercing_resistance_bonus" };
		   { e with e_opcode = target_opcode "Missile_resistance_bonus" };
                 ]
              end else if e.e_arg2 = 16 then 
		[ { e with e_arg1 = 10 - e.e_arg1 ; e_arg2 = 1 } ] 
              else if e.e_arg2 = 0 then (* all weapons *)
		[ { e with e_arg2 = 2 } ] 
              else if e.e_arg2 = 1 then (* crushing *)
		[ { e with e_arg2 = 4 } ] 
              else if e.e_arg2 = 2 then (* missile *)
		[ { e with e_arg2 = 7 } ] 
              else if e.e_arg2 = 4 then (* piercing *)
		[ { e with e_arg2 = 5 } ] 
              else if e.e_arg2 = 8 then (* slashing *) 
		[ { e with e_arg2 = 6 } ]
              else (error "OPCODE" "VERIFY AC %d\n" e.e_arg2 ; [ e ])

	  | "Animation_change" ->
              [ { e with e_arg2 = 0; e_arg1 = 
		  convert_ids_int e.e_arg1 "ANIMATE" "ANIMATE" } ] 

	  | "Change_sex" ->
              (if e.e_arg2 = 1 then e.e_arg1 <- convert_ids_int e.e_arg1 "GENDER" "GENDER") ;
              [ e ] 

	  | "Charm_creature" -> begin
              match convert_bg2_opcode_arg_to_iwd2 e.e_arg2 e.e_arg1 r with
              | None -> [] 
              | Some(id) -> 
		  let upper_r = String.uppercase r in 
		  if (upper_r = "MISC9X") then (* MIND FLAYER CONTROL CIRCLET HACK *) 
		    [ { e with e_opcode = target_opcode "Control" ;
			e_arg1 = 0 ;
			e_arg2 = id ;
			e_savetype = 0; e_savebonus = 0; } ] 
		  else if (upper_r = "MISC7Y") then  (* THRALL COLLAR HACK *) 
		    [ { e with e_opcode = target_opcode "AI_Change" ;
			e_arg1 = 255 ;
			e_arg2 = 0 ;
			e_savetype = 0; e_savebonus = 0; } ] 
		  else 
		    [ { e with e_opcode = target_opcode "Add_effects_list" ;
			e_arg1 = 0 ; 
			e_arg2 = id ; 
			e_savetype = 0; e_savebonus = 0; 
			e_resref = "__CHARM" ; } ]
          end 


	  | "Break_morale" when e.e_arg1 = 0 && e.e_arg2 = 1 -> [ e ] 

	  | "Casting_failure" -> [ { e with e_arg2 = 2; } ] 

	  | "Change_alignment" -> 
              [ { e with e_arg2 = convert_ids_int e.e_arg2 "ALIGN" "ALIGNMNT" } ]

	  | "Change_name" -> [ { e with e_arg1 = convert_strref e.e_arg1 } ] 

	  | "Movementrate_bonus" -> 
              e.e_arg1 <- 13; e.e_arg2 <- 1; [ e] (* boots of speed *) 

	  | "Immunity_to_effect"
            -> 
              begin
		( if e.e_disres = 2 then e.e_disres <- 0 ) ;
		( if e.e_disres = 1 then e.e_disres <- 3 ) ;
		(* should always bypass spell resistance *)  
		match e.e_arg2 with
		| 5 -> [ e ]
		| _ -> 
		    begin
		      (if (e.e_arg2 = 126) (* movementrate bonus *)
		      then e.e_arg2 <- 176 (* penalty *) 
		      ) ;
		      let eff' = convert_eff new_r r ce 
			  { e with e_opcode = e.e_arg2 ; e_arg1 = 0; e_arg2 = 0; } in
		      match eff' with
			[] -> []
		      | hd :: tl when hd.e_opcode = 13 -> [ { e with e_arg2 = 420 } ] 
		      | hd :: tl when hd.e_opcode = 413 -> [ ]
		      | hd :: tl -> [ { e with e_arg2 = hd.e_opcode } ] 
		    end 
              end 

	  | "Change_portrait" ->
              (if (e.e_arg2 = 1) then e.e_resref.[String.length e.e_resref -1]<-'L');
              e.e_resref <- convert e.e_resref "BAM" ;
              [ ] 

	  | "Acid_resistance_bonus" 
	  | "Fire_resistance_bonus" 
	  | "Electricity_resistance_bonus" 
	  | "Cold_resistance_bonus" 
	  | "Lockpicking_bonus" 
	  | "Pick_pockets_bonus" 
	  | "Hide_in_shadow_bonus"  
	  | "Find_traps_bonus" 
	  | "Lore_bonus" 
	  | "Reduced_damage_from_poison" 
	  | "Stealth_bonus" 
	  | "Slashing_resistance_bonus"
	  | "Crushing_resistance_bonus"
	  | "Piercing_resistance_bonus"
	  | "Missile_resistance_bonus"
	    -> [ { e with e_arg2 = 0 ; (* inc/dec *) 
                   e_arg1 = eff_scale e.e_arg1 } ]

	  | "Magic_damage_resistance_bonus"
	    -> let amt = eff_scale e.e_arg1 in  
            [ { e with e_arg2 = 0 ; (* inc/dec *) 
                e_arg1 = if amt = 100 then 9999 else amt } ] 

	  | "Magic_resistance_bonus" 
	    -> [ { e with e_arg2 = 0 ; (* inc/dec *) 
                   e_arg1 = 5 * (eff_scale e.e_arg1) / 2 } ]

	  | "Summon_creature"
	    -> [ { e with e_opcode = target_opcode "Summon_creature" ;
		   e_arg1 = 1 ; e_arg2 = 1; e_target = 1; 
		   e_duration = 1; e_resref = convert e.e_resref "CRE" } ] 

	  | "Replace_self" 
	    -> [ { e with e_resref = convert e.e_resref "CRE" } ] 

	  | "Luck_bonus" 
	  | "Max_HP_bonus" 
	    -> [ { e with e_arg2 = 0 ; (* inc/dec *) 
                   e_arg1 = 
                   (if e.e_arg2 = 2 || e.e_arg2 = 5 then e.e_arg1 - 100 else
                   e.e_arg1) } ] 

	  | "Immunity_to_projectile"
	    -> [ { e with e_opcode = target_opcode "Protection_from_arrows" ;
		   e_arg1 = 0 ; e_arg2 = 1; (* 10/+1 *) } ] 

	  | "Charisma_bonus" 
	  | "Attack_damage_bonus" 
	  | "Intelligence_bonus" 
	  | "Wisdom_bonus" 
	  | "Base_attack_bonus"
	  | "Missile_THAC0_bonus"
	  | "Break_morale"
	    -> begin
              (if name = "Base_attack_bonus" && e.e_arg2 = 1 then
		e.e_arg1 <- 20 - e.e_arg1 ) ;
              if e.e_arg2 = 1 && e.e_arg1 > 0 (* set pos *) then begin
		[ { e with e_arg2 = 0 ; (* inc/dec *) 
                    e_arg1 = (e.e_arg1 - 16) } ]
              end else if e.e_arg2 = 0 then begin
		[ { e with e_arg1 = (e.e_arg1) } ]
              end else if e.e_arg2 = 2 then begin
		[ { e with e_arg2 = 0 ;
                    e_arg1 = (e.e_arg1 - 50)/ 10 ; } ]
              end else begin
		error "OPCODE" "VERIFY (!) %s\n" name ; [ e ]
              end
	    end

	  | "Strength_bonus" 
	    -> begin
              if e.e_arg2 = 1 && e.e_arg1 > 0 (* set pos *) then begin
		[ { e with e_arg2 = 0 ; (* inc/dec *) 
                    e_arg1 = (e.e_arg1 - 16) } ]
              end else if e.e_arg2 = 0 then begin
		[ { e with e_arg1 = (e.e_arg1) } ]
              end else if e.e_arg2 = 2 then begin
		[ { e with e_arg2 = 0 ;
                    e_arg1 = (e.e_arg1 - 50)/ 5 ; } ]
              end else begin
		error "OPCODE" "VERIFY (!) %d %d %s\n" 
		  e.e_arg1 e.e_arg2 name ; [ e ]
              end
	    end

	  | "Constitution_bonus" 
	  | "Dexterity_bonus" 
	    -> begin
              if e.e_arg2 = 1 && e.e_arg1 > 0 (* set pos *) then begin
		[ { e with e_arg2 = 0 ; (* inc/dec *) 
                    e_arg1 = (e.e_arg1 - 16) * 2} ]
              end else if e.e_arg2 = 0 then begin
		[ { e with e_arg1 = (e.e_arg1) * 2} ]
              end else if e.e_arg2 = 2 then begin
		[ { e with e_arg2 = 0 ;
                    e_arg1 = (e.e_arg1 - 50)/ 5 ; } ]
              end else begin
		error "OPCODE" "VERIFY (!) %d %d %s\n" 
		  e.e_arg1 e.e_arg2 name ; [ e ]
              end
	    end

	  | "Play_sound" 
	    -> [ { e with e_resref = convert e.e_resref "WAV" } ]

	  | "Cast_spell" 
	  | "Learn_spell" 
	  | "Protection_from_spell" 
	    -> [ { e with e_resref = convert e.e_resref "SPL" } ]


	  | "Create_inventory_item" 
	  | "Remove_inventory_item" 
	  | "Create_item" 
	  | "Remove_item" 
	    -> [ { e with e_resref = convert e.e_resref "ITM" } ]

	  | "Display_string"
	    -> 
	      if e.e_arg1 = 40968 || (* two levels drained *) 
              e.e_arg1 = 40969 || (* 3 *)
              e.e_arg1 = 40979 || (* 4 *)
              e.e_arg1 = 41495 || (* 1 *) 
              e.e_arg1 = 41616 ||
              e.e_arg1 = 14000  (* slowed *) 
	      then
		[ ] (* IWD2 will already display something *) 
	      else 
		[ { e with e_arg1 = convert_strref e.e_arg1 } ]

	  | "Polymorph"
	    -> [ { e with e_arg1 = convert_ids_int e.e_arg1 "ANIMATE" "ANIMATE" ;
		   e_resref = convert e.e_resref "CRE" } ]

	  | "Immunity_to_weapons"
	    -> [ { e with e_opcode = target_opcode "Damage_reduction" ;
		   e_arg2 = e.e_arg1 + 1 ; (* ench to overcome *) 
		   e_arg1 = 999; (* damage to resist *) } ] 

	  | "Invisibility" 
	    -> if e.e_arg2 = 1 (* improved *) then begin
              [ { e with e_opcode = target_opcode "Blur" ; 
		  e_arg1 = 0; e_arg2 = 0; } ; ]
            end else begin [ e ] end

	  | "Display_portrait_icon" 
	  | "Remove_portrait_icon" 
	    -> [ { e with e_arg2 = convert_icon e.e_arg2 } ] 


	  | "Slay" -> begin
              if (e.e_arg1 = 186 && e.e_arg2 = 5)  || 
              (e.e_arg1 = 187 && e.e_arg2 = 5)  || 
              (e.e_arg1 = 188 && e.e_arg2 = 5)  
              then begin
		e.e_arg2 <- 4 ; (* RACE.IDS *) 
		e.e_arg1 <- 152; (* ELEMENTAL *) 
              end else if 
		(e.e_arg1 = 184 && e.e_arg2 = 5)  || 
		(e.e_arg1 = 185 && e.e_arg2 = 5)  
              then begin
		e.e_arg2 <- 4 ; (* RACE.IDS *) 
		e.e_arg1 <- 156; (* ELEMENTAL *) 
              end else if 
		(e.e_arg1 = 197 && e.e_arg2 = 5)  || 
		(e.e_arg1 = 195 && e.e_arg2 = 5)  
              then begin
		e.e_arg2 <- 4 ; (* RACE.IDS *) 
		e.e_arg1 <- 175; (* Djinni *) 
              end else if (e.e_arg1 = 167 && e.e_arg2 = 5) then begin
		e.e_arg2 <- 4 ; (* RACE.IDS *) 
		e.e_arg1 <- 165; (* TROLL *) 
              end else if (e.e_arg1 = 215 && e.e_arg2 = 4) then begin
		e.e_arg2 <- 4 ; (* RACE.IDS *) 
		e.e_arg1 <- 167; (* Undead *) 
              end else begin 
		let b,i = ids_pair_of_opcode_arg e.e_arg2 in 
		(if b <> "" then e.e_arg1 <- convert_ids_int e.e_arg1 b i) ;
              end ; 
              [ e ] 
          end

	  | "Kill_target" 
	    -> 
	      if String.length r > 2 && String.sub (String.uppercase r) 0 2 = "SP" 
              then [ { e with e_arg2 = 8 ; e_arg1 = 0; } ] 
	      else 
		[ { e with e_opcode = target_opcode "Death_magic" ;
                    e_arg2 = 8; e_arg1 = 0; } ] 

	  | "Movementrate_penalty" 
	    -> [ { e with e_opcode = target_opcode "Slow" ; e_arg1 = 0; e_arg2 = 0; } ]
	  | "Slow_poison"
	    -> [ { e with e_opcode = target_opcode "Cure_poison" ; 
		   e_arg1 = 0; e_arg2 = 0; } ]
	  | "Remove_fear"
	    -> [ { e with e_opcode = target_opcode "Remove_panic" ; 
		   e_arg1 = 0; e_arg2 = 0; } ]

	  | "Disease" when e.e_arg2 >= 1 || e.e_arg2 <= 15 -> [ e ] 

	  | "Poison" -> 
              begin
		match e.e_arg2 with
		| 3 -> [ { e with e_arg2 = 4 } ] (* HP per round *)
		| _ -> [ e ] (* HP per sec *)
              end 

	  | "Regeneration" -> 
              begin
		match e.e_arg2 with
		| 3 -> [ { e with e_arg1 = min 21 e.e_arg1 } ]
                      (* HP per round, not too slow! *) 
		| _ -> [ e ] 
              end 

	  | "Give_innate_ability" 
	  | "Remove_spell" ->
	      (try let res = convert e.e_resref "SPL" in
              Hashtbl.replace innate_ability_ht res true ; 
              [ { e with e_resref = res ; e_arg1 = 0; e_arg2 = 0; } ] 
	      with _ -> []) 


		(* ZERO ARGS! *)
	  | "Berserk" 
	  | "Confusion"
	  | "Cure_blindness" 
	  | "Cure_deafness" 
	  | "Cure_feeblemindedness" 
	  | "Defrost" 
	  | "Feeblemindedness" 
	  | "Free_action" 
	  | "Haste" 
	  | "Hold_creature" 
	  | "Immunity_to_backstab" 
	  | "Non-detection" 
	  | "Infravision" 
	  | "Panic" 
	  | "Petrification" 
	  | "Raise_dead" 
	  | "Remove_creature" 
	  | "Remove_hold" 
	  | "Silence" 
	  | "Sleep" 
	  | "Slow" 
	  | "Stun" 
	  | "Unsummon_creature" 
	  | "Unstun" 
	  | "Vocalize" 
	  | "Detect_invisible"
	  | "Remove_panic" 
	    -> [ { e with e_arg1 = 0; e_arg2 = 0; } ]

	  | "Stone_to_flesh" -> [ { e with e_opcode = target_opcode
				      "Petrification" ; e_arg1 = 0; e_arg2 = 0; } ] 

		(* ZERO/ONE ARGS! *) 
	  | "Dispel_effects" 
	    -> [ { e with e_arg1 = 0; e_arg2 = 1; e_savetype = 16 } ]

	  | "Invisible_detection_by_script" 
	    -> [ { e with e_arg1 = 0; e_arg2 = 1; } ]

	  | "XP_bonus" -> [ { e with e_arg1 = convert_xp_award_int e.e_arg1 } ] 

	  | "Set_global_variable"
	    -> 
	      if e.e_arg2 = 0 then begin
		let a = 
		  if e.e_arg1 = 0 then
		    { empty_a with action_id = target_action "SetGlobal" ;
		      a_8 = "GLOBAL" ^ e.e_resref ; a_4 = Int32.of_int e.e_arg1 } 
		  else 
		    { empty_a with action_id = target_action "IncrementGlobal" ;
		      a_8 = "GLOBAL" ^ e.e_resref ; a_4 = Int32.of_int e.e_arg1 } in 
		let cre = convert_overrider a in
		[ { e with e_opcode = target_opcode "Summon__as_ally" ;
		    e_arg1 = 1 ; e_arg2 = 1; e_target = 1; 
		    e_resref = cre } ] 
	      end else (error "OPCODE" "SIMULATE Set_global_variable %d\n" 
			  e.e_arg2 ; [] )
		  (* create a creature here that sets the global variable! *) 

	  | u -> error "OPCODE" "VERIFY %s %d %d [%s]\n" u 
		e.e_arg1 e.e_arg2 r ; [ e ]
	end
      with Not_found -> begin
	match name with
	| "Disallow_item" 
	| "Freedom" 
	| "Immunity_to_specific_animation" 
	| "Increase_casting_speed_factor" 
	| "Magical_fire_resistance_bonus"
	| "Magical_cold_resistance_bonus"
	| "Reenable_button" 
	| "Set_traps_bonus" 
	| "Attack_nearest_creature" 
	| "Creature_uninterruptable" 
	| "Exceptional_strength_bonus" 
	| "Remove_feet_circle" 
	| "Shake_window" 
	| "Disable_display_string"
	| "Drop_weapons"
	| "Protection_from_normal_missiles_overlay" 
	| "Change_title"
	| "Detect_illusions_bonus"
	| "Detect_alignment"
	| "Disable_visual_effect"
	| "Zone_of_sweet_air"
	| "Remove_animation"
	| "BG2_Unknown__125" (* AI Fast? *)
	| "Modify_script_state" 
	| "Remove_spell_protections"
	  -> [ ] 

	| "Imprisonment"
	  -> [ { e with e_opcode = target_opcode "Kill_target" ;
		 e_arg2 = 1024 ; e_arg1 = 0; } ] 

	| "Maze"
	  -> [ { e with e_opcode = target_opcode "Stun"  ;
		 e_arg1 = 0 ; e_arg2 = 0; 
		 e_time = e.e_time + 35 ; } ]

	| "True_sight" 
	  -> [ { e with e_opcode = target_opcode "Detect_invisible" } ]


	| "Spell_deflection"
	| "Spell_trap"
	  -> [ { e with e_opcode = target_opcode "Magic_resistance_bonus" ;
		 e_arg2 = 0 ; (* inc/dec *) e_arg1 = 30 ; }  ]

	| "Wing_buffet"
	  -> [ { e with e_opcode = target_opcode "Sleep" ;
		 e_arg1 = 0; e_arg2 = 1;
		 e_time = e.e_time * 7 ;
		 e_savebonus = e.e_savebonus * 5; } ] 

	| "Find_traps" 
	  -> [ { e with e_opcode = target_opcode "Detect_traps" } ] 

	| "Entangle_overlay"
	  -> [ { e with e_opcode = target_opcode "Visual_effect" ;
		 e_arg2 = 1 ; e_arg1 = 0 ; } ] 
	| "Grease_effect_overlay"
	  -> [ { e with e_opcode = target_opcode "Visual_effect" ;
		 e_arg2 = 4 ; e_arg1 = 0 ; } ] 
	| "Web_effect_overlay"
	  -> [ { e with e_opcode = target_opcode "Visual_effect" ;
		 e_arg2 = 5 ; e_arg1 = 0 ; } ] 

	| "Power_word__Sleep" 
	  -> [ { e with e_opcode = target_opcode "Sleep" ;
		 e_arg1 = 0; e_arg2 = 1; } ] 
	| "Power_word__Stun" 
	  -> [ { e with e_opcode = target_opcode "Stun" ;
		 e_arg1 = 0; e_arg2 = 0; } ] 

	| "No_collision_detection" 
	    (* THIS MAY BREAK THE WORLD!  *)
	  -> [ e ] 

	| "Save_vs_breath_bonus" 
	| "Save_vs_wands_bonus" 
	  -> [ { e with e_opcode = target_opcode "Save_vs_reflex_bonus" } ]
	| "Save_vs_death_bonus" 
	| "Save_vs_polymorph_bonus" 
	  -> [ { e with e_opcode = target_opcode "Save_vs_fortitude_bonus" } ]
	| "Save_vs_spell_bonus" 
	  -> [ { e with e_opcode = target_opcode "Save_vs_will_bonus" } ]

	| "Physical_mirror"
	  -> [ { e with e_opcode = target_opcode "Protection_from_arrows" ;
		 e_arg1 = 0 ; e_arg2 = 1; (* 10/+1 *) } ] 

	| "Power_word__Kill" 
	  -> [ { e with e_opcode = target_opcode "Death_magic" ;
		 e_arg1 = 0 ; e_arg2 = 4; (* death *) } ] 

	| "Attack_roll_penalty"
	| "Protection_from_creature_type"
	  -> [ { e with e_opcode = target_opcode "Protection_from_evil" ;
		 e_arg1 = 0 ; e_arg2 = 0; } ;
               { e with e_opcode = target_opcode "Set_state" ;
		 e_arg1 = 0 ; e_arg2 = 1; (* pro-evil *) } ] 

	| "Cast_spell__scroll"
	  -> [ { e with e_opcode = target_opcode "Cast_spell" ;
                 e_resref = convert e.e_resref "SPL" } ] 

	| "Burning_hands_overlay" 
	  -> [ { e with e_opcode = target_opcode "Visual_spell_hit" ;
		 e_arg1 = 0 ; e_arg2 = 2; (* alternation *) } ] 

	| "Disintegrate" 
	  -> [ { e with e_opcode = target_opcode "Kill_target" ;
		 e_arg1 = 0; e_arg2 = 512; (* disintegrate *) } ]

	|  "Level_drain" ->
	    [ { e with e_opcode = target_opcode "Energy_drain" } ] 

	| "Summon_monsters" ->
	    [ { e with e_opcode = target_opcode "Cast_spell" ;
		e_arg1 = 1; e_arg2 = 10; e_resref = "SPWI505" } ] 

	| "Fist_THAC0_bonus" 
	| "THACO_bonus" 
	| "THAC0_bonus" ->
	    [ { e with e_opcode = target_opcode "Base_attack_bonus" ; } ] 
	| "Fist_damage_bonus" ->
	    [ { e with e_opcode = target_opcode "Attack_damage_bonus" ; } ] 

	| "Minor_globe_overlay" ->
	    [ { e with e_opcode = target_opcode "Globe_of_invulnerability" ;
                e_arg1 = 0 ; e_arg2 = 0; } ] 

	| "Lighting_effects" 
	| "Play_animation_sequence" 
	| "Play_damage_animation"
	| "Play_3D_Effect" ->
	    [ { e with e_opcode = target_opcode "Visual_spell_hit" ;
                e_arg1 = 0 ;
                e_arg2 = (1 + (Random.int 8)) } ]

	| "Spell_type_deflection" (* cloak of mirroring *) 
	  -> [ { e with e_opcode = target_opcode "Magic_damage_resistance_bonus" ;
		 e_arg2 = 0; e_arg1 = 20; } ] 

	| "Use_EFF_file" 
	  -> (try convert_eff_file e e.e_resref with _ -> [])

	| "Cast_spell_on_condition"
	  when e.e_arg1 = 1 && e.e_arg2 = 0 
	  -> 
	    [ { e with e_opcode = target_opcode "When_struck_using_effects_list" ;
		e_arg1 = 0; e_arg2 = 0;
		e_resref = convert e.e_resref "SPL" ; } ] 

	| "Create_item_2"
	  -> [ { e with e_opcode = target_opcode "Create_item" ;
		 e_resref = convert e.e_resref "ITM" } ]

	| "Melee_weapon_damage_bonus"
	  -> [ { e with e_opcode = target_opcode "Attack_damage_bonus" ; } ]

	| "Reflect_specified_spell"
	  -> [ { e with e_opcode = target_opcode "Protection_from_spell" ;
		 e_arg1 = 0; e_arg2 = 0; e_resref = convert e.e_resref "SPL" ; } ] 

	| "Hold_creature_type"
	  -> [ { e with e_opcode = target_opcode "Hold_creature" ; 
		 e_arg1 = 0; e_arg2 = 0; } ] 

	| "Use_EFF_File_on_condition" 
	  when e.e_arg2 = 3 (* once per arg1 seconds *) 
	  -> [ { e with e_opcode = target_opcode "Persistant_using_effects_list" ;
		 e_arg2 = 0; e_resref = convert e.e_resref "EFF" } ]

	      (* OK, this is a little weird, but hey! *) 
	| "Drain_wizard_spells" -> 
            [ { e with e_opcode = target_opcode "Feeblemindedness" ;
		e_arg1 = 0; e_arg2 = 0; 
		e_savetype = 16 ; (* will *) } ] 

	| u -> error "OPCODE" "SIMULATE %s (%s)\n" u r ; [ ] 
      end 
    end 
  with ex -> 
    ( error "EFF" "Error converting EFF with opcode %d (0x%x) in %s\n" 
	e.e_opcode e.e_opcode r ;
      raise ex )

and strip_duplicates_eff_list l =
  let l = List.fast_sort (fun e1 e2 -> 
    let first = compare e1.e_opcode e2.e_opcode in
    if first = 0 then compare e1 e2 else first) l in
  (* look for duplicates, places where we can merge damage, etc. *)
  let rec walk lst = match lst with
    a :: b :: tl -> begin
      if a.e_opcode = b.e_opcode then begin
        let name = (ids_of_int (config.target) "TARGOP" 
		      (Int32.of_int a.e_opcode)).i_name 
        in 
        let merged_into_a = match name with 
        | "Damage" 
        | "Magic_resistance_bonus"
          -> 
            if a.e_arg2 = b.e_arg2 &&
              a.e_dicesize = b.e_dicesize && 
              a.e_prob1 = b.e_prob1 &&
              a.e_prob2 = b.e_prob2  then begin
		a.e_numdice <- a.e_numdice + b.e_numdice ;
		a.e_arg1 <- a.e_arg1 + b.e_arg1 ;
		a.e_savetype <- max a.e_savetype b.e_savetype ;
		a.e_savebonus <- max a.e_savebonus b.e_savebonus ;
		(if a.e_arg1 > 1000 then
		  a.e_time <- max a.e_time b.e_time
		else
		  a.e_time <- min a.e_time b.e_time) ; 
		true 
              end else false 

        | "Protection_from_arrows" 
        | "Save_vs_reflex_bonus" 
        | "Save_vs_fortitude_bonus" 
        | "Save_vs_will_bonus" -> true 

        | "Summon_creature" -> false 

(*
  | "Add_effects_list" -> 
  let ans = (a.e_resref = b.e_resref) && (a.e_arg2 = b.e_arg2) in
  log_and_print "Answer = %b\n" ans ;
  ans
 *)

        | _ -> a = b

        in
        if merged_into_a then begin
          (walk (a :: tl))
        end else
          a :: (walk (b :: tl))
      end else a :: (walk (b :: tl))
    end
  | _ -> lst 
  in 
  walk l 


and convert_eff_array new_r r ea ce =
  Stats.time "EFF" (fun () -> 
    let a_of_l = Array.map (convert_eff new_r r ce) ea in
    let l_of_l = Array.to_list a_of_l in
    let l = List.flatten l_of_l in
    let l = strip_duplicates_eff_list l in 
    let a = Array.of_list l in
    a
		   ) () 

and convert_bg2_opcode_arg_to_iwd2 arg2 arg1 r = 
  let bif, iff = ids_pair_of_opcode_arg arg2 in 
  if bif = "" then None
  else if arg1 = 0 then Some(0) 
  else begin 
    let bids = try (Bcs.ids_of_int (config.source) bif (Int32.of_int arg1))
    with Not_found -> 
      error "IDS" "[AEL.IDS] UNKNOWN %d in %s (%s)\n" arg1 bif r ;
      failwith "convert_bg2_opcode_arg_to_iwd2" 
    in 
    let sym = convert bids.i_name (String.lowercase bif) in 
    let num = (match sym with
    | "UNDEAD" | "WeakestOf" -> Some( 1)

    | "CHAOTIC_EVIL" 
    | "LAWFUL_EVIL" 
    | "NEUTRAL_EVIL" 
    | "MASK_EVIL" -> Some( 37)

    | "MASK_GOOD" 
    | "LAWFUL_GOOD" 
    | "NEUTRAL_GOOD" 
    | "CHAOTIC_GOOD" -> Some( 33)

    | "NEUTRAL"
    | "LAWFUL_NEUTRAL" 
    | "CHAOTIC_NEUTRAL"
    | "MASK_GENEUTRAL"
    | "MASK_LAWFUL"
    | "MASK_LCNEUTRAL"
    | "MASK_CHAOTIC" -> Some( -1)

    | "TROLL" -> Some( 83)
    | "GIANT" | "GIANTHUMANOID" -> Some( 81)
    | "ANIMAL" | "LeaderOf" -> Some( 7)
    | "ELEMENTAL" -> Some( 9)
    | "ELF" -> Some( 15)
    | "GOLEM" -> Some( 27)
    | "ORC" -> Some( 64)
    | "HUMAN" | "HUMANOID" -> Some( 5)

    | "GENERAL_ITEM" -> Some( 0)
    | "NIETHER" ->  None

    | _ -> 
        error "AEL" "[%8s] defaulting %s to 'not human'\n" r sym ;
        Some(6) (* not human *) )
    in 
    num
  end 

and convert_eff_file e r =
  let r = convert r "not_found" in 
  let buff = load_source_res r "EFF" in

  if String.sub buff 0 8 <> "EFF V2.0" then begin
    failwith "not a valid EFF V2.0 file (wrong sig)"
  end ; 

  let eff_file = read_bg2_eff_file buff in 
  let bg2_ids = try (Bcs.ids_of_int (config.source) "SRCOP" 
		       (Int32.of_int eff_file.e_opcode))
  with Not_found -> 
    log_and_print "ERROR: OPCODE UNKNOWN %d (convert_eff_file %s)\n" 
      eff_file.e_opcode r; exit 1
  in
  let name = convert bg2_ids.Ids.i_name "srcop" in
  let no_ce = { ce_is_shield = false } in 

  let handle_bane arg2 arg1 e name = 
    match convert_bg2_opcode_arg_to_iwd2 arg2 arg1 r with 
    | None -> []
    | Some(num) -> begin 
	let resref = begin match name with
	| "Kill_target" -> "EFFDIS14" 
	| "Blindness" -> "EFFDD1" 
	| "Break_morale" 
	| "Panic" -> "EFFFEAR" 
	| _ -> "EFFUHC1" 

	end in 
	if num = -1 then []
	else 
	  [ { empty_eff with e_opcode = target_opcode "Add_effects_list" ;
              e_arg1 = 0; e_arg2 = num ; 
              e_target = e.e_target ;
              e_duration = 1; (* inst/perm *)
              e_prob1 = 100;
              e_resref = resref ; } ] 
    end
  in 

  match name with
  | "Summon_creature" -> convert_eff r r no_ce 
	{ eff_file with e_target = 2 ; e_duration = 0 ;}

  | "THAC0_vs._type_bonus"
  | "Damage_vs._type_bonus" -> 
      handle_bane eff_file.e_arg2 eff_file.e_arg1 e name

  | "Kill_target"
  | "Blindness" 
  | "Break_morale" 
  | "Panic" 
  | "Damage"  ->
      handle_bane e.e_arg2 e.e_arg1 e name

  | "Display_string"
  | "Display_portrait_icon"
  | "Character_color_1"
  | "Character_color_2"
  | "Play_3D_Effect" 
  | "Play_sound" 
  | "Polymorph" 
  | "Remove_creature" (* used in true sight, etc *) 
    -> [ ] 

  | "Sleep" 
    -> [ { e with e_opcode = target_opcode "Sleep" ; e_time = 21 ; 
           e_arg1 = 0; e_arg2 = 1; } ]

  | _ -> 
      error "OPCODE" "MUST SIMULATE: %s in Eff_File %s\n" name r ; [ ] 

(***********************************************************************
 * ITM Files
 *            --> mutate an ITM to 3e rules for IWD2
 ***********************************************************************)
and print_xdy x y z =
  if x = 0 then Printf.sprintf "%+d" z
  else if z = 0 then Printf.sprintf "%dd%d" x y 
  else Printf.sprintf "%dd%d%+d" x y z 

and add_eff_list_name what = match what with 
| 01 -> "Undead"
| 05 -> "Humanoids" 
| 06 -> "Non-Humans"
| 07 -> "Animals"
| 09 -> "Elementals" 
| 15 -> "Elves"
| 27 -> "Golems"
| 33 -> "Good Creatures"
| 37 -> "Evil Creatures"
| 64 -> "Orcs" 
| 81 -> "Giants" 
| 83 -> "Trolls" 
| 00 -> "Any Target" 
| _ -> (Printf.sprintf "??? %d ???" what)

and basic_eff_name op_str e = match op_str with 
| "Set_item_color" 
| "Set_colorglow_solid" 
| "Set_colorglow_pulse" 
| "Display_string" 
| "Display_portrait_icon" 
| "Remove_portrait_icon" 
| "Character_color_1" 
| "Character_color_2"
| "Play_sound" 
| "Visual_spell_hit" 
| "Remove_hold" 
| "Unstun" 
| "Protection_from_spell" 
| "Set_state" 
| "Remove_item" 
| "Translucent" 
| "Visual_effect" 
| "Remove_spell"
| "Animation_change" 
  -> "" 

| "Damage_reduction" ->
    Printf.sprintf "Damage Reduction: %+d/%+d" e.e_arg1 e.e_arg2 

| "Death_magic" -> 
    Printf.sprintf "Magical Instant Death" 

| "Current_HP_bonus" ->
    Printf.sprintf "Healing: %+d" e.e_arg1

| "Energy_drain" ->
    Printf.sprintf "Energy Drain: -%d attack bonus, -%d to all saves, -%d hit points"
      e.e_arg2 e.e_arg2 (5*e.e_arg2) 

| "Disease" ->
    Printf.sprintf "Disease: %+d %s" e.e_arg1 
      (match e.e_arg2 with
      | 4 -> "Strength Bonus" 
      | 5 -> "Dexterity Bonus" 
      | 6 -> "Constitution Bonus"
      | 7 -> "Intelligence Bonus" 
      | 8 -> "Wisdom Bonus" 
      | 9 -> "Charisma Bonus"
      | 2 -> "Hit Point(s) of Damage per Second" 
      | 3 -> "Second(s) per Hit Point of Damage" 
      | _ -> "Special")

| "Poison" ->
    Printf.sprintf "Poison: %+d %s" e.e_arg1 
      (match e.e_arg2 with
      | 2 -> "Hit Point(s) of Damage per Second" 
      | 3 -> "Second(s) per Hit Point of Damage" 
      | 4 -> "Hit Point(s) of Damage per Round" 
      | _ -> "Special")

| "Stoneskin_effect" when e.e_arg2 = 0 -> 
    Printf.sprintf "Stoneskin: %d skin(s)" e.e_arg1 
| "Stoneskin_effect" when e.e_arg2 = 1 -> 
    Printf.sprintf "Ironskin: %d skin(s)" e.e_arg1 
| "Draw_upon_holy_might__non-cumulative" -> 
    Printf.sprintf "Draw Upon Holy Might: %+d" e.e_arg1 
| "Chant__non-cumulative" -> 
    Printf.sprintf "Chant: %+d (attack, damage and saving throws)" e.e_arg1 
| "Aid__non-cumulative" -> 
    Printf.sprintf "Aid: %+d (attacks and saves)" e.e_arg1 
| "Bless__non-cumulative" -> 
    Printf.sprintf "Bless: %+d (attack rolls)" e.e_arg1 
| "Increase_casting_speed_factor" -> 
    Printf.sprintf "Casting Speed: %+d" e.e_arg1 
| "Increase_attack_speed_factor" -> 
    Printf.sprintf "Attack Speed: %+d" e.e_arg1 
| "Spell_type_deflection" when e.e_arg2 = 10 -> 
    Printf.sprintf "Spell Deflection: Offensive Spells"
| "Spell_type_deflection" when e.e_arg2 = 11 -> 
    Printf.sprintf "Spell Deflection: Disabling Spells"
| "Protection_from_creature_type" when e.e_arg1 = 9 && 
    e.e_arg2 = 7 -> 
      Printf.sprintf "Protection From: Summoned Demons" 
| "Immunity_to_weapons" when e.e_arg1 = 0 && e.e_arg2 = 2 -> 
    Printf.sprintf "Immunity: Non-Magical Weapons" 
| "Maximum_damage_each_hit" -> 
    "Maximum Damage On Each Hit" 

| "Regeneration" ->
    Printf.sprintf "Regeneration: %+d %s" e.e_arg1 
      (match e.e_arg2 with
      | 2 -> "Hit Point(s) per Second" 
      | 3 -> "Second(s) per Hit Point" 
      | 4 -> "Hit Point(s) per Round" 
      | _ -> "Special")

| "Globe_of_invulnerability" -> 
    Printf.sprintf "Globe of Invulnerability: %s"
      (match e.e_arg2 with
      | 0 -> "Minor"
      | _ -> "Major") 

| "Immunity_to_backstab" -> "Immunity: Sneak Attacks"
| "Invisible_detection_by_script" -> "See Invisible Creatures" 
| "Non-detection" -> "Non-detection" 

| "Wing_buffet" -> "Knock back" 
| "Panic" -> "Panic" 
| "Petrification" -> "Petrification" 
| "Remove_panic" -> "Remove panic" 

| "Sleep" -> 
    Printf.sprintf "Sleep: %s"
      (match e.e_arg2 with
        0 -> "Awaken when damaged"
      | _ -> "Remain asleep when damaged") 

| "Cure_poison" -> "Cure poison" 
| "Cure_disease"-> "Cure disease" 
| "Dispel_effects" -> "Dispel magical effects" 
| "Detect_invisible" -> "Detect invisibility" 
| "Cure_feeblemindedness" -> "Cure feeblemind" 
| "Feeblemindedness" -> "Feeblemind" 

| "Intoxication_bonus" when e.e_arg1 = 0 && e.e_arg2 = 1 ->  "Cure intoxication" 
| "Fatigue_bonus" when e.e_arg1 = 0 && e.e_arg2 = 1 -> "Cure fatigue" 
| "Intoxication_bonus" ->  Printf.sprintf "Intoxication bonus: %+d" e.e_arg1  
| "Fatigue_bonus" ->  Printf.sprintf "Fatigue bonus: %+d" e.e_arg1  

| "Damage" ->
    let tau = e.e_arg2 lsr 16 in 
    Printf.sprintf "%s Damage: %s"
      (try (String.capitalize (String.lowercase (ids_of_int (config.target) "DAMAGES" (Int32.of_int tau)).i_name))
      with _ -> Printf.sprintf "0x%X" tau)
      (print_xdy e.e_numdice e.e_dicesize e.e_arg1)

| "Bonus_to_AC" -> 
    Printf.sprintf "AC Bonus: %+d (%s)" e.e_arg1 
      (match e.e_arg2 with
      | 0 -> "Generic, cumulative" 
      | 1 -> "Armor"
      | 2 -> "Deflection" 
      | 3 -> "Shield" 
      | 4 -> "vs. Bludgeoning, cumulative" 
      | 5 -> "vs. Piercing, cumulative" 
      | 6 -> "vs. Slashing, cumulative"
      | 7 -> "vs. Missiles, cumulative"
      | 16 -> "Base"
      | i -> Printf.sprintf "Special (%d)" i)

| "Mirror_image" 
  -> Printf.sprintf "Mirror Image: %+d images" e.e_arg1
| "Minimum_HP" 
  -> Printf.sprintf "Minimum HP: %+d" e.e_arg1
| "Max_HP_bonus" 
  -> Printf.sprintf "Maximum HP Bonus: %+d" e.e_arg1
| "Acid_resistance_bonus" 
  -> Printf.sprintf "Acid Resistance: %+d" e.e_arg1
| "Magic_damage_resistance_bonus"
  -> Printf.sprintf "Magic Damage Resistance: %+d" e.e_arg1
| "Fire_resistance_bonus"  
  -> Printf.sprintf "Fire Resistance: %+d" e.e_arg1
| "Electricity_resistance_bonus" 
  -> Printf.sprintf "Electricity Resistance: %+d" e.e_arg1
| "Cold_resistance_bonus" 
  -> Printf.sprintf "Cold Resistance: %+d" e.e_arg1
| "Lockpicking_bonus" 
  -> Printf.sprintf "Lockpicking Bonus: %+d" e.e_arg1
| "Pick_pockets_bonus" 
  -> Printf.sprintf "Pick Pockets Bonus: %+d" e.e_arg1
| "Find_traps_bonus" 
  -> Printf.sprintf "Search Bonus: %+d" e.e_arg1
| "Hide_in_shadow_bonus" 
  -> Printf.sprintf "Hide Bonus: %+d" e.e_arg1
| "Stealth_bonus" 
  -> Printf.sprintf "Move Silently Bonus: %+d" e.e_arg1
| "Magic_resistance_bonus" 
  -> Printf.sprintf "Spell Resistance Bonus: %+d" e.e_arg1
| "Slashing_resistance_bonus"
  -> Printf.sprintf "Slashing Resistance Bonus: %+d (cumulative)" e.e_arg1
| "Crushing_resistance_bonus"
  -> Printf.sprintf "Bludgeoning Resistance Bonus: %+d (cumulative)" e.e_arg1
| "Piercing_resistance_bonus"
  -> Printf.sprintf "Piercing Resistance Bonus: %+d (cumulative)" e.e_arg1
| "Missile_resistance_bonus" 
  -> Printf.sprintf "Missile Resistance: %+d (cumulative)" e.e_arg1
| "Reduced_damage_from_poison"
  -> Printf.sprintf "Poison Resistance Bonus: %+d (cumulative)" e.e_arg1

| "Protection_from_evil"
  -> Printf.sprintf "Protection from Evil: +2 AC Bonus (Deflection), +2 Saving Throw Bonus" 

| "Lore_bonus" 
  -> Printf.sprintf "Lore Bonus: %+d" e.e_arg1
| "Strength_bonus" 
  -> Printf.sprintf "Strength Bonus: %+d" e.e_arg1
| "Dexterity_bonus" 
  -> Printf.sprintf "Dexterity Bonus: %+d" e.e_arg1
| "Constitution_bonus" 
  -> Printf.sprintf "Constitution Bonus: %+d" e.e_arg1
| "Intelligence_bonus" 
  -> Printf.sprintf "Intelligence Bonus: %+d" e.e_arg1
| "Wisdom_bonus" 
  -> Printf.sprintf "Wisdom Bonus: %+d" e.e_arg1
| "Charisma_bonus" 
  -> Printf.sprintf "Charisma Bonus: %+d" e.e_arg1

| "Attack_damage_bonus" 
  -> Printf.sprintf "Attack Damage Bonus: %+d" e.e_arg1
| "Base_attack_bonus" 
  -> Printf.sprintf "Base Attack Bonus: %+d" e.e_arg1
| "Break_morale" 
  -> Printf.sprintf "Morale Bonus: %+d" e.e_arg1
| "Casting_failure" 
  -> Printf.sprintf "Spell Failure: %+d%%" e.e_arg1

| "Protection_from_spell_school" -> 
    Printf.sprintf "Protection From Spell School: %s"
      (match e.e_arg2 with
      | 1 -> "Abjuration"
      | 2 -> "Conjuration"
      | 3 -> "Divination"
      | 4 -> "Enchantment"
      | 5 -> "Illusion"
      | 6 -> "Evocation"
      | 7 -> "Necromancy"
      | 8 -> "Alteration"
      | _ -> "Unknown")

| "Attacks_per_round_bonus" 
  -> Printf.sprintf "Attacks per Round Bonus: %+d" e.e_arg1

| "Missile_THAC0_bonus" 
  -> Printf.sprintf "Missile Attack Bonus: %+d" e.e_arg1

| "Maze" 
| "Berserk" 
| "Blindness" 
| "Vocalize" 
| "Entangle" 
| "Slow" 
| "Stun" 
| "Confusion" 
| "Infravision" 
| "Silence" 
| "Deafness" 
  -> op_str

| "Hold_creature_type" when e.e_arg1 = 0 && e.e_arg2 = 2 -> "Hold Monster"
| "Kill_target" when e.e_arg1 = 0 && e.e_arg2 = 8 -> "Vorpal Hit"
| "Level_drain" -> Printf.sprintf "Level Drain: %d" e.e_arg1 
| "Luck_bonus" -> Printf.sprintf "Luck: %+d" e.e_arg1 
| "Teleport_field" -> "Teleport Field" 
| "Power_word__Kill" -> "Power Word, Kill" 
| "Power_word__Stun" -> "Power Word, Stun" 
| "Power_word__Blind" -> "Power Word, Blind" 
| "True_sight" -> "Dispel Illusions" 

| "Invisibility" -> 
    Printf.sprintf "Invisibility: %s" 
      (if (e.e_arg2 = 1) then "Improved" else "Normal") 

| "Grease_effect" -> "Grease"
| "Raise_dead" -> "Raise Dead" 
| "Web_effect" -> "Web"
| "Hold_creature" -> "Hold" 
| "Charm_creature"  -> "Charm" 
| "Control"  -> "Dominate" 
| "Free_action" -> "Freedom of Movement" 

| "Protection_from_arrows" -> 
    Printf.sprintf "Protection from Arrows: Damage Reduction 10/+%d"
      (e.e_arg2) 

| "Remove_spell_protections" 
  -> if e.e_arg2 = 3 then "Remove Illusionary Spell Protections" 
  else "Remove Combat Spell Protections" 
| "Remove_magical_protections" 
  -> "Remove Magical Spell Protections" 

| "Save_vs_death_bonus" 
  -> Printf.sprintf "Death save Bonus: %+d" e.e_arg1
| "Save_vs_wands_bonus" 
  -> Printf.sprintf "Wands save Bonus: %+d" e.e_arg1
| "Save_vs_polymorph_bonus" 
  -> Printf.sprintf "Polymorph save Bonus: %+d" e.e_arg1
| "Save_vs_breath_bonus" 
  -> Printf.sprintf "Breath save Bonus: %+d" e.e_arg1
| "Save_vs_spell_bonus" 
  -> Printf.sprintf "Spell save Bonus: %+d" e.e_arg1
| "Double_number_of_attacks"
  -> "Double Number Of Attacks Per Round"
| "Immune_to_timestop"
  -> "Immunity: Timestop" 
| "Immunity_to_spell_level"
  -> Printf.sprintf "Immunity: Level %d Spells" (e.e_arg1) 


| "Save_vs_reflex_bonus" 
  -> Printf.sprintf "Reflex save Bonus: %+d" e.e_arg1
| "Save_vs_fortitude_bonus" 
  -> Printf.sprintf "Fortitude save Bonus: %+d" e.e_arg1
| "Save_vs_will_bonus" 
  -> Printf.sprintf "Will save Bonus: %+d" e.e_arg1

| "Immunity_to_effect" 
  -> let op' = e.e_arg2 in
  let op_str' = (try (ids_of_int (config.target) "TARGOP" (Int32.of_int op')).i_name with _ -> (log_and_print "ERROR: OPCODE: Immunity to opcode %d which is not an IWD2 opcode\n" op' ; exit 1)) in
  let basic_name = basic_eff_name op_str' e in
  let colon_regexp = Str.regexp ":" in
  let basic_name = 
    try List.hd (Str.split colon_regexp basic_name) 
    with _ -> basic_name in 
  Printf.sprintf "Immunity: %s" basic_name 

| "Movementrate_bonus"   -> "Movement rate bonus" 
| "Movementrate_penalty" -> "Movement rate penalty" 

| "Blur" 
  -> Printf.sprintf "Blur: +20%% Concealment bonus (bonus)"

| "Haste" 
  -> Printf.sprintf "Haste: +4 AC bonus (Generic), +1 Attack per round, double movement rate"

| "Add_effects_list" ->
    begin 
      let what = add_eff_list_name e.e_arg2 in 
      match e.e_resref with 
      | "EFFUHC1" -> Printf.sprintf "Bane: +2d6 Magic Damage to %s" what
      | "EFFDIS14" -> Printf.sprintf "Disruption: %s (Fortitude save at -4)" what
      | "EFFDD1" -> Printf.sprintf "Blindness to %s" what 
      | "EFFFEAR" -> Printf.sprintf "Panic to %s" what 
      | "__CHARM" -> Printf.sprintf "Dire Charm: %s (Will save at +0)" what
      | _ -> Printf.sprintf "Magical Effect to %s" what 
    end 

| "Remove_inventory_item" ->
    let str = iwg2_name_of_itm e.e_resref in
    if str = "" then "Remove Item: Special"
    else Printf.sprintf "Remove Item: %s" str 
| "Create_inventory_item"
| "Create_item" ->
    let str = iwg2_name_of_itm e.e_resref in
    Printf.sprintf "Create Item: %s" str 

| "Cast_spell" -> 
    let str = iwg2_name_of_spl e.e_resref in
    Printf.sprintf "Cast Spell: %s" str 

| "Give_innate_ability" -> 
    let str = iwg2_name_of_spl e.e_resref in
    Printf.sprintf "Give Innate Ability: %s" str 

| "Polymorph" -> 
    let str = iwg2_name_of_spl e.e_resref in
    Printf.sprintf "Polymorph: %s" str 

| "Summon__as_ally" 
| "Summon_creature" -> 
    let str = iwg2_name_of_cre e.e_resref in
    Printf.sprintf "Summon: %s" str 


| "Slay" -> begin
    let str = 
      let b,i = ids_pair_of_opcode_arg e.e_arg2 in 
      try 
        if b = "" then "Special"
        else let str = (Bcs.ids_of_int (config.target) i (Int32.of_int e.e_arg1)).Ids.i_name in
        String.capitalize (String.lowercase str) 
      with _ -> "Special" 
    in 
    Printf.sprintf "Slay: %s" str 
end

| _ -> error "OPCODE" "DESCRIBE %s\n" op_str ; 
    Printf.sprintf "%s %d %d" op_str e.e_arg1 e.e_arg2 

and describe_eff tab r e b =
  let op_str = 
    (try (ids_of_int (config.target) "TARGOP" 
	    (Int32.of_int e.e_opcode)).i_name with _ ->
	      (log_and_print "ERROR: OPCODE: Describe [%s.ITM] opcode %d\n" r e.e_opcode ; exit 1))
  in
  let base = basic_eff_name op_str e in
  if (base <> "") then begin 
    let chance = e.e_prob1 - e.e_prob2 in 
    Printf.bprintf b "%*s" tab " " ;
    if chance <> 100 then Printf.bprintf b "%d%% chance of " chance ;
    Printf.bprintf b "%s" base ;
    if e.e_target = 1 then Printf.bprintf b " to the bearer" ;
    (* TODO: saves! *) 
    if ((e.e_disres = 1 || e.e_disres = 3) && e.e_target <> 1) then begin
      Printf.bprintf b " (bypasses magic resistance)" ; 
    end ;
    if (e.e_savetype <> 0 ) then begin
      match config.target_variant with
      | IWD2 -> 
          Printf.bprintf b " (%s save at %+d)" 
            (if e.e_savetype = 8 then "Reflex" else if e.e_savetype = 4 then
              "Fortitude" else "Will") (0 - e.e_savebonus)
      | _ -> 
          Printf.bprintf b " (save at %+d)" (e.e_savebonus)
    end ; 
    if (e.e_duration = 0 && e.e_time <> 0) then begin
      Printf.bprintf b " for %d seconds" e.e_time ;
    end ; 
    Printf.bprintf b "\n" 
  end 

and describe_itm r i = 
  (* returns a string describing the ITM *) 
  let b = Buffer.create 80 in 
  let cat = 
    (try (ids_of_int (config.target) "TARGCAT" (Int32.of_int i.i_cat)).i_name with e ->
      (log_and_print "ERROR: ITM: %s.ITM has cat %d\n" r i.i_cat ; exit 1)) 
  in
  let general_name = 
    (try (ids_of_int (config.target) "TARGNAME" (Int32.of_int i.i_cat)).i_name with e ->
      (log_and_print "ERROR: ITM: %s.ITM has cat %d\n" r i.i_cat ; exit 1)) 
  in
  Printf.bprintf b "%s properties:\n" general_name ;
  Printf.bprintf b "Weight: %d\n" i.i_weight ; 
  if (i.i_lore <> 0) then Printf.bprintf b "Lore to identify: %d\n" i.i_lore ; 
  if (i.i_price <> 0) then Printf.bprintf b "Price: %d\n" i.i_price ; 

  let is_weapon = (List.exists (fun a -> a.a_type <> 3) 
                     (Array.to_list i.i_abil)) in

  let is_two_handed = i.i_flags land 2 = 2 in 

  if is_two_handed then Printf.bprintf b "Hands required: Two\n" ;

  if is_weapon then begin 
    Printf.bprintf b "Enchantment: +%d\n" i.i_enchant ; 
    Printf.bprintf b "Feat Required: %s\n" (match cat with
    | "Arrow"  
    | "Bow" -> "Martial Weapon, Bow" 
    | "Bullet"
    | "Dart" 
    | "Sling" -> "Simple Weapon, Missile" 
    | "Bolt"
    | "Crossbow" -> "Simple Weapon, Crossbow" 
    | "Axe" -> "Martial Weapon, Axe" 
    | "Flail" -> "Martial Weapon, Flail" 
    | "Great_Sword" -> "Martial Weapon, Great Sword" 
    | "Hammer" -> "Martial Weapon, Hammer" 
    | "Long_sword" -> "Martial Weapon, Large Sword" 
    | "Halberd" 
    | "Spear" -> "Martial Weapon, Polearm" 
    | "Quarterstaff" -> "Simple Weapon, Quarterstaff" 
    | "Mace" 
    | "Morning_star" 
    | "Club" -> "Simple Weapon, Mace" 
    | "Bastard_Sword" -> "Exotic Weapon, Bastard Sword" 
    | "Dagger"
    | "Short_sword" -> "Simple Weapon, Small Blade" 
    | _ -> (log_and_print "ERROR: ITM: %s.ITM feat cat %s\n" r cat ; exit 1))
  end else begin 
    let d,e,f = match cat with
    | "Leather" -> 6,0,10
    | "Studded_leather" -> 5,0,15
    | "Chainmail" -> 2,-4,30
    | "Splint" -> 0,-7,40
    | "Half_plate" -> 0,-7,40
    | "Full_plate" -> 1,-5,35
    | "Buckler" -> 0,-1,5
    | "Small_Shield" -> 0,-1,5
    | "Medium_Shield" -> 0,-2,15
    | "Large_Shield" -> 0,-10,50
    | _ -> 0,0,0
    in
    (if d <> 0 then Printf.bprintf b "Max Dex Bonus: %+d\n" d ) ;
    (if e <> 0 then Printf.bprintf b "Armor Check Penalty: %+d\n" e ) ;
    (if f <> 0 then Printf.bprintf b "Arcane Spell Failure: %+d%%\n" f ) ;
  end 
      ;

  if Array.length i.i_equipped > 0 then begin
    Printf.bprintf b "\nEquipped effects:\n" ; 
    Array.iter (fun e -> describe_eff 2 r e b) i.i_equipped ; 
  end ; 

  Array.iter (fun a ->
    Printf.bprintf b "\n%s ability:\n" (match a.a_type with
      1 -> "Melee weapon" 
    | 2 -> "Ranged weapon" 
    | 3 -> "Magical"
    | 4 -> "Missile Launcher"
    | i -> (log_and_print "ERROR: ITM: %s ability type %d" r i ; exit 1)) ; 
    (if a.a_range <> 1 then Printf.bprintf b "  Range: %d\n" a.a_range) ; 
    (if a.a_type <> 3 then begin 
      Printf.bprintf b "  Weapon Speed: %d\n" a.a_speed ; 
      Printf.bprintf b "  Attack Bonus: +%d\n" a.a_tohit ; 
    end ) ; 
    let invalid a = a = 0 || a > 1000 in 
    if (invalid a.a_numdice & invalid a.a_dmgbonus) then begin
      if a.a_type <> 3 then Printf.bprintf b "  Base Damage: None\n" ;
    end else begin 
      Printf.bprintf b "  Base Damage: %s\n" 
        (print_xdy a.a_numdice a.a_dicesize a.a_dmgbonus) ;
      Printf.bprintf b "  Damage Type: %s\n" (match a.a_dmgtype with
        0 -> "None"
      | 1 -> "Piercing"
      | 2 -> "Bludgeoning" 
      | 3 -> "Slashing"
      | 4 -> "Missile (piercing)"
      | 5 -> "Non-lethal" 
      | i -> (log_and_print "WARNING: ITM: %s.ITM has damage type %d\n" r i ;
              a.a_dmgtype <- 3;
              "Slashing")
					     )
    end ; 
    (if a.a_type <> 3 then Printf.bprintf b "  Strength Damage Bonus: %s\n"
	(match a.a_allowstr,is_two_handed with
	| 0,_ -> "None" 
	| _,false -> "Yes"
	| _,true -> "Yes (1.5x)")) ;
    if (a.a_attacktype = 1) then Printf.bprintf b "  Bypasses Armor: ignores armor AC bonuses\n"  ;
    if (a.a_attacktype = 2) then Printf.bprintf b "  Keen: increases critical hit range\n"  ;
    if Array.length a.a_eff > 0 then begin 
      (if a.a_type = 3 then 
        Printf.bprintf b "  Magical effects:\n" 
      else 
        Printf.bprintf b "  Striking effects:\n") ;
      Array.iter (fun e -> describe_eff 4 r e b) a.a_eff ; 
    end 
	     ) i.i_abil ; 
  Buffer.contents b 

and random_weapon l = 
  let arr = random_melee.( min (l / 10) 2) in 
  let idx = Random.int (Array.length arr) in
  convert arr.(idx) "ITM" 

and random_bow_arrow l = 
  let a1,a2 = match Random.int 3 with
    0 -> random_bow, random_arrow
  | 1 -> random_sling, random_bullet
  | _ -> random_xbow, random_bolt
  in 
  let lvl = min (l / 10) 2 in 
  let a1,a2 = a1.(lvl) , a2.(lvl) in 
  let i1 = Random.int (Array.length a1) in
  let i2 = Random.int (Array.length a2) in
  (convert a1.(i1) "ITM"),(convert a2.(i2) "ITM")

and mutate_itm new_r r i =
  (* fix up the item *) 
  i.i_gname <- convert_strref i.i_gname ; 
  i.i_iname <- convert_strref i.i_iname ; 
  if (i.i_idesc = -1) then i.i_idesc <- i.i_gdesc ; 
  i.i_gdesc <- convert_strref i.i_gdesc ; 

  i.i_icon <- convert i.i_icon "BAM" ; 
  i.i_gicon <- convert i.i_gicon "BAM" ; 
  i.i_cicon <- convert i.i_cicon "BAM" ; 

  let is_scim = ref false in
  let is_katana = ref false in 

  let cat = (try (ids_of_int (config.target) "TARGCAT" (Int32.of_int i.i_cat)).i_name with e ->
    (log_and_print "ERROR: ITM: %s.ITM has cat %d\n" r i.i_cat ; exit 1)) 
  in

  (* fixup CAT if necessary *)
  let cat = (match cat, i.i_inv, i.i_prof with
  | "Hand-to-hand_weapon",_,_  -> "Short_sword" 
  | "Misc",_,_ -> 
      (if Array.fold_left (fun acc elt -> acc || elt.a_type = 1 ||
      elt.a_type == 2 || elt.a_type == 4) 
          false i.i_abil 
      then "Short_sword" else "Misc" )
  | "Shield","D1",_   
  | "Shield","D2",_ -> (i.i_inv <- "D2") ; "Buckler"
  | "Shield","D3",_ -> "Medium_Shield"
  | "Shield","D4",_ -> "Large_Shield"
  | _,_,89 -> "Bastard_Sword" 
  | _,_,90 -> "Long_Sword" 
  | _,_,95 -> (is_scim := true) ; i.i_inv <- "S1" ;
      "Long_Sword" ;
  | _,_,94 -> (is_katana := true) ; "Bastard_Sword" 
  | _,_,93 -> "Great_Sword" 
  | _,_,115 -> "Club" 
  | "Flail",_,100 -> "Flail" 
  | "Morning_star",_,100 -> "Morning_star"
  | _,_,101 -> "Mace"
  | "Bow",_,104 -> "Bow" (* long *) 
  | "Bow",_,105 -> "Bow" (* short *) 
  | "Armor","2A",_-> "Leather" 
  | "Armor","2W",_
  | "Armor","3W",_
  | "Armor","4W",_ -> "Robe" 
  | "Armor","3A",_ -> "Chainmail"
  | "Armor","4A",_ -> "Full_plate"
  | _,_,100 
  | "Shield",_,_ -> (log_and_print "ERROR: ITM: %s.ITM cat\n" r ; exit 1)
  | _ -> cat ) in

  let cat = 
    if Hashtbl.mem treat_as_robe_ht (String.uppercase r) then 
      (i.i_inv <- "4W" ; "Robe")
    else if Hashtbl.mem treat_as_elvenchain_ht (String.uppercase r) then 
      (i.i_inv <- "3A" ; "Robe")
    else cat 
  in 

  if config.target_variant = IWD2 then 
    i.i_cat <- Int32.to_int (ids_of_sym (config.target) "TARGCAT" cat).i_num ; 

  let d1,d2 = match cat with 
  | "Mace" -> 1,8
	(* | "Short_sword" -> 1,6 *)
  | "Long_Sword" -> if (!is_scim) then 1,6 else 1,8
  | "Hammer" -> 1,8
  | "Morning_star" -> 1,8
  | "Flail" -> 1,8
  | "Dart" -> 1,4
  | "Axe" -> 1,8
  | "Quarterstaff" -> 1,6
  | "Spear" -> 1,10 (* CHANGED *)
  | "Halberd" -> 1,10
  | "Great_Sword" -> 2,6
  | "Scimitar" -> 1,6
  | "Bastard_Sword" -> 1,10
  | _ -> 0,0
  in 

  (* katanas are masterword bastard swords *) 
  let masterwork = if !is_katana then 1 else 0 in 

  (* drop all weapon enchantments by 1 *) 
  (* 
     let masterwork = if i.i_enchant = 1 then 1 else 0 in
     i.i_enchant <- max (i.i_enchant - 1) 0 ; 

     (if (i.i_enchant = 0) && i.i_flags land 0x40 = 0x40 then 
     i.i_flags <- i.i_flags - 0x40 ) ; 
   *) 

  (* scale the lore to identify: 100 -> 40  *) 
  if config.target_variant = IWD2 then begin 
    i.i_lore <- if i.i_stack > 1 then 0 else ((i.i_lore * 40) / 100) ; 
    i.i_stack <- if i.i_stack > 1 then 9999 else i.i_stack ; 
  end ; 

  i.i_abil <- Array.of_list 
      (List.filter (fun a -> a.a_type >= 1 && a.a_type <= 4) 
	 (Array.to_list i.i_abil) ) ;

  let ce = {
    ce_is_shield = match cat with
    | "Buckler"
    | "Medium_Shield"
    | "Small_Shield"
    | "Large_Shield" -> true
    | _ -> false 
  } in 

  let move_eff_to_abil eff = (eff.e_opcode = 402) (* add_effects_list *) 
  in 

  let equipped = Array.to_list (convert_eff_array new_r r i.i_equipped ce) in
  let append_to_abil, remain_equipped =
    List.partition move_eff_to_abil equipped in 
  let append_to_abil = Array.of_list append_to_abil in
  Array.iter (fun eff -> eff.e_target <- 2) append_to_abil ; 

  let remain_equipped = List.filter (fun e ->
    let iwd2_ids = (Bcs.ids_of_int (config.target) "TARGOP" 
		      (Int32.of_int e.e_opcode)) in
    match iwd2_ids.i_name with
    | "Hide_in_shadow_bonus"
    | "Lockpicking_bonus"
    | "Stealth_bonus"
    | "Find_traps_bonus"
    | "Pick_pockets_bonus"
      when e.e_arg2 = 0 && e.e_arg1 < 0 -> false
    | _ -> true 
				    ) remain_equipped in 

  i.i_equipped <- Array.of_list (remain_equipped) ;

  Array.iter (fun a ->
    a.a_icon <- convert a.a_icon "BAM" ; 

    a.a_eff <- convert_eff_array new_r r a.a_eff ce ;

    if (a.a_type = 3) (* magical*) then begin
      (if cat = "Scroll" || cat = "Potion" || cat = "Misc" && 
      a.a_charges = 0 then begin
        a.a_charges <- 1 
      end) ;
      (if a.a_proj = 0 then a.a_proj <- 1
      else a.a_proj <- convert_projectl a.a_proj) ; 
    end else begin 
      (if (a.a_type = 1) then 
        Hashtbl.add already_converted (new_r,"is_melee_weapon") "yes" ) ;
      if (Array.length append_to_abil > 0 ) then begin 
        a.a_eff <- Array.append a.a_eff append_to_abil ; 
        a.a_eff <- Array.of_list (
          strip_duplicates_eff_list (Array.to_list a.a_eff)) ;
      end ; 
      if (a.a_type = 1 || a.a_type = 2) && d1 <> 0 then begin
        a.a_dicesize <- d2 ; a.a_numdice <- d1 ; 
      end ; 
      if (a.a_type = 2 && (cat = "Bullet")) then begin 
        a.a_allowstr <- 0 ;
      end ;
      if (a.a_type = 1) || (cat = "Dart") || (cat = "Dagger") 
    || (cat = "Axe") then begin 
        a.a_allowstr <- 1 ;
    end ; 
      if (!is_scim) then begin
        a.a_attacktype <- 2 ;  (* keen *) 
      end ; 
      if (a.a_type <> 4) then begin 
        a.a_dmgbonus <- max a.a_dmgbonus i.i_enchant ;
        a.a_tohit <- i.i_enchant + masterwork;
      end ;
    end ;

    (if a.a_dicesize > 200 then a.a_dicesize <- 0);
    (if a.a_numdice > 200 then a.a_numdice <- 0);
	     ) i.i_abil ; 

  let idx = i.i_idesc in 
  if (idx <= 0) || idx > (Array.length (config.source).Load.dialog) ||
  config.target_variant <> IWD2 then 
    ()
  else begin 
    let bg2_tlk = (config.source).Load.dialog.(idx) in 
    let new_text = 
      List.fold_left (fun acc (reg,rep) -> Str.global_replace reg rep acc)
	bg2_tlk.Tlk.text charname_regexp in 
    let first = 
      try List.hd (Str.split stat_regexp (new_text ^ "\n\nSTATISTICS:")) 
      with _ -> new_text 
    in
    let second = describe_itm r i in
    i.i_idesc <- debug_string (first^second) 
  end ;

  match config.source_variant, config.target_variant with
  | BG2, IWD2 -> begin 
      (* look for BG2 class requirements as shown by 0s in the 
       * unusability bitmask *)
      let bg2_use_list = [
	(CC_Wiz, 18) ;
	(CC_Sorc, 18) ;
	(CC_Wiz, 19) ; (* 19 is actually 'mage/thief' *) 
	(CC_Cler, 7) ;
	(CC_Drui, 30) ; 
	(CC_Bard, 6) ;
	(CC_Pala, 20) ;
	(CC_Rang, 21) ;
	(CC_Rogu, 22) ;
	(CC_Monk, 29) ;
	(CC_Figh, 11) ;
	(CC_Barb, 11) ;
      ] in 
      let bit_check i32 bit =
	let mask = Int32.shift_left Int32.one bit in 
	Int32.logand i32 mask = mask 
      in 
      let bg2_UNusability = List.map (fun (c,b) -> (c,bit_check i.i_u1 b)) 
	  bg2_use_list in
      let bg2_fighter_ok = not (bit_check i.i_u1 11) in 
      let bg2_ranger_ok = not (bit_check i.i_u1 21) in 

      if bg2_fighter_ok || bg2_ranger_ok then begin 
	(* do nothing, anyone can use it! *)
	i.i_u1 <- Int32.zero;
	i.i_u2 <- 0;
	i.i_u3 <- 0;
	i.i_u4 <- 0;
	i.i_u5 <- 0;
      end else begin
	i.i_u1 <- Int32.zero ; 
	i.i_u2 <- 0; 
	i.i_u3 <- 0xff;
	i.i_u4 <- 0xff;
	i.i_u5 <- 0xff;
	begin
	  match List.find_all (fun (c,b) -> not b) bg2_UNusability with
	  | [(CC_Bard,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Bards\n" r ;
              i.i_u1 <- Int32.of_int 0x7fd 
	  | [(CC_Monk,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Monks\n" r ;
              i.i_u1 <- Int32.of_int 0x7df ; i.i_u5 <- 0xc7
	  | [(CC_Pala,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Paladins\n" r ;
              i.i_u1 <- Int32.of_int 0x7bf ; i.i_u5 <- 0xf8
	  | [(CC_Rogu,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Rogues\n" r ;
              i.i_u1 <- Int32.of_int 0x6ff 
	  | [(CC_Rang,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Rangers\n" r ;
              i.i_u1 <- Int32.of_int (0x6ff+256-128 )
	  | [(CC_Drui,_)] -> 
              error "USE" "[%8s.ITM] may only be used by Druids\n" r ;
              i.i_u1 <- Int32.of_int 0x7f7
	  | _ when List.exists (fun (c,b) -> not b && c = CC_Rogu) bg2_UNusability -> 
              () (* anyone *) 
	  | _ -> 
              (error "USE" "[%8s.ITM] may only be used like a Wand\n" r ;
               i.i_u1 <- Int32.of_int 0x1f1 ; i.i_u2 <- 0; 
               i.i_u3 <- 0; i.i_u4 <- 0; i.i_u5 <- 0x3f) 
	end 
      end 
  end 
  | _, _ -> () 


(* item usability notes
 *
 * iwd2:
 *
 * 60hfslha - not BBCDFM RRSW
 *            hex bf 07 00 00 || 00 ff ff f8 
 *            1983 = 1024,512,256,128,- ,32,16,8,4,2,1
 *
 * 00amulds - not BBC FMPRRSW
 *            hex f7 07 00 00 || 00 ff ff ff
 *            2039 = 1024,512,256,128,64,32,16,-,4,2,1
 *
 * 00boot09 - yes BBC FMP RSW == cannot be used by dru, rang
 *            hex 88 00 00 00 || 00 00 00 00
 *             136 = ----,---,---,128,--,--,--,8,-,-,-
 *
 * 00bard01 - not B CDFMPRRSW
 *            hex fd 07 00 00 || ff ff ff
 *
 * 00wand01 - yes bard cleric sorc wizard druid
 *            hex f1 01 00 00 || 00 00 00 3f
 *             497 = ----,---,256,128,64,32,16,-,-,-,1
 *
 * zzm6uc   - yes BBCDFMPPRSW  NE
 *            hex 00 40 00 00 || 00 00 00
 *            80 = (good | evil) 
 *            40 = (neutral | evil)
 *            20 = (good | neutral)
 *            10 = (lawful | neutral)
 *
 * EVIL 1
 * GOOD 2
 * GEN  3
 * BARD 6
 * CLER 7
 * FIGH 11
 * MAGE 18
 * PAL 20
 * RAN 21
 * THF 22
 * MONK 29
 * DRUID 30
 * BG2: carso - not bard, cler, fig, mag, !pal, ran, thi, mnk, dru
 *              11000000 11111111 01101111 01100000
 *            - with not evil, becomes
 *              11000001 11111111 01101111 01100000
 *)

	(* i.i_idesc <- convert_strref i.i_idesc ;  *)

(***********************************************************************
 * SPL Files
 *            --> convert an SPL from BG2 
 ***********************************************************************)
and convert_spl new_r r = 
  let buff = load_source_res r "SPL" in 

  if String.sub buff 0 8 <> "SPL V1  " then begin
    failwith "not a valid SPL V1 file (wrong sig)"
  end ; 

  let i = try read_spl buff r with e -> 
    error "SPL" "[%s.SPL] error reading BG2 SPL\n" r ; raise e in

  Hashtbl.add lvl_of_spl_ht new_r (i.s_lev) ; 

  (try 
    mutate_spl new_r r i 
  with
    e -> error "SPL" "[%s.SPL] error mutating SPL: %s\n" r 
	(Printexc.to_string e); raise e);

  let buff = try serialize_spl i r
  with 
    e -> error "SPL" "[%s.SPL] error serializing SPL: %s\n" r 
	(Printexc.to_string e); raise e;
  in
  save_in_override new_r "SPL" buff 

and mutate_spl new_r r s =
  s.s_loc <- 0 ; (* IWD2 requires *) 
  s.s_gname <- convert_strref s.s_gname ; 
  s.s_iname <- convert_strref s.s_iname ; 
  s.s_sound <- convert s.s_sound "WAV" ; 
  s.s_icon <- convert s.s_icon "BAM" ; 
  let ce = {
    ce_is_shield = false ;
  } in 
  s.s_global <- convert_eff_array new_r r s.s_global ce ; 
  Array.iter (fun a ->
    a.a_proj <- convert_projectl a.a_proj ;
    a.a_icon <- convert a.a_icon "BAM" ; 
    a.a_eff <- convert_eff_array new_r r a.a_eff ce ; 
	     ) s.s_abil

(***********************************************************************
 * ITM Files
 *            --> convert an ITM from BG2 
 ***********************************************************************)
and convert_itm new_r r = 
  let buff = load_source_res r "ITM" in 

  if String.sub buff 0 8 <> "ITM V1  " then begin
    failwith "not a valid ITM V1 file (wrong sig)"
  end ; 

  let i = try read_itm buff with e -> 
    error "ITM" "[%s.ITM] error reading BG2 ITM: %s\n" r 
      (Printexc.to_string e); raise e in

  let can_drop = i.i_flags land 0x4 = 0x4 in 
  Hashtbl.add already_converted (new_r,"is_droppable")
    (if can_drop then "yes" else "no" ) ; 

  (try 
    mutate_itm new_r r i 
  with
    e -> error "ITM" "[%s.ITM] error mutating ITM: %s\n" r 
	(Printexc.to_string e); raise e);

  let buff = serialize_itm i in
  save_in_override new_r "ITM" buff 


(***********************************************************************
 * Projectiles
 *            --> used in ITM and SPL abilities
 ***********************************************************************)
and convert_projectl i = 
  let value_in_bg2_ids_file = (i - 1) in
  try 
    let value_in_iwd2_ids_file = 
      convert_ids_check_int value_in_bg2_ids_file "PROJECTL" "PROJECTL" in
    (value_in_iwd2_ids_file + 1)
  with
    Not_found -> begin
      let sb = try (ids_of_int config.source "PROJECTL" (Int32.of_int value_in_bg2_ids_file)).i_name with _ -> Printf.sprintf "%d" value_in_bg2_ids_file in
      let si = try (ids_of_int config.target "PROJECTL" (Int32.of_int value_in_bg2_ids_file)).i_name with _ -> Printf.sprintf "%d" value_in_bg2_ids_file in
      error "PROJECTL" "~%s.projectl~ [ ~%s.projectl~ ]\n" sb si ;
      i 
    end 

(***********************************************************************
 * Portrait Icons
 ***********************************************************************)
and convert_icon i =
  try 
    convert_ids_check_int i "SRCICON" "TARGICON" 
  with
    Not_found -> begin
      let sb = try (ids_of_int config.source "SRCICON" (Int32.of_int i)).i_name with _ -> Printf.sprintf "%d" i in
      let si = try (ids_of_int config.target "TARGICON" (Int32.of_int i)).i_name with _ -> Printf.sprintf "%d" i in
      error "ICON" "~%s.srcicon~ [ ~%s.targicon~ ]\n" sb si ;
      i 
    end 

(***********************************************************************
 * CRE Files
 *            --> interpret a CRE from BG2 
 ***********************************************************************)
and read_bg2_cre new_r r buff =  
  let variable = get_string_of_size buff 0x280 16 in 
  let variable = String.lowercase variable in 
  associate_variable_with_original_cre variable r ;
  let num_item = int_of_str_off buff 0x2c0 in
  let item_off = int_of_str_off buff 0x2bc in 
  let item_slot_offset = int_of_str_off buff 0x2b8 in 
  let item_aol = Array.init 36 (fun n ->
    let off = item_slot_offset + (2 * n) in
    let idx = short_of_str_off buff off in
    if idx >= 0 && idx <= 40 then begin
      let off = item_off + (idx * 20) in 
      try 
	[{ ci_name = convert_resref buff off "ITM" ;
           ci_q1 = short_of_str_off buff (off + 10) ;
           ci_q2 = short_of_str_off buff (off + 12) ;
           ci_q3 = short_of_str_off buff (off + 14) ;
           ci_flags = int_of_str_off buff (off + 16) ;
           ci_slot = convert_ids_int n "SRCSLOT" "TARGSLOT" ; 
	 }]
      with _ -> []
    end else []
			       ) in
  let item_list = List.flatten (Array.to_list item_aol) in 
  let mem_spell_off = int_of_str_off buff 0x2b0 in
  let num_mem_spell = int_of_str_off buff 0x2b4 in
  let mem_spell = Array.init num_mem_spell (fun n ->
    let off = mem_spell_off + (n * 12) in
    let spl = try convert_resref buff off "SPL" with _ -> "None" in
    (1,spl) 
					   ) in 
  let orig_anim = short_of_str_off buff 0x28 in 
  let orig_class = byte_of_str_off buff 0x273 in 
  let my_c_class = convert_ids_int (byte_of_str_off buff 0x273) "CLASS" "CLASS" in 
  { 
    c_name = 
    begin
      patch_strref buff 0x8 ; 
      int_of_str_off buff 0x8
    end ;
    c_xpv = short_of_str_off buff 0x14 ;
    c_status = int_of_str_off buff 0x20 ; 
    c_chp = short_of_str_off buff 0x24 ;
    c_mhp = short_of_str_off buff 0x26 ;
    c_anim = convert_ids_int (short_of_str_off buff 0x28) "ANIMATE" "ANIMATE";
    c_color = String.sub buff 0x2c 7 ;
    c_sport = (try convert_resref buff 0x34 "BMP" with _ -> "") ;
    c_lport = begin
      let lres = get_string_of_size buff 0x3c 8 in
      (if String.length lres > 0 && (String.uppercase lres) <> "NONE" then 
	lres.[(String.length lres) -1]<-'L') ;
      convert lres "BMP"
    end ;
    c_ac = 
    (let amt = short_of_str_off buff 0x46 in 
    if amt > 32767 then 
      amt - 65536 
    else 
      amt)
      ;
    c_s_f= 0;
    c_s_r= 0;
    c_s_w= 0;
    c_res = (Array.init 11 (fun i ->
      signed_byte_of (byte_of_str_off buff (0x59 + i)))) ;
    c_mdam_res = 0; (* TODO: magical damage resistance *) 
    c_class = my_c_class ;
    c_orig_level= Array.init 3 (fun i -> byte_of_str_off buff (0x234+i)) ;
    c_lev=(let a = byte_of_str_off buff 0x234 in 
    let b = byte_of_str_off buff 0x235 in
    let c = byte_of_str_off buff 0x236 in
    let sub_total = match number_of_classes_in_bg2_class orig_class with
    | 2 -> a+b
    | 3 -> a+b+c
    | _ -> a
    in 
    let total = 
      if sub_total < 1 then 1
      else if sub_total > 29 then 30
      else sub_total
    in 
    let total = if orig_anim = 0x7f0d ||
    orig_anim = 0x7f0e ||
    orig_anim = 0xe050 then max 20 total else total in 
    [CC_Figh, total]);
    c_sound = Array.init 100 (fun i -> let off = 0xa4 + (i*4) in 
    patch_strref buff off ; int_of_str_off buff off) ;
    c_skill = [] ;
    c_feat  = [] ;
    c_featweapon = 1; 
    c_subrace = 0 ;  (* TODO: read subrace *) 
    c_attr = (Array.init 6 (fun i -> 
      let off = if i = 0 then 0x238 else 0x239 + i in 
      byte_of_str_off buff off)) ;  
    c_orig_kit = int_of_str_off buff 0x244 ; 
    c_script = [ 
    convert_resref buff 0x268 "BCS" ;
    convert_resref buff 0x260 "BCS" ;
    convert_resref buff 0x258 "BCS" ;
    convert_resref buff 0x250 "BCS" ;
    convert_resref buff 0x248 "BCS" ;
  ] ;
    c_ea = convert_ids_int (byte_of_str_off buff 0x270) "EA" "EA" ; 
    c_general = convert_ids_int (byte_of_str_off buff 0x271) "GENERAL" "GENERAL" ; 
    c_race = convert_ids_int (byte_of_str_off buff 0x272) "RACE" "RACE" ; 
    c_orig_class = (byte_of_str_off buff 0x273) ; 
    c_specific = convert_ids_int (byte_of_str_off buff 0x274) 
      "SPECIFIC" "SPECIFIC" ; 
    c_gender = convert_ids_int (byte_of_str_off buff 0x275) "GENDER" "GENDER" ; 
    c_align = convert_ids_int (byte_of_str_off buff 0x27b) "ALIGN" "ALIGNMNT" ; 
    c_variable = variable ; 
    c_items = item_list ; 
    c_dialog = convert_resref buff 0x2cc "DLG" ; 
    c_spells = mem_spell ; 
  } 

(***********************************************************************
 * CRE Files
 *            --> mutate a CRE to use the 3e rules
 ***********************************************************************)
and mutate_cre new_r r c isnpc =

  let _,total_levels = List.hd c.c_lev in 
  let tl = ref total_levels in 

  let race = try (Bcs.ids_of_int (config.target) "RACE" 
		    (Int32.of_int c.c_race)).Ids.i_name with Not_found -> "HUMAN" in
  (match race with
  |"TROLL" ->    tl := !tl + 6; 
  |"MINDFLAYER" ->    tl := !tl + 10; c.c_orig_class <- 1 ;
  |"BEHOLDER" -> tl := !tl + 8;  c.c_orig_class <- 1 (* MAGE *) 
  | _ -> () 
  ) ; 
  let total_levels = !tl in 

  let all = total_levels in 
  let half = if (total_levels / 2) * 2 <> total_levels then
    (total_levels / 2) + 1 else total_levels / 2 in
  let third = if (total_levels / 3) * 3 <> total_levels then
    (total_levels / 3) + 1 else total_levels / 3 in 
  let lvl = 
    if !option_cre_fighter then c.c_lev
    else 
      match (try (Bcs.ids_of_int (config.source) "CLASS" 
                    (Int32.of_int c.c_orig_class)).Ids.i_name 
      with Not_found -> "FIGHTER" ) with
      | "CLERIC" -> [ (CC_Cler, all) ] 
      | "THIEF" -> [ (CC_Rogu, all) ] 
      | "BARD" -> [ (CC_Bard, all) ] 
      | "MAGE" -> [ (CC_Wiz, all) ] 
      | "PALADIN" -> [ (CC_Pala, all) ]
      | "FIGHTER_MAGE" -> [ (CC_Figh, half) ; (CC_Wiz, half) ] 
      | "FIGHTER_CLERIC" -> [ (CC_Figh, half) ; (CC_Cler, half) ]
      | "FIGHTER_THIEF" -> [ (CC_Figh, half) ; (CC_Rogu, half) ]
      | "FIGHTER_MAGE_THIEF" -> [ (CC_Figh, third) ; 
				  (CC_Wiz, third) ; (CC_Rogu, third) ] 
      | "DRUID" -> [ (CC_Drui, all) ] 
      | "RANGER" -> [ (CC_Rang, all) ] 
      | "MAGE_THIEF" -> [ (CC_Wiz, half) ; (CC_Rogu, half) ] 
      | "CLERIC_MAGE" -> [ (CC_Cler, half) ; (CC_Wiz, half) ] 
      | "CLERIC_THIEF" -> [ (CC_Cler,half) ; (CC_Rogu, half) ] 
      | "FIGHTER_DRUID" -> [ (CC_Figh, half) ; (CC_Drui, half) ] 
      | "FIGHTER_MAGE_CLERIC" -> [ (CC_Figh, third) ; (CC_Wiz,third); (CC_Cler,
								       third) ] 
      | "CLERIC_RANGER" -> [ (CC_Cler, half) ; (CC_Rang, half) ] 
      | "SORCERER" -> [ (CC_Sorc, all) ] 
      | "MONK" -> [ (CC_Monk, all) ] 
      | _ -> [ (CC_Figh, all) ] 
  in 
  let kit = (try (Bcs.ids_of_int (config.source) "KIT" 
		    (Int32.of_int c.c_orig_kit)).Ids.i_name with Not_found -> "TRUECLASS" ) in

  let lvl = 
    if kit = "BARBARIAN" then List.map (fun (c,i) -> if c = CC_Figh then
      (CC_Barb,i) else (c,i)) lvl else lvl 
  in

  let a_str i = c.c_attr.(0) <- c.c_attr.(0) + i in
  let a_int i = c.c_attr.(1) <- c.c_attr.(1) + i in
  let a_wis i = c.c_attr.(2) <- c.c_attr.(2) + i in
  let a_dex i = c.c_attr.(3) <- c.c_attr.(3) + i in
  let a_con i = c.c_attr.(4) <- c.c_attr.(4) + i in
  let a_chr i = c.c_attr.(5) <- c.c_attr.(5) + i in

  let has_class c = List.exists (fun (a,b) -> a = c) lvl 
  in 

  let rec adjusty lst = match lst with
  | (CC_Figh, l) :: tl when l > 7 -> 
      a_str (-4); a_con (-4) ; a_dex (-8) ; 
      (CC_Figh, 4) :: (CC_Rogu, 3) :: (CC_Barb,l - 7) :: (adjusty tl)
  | (CC_Figh, l) :: tl when l = 7 -> 
      a_dex (-8) ; (CC_Figh, 4) :: (CC_Rogu, 3) :: (adjusty tl)
  | (CC_Figh, l) :: tl when l = 6 -> 
      a_dex (-8) ; (CC_Figh, 4) :: (CC_Rogu, 2) :: (adjusty tl)
  | (CC_Figh, l) :: tl when l = 5 -> 
      a_dex (-8) ; (CC_Figh, 4) :: (CC_Rogu, 1) :: (adjusty tl)
  | hd :: tl -> hd :: (adjusty tl) 
  | [] -> []
  in 

  c.c_lev <- adjusty lvl ; 

  if !option_cre_in_name && (not isnpc) then begin
    let idx = c.c_name in
    let class_str = lev_to_short_str c.c_lev in 
    let res = 
      if (idx <= 0) || idx > (Array.length (config.target).Load.dialog) then 
	debug_string ("(" ^ (String.lowercase r) ^ " " ^ class_str ^ ")")
      else
	let old_text = (config.target).Load.dialog.(idx).Tlk.text in
	debug_string (old_text ^ " (" ^ (String.lowercase r) ^ " " ^
		      class_str ^ ")")
    in
    c.c_name <- res 
  end ;


  let bab,hp,spec,feat,conc,hide,fs,rs,ws = 
    List.fold_left (fun (bab',hp',spec',feat',conc',hide',f',r',w') (c,l) -> 
      let bab,hp,spec,feat,conc,hide,f,r,w = match c with
      | CC_Barb  -> a_str 4 ; a_con 4 ;
	  l,(12*l),false,0,(l/2),(l/2),((l/2)+2),(l/3),(l/3)
      | CC_Bard  -> a_chr 8 ; 
	  (l*6/8),(6*l),false,0,(l+3),(l/2),((l/2)+2),((l/2)+2),(l/3)
      | CC_Cler  -> a_wis 8 ;
	  (l*6/8),(8*l),false,0,(l+3),(l/2),((l/2)+2),(l/3),((l/2)+2)
      | CC_Drui  -> a_wis 8 ; 
	  (l*6/8),(8*l),false,0,(l+3),(l/2),((l/2)+2),(l/3),((l/2)+2)
      | CC_Figh  -> a_str 4 ; a_dex 4 ; a_con 4 ;
	  l,(10*l),(l>4),(l/2),(l/2),(l/2),((l/2)+2),(l/3),(l/3)
      | CC_Monk  -> (l*6/8),(8*l),false,0,(l/2),(l+3),((l/2)+2),((l/2)+2),((l/2)+2)
      | CC_Pala  -> l,(10*l),false,0,(l+3),(l/2),((l/2)+2),(l/3),((l/2)+2)
      | CC_Rang  -> a_dex 4 ; 
	  l,(10*l),false,0,(l+3),(l+3),((l/2)+2),(l/3),((l/2)+2)
      | CC_Rogu  -> a_dex 8; 
	  (l*6/8),(6*l),false,(min 0 ((l-10)/3)),(l/2),(l+3),(l/3),((l/2)+2),(l/3)
      | CC_Sorc  -> a_chr 8 ;
	  (l/2),(4*l),false,0,(l+3),(l/2),(l/3),(l/3),((l/2)+2)
      | CC_Wiz   -> a_int 8 ;
	  (l/2),(4*l),false,(l/5),(l+3),(l/2),(l/3),(l/3),((l/2)+2)
      in 
      (bab+bab'), (hp+hp'),(spec||spec'),(feat+feat'),(max conc conc'),(max
									  hide hide'), (f+f'),(r+r'),(w+w')
		   ) (0,0,false,1+(total_levels/3),0,0,0,0,0) c.c_lev in 

  c.c_skill <- [ (3, conc) ; (6, hide) ; (9, hide) ; (13, conc) ] ; 
  c.c_s_f<- fs;
  c.c_s_r<- rs;
  c.c_s_w<- ws; 
  c.c_chp <- if c.c_chp <= 2 then c.c_chp else hp ;
  c.c_mhp <- if c.c_mhp <= 2 then c.c_mhp else hp ;
  (* log_and_print "HOCC [%s] <- %d\n" new_r c.c_chp ;  *)
  Hashtbl.replace hp_of_converted_cre new_r c.c_chp ;

  for i = 0 to (Array.length c.c_res) - 1 do
    c.c_res.(i) <- eff_scale c.c_res.(i)
  done ; 

  (* TODO: handle AC *)
  if !option_damage_reduction && c.c_ac < 10 then begin
    c.c_res.(7) <- min ( c.c_res.(7) + (10 - c.c_ac) ) 100 ; (* slash *) 
    c.c_res.(8) <- min ( c.c_res.(8) + (10 - c.c_ac) ) 100 ; (* crush *) 
    c.c_res.(9) <- min ( c.c_res.(9) + (10 - c.c_ac) ) 100 ; (* pierce *) 
    c.c_res.(10) <- min ( c.c_res.(10) + (10 - c.c_ac) ) 100 ; (* pierce *) 
    c.c_ac <- 10; 
  end else begin
    c.c_ac <- 10; 
  end ;

  c.c_mdam_res <- eff_scale c.c_mdam_res ; 

  let max_attr = Array.fold_left (fun acc elt -> max acc elt) 0 c.c_attr in
  let min_attr = Array.fold_left (fun acc elt -> min acc elt) max_attr c.c_attr in
  (* log_and_print "min = %d, max = %d\n" min_attr max_attr ; *)

  let orig_attr = Array.copy c.c_attr in
  c.c_attr <- Array.make 6 7 ; 
  let points = ref (18+16) in 
  while !points > 0 do 
    (*
      log_and_print "points = %d [%d %d %d %d %d %d]\n" !points 
      c.c_attr.(0)
      c.c_attr.(1)
      c.c_attr.(2)
      c.c_attr.(3)
      c.c_attr.(4)
      c.c_attr.(5)
      ;  *)
    for i = min_attr to max_attr do
      for j = 0 to 5 do
        if !points > 0 && c.c_attr.(j) < 18 && orig_attr.(j) >= i then begin
          decr points ; c.c_attr.(j) <- c.c_attr.(j) + 1 
        end
      done 
    done 
  done ; 
  let points = ref (total_levels / 3) in 
  while !points > 0 do 
    (* log_and_print "points = %d [%d %d %d %d %d %d]\n" !points 
       c.c_attr.(0)
       c.c_attr.(1)
       c.c_attr.(2)
       c.c_attr.(3)
       c.c_attr.(4)
       c.c_attr.(5)
       ;  *)
    for i = max_attr downto min_attr do
      for j = 0 to 5 do
        if !points > 0 && orig_attr.(j) >= i then begin
          decr points ; c.c_attr.(j) <- c.c_attr.(j) + 1 
        end
      done 
    done 
  done ; 

  let a_str i = c.c_attr.(0) <- c.c_attr.(0) + i in
  let a_int i = c.c_attr.(1) <- c.c_attr.(1) + i in
  let a_wis i = c.c_attr.(2) <- c.c_attr.(2) + i in
  let a_dex i = c.c_attr.(3) <- c.c_attr.(3) + i in
  let a_con i = c.c_attr.(4) <- c.c_attr.(4) + i in
  let a_chr i = c.c_attr.(5) <- c.c_attr.(5) + i in
  let a_ac i  = c.c_ac <- c.c_ac + i in 

  let add_item_slot res slot num = 
    c.c_items <- ({ ci_name = res ; ci_q1 = num ; ci_q2 = 0 ; 
		    ci_q3 = 0; ci_flags = 0; ci_slot = slot }) :: c.c_items 
  in 
  let possible_slots = [ 0 ; 1 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 21 ] in 
  let add_equipped_item i = 
    let rec maybe lst = match lst with
    | hd :: tl ->
        begin
          if List.exists (fun a -> a.ci_slot = hd) c.c_items then 
            maybe tl
          else
            add_item_slot i hd 1
        end
    | [] -> error "CRE" "[%s.CRE] cannot give [%s.ITM]\n" r i 
    in 
    maybe possible_slots 
  in 

  (match c.c_anim with
  | 0x7f32 -> (* SLAYER *) a_con 10 ; a_str 10 ; a_dex 10 
  | _ -> ()
  ) ; 

  (match race with
  |"DWARF"->    a_con 2 ; a_chr (-2) 
  |"ELF"->      a_dex 2 ; a_con (-2)
  |"GNOME"->    a_con 2 ; a_str (-2)
  |"HUMAN"->    ()
  |"HALF_ELF"-> ()
  |"HALFLING"-> a_dex 2 ; a_str (-2)
  |"HALFORC"->  a_str 2 ; a_int (-2) ; a_chr (-2)
  |"DROW"->     a_dex 2 ; a_int 2 ; a_chr 2 ; a_con (-2) 

  |"ORC"
  |"GNOLL" ->   a_str 4 ; a_int (-4) ; a_chr (-4)
  |"OGRE"
  |"BEAR"  
  |"OTYUGH" ->  a_str 8 ; a_int (-6) ; a_chr (-6)
  |"GIANT" ->   a_str 12 ; a_int (-4) ; a_chr (-4)

  |"FAIRY"->    a_dex 4 ; a_con (-4) ; a_str (-4) 

  |"KOBOLD" 
  |"GIBBERLING" 
  |"ETTERCAP" 
  |"DOG" ->  a_str (-4) ; a_dex 2 ; a_ac 1
  |"WOLF" -> a_str (-2); a_dex 2; a_ac 1

  |"MINDFLAYER" 
  |"BEHOLDER" -> a_str (-2); a_int (4);

  |"DRAGON" -> a_str 16 ; a_con 20 ; a_int 5 ; a_chr 5; 
      add_equipped_item "00CIBAR1" ;
      add_equipped_item "00CIDRG1" 
  |"GOLEM" ->  a_str 4 ; a_con 8 ; a_dex (-4); a_int (-4); a_wis (-4);
      a_chr (-4); add_equipped_item "00CIDEAD" 
  |"ELEMENTAL" ->  a_str 4 ; a_con 4 ; a_dex (-4); 
      add_equipped_item "00CIDED1" 
  |"UNDEAD" -> add_equipped_item "00CIDEAD" 
  |"TANARI" -> add_equipped_item "00CIRNG1" ; add_equipped_item "00CIDED1" ;
      a_str 4 ; a_con 4; a_dex (-4); 

  |"SLIME" 
  |"MACE"
  |"PLATE"
  |"SWORD" -> add_equipped_item "00CIBAR1" 


  |"NO_RACE" 
    -> () 

  | _ -> error "RACE" "MUTATE: RACE %s\n" race ) ; 

  (if c.c_xpv >= 5000 && c.c_mhp < 30 then begin
    c.c_mhp <- 30;
    c.c_chp <- 30; (* HACK for mephit portals and whatnot *)
  end ) ; 

  (* FEATS *) 
  let lvl_in c =
    let rec find lst = match lst with
      (a,b) :: tl when a = c -> b
    | hd :: tl -> find tl
    | [] -> 0
    in find lvl 
  in 
  let has_feat f =
    let rec find lst = match lst with
      (a,b) :: tl when a = f -> true
    | hd :: tl -> find tl;
    | [] -> false
    in find c.c_feat
  in 
  let feat_left = ref ( feat + 1 (* bastard sword *) ) in  
  let give_feat f i = 
    let i = if i > !feat_left then !feat_left else i in 
    c.c_feat <- (f,i) :: c.c_feat ;
    feat_left := !feat_left - i 
  in 

  let has_weapon = List.exists (fun ci -> ci.ci_slot = 9) c.c_items in
  let has_bow = List.exists (fun ci -> ci.ci_slot = 11) c.c_items in
  let has_melee_weapon2 = List.exists (fun ci -> ci.ci_slot = 10 &&
    (convert ci.ci_name "is_melee_weapon" = "yes")) c.c_items in

  give_feat (Int32.of_int 18) 1; (* bastard sword *)

  while !feat_left > 0 do
    (* Spirit of Storms, etc. *) 
    if(lvl_in CC_Wiz >= 7 || lvl_in CC_Sorc >= 8 || lvl_in CC_Cler >= 7 ||
    lvl_in CC_Drui >= 7 || lvl_in CC_Bard >= 7 || lvl_in CC_Pala >= 15 ||
    lvl_in CC_Rang >= 15) && (* flame *) 
      not (has_feat (Int32.of_int 65)) then give_feat (Int32.of_int 65) 1
    else
      if(lvl_in CC_Wiz >= 7 || lvl_in CC_Sorc >= 8 || lvl_in CC_Cler >= 7 ||
      lvl_in CC_Drui >= 7 || lvl_in CC_Bard >= 7 || lvl_in CC_Pala >= 15 ||
      lvl_in CC_Rang >= 15) && (* cold *) 
	not (has_feat Int32.zero) then give_feat Int32.zero 1
      else 
	if(lvl_in CC_Wiz >= 7 || lvl_in CC_Sorc >= 8 || lvl_in CC_Cler >= 7 ||
	lvl_in CC_Drui >= 7 || lvl_in CC_Bard >= 7 || lvl_in CC_Pala >= 15 ||
	lvl_in CC_Rang >= 15) &&  (* elec *) 
	  not (has_feat (Int32.of_int 51)) then give_feat (Int32.of_int 51) 1
	else 
	  if(lvl_in CC_Wiz >= 7 || lvl_in CC_Sorc >= 8 || lvl_in CC_Cler >= 7 ||
	  lvl_in CC_Drui >= 7 || lvl_in CC_Bard >= 7 || lvl_in CC_Pala >= 15 ||
	  lvl_in CC_Rang >= 15) && (* acid *) 
	    not (has_feat (Int32.of_int 2)) then give_feat (Int32.of_int 2) 1
	  else

	    (* Spell Focus *) 
	    if(lvl_in CC_Wiz >= 1 || lvl_in CC_Sorc >= 1 || lvl_in CC_Cler >= 1 ||
	    lvl_in CC_Drui >= 1 || lvl_in CC_Bard >= 3) && 
	      not (has_feat (Int32.of_int 60)) then give_feat (Int32.of_int 60) 2
	    else 
	      if(lvl_in CC_Wiz >= 1 || lvl_in CC_Sorc >= 1 || lvl_in CC_Cler >= 1 ||
	      lvl_in CC_Drui >= 1 || lvl_in CC_Bard >= 3) && 
		not (has_feat (Int32.of_int 61)) then give_feat (Int32.of_int 61) 2
	      else
		if(lvl_in CC_Wiz >= 1 || lvl_in CC_Sorc >= 1 || lvl_in CC_Cler >= 1 ||
		lvl_in CC_Drui >= 1 || lvl_in CC_Bard >= 3) && 
		  not (has_feat (Int32.of_int 62)) then give_feat (Int32.of_int 62) 2
		else
		  if(lvl_in CC_Wiz >= 1 || lvl_in CC_Sorc >= 1 || lvl_in CC_Cler >= 1 ||
		  lvl_in CC_Drui >= 1 || lvl_in CC_Bard >= 3) && 
		    not (has_feat (Int32.of_int 63)) then give_feat (Int32.of_int 63) 2
		  else
		    if(lvl_in CC_Wiz >= 1 || lvl_in CC_Sorc >= 1 || lvl_in CC_Cler >= 1 ||
		    lvl_in CC_Drui >= 1 || lvl_in CC_Bard >= 3) && 
		      not (has_feat (Int32.of_int 64)) then give_feat (Int32.of_int 64) 2

		    else
		      (* two-weapon *)
		      if(has_melee_weapon2 && 
			 not (has_feat (Int32.of_int 70))) then give_feat (Int32.of_int 70) 1
		      else
			(* ambi-dexterity *) 
			if(has_melee_weapon2 && c.c_attr.(3) >= 15 && 
			   not (has_feat (Int32.of_int 1))) then give_feat (Int32.of_int 1) 1
			else 

			  (* Imp Crit *) 
			  if bab >= 8 && 
			    not (has_feat (Int32.of_int 30)) then give_feat (Int32.of_int 30) 1
			  else 
			    (* Imp Evade *) 
			    if(lvl_in CC_Rogu >= 10) &&
			      not (has_feat (Int32.of_int 31)) then give_feat (Int32.of_int 31) 1
			    else 
			      (* weapon finesse *) 
			      if c.c_attr.(3) > c.c_attr.(0) && 
				not (has_feat (Int32.of_int 71)) then give_feat (Int32.of_int 71) 1
			      else 
				(* Spec, Focus *) 
				if(lvl_in CC_Figh >= 4) && c.c_featweapon < 3 then begin
				  (c.c_featweapon <- c.c_featweapon + 1; decr feat_left) ;
				end 
				else
				  (* dirty fighting *) 
				  if bab >= 2 && 
				    not (has_feat (Int32.of_int 14)) then give_feat (Int32.of_int 14) 1
				  else
				    (* heretics bane *) 
				    if(((lvl_in CC_Cler >= 6) || (lvl_in CC_Pala >= 6) ||
				    (lvl_in CC_Rang >= 6)) &&
				       not (has_feat (Int32.of_int 28))) then give_feat (Int32.of_int 28) 1
				    else
				      (* heroic inspiration *) 
				      if(((lvl_in CC_Barb >= 1) || (lvl_in CC_Pala >= 1) ||
				      (lvl_in CC_Pala >= 1)) &&
					 not (has_feat (Int32.of_int 29))) then give_feat (Int32.of_int 29) 1
				      else
					(* saving throws *) 
					if not (has_feat (Int32.of_int 34)) then give_feat (Int32.of_int 34) 1 
					else 
					  if not (has_feat (Int32.of_int 35)) then give_feat (Int32.of_int 35) 1 
					  else
					    if not (has_feat (Int32.of_int 26)) then give_feat (Int32.of_int 26) 1 
					    else 
					      (* dodge *) 
					      if(c.c_attr.(3) >= 13 && 
						 not (has_feat (Int32.of_int 16))) then give_feat (Int32.of_int 16) 1
					      else 
						(* toughness *) 
						if(not (has_feat (Int32.of_int 69))) then give_feat (Int32.of_int 69) 5
						else (
						  error "INFO" "CRE: FEATS: %s.CRE had %d feats left\n" r !feat_left ;
						  feat_left := 0
						 ) 
  done ; 
  c.c_feat <- (Int32.of_int 4,20) :: c.c_feat ; (* hack! hack! *)

  (* tackle spells *) 
  let slist = Array.to_list c.c_spells in 
  let max_spell = try max_spell_of c 
  with e -> log_and_print "ERROR: MUTATE: MAX_SPELL: %s: %s\n" r
      (Printexc.to_string e) ; exit 1 
  in
  let slist = List.filter (fun (num,res) -> 
    if String.uppercase res = "NONE" then false else 
    if convert res "listspll" <> "" then begin
      true 
    end else begin
      ((* error "CRE" "MUTATE: MEMORIZE: %8s has spell %8s (not in IWD2 LISTSPLL.2DA)\n" r res ; *) 
      false)
    end 
			  ) slist in 
  let slist = ref slist in 
  Array.iteri (fun i amt ->
    if (amt > 0) then begin 
      error "INFO" "CRE: MUTATE: MEMORIZE: %s needs %d more lvl %d spells based on XP level, adding them\n"
        r amt (i+1) ;
      let choices = random_memorize.(i+1) in 
      if Array.length random_memorize.(i+1) < 1 then 
        ( log_and_print "ERROR: CRE: MUTATE: MEMORIZE: %s needs random lvl %d spells\n" r (i+1) ; exit 1 ) else 
	for j = 0 to amt do
          let choice = choices.(Random.int (Array.length choices)) in 
          slist := (1,choice) :: !slist  
	done 
    end 
	      ) max_spell ; 
  let slist = List.fast_sort (fun (num,res) (num',res') ->
    compare res res') !slist in 
  let rec dup lst = match lst with
    (a,ares) :: (b,bres) :: tl ->
      if (ares = bres) then dup ((a+b,ares) :: tl )
      else (a,ares) :: (dup ((b,bres) :: tl))
  | _ -> lst 
  in 
  c.c_spells <- Array.of_list (dup slist) ;

  if (Array.length c.c_spells > 0) && (not isnpc) then begin
    (* Riklaunim's dgtrol02 cheating *) 
    let dead_troll = c.c_chp = 1 && c.c_mhp > 1 && race = "TROLL" in 
    if not dead_troll && not (List.mem "__MAGIC" c.c_script) then 
      c.c_script <- 
        List.filter (fun s -> s <> "None" && s <> "__MELEE") 
          ("__MELEE" :: "__MAGIC" :: c.c_script)
  end ;

  let open_quiv_slot = 
    if not (List.exists (fun ci -> ci.ci_slot = 17) c.c_items) then 
      17
    else if not (List.exists (fun ci -> ci.ci_slot = 18) c.c_items) then 
      18
    else if not (List.exists (fun ci -> ci.ci_slot = 19) c.c_items) then 
      19
    else 0 
  in 

  let can = cre_can_use_weapons c in 

  List.iter (fun ci ->
    let can_drop_in_general = (convert ci.ci_name "is_droppable" = "yes") in
    let this_one_undroppable = ci.ci_flags land 0x8 = 0x8 in 
    if (this_one_undroppable && can_drop_in_general) then begin
      error "INFO" "CRE: NODROP: %8s.CRE had undroppable %s\n" r ci.ci_name;
      ci.ci_flags <- ci.ci_flags - 8 
    end 
	    ) c.c_items ;  

  if can && (lvl_in CC_Drui >= 5) && (not isnpc) then begin
    error "INFO" "CRE: Moonblade for %8s.CRE\n" r ;
    add_item_slot "MOONBLA" 49 1
  end ; 

  if not has_bow && not isnpc && can then begin
    let b,a = random_bow_arrow total_levels in
    error "INFO" "CRE: UNARMED: %8s.CRE has no bow, adding %s %s\n" r b a ;
    add_item_slot b 11 1 ;
    add_item_slot a 17 (60 + ( (Random.int 6) * 10)) ;
  end ;

  if not has_weapon && not isnpc then begin
    if can then begin
      let res = random_weapon total_levels in
      error "INFO" "CRE: UNARMED: %8s.CRE has no weapon, adding %s\n" r res;
      add_item_slot res 9 1
    end else begin 
      let idx = min ((total_levels / 3)+1) 9 in
      let res = Printf.sprintf "00MFIST%d" idx in
      error "INFO" "CRE: INNATE: %8s.CRE has no weapon, adding %s\n" r res;
      add_item_slot res 9 1
    end 
  end else if can && not isnpc then begin
    (* has weapon, can use weapons *)
    let wep = List.find (fun ci -> ci.ci_slot = 9) c.c_items in 
    let can_drop_in_general = (convert wep.ci_name "is_droppable" = "yes") in
    let this_one_undroppable = (wep.ci_flags land 0x8) = 0x8 in 
    if this_one_undroppable && can_drop_in_general then begin
      error "INFO" "CRE: LOCALS: %8s.CRE had nodrop %8s, making droppable\n" 
        r wep.ci_name;
      wep.ci_flags <- wep.ci_flags - 8 ;
    end else if this_one_undroppable && not can_drop_in_general then begin
      () 
    end else if not can_drop_in_general then begin
      () (*
	   let res = random_weapon total_levels in 
	   error "INFO" "CRE: GLOBAL: %8s.CRE had nodrop %8s, using %s\n" r 
           wep.ci_name res;
	   wep.ci_name <- res ; *)
    end
  end  ;

  if not isnpc then begin 

    (if(lvl_in CC_Wiz >= 17 || lvl_in CC_Sorc >= 17 || lvl_in CC_Bard >= 26) then 
      add_equipped_item "__MAGE17"
    else if(lvl_in CC_Wiz >= 9 || lvl_in CC_Sorc >= 9 || lvl_in CC_Bard >= 13) 
    then 
      add_equipped_item "__MAGE09"
    ); 

    (if(lvl_in CC_Cler >= 17 || lvl_in CC_Drui >= 17) then 
      add_equipped_item "__CLER17"
    else if(lvl_in CC_Cler >= 9 || lvl_in CC_Drui >= 9 ||
    lvl_in CC_Pala >= 22 || lvl_in CC_Rang >= 22) then 
      add_equipped_item "__CLER09"
    );

  end ; 

  if Hashtbl.mem cre_script_tweak_ht r then begin
    let script_tweak = Hashtbl.find cre_script_tweak_ht r in
    c.c_script <- List.map (fun s ->
      if Hashtbl.mem script_tweak s then 
        convert (Hashtbl.find script_tweak s) "BCS" 
      else 
        s
			   ) c.c_script 
  end ; 

  () 

(***********************************************************************
 * CRE Files
 *            --> prepare a CRE in IWD2 format
 * returns a buffer with a valid CRE v2.2
 ***********************************************************************)
and prepare_cre_sound buff c =
  write_int buff (0xac + (( 0)*4)) c.c_sound.( 1) ;  (* morale *) 
  write_int buff (0xac + (( 2)*4)) c.c_sound.( 9) ;  (* battle cry 1 *)
  write_int buff (0xac + (( 3)*4)) c.c_sound.(10) ; 
  write_int buff (0xac + (( 4)*4)) c.c_sound.(11) ; 
  write_int buff (0xac + (( 5)*4)) c.c_sound.(12) ; 
  write_int buff (0xac + (( 6)*4)) c.c_sound.(13) ; (* battle cry 5 *)
  write_int buff (0xac + (( 7)*4)) c.c_sound.( 6) ; (* leader *) 
  write_int buff (0xac + (( 9)*4)) c.c_sound.( 7) ; (* tired *) 
  write_int buff (0xac + ((11)*4)) c.c_sound.( 8) ; (* bored *) 
  write_int buff (0xac + ((13)*4)) c.c_sound.(20) ; (* hurt *) 
  write_int buff (0xac + ((15)*4)) c.c_sound.(26) ; (* select com 1 *) 
  write_int buff (0xac + ((16)*4)) c.c_sound.(27) ; 
  write_int buff (0xac + ((17)*4)) c.c_sound.(28) ; 
  write_int buff (0xac + ((18)*4)) c.c_sound.(29) ; 
  write_int buff (0xac + ((19)*4)) c.c_sound.(30) ; 
  write_int buff (0xac + ((20)*4)) c.c_sound.(31) ; (* select com 6 *) 
  write_int buff (0xac + ((22)*4)) c.c_sound.(32) ; (* select act 1 *) 
  write_int buff (0xac + ((23)*4)) c.c_sound.(33) ; 
  write_int buff (0xac + ((24)*4)) c.c_sound.(34) ; 
  write_int buff (0xac + ((25)*4)) c.c_sound.(35) ; 
  write_int buff (0xac + ((26)*4)) c.c_sound.(36) ; 
  write_int buff (0xac + ((27)*4)) c.c_sound.(37) ; 
  write_int buff (0xac + ((28)*4)) c.c_sound.(38) ; (* select act 7 *) 
  write_int buff (0xac + ((29)*4)) c.c_sound.(63) ; (* select rare 1 *) 
  write_int buff (0xac + ((30)*4)) c.c_sound.(64) ; (* select rare 2 *) 
  write_int buff (0xac + ((33)*4)) c.c_sound.(18) ; (* damage *)
  write_int buff (0xac + ((36)*4)) c.c_sound.(19) ; (* dying *)
  write_int buff (0xac + ((38)*4)) c.c_sound.(53) ; (* react die 1 *)
  write_int buff (0xac + ((39)*4)) c.c_sound.(54) ; (* react die 2 *)
  write_int buff (0xac + ((40)*4)) c.c_sound.(65) ; (* crit hit *)
  write_int buff (0xac + ((63)*4)) c.c_sound.(74) ; (* biography *)
  ()

and prepare_bg2_cre r c = 
  let buff = String.copy !charbase_buff in 

  write_int    buff   0x008 c.c_name ;
  write_short  buff   0x014 c.c_xpv ;
  write_int    buff   0x020 c.c_status ;
  write_short  buff   0x024 c.c_chp ;
  write_short  buff   0x026 c.c_mhp ;
  write_short  buff   0x028 c.c_anim ;
  String.blit c.c_color 0 buff 0x02c 7 ;
  write_resref buff   0x034 c.c_sport ; 
  write_resref buff   0x03c c.c_lport ; 
  write_short  buff   0x046 c.c_ac ;
  write_short  buff   0x048 c.c_ac ;
  (* FIXME: saving throws *) 
  for i = 0 to (Array.length c.c_res) - 1 do
    let value = min c.c_res.(i) 127 in 
    write_byte buff (0x59 + i) value 
  done ; 
  (* FIXME: profs *) 
  for i = 0 to 100 do 
    write_int buff (0xa4 + 4*i) c.c_sound.(i) 
  done ;
  for i = 0 to 2 do 
    write_byte buff (0x234 + i) c.c_orig_level.(i) 
  done ;
  for i = 0 to 5 do
    let off = if i = 0 then 0x238 else 0x239 + i in
    write_byte buff off c.c_attr.(i) 
  done ; 
  let off = ref 0x268 in 
  List.iter (fun s -> write_resref buff !off s ; off := !off - 8)
    c.c_script ;
  write_byte buff     0x270 c.c_ea ;
  write_byte buff     0x271 c.c_general ;
  write_byte buff     0x272 c.c_race ;
  write_byte buff     0x273 c.c_class ;
  write_byte buff     0x274 c.c_specific ;
  write_byte buff     0x275 c.c_gender ;
  write_byte buff     0x27b c.c_align ;
  String.blit (str_to_exact_size c.c_variable 16) 0 buff 0x280 16  ;
  write_resref buff   0x2cc c.c_dialog ; 
(*
  let item_buff_size = 0 in
  let spells_buff_size = 0 in 
  let res = Buffer.create (804 + item_buff_size + spells_buff_size) in 
 *)
  buff


and prepare_iwd2_cre r c = 
  let buff = String.copy !charbase_buff in 
  write_int buff   0x008 c.c_name ;
  (* write_int buff   0x010 (0x800) ; 
   * CRASHES THE GAME WITH RANGERS! *)
  write_int buff   0x010 (0x0) ; 
  write_int buff   0x00c c.c_name ;
  write_int buff      0x014 c.c_xpv ;
  write_int buff   0x020 c.c_status ;
  write_short buff    0x028 c.c_anim ;
  for i = 0 to 6 do
    if c.c_color.[i] > Char.chr 119 then begin
      c.c_color.[i] <- Char.chr (match i with
      | 3 -> !random_skin.(Random.int (Array.length !random_skin))
      | 6 -> !random_hair.(Random.int (Array.length !random_hair))
      | _ -> Random.int 60)
    end
  done ;
  String.blit c.c_color 0 buff 0x02c 7 ;
  write_resref buff   0x034 c.c_sport ; 
  write_resref buff   0x03c c.c_lport ; 
  write_short buff    0x046 c.c_ac ;

  (* sound slot conversion *) 
  prepare_cre_sound buff c ;

  write_byte buff     0x08f 0; (* fighter levels *) 
  let total_lev = ref 0 in
  List.iter (fun (c,i) -> let off = match c with
    CC_Barb -> 0x8b
  | CC_Bard -> 0x8c
  | CC_Cler -> 0x8d
  | CC_Drui -> 0x8e
  | CC_Figh -> 0x8f
  | CC_Monk -> 0x90
  | CC_Pala -> 0x91
  | CC_Rang -> 0x92
  | CC_Rogu -> 0x93
  | CC_Sorc -> 0x94
  | CC_Wiz ->  0x95
  in write_byte buff off i ; total_lev := i + !total_lev  
	    ) c.c_lev; 

  write_byte buff 0x8a !total_lev ;
  write_byte buff 0x25a (convert_challenge_rating !total_lev) ; 
  (* challenge rating *) 

  if !option_cre_no_feats then 
    ()
  else begin
    for i = 0 to 11 do
      write_byte buff (0x1d8 + i) c.c_featweapon ;
    done ;
    write_byte buff (0x1f1) c.c_featweapon ;
    List.iter (fun (f,i) ->
      let f = Int32.to_int f in 
      let off = 0x1c0 + ((f / 32) *4) in
      let bit = f mod 32 in
      let word_with_bit_on = Int32.shift_left Int32.one bit in
      let current_word = int32_of_str_off buff off in
      let new_word = Int32.logor word_with_bit_on current_word in
      write_int32 buff off new_word ;
      match f with
      | 4 -> write_byte buff 0x1e5 i 
      | 8 -> write_byte buff 0x1e6 i 
      | 60 -> write_byte buff 0x1e8 i 
      | 61 -> write_byte buff 0x1e9 i 
      | 62 -> write_byte buff 0x1ea i 
      | 63 -> write_byte buff 0x1eb i 
      | 64 -> write_byte buff 0x1ec i 
      | 69 -> write_byte buff 0x1e4 i ;
          (c.c_chp <- c.c_chp + (i*3) ; c.c_mhp <- c.c_mhp + (i*3))
      | 35 -> c.c_s_r <- c.c_s_r + 2
      | 34 -> c.c_s_w <- c.c_s_w + 2
      | 26 -> c.c_s_f <- c.c_s_f + 2
      | 65 -> c.c_res.(0) <- c.c_res.(0) + 5 (* fire *) 
      | 0  -> c.c_res.(1) <- c.c_res.(1) + 5 (* cold *)
      | 51 -> c.c_res.(2) <- c.c_res.(2) + 5 (* elec *)
      | 2  -> c.c_res.(3) <- c.c_res.(3) + 5 (* acid *)
      | _ -> () 
	      ) c.c_feat ; 
  end ; 
  write_short buff    0x024 c.c_chp ;
  write_short buff    0x026 c.c_mhp ;
  write_byte buff     0x052 c.c_s_f ;
  write_byte buff     0x053 c.c_s_r ;
  write_byte buff     0x054 c.c_s_w ;
  for i = 0 to (Array.length c.c_res) - 1 do
    let value = min c.c_res.(i) 127 in 
    write_byte buff (0x55 + i) value 
  done ; 
  write_byte buff     0x060 c.c_mdam_res ;

  write_short buff 0x3b6 0 ; 
  List.iter (fun (c,_) -> 
    let mask = mask_of_class c in 
    let cur = short_of_str_off buff 0x3b6 in
    write_short buff 0x3b6 (cur lor mask)
	    ) c.c_lev ; 

  (* TODO: save sounds *) 
  List.iter (fun (skill_num,value) ->
    write_byte buff (0x218 + skill_num) value 
	    ) c.c_skill ; 

  write_byte buff 0x50 4;

  write_byte buff     0x263 c.c_subrace ;
  for i = 0 to 5 do 
    write_byte buff     (0x266+i) c.c_attr.(i) ;
  done ; 
(*  write_byte buff     0x270 c.c_orig_kit ; *)
  let off = ref 0x294 in 
  List.iter (fun s -> write_resref buff !off s ; off := !off - 8)
    c.c_script ;
  write_byte buff     0x384 c.c_ea ;
  write_byte buff     0x385 c.c_general ;
  write_byte buff     0x386 c.c_race ;
  write_byte buff     0x387 c.c_class ;
  write_byte buff     0x388 c.c_specific ;
  write_byte buff     0x389 c.c_gender ;
  write_byte buff     0x38f c.c_align ;
  String.blit (str_to_exact_size c.c_variable 16) 0 buff 0x394 16  ;
  write_resref buff   0x626 c.c_dialog ; 

  (* SPLICE IN SPELLS *) 
  (if (!option_cre_no_spells) then c.c_spells <- [||]) ;

  let orig_wiz1_spells_offset = 0x7de in

  let fixup_offset_by off amount =
    let cur_val = int_of_str_off buff off in
    write_int buff off (cur_val + amount) 
  in 

  let num_spells = Array.length c.c_spells in 
  let spells_buff_size = num_spells * 16 in
  let spells_buff = String.make (spells_buff_size) '\000' in 
  Array.iteri (fun i (num,res) ->
    let off = i * 16 in 
    let str_val = convert res "listspll" in
    (if str_val = "" then begin 
      error "CRE" "SERIALIZE: %s is not in LISTSPLL.2DA\n" res ;
      failwith (res ^ " not in listspll.2da") 
    end) ; 
    let int_val = try int_of_string str_val with _ ->
      log_and_print "ERROR: CRE: SERIALIZE: %s convert %s listspll -> %s\n" r res str_val ; exit 1 
    in 
    write_int spells_buff off int_val ; 
    write_int spells_buff (off+4) num ;
    write_int spells_buff (off+8) num ;
	      ) c.c_spells ; 

  for i = 0 to 7 do fixup_offset_by (0x496 + (4*i)) spells_buff_size ; done ; 
  for i = 0 to 8 do fixup_offset_by (0x5b2 + (4*i)) spells_buff_size ; done ;
  fixup_offset_by 0x5fa spells_buff_size ; 
  fixup_offset_by 0x602 spells_buff_size ; 
  fixup_offset_by 0x60a spells_buff_size ; 
  fixup_offset_by 0x612 spells_buff_size ; 
  fixup_offset_by 0x61e spells_buff_size ; 

  write_int buff 0x58e num_spells ; 

  (* SPLICE IN ITEMS *) 
  (if !option_cre_no_items then c.c_items <- [] ) ;  

  let items_offset = 0x8ee + spells_buff_size in 
  write_int  buff     0x616 (items_offset); (* our items start here *) 
  let num_items = List.length c.c_items in 
  write_int buff      0x61a num_items ;
  let item_buff_size = num_items * 20 in 
  let item_buff = String.make (item_buff_size) '\000' in
  let off = ref 0 in 
  let item_index = ref 0 in 
  List.iter (fun i -> 
    write_resref item_buff !off i.ci_name ;
    write_short item_buff (!off + 10) i.ci_q1;
    write_short item_buff (!off + 12) i.ci_q1;
    write_short item_buff (!off + 14) i.ci_q1;
    write_int item_buff (!off + 16) i.ci_flags;
    write_short buff (0x886 + (2 * i.ci_slot)) !item_index ;
    incr item_index ;
    off := !off + 20 ;
	    ) c.c_items ; 

  let res = Buffer.create (2286 + item_buff_size + spells_buff_size) in 

  let buff_before_wiz = Str.string_before buff orig_wiz1_spells_offset in
  let buff_after_wiz = Str.string_after buff orig_wiz1_spells_offset in

  Buffer.add_string res buff_before_wiz ;
  Buffer.add_string res spells_buff ;
  Buffer.add_string res buff_after_wiz ;
  Buffer.add_string res item_buff ;
  res
    (* Buffer.contents res *)

and prepare_iwd2_bg2_cre c buff = 
  write_int buff   0x008 c.c_name ;
  write_int buff   0x00c c.c_name ;
  write_int buff   0x020 c.c_status ;
  for i = 0 to 6 do
    if c.c_color.[i] > Char.chr 119 then 
      c.c_color.[i] <- Char.chr (Random.int 60)
  done ;
  String.blit c.c_color 0 buff 0x02c 7 ;
  write_resref buff   0x034 c.c_sport ; 
  write_resref buff   0x03c c.c_lport ; 

  prepare_cre_sound buff c ;

  let off = ref 0x294 in 
  List.iter (fun s -> write_resref buff !off s ; off := !off - 8)
    c.c_script ;
  write_byte buff     0x384 c.c_ea ;
  write_byte buff     0x385 c.c_general ;
  write_byte buff     0x386 c.c_race ;
  write_byte buff     0x387 c.c_class ;
  write_byte buff     0x388 c.c_specific ;
  write_byte buff     0x389 c.c_gender ;
  write_byte buff     0x38f c.c_align ;
  String.blit (str_to_exact_size c.c_variable 16) 0 buff 0x394 16  ;
  write_resref buff   0x626 c.c_dialog ; 

  (* SPLICE IN ITEMS *) 
  (if !option_cre_no_items then c.c_items <- [] ) ;  

  let orig_buff_size = String.length buff in 

  let items_offset = orig_buff_size in 
  let item_slots_offset = int_of_str_off buff 0x612 in 
  write_int  buff     0x616 (items_offset); (* our items start here *) 
  let num_items = List.length c.c_items in 
  write_int buff      0x61a num_items ;
  let item_buff_size = num_items * 20 in 
  let item_buff = String.make (item_buff_size) '\000' in
  let off = ref 0 in 
  let item_index = ref 0 in 
  List.iter (fun i -> 
    write_resref item_buff !off i.ci_name ;
    write_short item_buff (!off + 10) i.ci_q1;
    write_short item_buff (!off + 12) i.ci_q1;
    write_short item_buff (!off + 14) i.ci_q1;
    write_int item_buff (!off + 16) i.ci_flags;
    write_short buff (item_slots_offset + (2 * i.ci_slot)) !item_index ;
    incr item_index ;
    off := !off + 20 ;
	    ) c.c_items ; 


  let res = Buffer.create (orig_buff_size + item_buff_size) in 
  Buffer.add_string res buff ;
  Buffer.add_string res item_buff ;
  Buffer.contents res

(***********************************************************************
 * CRE Files
 ***********************************************************************)
and convert_cre new_r r = begin
  let buff = load_source_res r "CRE" in 

  if String.sub buff 0 8 <> "CRE V1.0" then begin
    failwith "not a valid CRE 1.0 file (wrong sig)"
  end ; 

  (match config.source_variant, config.target_variant with
  | BG2, IWD2 -> 
      begin 
        let cre = 
          try read_bg2_cre new_r r buff with e -> 
            error "CRE" "[%8s.CRE] error reading Source CRE: %s\n" r 
              (Printexc.to_string e); raise e 
        in

        (try mutate_cre new_r r cre false with e -> 
          error "CRE" "[%8s.CRE] error mutating CRE: %s\n" r 
            (Printexc.to_string e); raise e) ;

        let result_buffer = 
          try prepare_iwd2_cre r cre with e ->
            error "CRE" "[%8s.CRE] error preparing Target CRE: %s\n" r 
              (Printexc.to_string e); raise e 
        in

        save_in_override_generic new_r "CRE" (fun oc ->
          Buffer.output_buffer oc result_buffer)
      end

  | BG1, BG2 -> 
      begin 
        let cre = try read_bg2_cre new_r r buff with e -> 
          error "CRE" "[%8s.CRE] error reading Source CRE: %s\n" r 
            (Printexc.to_string e); raise e in

        let output = try prepare_bg2_cre r cre with e ->
          error "CRE" "[%8s.CRE] error preparing Target CRE: %s\n" r 
            (Printexc.to_string e); raise e in

        save_in_override new_r "CRE" output 
      end

  | _ -> ()
  ) 
end 

(***********************************************************************
 * Banter -- special cutscene to simulate banter
 ***********************************************************************)
(*
  and convert_banter_counter = ref 0 
  and convert_banter a = begin
  if Hashtbl.mem convert_banter_ht a then 
  Hashtbl.find convert_banter_ht a 
  else begin
  let new_r = Printf.sprintf "__ba%d" !convert_banter_counter in
  incr convert_banter_counter ; 
  let bcs = ref [] in 
  let myvar = convert a "is_npc_script" in  
  Hashtbl.iter (fun (new_bcs) (r,new_r,(npc_var:string),x,y) ->
  let trim_npc_var = 
  if String.length npc_var > 8 then String.sub npc_var 0 8 else npc_var 
  in
  try 
  let new_dlg = convert (convert trim_npc_var "interdia") "DLG" in 
  if String.uppercase new_dlg <> "NONE" then begin 
  let bcs_block = 
  ( [ { empty_t with trigger_id = target_trigger "True" } ],
  [ (100,
  [ 
  { empty_a with action_id = target_action "CutSceneId" ;
  a_2 = { empty_op with o_name = npc_var } } ;
  { empty_a with action_id = target_action "SetDialog" ;
  a_8 = new_dlg; } ;
  { empty_a with action_id = target_action "SetGlobal" ;
  a_8 = "GLOBAL_reset" ^ npc_var ; a_4 = Int32.one ; } ;
  { empty_a with action_id = target_action "DisplayString" ; 
  a_2 = myself ; 
  a_4 = Int32.of_int (debug_string 
  (Printf.sprintf "%s: use %s.DLG for banter with %s"
  r new_dlg myvar))} ; 
  ])
  ]
  ) 
  in 
  bcs := bcs_block :: !bcs 
  end 
  with _ -> () 
  ) area_npc_ht ;
  let last = 
  ( [ { empty_t with trigger_id = target_trigger "True" } ],
  [ (100,
  [ 
  { empty_a with action_id = target_action "CutSceneId" ;
  a_2 = { empty_op with o_name = myvar } } ;
  { empty_a with action_id = target_action "SmallWait" ; 
  a_4 = Int32.of_int 4 } ; 
  { empty_a with action_id = target_action "StartDialog" ;
  a_8 = convert (convert myvar "interdia") "DLG" ; a_2 = player1 } 
  ])
  ])
  in
  bcs := !bcs @ [last] ; 

  save_in_override_generic new_r "BCS" (fun oc -> 
  save_bcs (config.target) (Save_BCS_OC(oc)) !bcs) ;

  Hashtbl.add convert_banter_ht a new_r ;

  new_r 
  end
  end
 *)

(***********************************************************************
 * Overrider -- special CREs that simulate dialogue action-overrides
 ***********************************************************************)
and convert_overrider_counter = ref 0 
and convert_overrider a = begin
  if Hashtbl.mem convert_overrider_ht a then 
    Hashtbl.find convert_overrider_ht a 
  else begin
    let new_r = Printf.sprintf "__ao%d" !convert_overrider_counter in
    incr convert_overrider_counter ; 
    let buff = Buffer.create 80 in 
    print_script_text config.target (Save_BCS_Buffer(buff)) 
      (BCS_Print_ActionList([a])) false None ;
    let s1 = debug_string (Buffer.contents buff) in 
    let new_bcs = 
      [ 
        ( [ { empty_t with trigger_id = target_trigger "True" } ],
          [ (100,
             [  
                { empty_a with action_id = target_action "DisplayString" ;
                  a_2 = myself; a_4 = Int32.of_int s1 ; } ; 
                a ; 
                { empty_a with action_id = target_action "Wait" ;
                  a_4 = Int32.of_int 7 ; } ;
                { empty_a with action_id = target_action "DestroySelf" } ]
            )
          ]
         ) ; 
      ] 
    in 
    let new_bcs_name = new_r in 
    save_in_override_generic new_bcs_name "BCS" (fun oc -> 
      save_bcs (config.target) (Save_BCS_OC(oc)) new_bcs) ;

    let bringer_buff = String.copy config.bringer_cre in 

    write_resref bringer_buff 0x274 new_bcs_name ;

    save_in_override new_r "CRE" bringer_buff  ;

    Hashtbl.add convert_overrider_ht a new_r ;

    new_r 
  end 
end

(***********************************************************************
 * Bringers -- special CREs that bring things to them 
 ***********************************************************************)
and convert_bringer_counter = ref 0 
and convert_bringer op pt ctx = begin

  if Hashtbl.mem convert_bringer_ht (op,pt) then
    Hashtbl.find convert_bringer_ht (op,pt)
  else begin

    let new_r = Printf.sprintf "__br%d" !convert_bringer_counter in
    incr convert_bringer_counter ; 

    let x,y = pt in 
    let s1 = debug_string (Printf.sprintf "%s: %s: JumpToPointHack: Come To %ld.%ld" ctx new_r x y) in
    let s2 = debug_string (Printf.sprintf "%s: %s: JumpToPointHack: Done!" ctx new_r) in 

    let new_bcs = 
      [ 
        ( [ { empty_t with trigger_id = target_trigger "Range" ;
              t_5 = op ; t_1 = Int32.one ; t_2 = Int32.of_int 2 ;} ] , 
          [ (100,
             [ 
               { empty_a with action_id = target_action "DisplayString" ;
                 a_2 = myself; a_4 = Int32.of_int s2 ; } ; 
               { empty_a with action_id = target_action "DestroySelf" } ]
            )
          ]
         ) ; 
        ( [ { empty_t with trigger_id = target_trigger "True" } ] ,
          [ (100, 
             [ 
               { empty_a with action_id = target_action "DisplayString" ;
                 a_2 = myself; a_4 = Int32.of_int s1 ; } ; 
               { empty_a with action_id = target_action "JumpToPoint" ;
                 a_1 = op ; a_5 = pt ; } ]
            )
          ]
         ) 
      ] 
    in 
    let new_bcs_name = new_r in 
    save_in_override_generic new_bcs_name "BCS" (fun oc -> 
      save_bcs (config.target) (Save_BCS_OC(oc)) new_bcs) ;

    let bringer_buff = String.copy config.bringer_cre in 

    write_resref bringer_buff 0x274 new_bcs_name ;

    save_in_override new_r "CRE" bringer_buff  ;

    Hashtbl.add convert_bringer_ht (op,pt) new_r ;

    new_r 
  end 

end 

(***********************************************************************
 * Random Spawns Files
 ***********************************************************************)
and convert_random_spawn new_r r = begin

  let monsters = Hashtbl.find random_spawn_ht r in  
  let monsters = List.map (fun r -> match r with
    Converted(r) -> r
  | Unconverted(r) -> convert r "CRE") monsters in 

  let monsters = List.filter (fun r -> r <> "None") monsters in 

  let make_mon_actions = 
    (List.map (fun r -> 
      { empty_a with action_id = target_action "CreateCreatureObject";
        a_8 = r ; a_2 = myself } 
	      ) (List.tl monsters))  @ 
    [ { empty_a with action_id = target_action "ChangeAnimation" ;
        a_8 = List.hd monsters ; } ] 
  in 


  let str = List.fold_left (fun acc elt ->
    acc ^ " " ^ elt) (Printf.sprintf "%s: spawning:" r) monsters in 
  let s2 = debug_string str in 

  let new_bcs = 
    [ 
      ( [ (* { empty_t with trigger_id = target_trigger "Range" ;
             t_1 = Int32.of_int 10 ; t_2 = Int32.of_int 2 ; negated = true ; 
             t_5 = { empty_op with 
             o_ea = Int32.of_int 255 } } *) 
        { empty_t with trigger_id = target_trigger "True" } 
      ] ,
        [ (100, 
           { empty_a with action_id = target_action "DisplayString" ;
             a_2 = myself; a_4 = Int32.of_int s2 ; } ::
           make_mon_actions 
             (* @ [ { empty_a with action_id = target_action "DestroySelf" } ] *) 
          )
        ]
       ) 
    ] 
  in 

  let new_bcs_name = new_r in 
  save_in_override_generic new_bcs_name "BCS" (fun oc -> 
    save_bcs (config.target) (Save_BCS_OC(oc)) new_bcs) ;

  let premade_buff = String.copy config.random_spawn_cre in 

  write_resref premade_buff 0x274 new_bcs_name ;

  save_in_override new_r "CRE" premade_buff  ;

  Hashtbl.add already_converted (r,"CRE") new_r ; 
  associate_variable_with_original_cre r new_r ; 
end 

and convert_random_spawn_list_counter = ref 0 

and convert_random_spawn_list cre_list = begin
  if Hashtbl.mem random_spawn_list_ht cre_list then
    Hashtbl.find random_spawn_list_ht cre_list
  else begin
    let new_r = Printf.sprintf "_RS%d" !convert_random_spawn_list_counter in
    incr convert_random_spawn_list_counter ;
    Hashtbl.add random_spawn_ht new_r 
      ((List.map (fun r -> Converted(r))) cre_list) ; 
    convert new_r "CRE" 
  end 
end 

(***********************************************************************
 * DLG Files
 ***********************************************************************)
and convert_dlg_internal new_r incoming_lst = begin
  let result = { Dlg.name = new_r ;
                 Dlg.state = [| |] ;
                 Dlg.dlg_flags = 0; } in 

  let convert_tlk_string t = match t with
    TLK_Index(i) -> TLK_Index(convert_strref i)
  | _ -> failwith "convert_tlk_string" 
  in 

  List.iter (fun (r,buff,state_trigger_prefix) -> 
    let context = r ^ ".DLG" in 

    (* Convert Actions *) 
    let convert_action_string s fn = if s = "" then "" else begin
      let line_list = Str.split newline_regexp s in
      let converted_line_list = (List.flatten (List.map (fun s -> 
        let s = balance_quotes fn s in 
        let lexbuf : Lexing.lexbuf = lex_init_from_string fn s in
        let al = try Stats.time "parsing .BAF" 
            (fun () -> Bafparser.action_list Baflexer.initial lexbuf) () 
        with e -> 
          error "DLG" "[%s] PARSE ACTION ~%s~ : %s\n" fn s 
            (Printexc.to_string e) ; [] 
        in 
        let new_al = try 
          let args = default_convert_bcs_args in 
          List.flatten (List.map 
			  (fun a -> convert_bcs_action a context args true ) al) 
        with e -> 
          error "DLG" "[%s] CONVERT ACTION ~%s~ : %s\n" fn s 
            (Printexc.to_string e) ; [] 
        in
        let new_al = List.filter (fun a ->
          a.action_id <> Int32.of_int 121 (* start cut scene mode *)
				 ) new_al in 
        new_al 
							) line_list)) in
      (try 
        let buff = Buffer.create 40 in 
        print_script_text config.target (Save_BCS_Buffer(buff))
          (BCS_Print_ActionList(converted_line_list)) false None ;
        Buffer.contents buff 
      with e -> 
        error "DLG" "[%s] PRINT ACTION ~%s~ : %s\n" fn s 
          (Printexc.to_string e) ; "")
    end in 

    (* Convert Triggers *) 
    let convert_trigger_string s fn = if s = "" then "" else begin
      let line_list = Str.split newline_regexp s in
      let converted_line_list = (List.flatten (List.map (fun s -> 
        let s = balance_quotes fn s in 
        let lexbuf : Lexing.lexbuf = lex_init_from_string fn s in
        let al = try Stats.time "parsing .BAF" 
            (fun () -> Bafparser.trigger_list Baflexer.initial lexbuf) () 
        with e -> 
          error "DLG" "[%s] PARSE TRIGGER ~%s~ : %s\n" fn s 
            (Printexc.to_string e) ; [] 
        in 
        let new_al = try 
          List.flatten (List.map (fun a -> convert_bcs_trigger a context) al ) 
        with e -> begin
          error "DLG" "[%s] CONVERT TRIGGER ~%s~ : %s\n" fn s 
            (Printexc.to_string e) ; [] 
        end
        in
        new_al
							) line_list)) in
      (try 
        let buff = Buffer.create 40 in 
        print_script_text config.target (Save_BCS_Buffer(buff))
          (BCS_Print_TriggerList(converted_line_list)) false None ;
        Buffer.contents buff 
      with e -> 
        error "ERROR" "DLG: [%s] TRIGGER ~%s~ : %s\n" fn s 
          (Printexc.to_string e) ; "")
    end in

    let d = Stats.time "DLG load" (Dlg.load_dlg r) buff in 

    (* Convert States *) 
    Array.iteri (fun i s ->
      s.resp_str <- convert_tlk_string s.resp_str ;
      let fn = Printf.sprintf "%s.DLG state %d" r i in 

      let strig = (convert_trigger_string s.state_trigger fn) in

      ( if strig <> "" then begin 
        s.state_trigger <- state_trigger_prefix ^ strig ;
        match s.state_trigger_weight with
        | Not_Specified -> ()
        | Offset(i) -> s.state_trigger_weight <- 
            Offset(i + (Array.length result.state))
      end else 
        s.state_trigger <- "" ) ;

      Array.iter (fun t ->
        (match t.trans_str with
        | Some(ts) -> t.trans_str <- Some(convert_tlk_string ts)
        | None -> () ) ; 
        (match t.journal_str with
        | Some(tau,ts) -> t.journal_str <- Some(tau, convert_tlk_string ts)
        | None -> () ) ;
        (match t.trans_trigger with
        | Some(s) -> t.trans_trigger <- Some(convert_trigger_string s fn)
        | None -> () ) ;
        (match t.action with
        | Some(s) -> t.action <- Some(convert_action_string s fn)
        | None -> () ) ;
        (match t.next with
        | Exit -> ()
        | Absolute(s,i) -> 
            begin
              if Hashtbl.mem merged_dlg_ht s then begin
		(* target DLG file has been merged into some other file *) 
		let new_file, offset = Hashtbl.find merged_dlg_ht s in 
		t.next <- Absolute(new_file,i+offset) 
              end else begin
		t.next <- (try Absolute(convert s "DLG",i) with e -> Exit) ;
              end 
            end 
        | Symbolic _
        | Copy _ -> failwith "symbolic transition in game DLG file") 
		 ) s.trans ; 
      (*
	if !prepend_exit then begin
        let exit_trans = Dlg.make_trans_of_next Dlg.Exit in 
        s.trans <- Array.append [| exit_trans |] s.trans 
	end 
       *)
		) d.state ; 

    result.state <- Array.append result.state d.state ; 

	    ) incoming_lst ;
  (*
    let new_final_state = {
    resp_str = TLK_Index(
    debug_string 
    (Printf.sprintf "... has nothing to say to you.\n(%s.DLG)" new_r)) ;
    state_trigger = "True()" ;
    trans = [| (make_trans_of_next Exit) |] ;
    state_trigger_weight = Not_Specified ;
    symbolic_label = "final_state" ;
    } in 
    d.state <- Array.append d.state [| new_final_state |] ; 
   *)
  let buff = Buffer.create 1024 in
  save_dlg result buff ;
  save_in_override new_r "DLG" (Buffer.contents buff) ;

end 

and convert_dlg new_r r = begin
  let buff = load_source_res r "DLG" in 
  convert_dlg_internal new_r [(r,buff,"")] 
end 

(***********************************************************************
 * BCS Files
 *
 * We must convert all BG2 triggers and actions to IWD2 ones. Where that is
 * not possible we must simulate. In all cases we must continue to patch up
 * strrefs and resrefs. 
 ***********************************************************************)

and convert_bcs_obj o = 
  let rec convert_id_list lst = match lst with
  | hd :: tl -> 
      if hd = Int32.zero then convert_id_list tl 
      else begin
        let i' = convert_ids hd "OBJECT" "OBJECT" in 
        let i'' = Int32.to_int i' in 
        match i'' with
        | 19 -> [Int32.of_int 27]
        | _ -> i' :: (convert_id_list tl)
      end 
  | [] -> []
  in 
  let a,b,c,d,e = match convert_id_list  [ o.o_unknown4 ; o.o_unknown3 ;
					   o.o_unknown2 ; o.o_unknown1 ; o.o_identifiers ; ] with
  | a::b::c::d::e::tl -> (a,b,c,d,e)
  | [a;b;c;d] -> (Int32.zero,a,b,c,d)
  | [a;b;c] -> (Int32.zero,Int32.zero,a,b,c)
  | [a;b] -> (Int32.zero,Int32.zero,Int32.zero,a,b) 
  | [a] -> (Int32.zero,Int32.zero,Int32.zero,Int32.zero,a) 
  | [] -> (Int32.zero,Int32.zero,Int32.zero,Int32.zero,Int32.zero) 
  in 
  { o with
    o_ea = convert_ids o.o_ea "EA" "EA" ;
    o_general = convert_ids o.o_general "GENERAL" "GENERAL" ;
    o_race = convert_ids o.o_race "RACE" "RACE" ;
    o_subrace = convert_ids o.o_subrace "SUBRACE" "SUBRACE" ;
    o_class = convert_ids o.o_class "CLASS" "CLASS" ;
    o_alignment = convert_ids o.o_alignment "ALIGN" "ALIGNMNT" ;
    o_unknown4 = a;
    o_unknown3 = b; 
    o_unknown2 = c; 
    o_unknown1 = d; 
    o_identifiers = e; 
    o_name = convert o.o_name "variable" ;
  } 

(***********************************************************************
 * Global and Local Namespaces
 *
 * Given a string like "GLOBALsomevar" or "LOCALSsomevar" or
 * "AR0602somevar", conver it .
 ***********************************************************************)
and namespace str =
  let orig = if String.length str > 6 then String.sub str 0 6 else str in 
  let orig = String.uppercase orig in 
  match orig with
  | "GLOBAL" -> Str.global_replace sprite_is_dead_regexp "_DEAD" str
  | "LOCALS" -> str
  | _ -> 
      if String.length orig = 6 && orig.[0] = 'A' && orig.[1] = 'R' then begin
        let res = Str.string_after str 6 in 
        (rename orig "ARE") ^ res
      end else str 

(***********************************************************************
 * BCS Files
 *            -> TRIGGERS 
 ***********************************************************************)
and make_false c =
  c.trigger_id <- target_trigger "False" 

and convert_trigger_directly b c context =
  if (List.length b.i_args) > 0 then (match b.i_name with
  | "Global" 
  | "GlobalGT"  
  | "GlobalLT" -> 
      c.t_3 <- namespace c.t_3 ;
      if (String.uppercase c.t_3) = "GLOBALCHAPTER" then 
        c.t_1 <- Int32.pred c.t_1 

  | "GlobalTimerExpired" 
  | "GlobalTimerNotExpired" ->
      c.t_4 <- namespace c.t_4 

  | "PartyHasItem" 
  | "Contains" 
  | "HasItem" -> (try c.t_3 <- convert c.t_3 "ITM" with _ -> 
      make_false c)

  | "HaveSpell" -> 
      c.t_1 <- convert_ids c.t_1 "SPELL" "SPELL"

  | "TimeOfDay" -> 
      c.t_1 <- convert_ids c.t_1 "TIMEODAY" "TIMEODAY"
	  
  | "NumDead" 
  | "NumDeadGT" 
  | "NumDeadLT" -> c.t_3 <- convert c.t_3 "variable" 

  | "General" -> c.t_1 <- convert_ids c.t_1 "GENERAL" "GENERAL" 

  | "Class" -> begin
      let i = convert_ids c.t_1 "CLASS" "CLASS" in
      match Int32.to_int i with
	1 | 2 | 3 | 4 | 6 | 7 | 8 | 9 | 10 | 11 ->
	  c.t_1 <- Int32.zero ; c.t_2 <- i;
	  c.trigger_id <- target_trigger "LevelInClassGT" 
      | 255 -> c.trigger_id <- target_trigger "False" 
      | _ -> c.trigger_id <- target_trigger "True" 
  end

  | "Gender" -> c.t_1 <- convert_ids c.t_1 "GENDER" "GENDER" 
  | "Race" -> c.t_1 <- convert_ids c.t_1 "RACE" "RACE" 
  | "Specific" -> c.t_1 <- convert_ids c.t_1 "SPECIFIC" "SPECIFIC" 

  | "Allegiance" -> 
      c.t_1 <- convert_ids c.t_1 "EA" "EA" ;
      if (c.t_5.o_name <> "") then begin 
        error "TRIGGER" "ALLEGIANCE: %s includes Allegiance(\"%s\")\n" 
          context c.t_5.o_name ;
        c.trigger_id <- target_trigger "False" 
      end 

  | "CheckStatGT"
  | "CheckStatLT"
  | "CheckStat" -> begin
      c.t_2 <- convert_ids c.t_2 "STATS" "STATS" ;
      match Int32.to_int c.t_2 with
      | 2 -> (* asking for AC crashes IWD2 *) 
          c.trigger_id <- target_trigger "True" 

      | 34 | 68 | 69 -> 
          (* asking for level *) 
          begin 
            c.trigger_id <- target_trigger (match b.i_name with
            | "CheckStatGT" -> "LevelGT"
            | "CheckStatLT" -> "LevelLT"
            | "CheckStat" -> "Level"
            | _ -> failwith "CheckStat: impossible") ;
          end 
      | _ -> 
          begin 
            match Int32.to_int c.t_2 with 
            | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 
            | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 -> 
		c.t_1 <- eff_scale_int32 c.t_1 
            | 44 (*XP*) ->
		c.t_1 <- convert_xp_to_level_int32 c.t_1 ;
		c.t_2 <- Int32.of_int 34; (* LEVEL *)

            | 36 | 38 | 39 | 40 | 41 | 42 -> 
		begin
		  c.trigger_id <- target_trigger (match b.i_name with
		  | "CheckStatGT" -> "CheckSkillGT"
		  | "CheckStatLT" -> "CheckSkillLT"
		  | "CheckStat" -> "CheckSkill"
		  | _ -> failwith "CheckStat: impossible") ;
		  c.t_2 <- (match Int32.to_int c.t_2 with
		  | 36 (* STR *) -> Int32.of_int 7 (* intimidate *) 
		  | 38 (* INT *) -> Int32.of_int 8 (* knowledge *) 
		  | 39 (* WIS *) -> Int32.of_int 15 (* wilderness *) 
		  | 40 (* DEX *) -> Int32.of_int 6 (* hide *) 
		  | 41 (* CON *) -> Int32.of_int 3 (* concentration *) 
		  | 42 (* CHR *) -> Int32.of_int 4 (* diplomacy *) 
		  | _ -> failwith "CheckStat: impossible2" 
			   ); 
		  c.t_1 <- Int32.add c.t_1 (Int32.of_int 6) 
		end 
            | _ -> () 
          end 
  end
	

  | "TimeGT" 
  | "TimeLT" 
  | "Time" 
    -> c.t_1 <- convert_ids c.t_1 "TIME" "TIME" 

  | "HPGT" 
    -> c.t_1 <- if c.t_1 < Int32.of_int 10 then Int32.of_int 10 else c.t_1 

      (* object parameter *) 
  | "Clicked" 
  | "Delay" 
  | "Detect" 
  | "Entered" 
  | "Exists" 
  | "FallenPaladin" 
  | "HP" 
  | "HPLT" 
  | "HPPercentGT" 
  | "HPPercentLT" 
  | "HappinessGT" 
  | "HappinessLT" 
  | "HasWeaponEquiped" 
  | "Help" 
  | "HitBy" 
  | "InParty" 
  | "InWeaponRange" 
  | "IsOverMe" 
  | "Level"
  | "LevelGT"
  | "LevelLT" 
  | "NumCreature"
  | "NumCreatureGT"
  | "NumCreatureLT"
  | "NumInParty" 
  | "NumInPartyGT" 
  | "NumInPartyLT" 
  | "NumTimesTalkedToGT" 
  | "NumTimesTalkedToLT" 
  | "OpenState" 
  | "Opened" 
  | "PartyGoldGT" 
  | "PartyGoldLT" 
  | "RandomNum" 
  | "RandomNumGT" 
  | "RandomNumLT" 
  | "ReactionGT" 
  | "ReactionLT" 
  | "Reputation"
  | "ReputationGT"
  | "ReputationLT"
  | "TimerExpired" 
  | "StealFailed" 
  | "Died" 
  | "HPPercent" 
  | "NumberOfTimesTalkedTo" 
  | "PartyGold" 
  | "IsActive" 
  | "Killed" 
  | "Heard"
  | "TargetUnreachable" 
  | "AreaType" 
  | "Detected"
  | "Disarmed" 
    -> () 
	(* t_5.o_name is handled above *) 

  | "NumItemsGT"
  | "NumItemsLT"
  | "NumItems"
  | "NumItemsParty"
  | "NumItemsPartyLT"
  | "NumItemsPartyGT"
    -> c.t_3 <- convert c.t_3 "ITM"

  | "StateCheck"
  | "NotStateCheck"
    -> c.t_1 <- convert_ids c.t_1 "STATE" "STATE" 

  | "HaveSpellRES"
    -> c.t_3 <- convert c.t_3 "SPL"

  | "InPartySlot" 
    -> if c.t_5 = protagonist then begin 
      c.trigger_id <- target_trigger "True" 
    end 

  | u -> 
      error "TRIGGER" "SIDE-EFFECTS %s\n" u ; 
				     ) ; 


and convert_bcs_trigger c context = 
  Stats.time "BCS trigger" (fun () -> 
    let orig_c = { c with trigger_id = c.trigger_id } in 
    let b = 
      try 
	best_ids_of_trigger (config.source) c 
      with e ->
	error "BCS" "[%s.BCS] BEST BG2 TRIGGER %lx: %s\n" context c.trigger_id 
          (Printexc.to_string e); raise e
    in
    c.t_5 <- convert_bcs_obj c.t_5 ; 
    (* try to find a similar trigger in IWD2 *) 
    let exists_in_iwd2 = 
      try Some(Bcs.ids_of_sym (config.target) "TRIGGER" b.Ids.i_name) 
      with Not_found -> None 
    in
    try 
      begin match exists_in_iwd2 with
      | Some(i) -> 
	  if b.i_name = i.i_name && b.i_args = i.i_args then begin
            (* we are in luck! a direct match! *) 
            c.trigger_id <- i.i_num ; 
            convert_trigger_directly b c context ;
            [ c ] 
	  end else begin
            (* the arguments don't quite match *) 
            let almost = { c with trigger_id = i.i_num ; } in 
            convert_trigger_almost b c almost context 
	  end
      | None -> 
	  (* This action does not exist in IWD2, we must simulate *) 
	  convert_trigger_simulate b c context 
      end
    with e -> 
      ( error "TRIGGER" "FAILED: %s: %s\n" 
	  (print_trigger orig_c) (Printexc.to_string e)) ; raise e
			   ) () 

and convert_trigger_almost b c almost context = 
  match b.i_name with 
  | "OR" 
  | "AttackedBy" 
  | "Heard" 
    -> [ almost ]

  | "See" 
    -> [ almost ] (* WEIMER, FIXME *) 

  | "Dead" -> [ { almost with trigger_id = target_trigger "GlobalGT" ;
		  t_3 = Printf.sprintf "GLOBAL_DEAD%s" c.t_3 ; } ] 

  | "Range" -> [ { almost with t_2 = Int32.of_int 2 (* less-than *) } ]

  | "Kit" -> [ { almost with t_1 = convert_ids almost.t_1 "KIT" "KIT" } ] 

  | "Alignment" -> 
      if String.sub (String.uppercase context) 0 2 = "AR" then begin
        let t = convert_ids almost.t_1 "ALIGN" "ALIGNMNT" in 
        let l = match Int32.to_int t with
          0x01 -> (* mask good *) [ 0x11 ; 0x21 ; 0x31 ] 
        | 0x02 -> (* mask gen *)  [ 0x12 ; 0x22 ; 0x32 ]
        | 0x03 -> (* mask evi *)  [ 0x13 ; 0x23 ; 0x33 ]
        | 0x10 -> (* mask law *)  [ 0x11 ; 0x12 ; 0x13 ]
        | 0x20 -> (* mask neu *)  [ 0x21 ; 0x22 ; 0x23 ]
        | 0x30 -> (* maek cha *)  [ 0x31 ; 0x32 ; 0x33 ]
        | other -> [ other ]
        in 
        [ { empty_t with trigger_id = target_trigger "Or" ;
            t_1 = Int32.of_int (List.length l) ; } ; ]
        @
          (List.map (fun t -> { almost with t_1 = Int32.of_int t } ) l)
      end else begin 
        [ { almost with
            t_1 = convert_ids almost.t_1 "ALIGN" "ALIGNMNT" } ] 
      end 
	  (*
	    let b = Buffer.create 51 in 
	    print_script_text (config.target) (Save_BCS_Buffer(b)) 
            (BCS_Print_TriggerList([almost])) false ; 
	    error "ALIGN" "%s %s" context (Buffer.contents b) ;
	   *)

  | "Difficulty"
    -> [ { almost with t_2 = Int32.one (* EQUAL *) } ] 


  | u -> error "TRIGGER" "CLOSE %s\n" u  ; [] 

and convert_trigger_simulate b c context = 
  match b.i_name with 
  | "AreaCheck" -> 
      let str = rename c.t_3 "ARE" in 
      let area_as_int = Int32.of_string (String.sub str 2 4) in 
      [ { c with 
          trigger_id = target_trigger "CurrentAreaIs" ;
          t_1 = area_as_int ; t_5 = myself ; } ] 

  | "CombatCounter" when c.t_1 = Int32.zero ->
      [ { c with trigger_id = target_trigger "AnyPCSeesEnemy" ;
          negated = not c.negated } ] 

  | "CombatCounter"  
  | "CombatCounterLT"  
  | "CombatCounterGT" -> 
      [ { c with trigger_id = target_trigger "AnyPCSeesEnemy" } ] 

  | "ActuallyInCombat" -> 
      [ { c with trigger_id = target_trigger "AnyPCSeesEnemy" } ] 

  | "RealGlobalTimerExpired" -> 
      [ { c with trigger_id = target_trigger "GlobalTimerExpired" ; 
          t_3 = namespace c.t_3 ; } ]

  | "G" -> [ { c with trigger_id = target_trigger "Global" ;
               t_3 = namespace ("GLOBAL" ^ c.t_3) ; } ] 
  | "GGT" -> [ { c with trigger_id = target_trigger "GlobalGT" ;
		 t_3 = namespace ("GLOBAL" ^ c.t_3) ; } ] 
  | "GLT" -> [ { c with trigger_id = target_trigger "GlobalLT" ;
		 t_3 = namespace ("GLOBAL" ^ c.t_3) ; } ] 

  | "TookDamage" -> 
      [ { c with trigger_id = target_trigger "HPPercentLT" ;
          t_5 = myself ; 
          t_1 = Int32.of_int 100 ; } ]

  | "SpellCast"
  | "SpellCastInnate"
  | "SpellCastOnMe"
  | "SpellCastPriest" 
  | "InWatchersKeep" 
  | "CharName" 
  | "InventoryFull"  
    -> [ { c with trigger_id = target_trigger "False" } ] 

  | "Name" 
    -> [ { c with trigger_id = target_trigger "IsScriptName" ; 
           t_3 = convert c.t_3 "variable" ; } ]

  | "PartyRested" 
    -> [ { empty_t with trigger_id = target_trigger "Global" ;
           t_3 = "GLOBAL_RestedParty" ; t_1 = Int32.one } ] 

  | "DifficultyGT" ->
      [ { c with trigger_id = target_trigger "Difficulty" ;
          t_2 = Int32.of_int 3 (* greater-than *) ; } ] 

  | "DifficultyLT" ->
      [ { c with trigger_id = target_trigger "Difficulty" ;
          t_2 = Int32.of_int 2 (* less-than *) ; } ] 

  | "IsGabber" -> [ { c with trigger_id = target_trigger "See" } ] 
  | "InPartyAllowDead" -> [ { c with trigger_id = target_trigger "InParty" } ] 

	(* TODO: *)
  | "TimeGT" -> [ { c with trigger_id = target_trigger "Time" } ] 
  | "TimeLT" -> [ { c with trigger_id = target_trigger "Time" } ] 

  | "HasItemSlot" ->
      [ { c with trigger_id = target_trigger "GlobalLT" ;
          t_3 = Printf.sprintf "LOCALS_SLOT_%ld" c.t_1 ;
          t_1 = Int32.of_int 5 ; } ]

  | "AreaCheckObject" 
  | "AreaType" -> 
      error "INFO" "TRIGGER: IGNORING %s\n" b.i_name; [] 

  | "HasBounceEffects" 
  | "HasImmunityEffects" 
    -> 
      [ { c with trigger_id = target_trigger "CheckSkillGT" ;
          t_1 = Int32.of_int 10 ;
          t_2 = Int32.of_int 3 ; (* concentration *) } ] 

  | "IfValidForPartyDialogue" -> [ { c with
				     trigger_id = target_trigger "See" ;
				     t_1 = Int32.zero  ; } ]

  | "XPLT"
    -> [ { c with t_1 = convert_xp_to_level_int32 c.t_1 ;
           trigger_id = target_trigger "LevelLT" } ]
  | "XPGT"
    -> [ { c with t_1 = convert_xp_to_level_int32 c.t_1 ;
           trigger_id = target_trigger "LevelLT" ; 
           negated = not c.negated } ]
  | "XP"
    -> [ { c with t_1 = convert_xp_to_level_int32 c.t_1 ;
           trigger_id = target_trigger "Level" } ]

  | "InMyArea"
    -> [ { c with trigger_id = target_trigger "AnyPCOnMap" ; } ] 

  | "WalkedToTrigger"
    -> [ { c with trigger_id = target_trigger "Clicked" } ; ]
        
  | "HasItemEquipedReal" 
  | "HasItemEquiped" 
    -> [ try 
      { c with trigger_id = target_trigger "HasItem" ;
        t_3 = convert c.t_3 "ITM" ; }
    with _ -> { c with trigger_id = target_trigger "False" } ]

  | "GlobalsGT"
    -> [ { c with trigger_id = target_trigger "True" } ]

  | "GlobalsLT"
  | "GlobalsEqual"
    -> [ { c with trigger_id = target_trigger "False" } ]

  | u -> error "TRIGGER" "MUST SIMULATE %s (%s)\n" u context ;  
      [ { c with trigger_id = target_trigger "False" } ] 

(***********************************************************************
 * BCS Files
 *            -> ACTIONS 
 ***********************************************************************)
and convert_action_directly a b context args =
  (match b.i_name with

  | "SetGlobal" 
  | "SetGlobalTimer" 
  | "IncrementGlobal" -> 
      a.a_8 <- namespace a.a_8 ; 
      if (String.uppercase a.a_8) = "GLOBALCHAPTER" then 
	[ { a with a_4 = Int32.pred a.a_4 } ] 
      else 
	[ a] 

  | "PlaySound" -> a.a_8 <- convert a.a_8 "WAV" ; [ a] 

  | "CreateItem" 
  | "GiveItem" 
  | "GiveItemCreate" 
  | "DestroyItem" 
  | "TakePartyItem" 
  | "TakePartyItemAll" 
  | "DropItem" 
  | "UseItem" -> 
      if (a.a_2.o_ea = Int32.of_int 2) (* PC *) then begin
	a.a_2.o_ea <- Int32.zero;
	a.a_2.o_identifiers <- Int32.of_int 21; (* player1 *) 
      end; 
      (*
	let b = Buffer.create 15 in 
	print_script_text (config.source) (Save_BCS_Buffer(b)) 
	(BCS_Print_ActionList([a])) false ; 
	let str = Buffer.contents b in
	error "ITEM" "%s\n" str ;
       *)
      a.a_8 <- convert a.a_8 "ITM" ; [ a] 

  | "CreateCreatureObject"
  | "CreateCreatureImpassable"
    -> a.a_8 <- convert a.a_8 "CRE" ; [a] 

  | "StartCutScene" -> a.a_8 <- convert a.a_8 "BCS" ; 
      if not !option_bcs_debug then 
	[ { empty_a with action_id = target_action "DisplayString" ;
            a_2 = player1 ; a_4 = Int32.of_int (debug_string 
						  (Printf.sprintf "Starting CutScene %s.bcs" a.a_8)) } ;
          a
	]
      else [ a ] 

  | "LeaveAreaLUA" 
  | "LeaveAreaLUAPanic" 
    -> a.a_8 <- convert a.a_8 "ARE" ; 
      if a.a_1 = player1 || b.i_name = "LeaveAreaLUAPanic" then 
	[ { empty_a with action_id = target_action "Wait" ; 
            a_4 = Int32.of_int 3 } ; 
          a 
	] 
      else [ a ] 

  | "ChangeAnimation" -> a.a_8 <- convert a.a_8 "CRE" ; 
      (* log_and_print "HOCC [%s] <- %b\n" a.a_8 
	 (Hashtbl.mem hp_of_converted_cre a.a_8) ;  *)
      if Hashtbl.mem hp_of_converted_cre a.a_8 then begin
	let amt = Hashtbl.find hp_of_converted_cre a.a_8 in 
	[ { a with action_id = target_action "SetHP" ;
            a_2 = myself ; a_4 = Int32.of_int amt } ; a] 
      end else [ a ] 

  | "ChangeAIScript" -> 
      a.a_8 <- convert a.a_8 "BCS" ; [ a] 
	(* a.a_4 <- convert a.a_4 "SCRLEV" *)

  | "StartStore" -> a.a_2 <- protagonist ; a.a_8 <- convert a.a_8 "STO" ; [
      a] 

  | "Polymorph" -> a.a_4 <- convert_ids a.a_4 "ANIMATE" "ANIMATE"; [ a] 

  | "MoveBetweenAreas" -> a.a_8 <- convert a.a_8 "ARE" ; [ a] 

  | "DisplayString" ->
      a.a_4 <- Int32.of_int (convert_strref (Int32.to_int a.a_4)); [ a] 

  | "TextScreen" 
  | "SetAreaRestFlag" 
  | "SetHomeLocation" 
  | "SetPlayerSound" 
  | "StartMovie" 
    -> [] (* failwith "wes doesn't understand this action"  *) 

  | "RemoveSpell" ->
      a.a_4 <- convert_ids_check a.a_4 "SPELL" "SPELL" ; [ a ] 

  | "ForceSpell" 
  | "Spell" 
  | "SpellNoDec" 
  | "ApplySpell" 
  | "ReallyForceSpell" 
  | "ForceSpellPoint" -> 
      begin
        (try 
          a.a_4 <- convert_ids_check a.a_4 "SPELL" "SPELL" ;
        with Not_found -> begin
          let old_res = spell_num_to_res a.a_4 in 
          (* log_and_print "\n\nGOT HERE %s\n\n" old_res ;  *)
          a.a_4 <- Int32.zero ;
          a.a_8 <- convert old_res "SPL"  ;
          if a.a_2 = myself && a.a_1.o_name <> "" then 
            a.a_2 <- a.a_1 
        end) ; 
        begin 
          match Int32.to_int a.a_4 with 
          | 2805 (* wilting *) -> 
              if a.a_2 = myself then begin (* don't ever do that! *)
		a.a_2 <- { empty_op with 
			   o_unknown1 = Int32.of_int 12 ; (* NearestEnemyOf *) 
			   o_identifiers = Int32.one ; (* myself *) } 
              end
          | 2995 (* dim door unsummon *) -> 
              if a.a_2 <> myself then begin
		a.a_1 <- a.a_2 ; 
              end 
          | _ -> ()
        end ; 
        if (args.cb_is_cutscene) then begin
          let match_by_name = 
            match a.a_8 with 
            | "SPWI615" (* flesh to stone *)
            | "SPWI903" (* power word kill *) 
            | "SPWI607" (* disint *) 
            | "SPWI303" (* flame arrow *) 
              -> true
            | _ -> false 
          in
          let match_by_number = 
            let code = Int32.to_int a.a_4 in 
            if code = 2903 (* PW:K *) ||
            code = 2615 (* flesh stone *) ||
            code = 2607 (* disint *) || 
            code = 3950 (* cutscene flesh to stone *) ||
            code = 2056 (* wizard disint 2, ignore res *) ||
            code = 2053 (* wizard flesh to stone, ignore res *) ||
            code = 2054 (* wizard finger of death, ignore res *) ||
            code = 2303 (* flame arrow *) then true else false
          in
          if (match_by_name || match_by_number) then 
            let b = { empty_a with action_id = target_action "Kill" ;
                      a_2 = a.a_2 ; } in
            [ a ; b ] 
          else [ a] 
        end else [ a ] 
      end 

  | "SpellRES" 
  | "ForceSpellRES"
  | "ApplySpellRES"
  | "ForceSpellPointRES"
  | "ReallyForceSpellRES"
  | "SpellNoDecRES" -> 
      a.a_8 <- convert a.a_8 "SPL" ; [ a] 

  | "RevealAreaOnMap" 
  | "SetMasterArea" 
    -> a.a_8 <- convert a.a_8 "ARE" ; [ a] 

  | "MoveGlobal" 
    -> a.a_8 <- convert a.a_8 "ARE" ;
      [ { a with action_id = target_action "MoveBetweenAreas" } ] 

  | "ChangeEnemyAlly" -> a.a_4 <- convert_ids a.a_4 "EA" "EA" ; [ a] 


  | "AttackOneRound" 
  | "AttackReevaluate" -> 
      a.action_id <- target_action "AttackOneRound" ; 
      a.a_4 <- Int32.zero ; [ a ]

  | "Interact" ->
      (*
	let cres,cext = split context in 
	let banter_bcs = convert_banter cres in 
	[ { a with action_id = target_action "StartCutSceneMode" ; } ;
        { a with action_id = target_action "StartCutScene" ; 
        a_8 = banter_bcs } ] 
       *)
      [ { empty_a with action_id = target_action "StartDialogueNoSet" ; } ]
	(*
	  let cres,cext = split context in 
	  let dlg_file = convert cres "interdia"  in
	  let join_dlg_file = convert cres "join_dialog" in 
	  a.action_id <- target_action "StartDialog" ;
	  a.a_8 <- convert dlg_file "DLG" ;
	 *)

  | "Kill"  ->
      if a.a_1.o_name <> "" && a.a_2 = myself then 
        [ { a with a_2 = a.a_1 } ] 
      else [ a ]

  | "CutSceneId"  ->
      [ a ; { empty_a with action_id = target_action "StartCutSceneMode" } ]

  | "Attack" 
  | "SetInterrupt" 
  | "SetNumTimesTalkedTo" 
  | "MoveViewObject" 
  | "ReputationInc" 
  | "MakeUnselectable"  
  | "GiveGoldForce"
  | "FaceObject" 
  | "StartTimer" 
  | "RunAwayFrom" 
  | "RunAwayFromNoInterrupt" 
  | "MoveToObject" 
  | "FadeFromColor" 
  | "FadeToColor" 
  | "MoveToPoint" 
  | "MoveViewPoint" 
  | "OpenDoor" 
  | "CloseDoor" 
  | "PlayDead" 
  | "SmallWait" 
  | "Unlock" 
  | "TakePartyGold" 
  | "TriggerActivation" 
  | "Wait" 
  | "ClearActiosn"
  | "DayNight"
  | "GivePartyGold"
  | "IncrementChapter"
  | "Lock"
  | "MoraleInc" 
  | "Weather" 
  | "ClearActions"
  | "SelectWeaponAbility" 
  | "MoveToObjectFollow" 
  | "RemoveTraps" 
  | "Shout"
  | "ClickLButtonObject"
  | "MoveToObjectNoInterrupt"
  | "MoveToPointNoInterrupt"
    -> [ a] 

  | "Deactivate"
    -> [ { a with action_id = target_action "HideCreature" ;
           a_4 = Int32.one } ; a ] 

  | "Activate"
    -> [ a ; { a with action_id = target_action "HideCreature" ;
               a_4 = Int32.zero } ; ] 

  | "JumpToPoint"
    -> if a.a_1 = player1 then begin
      let res = convert_bringer a.a_1 a.a_5 context in 
      [ { a with action_id = target_action "CreateCreature" ;
          a_8 = res;
          a_9 = res; 
          a_1 = empty_op ; } ] 
    end else [ a ] 


  | "AddexperienceParty"
    -> [ { a with a_4 = convert_xp_award a.a_4 } ] 

  | "Swing" -> 
      [ { empty_a with action_id = target_action "SpellHitEffectSprite" ;
          a_2 = myself; a_3 = myself; 
          a_4 = Int32.of_int 20; (* mordy! *) 
          a_6 = Int32.of_int 20; 
        } ] 

  | u -> 
      (if (List.length b.i_args) > 0 then 
	error "ACTION" "SIDE-EFFECTS %s\n" u ); [a] 
  ) 

and convert_action_almost a b almost context args =
  match b.i_name with
  | "SaveGame" -> 
      error "INFO" "ACTION: IGNORING %s\n" b.i_name ; [] 

  | "ChangeAlignment" -> 
      [ { almost with a_4 = convert_ids a.a_4 "ALIGN" "ALIGNMNT" } ] 

  | "CreateCreature" ->
      [ { almost with a_8 = convert a.a_8 "CRE" ;
          a_9 = convert a.a_8 "variable_of_cre" ; } ]

  | "TakePartyItemNum" ->
      [ { almost with a_8 = convert a.a_8 "ITM" ; } ] 

  | "AddJournalEntry" 
    -> [ { almost with a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4)) } ] 

  | "Face" 
  | "Shout" 
    -> [ almost ] 

  | "ScreenShake" -> [ { almost with a_6 = Int32.of_int 15;
                         a_7 = Int32.of_int 15; } ] 

  | u -> (error "ACTION" "CLOSE %s\n" u ; raise Not_found)

and convert_action_simulate a b context args = match b.i_name with 
  (* | "DestroyAllEquipment" *)
| "SoundActivate" 
| "AmbientActivate" 
| "SetBeenInPartyFlags" 
| "SetGabber" 
| "SetHomeLocation" 
| "SetPlayerSound" 
| "StateOverrideFlag" 
| "MoveToCenterOfScreen"
| "SpawnPtDeactivate" 
  -> (* error "INFO" "ACTION: IGNORING %s\n" b.i_name ; *) [] 

| "UseItemSlot" 
  -> (* almost invariably: drink an invis potion *) 
    [ { a with action_id = target_action "IncrementGlobal" ;
        a_8 = Printf.sprintf "LOCALS_SLOT_%ld" a.a_4 ;
        a_4 = Int32.one } ; 
      { a with action_id = target_action "ApplySpellRES" ;
        a_2 = myself ; a_8 = "SPWI206" } ;
      { a with action_id = target_action "ApplySpellRES" ;
        a_2 = myself ; a_8 = "SPPR401" } ; (* cure moderate wounds *) 
    ]

| "PlayDeadInterruptible" 
  -> [ { a with action_id = target_action "PlayDead" } ] 

| "ChangeAnimationNoEffect" 
| "PolymorphCopy" 
| "PolymorphCopyBase" 
  -> 
    a.a_8 <- convert a.a_8 "CRE" ; 
    (* log_and_print "HOCC [%s] <- %b\n" a.a_8 
       (Hashtbl.mem hp_of_converted_cre a.a_8) ;  *)
    if Hashtbl.mem hp_of_converted_cre a.a_8 then begin
      let amt = Hashtbl.find hp_of_converted_cre a.a_8 in 
      [ { a with action_id = target_action "SetHP" ;
          a_2 = myself ; a_4 = Int32.of_int amt } ;
        { a with action_id = target_action "ChangeAnimation" } ; ] 
    end else [ { a with action_id = target_action "ChangeAnimation" } ] 

| "CreateVisualEffect"
  -> [ { a with action_id = target_action "SpellHitEffectPoint" ;
         a_4 = Int32.of_int (1+(Random.int 50)) ;
         a_6 = Int32.of_int 20; 
         a_8 = "" ; } ] 


| "SetName" -> 
    let strref = Int32.of_int (convert_strref (Int32.to_int a.a_4)) in 
    [ { a with action_id = target_action "SetApparentNameSTRREF" ;
        a_2 = myself; a_4 = strref ; } ;
      { a with action_id = target_action "SetRegularNameSTRREF" ;
        a_2 = myself; a_4 = strref ; } ] 

| "StorePartyLocations" ->
    [ { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player1 ; } ;
      { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player2 ; } ;
      { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player3 ; } ;
      { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player4 ; } ;
      { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player5 ; } ;
      { a with action_id = target_action "SetSavedLocation" ;
        a_1 = player6 ; } ; ] 

| "RestorePartyLocations" ->
    [ { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player1 ; } ;
      { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player2 ; } ;
      { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player3 ; } ;
      { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player4 ; } ;
      { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player5 ; } ;
      { a with action_id = target_action "JumpToSavedLocation" ;
        a_1 = player6 ; } ; ]


| "SetLeavePartyDialogueFile" ->
    [ { a with action_id = target_action "SetLeavePartyDialogFile" ;
        a_8 = convert a.a_8 "DLG" } ] 

| "ReallyForceSpellDead" -> 
    a.action_id <- target_action "ReallyForceSpell" ;
    let b = (Bcs.ids_of_sym (config.target) "ACTION" "ReallyForceSpell") in
    let lst = convert_action_directly a b context args in
    lst 

| "RealSetGlobalTimer" -> 
    [ { a with action_id = target_action "SetGlobalTimer" ; 
        a_8 = namespace a.a_8 ; } ]

| "CreateCreatureImpassableAllowOverlap" -> 
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { a with action_id = target_action "CreateCreature" ; 
        a_8 = cre ; a_9 = var ; } ] 

| "CreateCreatureDoor" -> 
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { a with action_id = target_action "CreateCreature" ; 
        a_8 = cre ; a_9 = var ; } ;
      { a with action_id = target_action "SpellHitEffectPoint" ;
        a_2 = { empty_op with o_name = var } ; 
        a_4 = Int32.of_int 60 ; (* dim door dest *) 
        a_6 = Int32.of_int 20; } ] 

| "DestroyGold" -> [ ]
      (* [ { a with action_id = target_action "GiveGoldForce" ;
         a_4 = Int32.neg a.a_4 } ]  *)

| "GivePartyAllEquipment" -> 
    [ { a with action_id = target_action "TransferInventory" ;
        a_2 = myself ; a_3 = player1 } ] 

| "PlaySong"-> 
    [ { a with action_id = target_action "StartMusic" ;
        a_6 = Int32.of_int 2 (* PLAY *) } ] 

| "SetAreaRestFlag" ->
    [ { a with action_id = target_action "SetAreaFlags" ;
        a_4 = Int32.of_int 2 (* NOREST *) ;
        a_6 = if (a.a_4 = Int32.one) then Int32.zero else Int32.one ; } ]

| "CreateCreatureOffScreen" ->
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { empty_a with action_id = target_action "CreateCreature";
        a_8 = cre ; a_9 = var ;}; 
      { empty_a with action_id = target_action "JumpToObject";
        a_1 = {empty_op with o_name = var} ; a_2 = player1 ; } ;
      (* FIXME: this makes the spell hit point 0.0 *) 
      { a with action_id = target_action "SpellHitEffectPoint" ;
        a_4 = Int32.of_int 60 ; (* dim door dest *) 
        a_6 = Int32.of_int 20; } ] 

| "CreateCreatureObjectCopy" -> 
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { empty_a with action_id = target_action "CreateCreatureObject";
        a_1 = a.a_2 ; 
        a_2 = a.a_2 ; 
        a_8 = cre ; a_9 = var ;}; 
    ] 

| "CreateCreatureObject" 
| "CreateCreatureObjectOffScreen" ->
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { empty_a with action_id = target_action "CreateCreatureObject";
        a_8 = cre ;  }
        (* a_9 = var ;}; 
	   { empty_a with action_id = target_action "JumpToObject";
           a_1 = { empty_op with o_name = var } ; 
           a_2 = a.a_2 ; }  *) 
    ] 

| "CreateCreatureObjectDoor" 
| "CreateCreatureObjectEffect" -> 
    let cre = convert a.a_8 "CRE" in
    let var = convert a.a_8 "variable_of_cre" in 
    [ { empty_a with action_id = target_action "CreateCreature";
        a_8 = cre ; a_9 = var ;}; 
      { empty_a with action_id = target_action "JumpToObject";
        a_1 = { empty_op with o_name = var } ; 
        a_2 = a.a_2 ; } ;
      { empty_a with action_id = target_action "SpellHitEffectSprite";
        a_1 = { empty_op with o_name = var } ; 
        a_2 = { empty_op with o_name = var } ; 
        a_3 = { empty_op with o_name = var } ; 
        a_4 = Int32.of_int 60 ; (* dim door dest *) 
        a_6 = Int32.of_int 20 ; } ]


| "EscapeAreaObject"  
| "EscapeAreaObjectMove" -> 
    [ { a with action_id = target_action "EscapeArea" ; } ] 


| "EscapeAreaMove" -> 
    [ { empty_a with action_id = target_action "MoveBetweenAreas" ;
        a_8 = convert a.a_8 "ARE"  ;
        a_5 = ((a.a_4),(a.a_6)) ;
        a_4 = a.a_7 ; 
        a_1 = a.a_1 ; 
      } ] 

| "AddXPObject" -> 
    [ { empty_a with action_id = target_action "AddExperienceParty" ;
        a_4 = convert_xp_award (Int32.div a.a_4 (Int32.of_int 6)) ; } ] 

| "DisplayStringHead"  
| "DisplayStringNoNameHead" -> 
    let aint = Int32.to_int a.a_4 in 
    let arr = (config.source).Load.dialog in
    let snd = 
      if (aint >= 0 && aint < Array.length arr) then 
        convert arr.(aint).Tlk.sound_name "WAV"
      else
        ""
    in 
    if snd <> "" then 
      [ { a with action_id = target_action "FloatMessage" ;
          a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4))} ;
	{ a with action_id = target_action "PlaySound" ;
          a_8 = snd; } ] 
    else
      [ { a with action_id = target_action "FloatMessage" ;
          a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4))} ] 

| "DisplayStringNoName" ->
    [ { a with action_id = target_action "DisplayString" ;
        a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4))} ] 
| "DisplayStringWait" -> 
    [ { empty_a with action_id = target_action "Wait" ;
        a_4 = Int32.of_int 5 ; a_1 = a.a_1 } ;
      { a with action_id = target_action "DisplayString" ;
        a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4))} ; ] 

| "CreateVisualEffectObject" -> 
    [ { a with action_id = target_action "SpellHitEffectSprite" ;
        a_3 = a.a_2 ; 
        a_4 = Int32.of_int (1+(Random.int 8)) ;
        a_6 = Int32.of_int 40; 
      } ;
      { a with action_id = target_action "ReallyForceSpell" ;
        a_3 = a.a_2 ;
        a_4 = Int32.of_int 3157 ; 
        a_8 = ""; }
    ] 

| "SetDialogue" ->
    [ { a with action_id = target_action "SetDialog" ;
        a_8 = convert a.a_8 "DLG" } ] 

| "Dialog" ->
    [ { a with action_id = target_action "Dialogue" } ] 
| "StartDialogue" ->
    [ { a with action_id = target_action "StartDialog" ;
        a_8 = convert a.a_8 "DLG" } ] 
| "StartDialogNoSet" -> 
    [ { a with action_id = target_action "StartDialogueNoSet" } ]  
| "DialogueInterrupt" -> 
    [ { a with action_id = target_action "DialogInterrupt" } ] 
| "EraseJournalEntry" -> 
    [ { a with action_id = target_action "DeleteJournalEntry" ;
        a_4 = Int32.of_int (convert_strref (Int32.to_int a.a_4)) } ] 

| "TakeItemListPartyNum" -> 
    let buff = load_source_res a.a_8 "2DA" in
    let lines = Str.split newline_regexp buff in 
    let itm = List.hd (List.tl lines) in
    let itm = convert itm "ITM" in 
    [ { a with action_id = target_action "CreateItem" ;
        a_8 = itm ; a_4 = Int32.one ; } ] 

| "CreateCreatureAtLocation" ->
    let dest_cre = Str.string_after a.a_8 9 in
    let dest_cre = convert dest_cre "variable_of_cre" in 
    let cre_to_make = convert a.a_9 "CRE" in
    [ { a with action_id = target_action "CreateCreatureObject" ;
        a_8 = cre_to_make ;
        a_2 = { empty_op with o_name = dest_cre } } ] 

| "DestroyAllDestructableEquipment" ->
    [ { a with action_id = target_action "DestroyAllEquipment" } ] 

| "EscapeAreaNoSee" -> 
    [ { a with action_id = target_action "EscapeArea" } ] 

| "MoveBetweenAreasEffect" ->
    [ { a with action_id = target_action "SpellHitEffectSprite" ;
        a_3 = a.a_2 ; 
        a_4 = Int32.of_int (1+(Random.int 8)) ;
        a_6 = Int32.of_int 40; 
        a_8 = "" ; a_9 = "" ; 
      } ;
      { a with action_id = target_action "MoveBetweenAreas" ;
        a_9 = "" } ] 

| "TakeItemReplace" ->
    let take = a.a_9 in
    let give = a.a_8 in 
    [ { a with action_id = target_action "DestroyItem" ;
        a_8 = take ; a_9 = "" ; a_1 = a.a_2 ; } ;
      { a with action_id = target_action "CreateItem" ;
        a_8 = give ; a_9 = "" ; a_1 = a.a_2 ; } 
    ] 

| u -> (error "ACTION" "MUST SIMULATE %s (%s)\n" u context ; 
        raise Not_found)

and print_action a =
  try 
    let b = Buffer.create 15 in 
    print_script_text (config.source) (Save_BCS_Buffer(b)) 
      (BCS_Print_ActionList([a])) false None ; 
    let str = Buffer.contents b in
    let regexp = Str.regexp "[\\[\\]]" in
    Str.global_replace regexp " " str 
  with e -> 
    Printf.sprintf "cannot print source: %s" (Printexc.to_string e)

and print_trigger a =
  try 
    let b = Buffer.create 15 in 
    print_script_text (config.source) (Save_BCS_Buffer(b)) 
      (BCS_Print_TriggerList([a])) false None ; 
    let str = Buffer.contents b in
    let regexp = Str.regexp "[\\[\\]]" in
    Str.global_replace regexp " " str 
  with e -> 
    Printf.sprintf "cannot print source: %s" (Printexc.to_string e)



and convert_bcs_action a context args in_dlg : Bcs.action list = 
  Stats.time "BCS action" (fun () -> 
    (* find the best way to print out this action *) 
    let orig_a = { a with action_id = a.action_id } in 
    let b = 
      try 
	best_ids_of_action (config.source) a 
      with e ->
	error "ACTION" "[%s.BCS] BEST BG2 ACTION %lx: %s\n" context a.action_id 
          (Printexc.to_string e); raise e
    in
    (* convert the object parameters *) 
    (try a.a_1 <- convert_bcs_obj a.a_1 ;
      a.a_2 <- convert_bcs_obj a.a_2 ;
      a.a_3 <- convert_bcs_obj a.a_3 ;
    with e -> 
      error "ACTION" "CONVERT OBJ OF ACTION %ld: %s\n" a.action_id 
	(Printexc.to_string e);
      raise e
    ) ;
    (* the ActionOverride("foo",DoThis(Myself)) fixup! *) 
    (if a.a_1.o_name <> "" && a.a_2 = myself then a.a_2 <- a.a_1) ;
    (* try to find a similar action in IWD2 *) 
    let exists_in_iwd2 = 
      try Some(Bcs.ids_of_sym (config.target) "ACTION" b.Ids.i_name) 
      with Not_found -> None 
    in
    try 
      let converted_actions = begin match exists_in_iwd2 with
      | Some(i) -> 
	  if b.i_name = i.i_name && b.i_args = i.i_args then begin
            (* we are in luck! a direct match! *) 
            a.action_id <- i.i_num ; 
            convert_action_directly a b context args 
	  end else begin
            (* the arguments don't quite match *) 
            let almost = { a with action_id = i.i_num ; } in 
            convert_action_almost a b almost context args
	  end
      | None -> 
	  (* This action does not exist in IWD2, we must simulate *) 
	  convert_action_simulate a b context args
      end
      in 
      if in_dlg then List.map (fun a -> 
        if non_empty_object a.a_1 then 
          let new_cre = convert_overrider a in 
          { empty_a with action_id = target_action "CreateCreatureObject" ;
            a_2 = myself ; a_8 = new_cre }
        else a
			      ) converted_actions
      else converted_actions
    with e -> 
      [ { empty_a with  action_id = target_action "DisplayString" ;
          a_2 = player1 ; a_4 = Int32.of_int (debug_string 
						(Printf.sprintf "WeiDU: %s %s : %s (do not report this unless something actually goes wrong)" context 
						   (print_action orig_a) (Printexc.to_string e))); } ] 
			  ) () 

and convert_bcs new_r r args prepend_this = begin

  let context = r ^ ".BCS" in
  let counter = ref 0 in 

  let timer_list = Hashtbl.create 1 in
  let any_csid = ref false in 
  let any_wait = ref false in 

  let npc_var = convert r "is_npc_script" in 

  let buff = load_source_res r "BCS" in 

  let prepend_this = 
    if npc_var <> "" && config.target_variant = IWD2 then begin
      let ds = convert npc_var "dream_script_of_var" in 
      try 
        let ds_buff = load_source_res ds "BCS" in 
        let lexbuf = lex_init_from_string ds ds_buff in 
        let s = Stats.time "parsing .BCS"  
            (fun () -> Bcsparser.bcs_file Bcslexer.initial lexbuf) () in
        let sleeping = { empty_t with trigger_id = target_trigger "Global" ;
			 t_3 = "GLOBAL_RestedParty" ; t_1 = Int32.one }  
        in 
        let new_script = List.map (fun (c,rl) -> ((sleeping :: c),rl)) s in
        new_script @ prepend_this 
      with _ -> prepend_this
    end else prepend_this
  in 


  try 
    let lexbuf = lex_init_from_string r buff in 
    let s = Stats.time "parsing .BCS"  
	(fun () -> Bcsparser.bcs_file Bcslexer.initial lexbuf) () in
    let s = prepend_this @ s in 
    let new_cr_list = List.flatten (List.map (fun (cl,rl) -> 
      let new_c_list = List.flatten (List.map (fun c -> convert_bcs_trigger c context) cl) in
      List.iter (fun c ->
        if c.trigger_id = opcode_GlobalTimerExpired then 
          Hashtbl.replace timer_list (c.t_3 ,c.t_4) true ;
		) new_c_list ;
      let new_r_list = (List.map (fun (w,al) ->
        (if (al <> [] && (List.hd al).action_id = opcode_CutSceneID) then 
          any_csid := true) ;

        let args = { args with cb_is_cutscene = !any_csid } in 
        let new_al = (List.flatten 
			(List.map (fun a -> convert_bcs_action a context args false) al))
        in 

        any_wait := List.fold_right (fun elt acc -> 
          acc || elt.action_id = opcode_Wait || 
          elt.action_id = opcode_SmallWait) new_al false ;

        let new_al = match new_al with
          hd :: tl when hd.action_id = opcode_CutSceneID && 
            hd.a_2 = player1 -> 
              List.filter (fun a ->
		if List.mem a.action_id not_in_p1_cs then false else true
			  ) new_al
        | hd :: tl when hd.action_id <> opcode_CutSceneID && 
            not !option_bcs_debug ->
              ({ empty_a with action_id = target_action "DisplayString" ;
                 a_2 = player1 ;
                 a_4 = Int32.of_int (debug_string 
				       (Printf.sprintf "%s.bcs block %d activated" r !counter)) }) 
              :: hd :: tl 
        | _ -> new_al 
        in 

        (* if this is not a cut-scene, pull SetGlobals to the front *)
        let new_al = if not !any_csid then begin
          let sg, other = List.partition
              (fun al -> al.action_id = opcode_SetGlobal ||
              al.action_id = opcode_SetGlobalTimer || 
              al.action_id = opcode_IncrementGlobal) 
              new_al in
          sg @ other
        end else new_al 
        in 

        (* pull party-leaving and whatnot to the beginning of cut-scenes *)
        let new_al = if !any_csid then begin
          let cs = List.hd new_al in 
          let lp, other = List.partition 
              (fun al -> al.action_id = opcode_LeaveParty || 
              al.action_id = opcode_TransferInventory || 
              al.action_id = opcode_SetGlobal) 
              (List.tl new_al) in
          cs :: (lp @ other)
        end else new_al
        in 

        (* pull CreateCreature(), OpenDoor around things with Waits() *) 
        let new_al = if (not !any_csid) && !any_wait then 
          let lp, other = List.partition 
              (fun al -> 
                al.action_id = opcode_CreateCreature  ||
                al.action_id = opcode_CreateCreatureObject ||
                al.action_id = opcode_CreateCreatureImpassable ||
                al.action_id = opcode_OpenDoor || 
                al.action_id = opcode_Unlock) ( new_al ) 
          in lp @ other 
        else new_al 
        in 

        incr counter ;
        (w, new_al)
				 ) rl)
      in 

      [ (new_c_list,new_r_list) ]
					     ) s) in 

    let blank_on_cutscenes = match config.target_variant with
    | IWD2 -> 
	[ (([ { empty_t with trigger_id = targcode_InCutsceneMode ; } ]),
	   [(100,[])]) ]
    | _ -> [] (* not necessary *) 
    in 

    let new_cr_list = 
      if not !any_csid then begin
        let timer_setting_list = ref [] in
        Hashtbl.iter (fun (a,b) _ -> 
          let var = b ^ a in
          let new_c = [ { empty_t with 
			  trigger_id = target_trigger "Global" ; t_3 = var } ] in
          let new_r = ( 100 , 
			[ { empty_a with action_id = target_action "SetGlobal" ;
			    a_8 = var ; a_4 = Int32.neg Int32.one } ; 
			  { empty_a with action_id = target_action "Continue" } ] ) in
          timer_setting_list := (new_c,[new_r]) :: !timer_setting_list 
		     ) timer_list ;
        blank_on_cutscenes @ (!timer_setting_list @ new_cr_list)
      end else new_cr_list
    in 
    let new_cr_list = 
      if npc_var <> "" && config.target_variant = IWD2 then begin
	(*
          let join_dlg = convert (String.uppercase npc_var) "join_dialog" in 
          let join_dlg = convert join_dlg "DLG" in *) 
        (* let banter_bcs = convert_banter r in *) 
        ([{ empty_t with trigger_id = target_trigger "InParty" ; t_5 = myself};
          { empty_t with trigger_id = target_trigger "NumTimesTalkedTo" } ] ,
         [(100,
           [{ empty_a with action_id = target_action "SetNumTimesTalkedTo" ;
              a_4 = Int32.one } ])]) ::

        ([{ empty_t with trigger_id = target_trigger "GlobalGT" ; 
            t_3 = "GLOBAL" ^ "_DEAD" ^ npc_var ; t_1 = Int32.one ; }  ;
          { empty_t with trigger_id = target_trigger "HPGT" ;
            t_5 = myself; t_1 = Int32.zero ; } ] ,
         [(100,
           [{ empty_a with action_id = target_action "SetGlobal" ;
              a_8 = "GLOBAL" ^ "_DEAD" ^ npc_var ; a_4 = Int32.zero ; }  ;
          ])])  ::

(*
  ([{ empty_t with trigger_id = target_trigger "GlobalGT" ; 
  t_3 = "GLOBAL_reset" ^ npc_var ; t_1 = Int32.zero ; } ],
  [(100,
  [{ empty_a with action_id = target_action "SetDialog" ;
  a_8 = join_dlg ; }  ;
  { empty_a with action_id = target_action "SetGlobal" ;
  a_8 = "GLOBAL_reset" ^ npc_var ; a_4 = Int32.zero ; }  ;
  { empty_a with action_id = target_action "DisplayString" ;
  a_2 = myself ; a_4 = Int32.of_int (debug_string 
  (Printf.sprintf "%s: reset %s to %s.DLG" r npc_var join_dlg)) }
  ])])  ::
 *)

        ([{ empty_t with trigger_id = target_trigger "Global" ;
            t_3 = "GLOBAL_DoBanter" ; t_1 = Int32.one ; } ; 
          { empty_t with trigger_id = target_trigger "RandomNum" ;
            t_1 = Int32.of_int 5 ; t_2 = Int32.one ; } ],
         [(100, [ { empty_a with action_id = target_action "SetGlobal" ;
                    a_8 = "GLOBAL_DoBanter" ; a_4 = Int32.zero } ; 
                  { empty_a with action_id = target_action "DisplayString" ;
                    a_2 = myself ; 
                    a_4 = Int32.of_int (debug_string
					  (Printf.sprintf "%s will try to banter" npc_var)) ; } ;
                  { empty_a with action_id = target_action
                      "StartDialogueNoSet" ; a_2 = player1 } ;
                  (*
                    { empty_a with action_id = target_action "StartCutSceneMode" ; } ;
                    { empty_a with action_id = target_action "StartCutScene" ; 
                    a_8 = banter_bcs }  
                   *)
		])])

        :: new_cr_list
      end else new_cr_list 
    in 
    (* Hack in NPCs that should start in this area *) 
    let new_cr_list = match Hashtbl.find_all area_npc_ht new_r with
      [] -> new_cr_list
    | lst -> begin
        let var = Printf.sprintf "GLOBAL%s_NPC_INIT" new_r in 
        let new_c = [ { empty_t with 
			trigger_id = target_trigger "Global" ; t_3 = var } ] in
        let new_r = 
          { empty_a with action_id = target_action "SetGlobal" ;
            a_8 = var ; a_4 = Int32.neg Int32.one } ::
          (List.map (fun (orig_cre_r,cre_r,variable,x,y) ->
            { empty_a with action_id = target_action "CreateCreature" ;
              a_8 = cre_r ; a_9 = variable ; a_5 = (x,y) } ) lst)   in
        (new_c, [(100,new_r)]) :: new_cr_list 
    end 
    in 
    let new_cr_list = 
      if args.cb_is_infopoint then convert_infopoint_bcs new_cr_list 
      else new_cr_list
    in 
    save_in_override_generic new_r "BCS" (fun oc -> 
      save_bcs (config.target) (Save_BCS_OC(oc)) new_cr_list) ;
  with e -> 
    (error "BCS" "FAILED: %s: %s\n" r (Printexc.to_string e) ;
     raise e)
end 

(***********************************************************************
 * Special BCS Filters
 ***********************************************************************)
and convert_infopoint_bcs s =
  List.map (fun (c,rl) ->
    let rl = List.map (fun (i,al) ->
      let al = List.map (fun a -> match Int32.to_int a.action_id with
      | 241 (* FloatMessage *) -> 
          { a with a_2 = { empty_op with 
			   o_unknown1 = Int32.of_int 17 ; (* LastTrigger *) 
			   o_identifiers = Int32.one ; (* myself *) } }
      | 113 (* ForceSpell *) 
      | 181 (* ReallyForceSpell *) when is_summon_spell a 
        ->
          let cre = convert_overrider { a with 
					action_id = Int32.of_int 181 ; a_2 = myself } in
          { empty_a with action_id = target_action "CreateCreatureObject" ;
            a_8 = cre ; a_2 = a.a_2 } 
      | _ -> a 
			) al in
      (i,al)
		      ) rl in
    (c,rl) 
	   ) s

(***********************************************************************
 * STO Files
 ***********************************************************************)
and convert_sto new_r r = begin
  let buff = load_source_res r "STO" in 

  if String.sub buff 0 8 <> "STORV1.0" then begin
    failwith "not a valid STO 1.0 file (wrong sig)"
  end ; 

  String.blit "STORV9.0" 0 buff 0 8 ; 
  let before = Str.string_before buff 0x9c in
  let after = Str.string_after buff 0x9c in 
  let junk_size = 0xf0 - 0x9c in 
  let junk = String.make junk_size '\000' in
  (* junk.[0] <- Char.chr 255 ; *)
  let buff = (before ^ junk) ^ after in 

  patch_strref buff 0xc ;
  patch_resref buff 0x44 "DLG" ;

  (* incremenet all of the offsets by junk-size *)
  List.iter (fun o -> let cur = int_of_str_off buff o in
  String.blit (str_of_int (cur+junk_size)) 0 buff o 4) [
  0x2c ; (* items purchased *)
  0x34 ; (* items for sale *)
  0x4c ; (* drinks offset *) 
  0x70 ; (* cures offset *) 
  ] ;

  (* handle the items for sale *) 
  let offset = int_of_str_off buff 0x34 in 
  let num = int_of_str_off buff 0x38 in
  for i = 0 to num - 1 do 
    let offset = offset + (i * (0x10c - 0xf0)) in 
    patch_resref buff offset "ITM" ;
    let quantity_charges_1 = short_of_str_off buff (offset + 0xa) in
    if quantity_charges_1 < 1 then
      write_short buff (offset + 0xa) 1 
  done ;

  (* handle the spells for sale *) 
  let offset = int_of_str_off buff 0x70 in 
  let num = int_of_str_off buff 0x74 in
  for i = 0 to num - 1 do 
    let offset = offset + (i * (0xfc - 0xf0)) in 
    patch_resref buff offset "SPL" 
  done ;

  (* handle the drinks for sale *) 
  let offset = int_of_str_off buff 0x4c in 
  let num = int_of_str_off buff 0x50 in
  for i = 0 to num - 1 do 
    let offset = offset + (i * (0xb0 - 0x9c)) in 
    patch_strref buff (offset + 8) 
  done ;

  (* purchase everything *) 
  let num_items_purchased = int_of_str_off buff 0x30 in
  let buff = 
    if num_items_purchased > 0 then begin
      let ipo = int_of_str_off buff 0x2c in
      write_int buff 0x30 70 ;
      let before = Str.string_before buff ipo in 
      let after = String.create (70 * 4) in
      for i = 0 to 69 do
        write_int after (i*4) (i +1)
      done;
      before ^ after 
    end else buff
  in 

  save_in_override new_r "STO" buff 
end 

(***********************************************************************
 * WED Files
 ***********************************************************************)
and convert_wed new_r r = begin
  let buff = load_source_res r "WED" in 

  if String.sub buff 0 8 <> "WED V1.3" then begin
    failwith "not a valid WED 1.3 file (wrong sig)"
  end ; 

  (match config.source_variant , config.target_variant with 
  | BG1, BG2 -> 
      (* Japh's WED fixup *) 
      if Hashtbl.mem bg1_wed_overlay r || true then
	(* let lst = Hashtbl.find bg1_wed_overlay r in  *)
	let tile_offset = int_of_str_off buff 0x30 in
	let width = short_of_str_off buff 0x20 in 
	let height = short_of_str_off buff 0x22 in 
	for i = 0 to width * height - 1 do 
	  (* List.iter (fun i ->  *) 
          (* let's do a sanity check first *) 
          if (i < 0 || i >= (width * height)) then begin
            error "WED" "PROGRAMMER ERROR: overlay hack 0 <= %d < %d\n"
              i (width * height) 
          end ; 
          let offset = tile_offset + (i * 10) in 
          let num_overlay = short_of_str_off buff (offset + 6) in
          if (num_overlay > 0) then begin
            let primary_tile_index = short_of_str_off buff offset in
            (* give the secondary tile index the same value as the primary tile
               index *) 
            (* write_short buff (offset + 4) (primary_tile_index) ; *)
            (* write_short buff (offset + 4) i ; *)
            write_short buff (offset + 6 ) 0 ; 
            ()
          end 
	done (* ) lst  *) 
  | _, _ -> () 
  ) ; 

  let offset = int_of_str_off buff 0x10 in
  let num = short_of_str_off buff 0x8 in 
  for i = 0 to num - 1 do 
    let offset = offset + i * (0x38 - 0x20) in 
    let width = short_of_str_off buff offset in
    let height = short_of_str_off buff (offset+2) in
    (match i, config.source_variant, config.target_variant with
    | 0, BG1, BG2 ->
	(* Japh: In addition to that procedure I showed you before, can you
	   also make sure that when hacking up the night .wed's, (ARXXXXN.WED),
	   you point the first overlay to the actual night .tis file?

	   BG1 handles night tis' differently than BGII.  In BGII, the night .wed
	   file actually points to the night .tis file.  In BGI, the night .wed
	   file just points to the normal day .tis file. *)
	let r = get_string_of_size buff (offset + 4) 8 in 
	(* Weimer: I'm assuming the night TIS file is just the old file with
	   N appended *) 
	let new_r = str_to_exact_size (try convert (r) "TIS" with _ -> 
          convert r "TIS") 8 in 
	String.blit new_r 0 buff (offset + 4) 8  

    | _, _, _ -> patch_resref buff (offset + 4) "TIS" ;
    ) ;

    let tilemap_offset = int_of_str_off buff (offset + 0x10) in
    let num_tilemap = width * height in
    let negone = String.make 2 '\255' in 
    (* remove all overlays in IWD2 *) 
    if config.target_variant = IWD2 then 
      for j = 0 to num_tilemap - 1 do
        let offset = tilemap_offset + (j * 10) in 
        (* String.blit negone 0 buff (offset + 4) 2 ; *)
        write_byte buff (offset+6) 0;
      done
  done ;
  (* write_short buff 0x8 1 ; *) (* one OVERLAY only *)

  (* polygons *)
  let max_poly = 255 in 
  let handle_poly offset = 
    let num_vertices = int_of_str_off buff (offset + 4) in
    if num_vertices >= max_poly then begin
      error "INFO" "WED: %s polygon has %d vertices, dropping some\n" 
        r num_vertices ; 
      write_int buff (offset + 4) max_poly 
    end 
  in 

  let offset = int_of_str_off buff 0x9c in
  let num = int_of_str_off buff 0x98 in
  for i = 0 to num - 1 do
    let offset = offset + (i * (0xe40c - 0xe3fa)) in 
    handle_poly offset ;
  done ; 
  (* also get the doors! *) 
  let offset = int_of_str_off buff 0x18 in
  let num = int_of_str_off buff 0xc in
  for i = 0 to num - 1 do
    let d_offset = offset + (i * (0xc6 - 0xac)) in 
    let handle_door_poly num offset = 
      for j = 0 to num - 1 do
        handle_poly (offset + (j * 18))
      done 
    in 
    handle_door_poly (short_of_str_off buff (d_offset + 0xba - 0xac)) 
      (int_of_str_off buff (d_offset + 0xbe - 0xac)) ;
    handle_door_poly (short_of_str_off buff (d_offset + 0xbc - 0xac)) 
      (int_of_str_off buff (d_offset + 0xc2 - 0xac)) ;
  done ;

  save_in_override new_r "WED" buff 
end 

(***********************************************************************
 * WMP Files
 ***********************************************************************)
and convert_wmp new_r r = begin
  let buff= load_source_res r "WMP" in 

  if String.sub buff 0 8 <> "WMAPV1.0" then begin
    failwith "not a valid WMAP V1.0 file (wrong sig)"
  end ; 

  let wmp = try read_wmp buff with e -> 
    log_and_print "ERROR: WMP: error reading %s\n" r ; exit 1 
  in 

  read_xnewarea wmp.(0) ; 

  let loadscrn = "None" (* convert "GTRSCRN" "MOS" *) in 

  let old_areaload = load_target_res "AREALOAD" "2DA" in 
  save_in_override_generic "AREALOAD" "2DA" 
    (fun oc -> 
      output_string oc old_areaload ; 
      Array.iter (fun a ->
	Array.iter (fun ae ->
          ae.wa_longname <- ae.wa_name ; 
          ae.wa_status <- ae.wa_status lor 4 ;
          (if ae.wa_status land 15 = 15 then ae.wa_status <- ae.wa_status - 15);
          let str = Printf.sprintf "%s\t%s\t%s\r\n" ae.wa_name loadscrn ae.wa_name 
          in
          output_string oc str ;
		   ) a.w_area_entry ; 
		 ) wmp ;
    ) ;

(*
  wmp.(0).w_area_entry <- Array.sub wmp.(0).w_area_entry 0 2 ;
  wmp.(0).w_area_link <- Array.sub wmp.(0).w_area_link 0 1 ;
 *)

  let new_wmp = 
    [| wmp.(0) ;
       { wmp.(0) with w_area_entry = [| |] ; } ;
       { wmp.(0) with w_area_entry = [| |] ; } ;
     |]
  in 

  let buff = try serialize_wmp new_wmp with _ -> 
    log_and_print "ERROR: WMP: error serializing %s\n" r ; exit 1 
  in 

  save_in_override new_r "WMP" buff ;
  save_in_override "WORLDMAP" "WMP" buff 
end 

and read_xnewarea wmp =
  try
    let t = load_2da (config.source) "XNEWAREA" in 
    List.iter (fun key ->
      let row, timefac = 
        if key = "-internal-" then begin
          [| "AR0043" ; "AR0043" ; "7" ; "16" ; 
             Printf.sprintf "%d" ((363 + 902) / 2) ;
             Printf.sprintf "%d" ((324 + 222) / 2) ;
             "16493" ;
             "16493" ; 
             "XL3000" ;
             "0" ; "0" ; "5" ; "0" ; "10" 
           |], 3
        end else (Hashtbl.find t key) , 1 
      in

      let xl = load_2da (config.source) row.(8) in  
      let xl_row = ref 0 in 

      let find_index_of str = 
        let res = ref 0 in
        for i = 0 to (Array.length wmp.w_area_entry) - 1 do
          if wmp.w_area_entry.(i).wa_name = str then
            res := i
        done ;
        !res
      in 

      let handle_xl str = 
        let amt = int_of_string str in
        Array.init amt (fun i -> 
          incr xl_row ; 
          let row = Hashtbl.find xl (Printf.sprintf "%d" !xl_row) in
          {
           wl_area_index = find_index_of (convert row.(0) "ARE") ;  
           wl_entry = row.(2) ;
           wl_time = (int_of_string row.(3)) / timefac ;
           wl_unknown = int_of_string row.(1) ;
           wl_rand1 = "*" ;
           wl_rand2 = "*" ;
           wl_rand3 = "*" ;
           wl_rand4 = "*" ;
           wl_rand5 = "*" ;
           wl_rand_prob = 0 ;
         } 
		       ) 
      in 
      let new_area_entry = {
        wa_name = convert row.(0) "ARE" ;
        wa_area = convert row.(1) "ARE" ;
        wa_longname = "XNEWAREA" ;
        wa_status = int_of_string row.(2) ;
        wa_bamseq = 
        begin 
          let img = int_of_string row.(3) in
          if img > 16 then 6 else img  
        end ;
        wa_x = int_of_string row.(4) ; 
        wa_y = int_of_string row.(5) ; 
        wa_name2 = convert_strref (int_of_string row.(6)) ;
        wa_name3 = convert_strref (int_of_string row.(7)) ;
        wa_loading = "None" ; 
        wa_north = handle_xl row.(9) ; 
        wa_west = handle_xl row.(10) ; 
        wa_south = handle_xl row.(11) ; 
        wa_east = handle_xl row.(12) ; 
      } in
      wmp.w_area_entry <- Array.append wmp.w_area_entry [| new_area_entry |];
      for i = 1 to (int_of_string row.(13)) do
        incr xl_row ; 
        let row = Hashtbl.find xl (Printf.sprintf "%d" !xl_row) in
        let new_al = 
          { 
            wl_area_index = (Array.length (wmp.w_area_entry)) - 1 ;
            wl_entry = row.(2) ;
            wl_time = int_of_string row.(3) ;
            wl_unknown = int_of_string row.(1) ;
            wl_rand1 = "*" ;
            wl_rand2 = "*" ;
            wl_rand3 = "*" ;
            wl_rand4 = "*" ;
            wl_rand5 = "*" ;
            wl_rand_prob = 0 ;
          } in
        (* these other areas now point to us! *) 
        let i = find_index_of (convert row.(0) "ARE") in

        wmp.w_area_entry.(i).wa_north <- Array.append 
            wmp.w_area_entry.(i).wa_north [| new_al |] ;

        wmp.w_area_entry.(i).wa_south <- Array.append 
            wmp.w_area_entry.(i).wa_south [| new_al |] ;

        wmp.w_area_entry.(i).wa_east <- Array.append 
            wmp.w_area_entry.(i).wa_east [| new_al |] ;

        wmp.w_area_entry.(i).wa_west <- Array.append 
            wmp.w_area_entry.(i).wa_west [| new_al |] ;

      done 

	      ) [ "1" ; "-internal-" ] 
  with _ -> () 

and read_wmp buff =
  (if (String.length buff < 16) then failwith "WMP is too small") ;
  let num_entry = int_of_str_off buff 0x8 in 
  let entry_off = int_of_str_off buff 0xc in 
  Array.init num_entry (fun i ->
    let off = entry_off + (i * 184) in 
    (* log_and_print "entry %d, off %d\n" i off ;  *)
    let num_ae = int_of_str_off buff (off + 0x20) in 
    let ae_off = int_of_str_off buff (off + 0x24) in 
    let al_off = int_of_str_off buff (off + 0x28) in 
    let num_al = int_of_str_off buff (off + 0x2c) in 

    let load_area_link o_first o_number = 
      let first = int_of_str_off buff o_first in
      let number = int_of_str_off buff o_number in
      Array.init number (fun j ->
        let off = al_off + ((first + j) * 216) in 
        (* log_and_print "WMP link entry %d, off %x\n" (first+j) off ;   *) 
        {
         wl_area_index = int_of_str_off buff (off) ;
         wl_entry = get_string_of_size buff (off + 0x4) 32 ;
         wl_time = int_of_str_off buff (off + 0x24) ;
         wl_unknown = int_of_str_off buff (off + 0x28) ; 
         wl_rand1 = convert_resref buff (off + 0x2c) "ARE" ;
         wl_rand2 = convert_resref buff (off + 0x34) "ARE" ;
         wl_rand3 = convert_resref buff (off + 0x3c) "ARE" ;
         wl_rand4 = convert_resref buff (off + 0x44) "ARE" ;
         wl_rand5 = convert_resref buff (off + 0x4c) "ARE" ;
         wl_rand_prob = int_of_str_off buff (off + 0x50) + 99 ; 
       } 
			) 
    in 
    {
     w_background = convert_resref buff (off+0x0) "MOS" ;
     w_width = int_of_str_off buff (off+0x8) ;
     w_height = int_of_str_off buff (off+0xc) ;
     w_unknown1 = int_of_str_off buff (off+0x10) ;
     w_name = convert_strref_of buff (off+0x14) ;
     w_unknown2 = int_of_str_off buff (off+0x18) ;
     w_unknown3 = int_of_str_off buff (off+0x1c) ;
     w_mapicons = convert_resref buff (off+0x30) "BAM" ; 
     w_area_entry = Array.init num_ae (fun j ->
       (* log_and_print "area entry %d, off %d\n" j off ;  *)
       let off = ae_off + (j * 240) in 
       {
        wa_name = convert_resref buff (off + 0x0) "ARE" ;
        wa_area = convert_resref buff (off + 0x8) "ARE" ;
        wa_longname = get_string_of_size buff (off+0x10) 32 ;
        wa_status = int_of_str_off buff (off + 0x30) ; 
        wa_bamseq = int_of_str_off buff (off + 0x34) ; 
        wa_x = int_of_str_off buff (off + 0x38) ; 
        wa_y = int_of_str_off buff (off + 0x3c) ; 
        wa_name2 = convert_strref_of buff (off + 0x40) ;
        wa_name3 = convert_strref_of buff (off + 0x44) ;
        wa_loading = convert_resref buff (off + 0x48) "MOS" ;
        wa_north = load_area_link (off + 0x50) (off + 0x54) ; 
        wa_west  = load_area_link (off + 0x58) (off + 0x5c) ; 
        wa_south = load_area_link (off + 0x60) (off + 0x64) ; 
        wa_east  = load_area_link (off + 0x68) (off + 0x6c) ; 
      } 
				      )  ;
   } 
		       ) 
(***********************************************************************
 * BG2 -> IWD2 hackery 
 ***********************************************************************)
and convert_bg2_iwd2_pdialog_interdia () = begin
  error "GAM" "[PDIALOG.2DA] Converting\n" ; 
  let s = load_2da (config.source) "PDIALOG" in 
  save_in_override_truly_generic "PDIALOG" "2DA" 
    [Open_wronly; Open_creat; Open_trunc; Open_text] 
    (fun oc -> 
      Printf.fprintf oc " 2DA V1.0\n multig\n\t\tPOST_DIALOG_FILE\tJOIN_DIALOG_FILE\n" ; 
      Hashtbl.iter (fun lhs arr ->
	if Array.length arr > 3 && lhs <> "" then begin
	  if Hashtbl.mem npc_banter_ht (String.uppercase lhs) then begin 
            let npc = Hashtbl.find npc_banter_ht (String.uppercase lhs) in 
            Hashtbl.add already_converted 
              ((String.uppercase lhs),"dream_script_of_var") (arr.(2)) ;
            let lhs = if String.length lhs > 8 then String.sub lhs 0 8 else lhs in
            npc.npc_pdialog <- String.uppercase arr.(1) ; 
            Printf.fprintf oc "%-12s%20s%20s\n" lhs 
              (rename npc.npc_cre_dlg "DLG")
              (rename npc.npc_cre_dlg "DLG") ;
            Hashtbl.add already_converted ((String.uppercase arr.(2)),"is_dream_script") lhs ; 
            Hashtbl.add already_converted 
              ((String.uppercase lhs),"dream_script_of_var") (arr.(2)) ;
	  end else begin
            error "GAM" "[PDIALOG.2DA] NPC %s not in GAM file, skipping\n" 
              lhs
	  end 
	end 
		   ) s ;
    ) ;
  error "GAM" "[INTERDIA.2DA] Converting\n" ; 
  let s = load_2da (config.source) "INTERDIA" in 
  Hashtbl.iter (fun lhs arr ->
    if Array.length arr > 0 then begin
      if Hashtbl.mem npc_banter_ht (String.uppercase lhs) then begin 
        let npc = Hashtbl.find npc_banter_ht (String.uppercase lhs) in 
        npc.npc_interdia <- arr.(0) ; 
      end else begin
        error "GAM" "[INTERDIA.2DA] NPC %s not in GAM file, skipping\n" 
          lhs
      end 
    end 
	       ) s
end 

(***********************************************************************
 * GAM Files
 ***********************************************************************)
and convert_gam new_r r = begin
  let buff= load_source_res r "GAM" in 

  (match config.source_variant with
  | BG2 -> if String.sub buff 0 8 <> "GAMEV2.0" then 
      failwith "not a valid GAME V2.0 file (wrong sig)"
  | BG1 -> if String.sub buff 0 8 <> "GAMEV1.1" then 
      failwith "not a valid GAME V1.1 file (wrong sig)"
  | _ -> failwith "unsupported variant" ) ;

  let npc_offset = int_of_str_off buff 0x30 in
  let num_npc = int_of_str_off buff 0x34 in

  for i = 0 to num_npc - 1 do 
    try 
      let off = npc_offset + (i * 352) in 
      let r = get_string_of_size buff (off + 0xc0 - 0xb4) 8 in
      let orig_are = get_string_of_size buff (off + 0xcc - 0xb4) 8 in 
      let x = short_of_str_off buff (off + 0xd4 - 0xb4) in 
      let y = short_of_str_off buff (off + 0xd6 - 0xb4) in 
      let new_r = rename r "CRE" in 
      error "GAM" "[%s.CRE] considering\n" r ; 
      let new_bcs = rename orig_are "BCS" in 
      Hashtbl.replace already_converted (r,"CRE") new_r ; 
      let x = Int32.of_int x in
      let y = Int32.of_int y in

      let buff = load_source_res r "CRE" in 
      let variable = get_string_of_size buff 0x280 16 in 
      let variable = convert variable "variable" in 
      let script = get_string_of_size buff 0x248 8 in
      let script = String.uppercase script in 
      let dlg = get_string_of_size buff 0x2cc 8 in 
      let dlg = String.uppercase dlg in 
      error "GAM" "[%s.CRE] %s.variable %s.script %s.dlg\n"
        r variable script dlg ;
      Hashtbl.add npc_banter_ht (String.uppercase variable)
        { npc_variable = variable ; 
          npc_cre_dlg = dlg ;
          npc_pdialog = "" ;
          npc_interdia = "" } ; 
      Hashtbl.add already_converted (script,"is_npc_script") variable ;
      Hashtbl.add area_npc_ht (new_bcs) (r,new_r,variable,x,y) ; 
    with _ -> () 
  done ;

  (* BG2 -> IWD2 -- figure out how to merge banter files *) 
  (match config.source_variant, config.target_variant with
  | BG2, IWD2 -> 
      convert_bg2_iwd2_pdialog_interdia () ;
      error "GAM" "[PDIALOG.2DA] [INTERDIA.2DA] Converted\n" ; 
      Hashtbl.iter (fun var npc ->
	error "GAM" "%s.variable: considering\n" var ; 
	if npc.npc_pdialog <> "" then begin
          let res = rename npc.npc_cre_dlg "DLG" in 
          (if (String.uppercase npc.npc_cre_dlg) <> "NONE" && 
            npc.npc_cre_dlg <> "" then Hashtbl.add already_converted (npc.npc_cre_dlg,"DLG") res );
          (if (String.uppercase npc.npc_pdialog) <> "NONE" &&
            npc.npc_pdialog <> "" then Hashtbl.add already_converted (npc.npc_pdialog,"DLG") res );
          (if (String.uppercase npc.npc_interdia) <> "NONE" &&
            npc.npc_interdia <> "" then Hashtbl.add already_converted (npc.npc_interdia,"DLG") res );
          let cre_dlg_buff = load_source_res npc.npc_cre_dlg "DLG" in 
          let cre_dlg = Dlg.load_dlg npc.npc_cre_dlg cre_dlg_buff in 
          let cre_dlg_num = Array.length cre_dlg.Dlg.state in 
          Hashtbl.add merged_dlg_ht npc.npc_pdialog (res,cre_dlg_num) ; 
          let pdlg_buff = load_source_res npc.npc_pdialog "DLG" in 
          let pdlg = Dlg.load_dlg npc.npc_pdialog pdlg_buff in 
          let pdlg_num = Array.length pdlg.Dlg.state in 
          if (npc.npc_interdia <> "") then begin 
            Hashtbl.add merged_dlg_ht npc.npc_interdia (res,cre_dlg_num + pdlg_num);
          end ; 
          error "GAM" "%s.variable: count '%s' %d '%s' %d '%s'\n" var 
            npc.npc_cre_dlg cre_dlg_num npc.npc_pdialog 
            (cre_dlg_num + pdlg_num) npc.npc_interdia ; 
	end 
		   ) npc_banter_ht 

  | _ -> () ) ; 

  for i = 0 to num_npc - 1 do 
    try 
      let off = npc_offset + (i * 352) in 
      let r = get_string_of_size buff (off + 0xc0 - 0xb4) 8 in
      (* Hashtbl.remove  already_converted (r,"variable_of_cre") ;  *)
      let new_r = Hashtbl.find already_converted (r,"CRE") in 

      let buff = load_source_res r "CRE" in 

      if String.sub buff 0 8 <> "CRE V1.0" then begin
        failwith "not a valid CRE 1.0 file (wrong sig)"
      end ; 

      (match config.source_variant, config.target_variant with
      | BG2, IWD2 -> 
          let cre = try read_bg2_cre new_r r buff with e -> 
            error "GAM" "[%s.CRE] error reading BG2 CRE\n" r ; 
            raise e 
          in

          (try mutate_cre new_r r cre true with e -> 
            error "GAM" "[%s.CRE] error mutating CRE\n" r ; 
            raise e) ;

          cre.c_ea <- 128 ; (* Neutral *) 
          cre.c_script <- List.filter (fun r ->
            if r = "None" then false else 
            try let _ = String.index r 'X' in 
            error "INFO" "GAM: ~%s.BCS~ [ ~None.bcs~ ]\n" r ; 
            false
            with Not_found -> true 
				      ) cre.c_script ;

          while (List.length cre.c_script) < 5 do
            cre.c_script <- "None" :: cre.c_script 
          done ; 

          let premade_buff = load_file 
              (config.premade_npc_directory ^ "/" ^ r ^ ".cre") in 

          let out_buff = prepare_iwd2_bg2_cre cre premade_buff in 

          save_in_override new_r "CRE" out_buff 

      | BG1, BG2 -> ()
      | _ -> () 
      ) 
    with _ -> () 
  done ;

  (* BG2 -> IWD2 -- actually merge banter files *) 
  (match config.source_variant, config.target_variant with
  | BG2, IWD2 -> 
      convert_bg2_iwd2_pdialog_interdia () ;
      Hashtbl.iter (fun var npc ->
	let res = rename npc.npc_cre_dlg "DLG" in 
	try 
	  if npc.npc_pdialog <> "" && npc.npc_cre_dlg <> "" then begin
            let cre_dlg_buff  = load_source_res npc.npc_cre_dlg "DLG" in 
            let pdlg_buff     = load_source_res npc.npc_pdialog "DLG" in 
            let interdia_buff = 
              try load_source_res npc.npc_interdia "DLG" 
              with _ -> "" 
            in 
            let pos_trig = Printf.sprintf "InParty(Myself)\n" in
            let neg_trig = Printf.sprintf "!InParty(Myself)\n" in

            error "GAM" "%s.variable: merging '%s' '%s' '%s' -> %s\n" 
              var 
              npc.npc_cre_dlg npc.npc_pdialog npc.npc_interdia res ; 

            if interdia_buff <> "" then 
              convert_dlg_internal res
		[ (npc.npc_cre_dlg,  cre_dlg_buff,  neg_trig) ;
		  (npc.npc_pdialog,  pdlg_buff,     pos_trig) ;
		  (npc.npc_interdia, interdia_buff, pos_trig) ]  
            else 
              convert_dlg_internal res
		[ (npc.npc_cre_dlg,  cre_dlg_buff,  neg_trig) ;
		  (npc.npc_pdialog,  pdlg_buff,     pos_trig) ] 
	  end 
        with e -> 
          error "GAM" "%s.variable: error merging '%s' '%s' '%s' -> %s: %s\n" 
            var 
            npc.npc_cre_dlg npc.npc_pdialog npc.npc_interdia res 
            (Printexc.to_string e); 
		   ) npc_banter_ht 

  | _ -> () ) ; 

  let main_area = convert_resref buff 0x40 "ARE" in 

(*
  let igam = load_target_res "ICEWIND2" "GAM" in 
  write_int igam 0x30 (String.length igam) ; 
  write_int igam 0x34 1 ;
  let npcbuf = String.make (832) '\000' in
  write_short npcbuf 0x2 0xffff ;
  let yoshi_off = (String.length igam) + (String.length npcbuf) in 
  let yoshi_buff = load_target_res "_YOSHI7" "CRE" in 
  write_int npcbuf 4 yoshi_off ;
  write_int npcbuf 8 (String.length yoshi_buff) ;
  (* write_resref npcbuf 0xc "_YOSHI7" ; *)
  write_resref npcbuf 0x18 "AR1000" ;
  write_short npcbuf 0x20 2490;
  write_short npcbuf 0x22 860;
  write_resref npcbuf (0xa46 - 0x888) "Yoshimo" ;
  let final_buff = (igam ^ npcbuf) ^ yoshi_buff in
  save_in_override "ICEWIND2" "GAM" final_buff ;
 *)
  () 

end 


(***********************************************************************
 * ARE Files
 ***********************************************************************)
and convert_are_counter = ref 0 
and convert_are new_r r = begin
  let buff= load_source_res r "ARE" in 

  if String.sub buff 0 8 <> "AREAV1.0" then begin
    failwith "not a valid ARE 1.0 file (wrong sig)"
  end ; 

  let orig_wed = get_string_of_size buff 8 8 in 
  (* In IWD2, the WED, HT, LM, SR and MOS files must all have the same 
   * name as the area! *) 
  let converted_wed = convert orig_wed "WED" in  
  if converted_wed <> new_r && config.target_variant = IWD2 then begin
    let converted_wed_path = Printf.sprintf "%s/%s.WED" !override 
	converted_wed in
    let copy_wed_path = Printf.sprintf "%s/%s.WED" !override new_r in
    copy_one_file converted_wed_path copy_wed_path
  end ; 
  write_resref buff 8 new_r ; 

(*
  let _ = convert (orig_wed^"HT") "BMP" in
  let _ = convert (orig_wed^"LM") "BMP" in
  let _ = convert (orig_wed^"SR") "BMP" in
  let _ = convert (orig_wed) "MOS" in
 *)
  begin
    log_and_print "ARE: Making %sHT.BMP, etc.\n" new_r ; 
    add_to_key (new_r^"HT") (orig_wed^"HT") "BMP" ;
    add_to_key (new_r^"LM") (orig_wed^"LM") "BMP" ;
    add_to_key (new_r^"SR") (orig_wed^"SR") "BMP" ;
    add_to_key (new_r)      (orig_wed) "MOS" ;
    add_to_key (new_r^"N")      (orig_wed^"N") "MOS" ;
    add_to_key (new_r^"N")      (orig_wed^"N") "TIS" ;
  end ;

  (* first, add in the extra 16 bytes after unknown but before actors *)
  (match config.target_variant with
  | IWD2 -> String.blit "AREAV9.1" 0 buff 0 8 
  | BG2 -> String.blit "AREAV1.0" 0 buff 0 8 
  | _ -> failwith "unsupported target variant" ) ; 
  let before = Str.string_before buff 0x54 in
  let after = Str.string_after buff 0x54 in 
  let junk = String.make 16 (Char.chr 0) in
  junk.[0] <- Char.chr 2 ;
  junk.[1] <- Char.chr 3 ;
  let buff = (before ^ junk) ^ after in 

  let flags = short_of_str_off buff 0x48 in
  let flags = if flags land 0x40 = 0x40 then (* extended night! *) 
    flags - 0x40 else flags in
  write_short buff 0x48 flags ; 

  (* handle the header *) 
  List.iter (fun (o,e) -> patch_resref buff o e) [ 
  (0x18, "ARE") ;
  (0x24, "ARE") ;
  (0x30, "ARE") ;
  (0x3c, "ARE") ; 
  ] ; 

  let script = get_string_of_size buff 0xa4 8 in
  let script = convert script "BCS" in
  write_resref buff 0xa4 script ; 
  let new_bcs = rename r "BCS" in 
  if (script = "None" || script = "" )
      && Hashtbl.mem area_npc_ht new_bcs then begin
	(* we must invent this script! *) 
	let dst = (config.source).Load.game_path ^ "/override/" ^ r ^ ".BCS" in 
	copy_one_file  config.blank_script_bcs dst ; 
	Hashtbl.remove already_converted (r,"BCS") ; 
	let script = convert r "BCS" in 
	write_resref buff 0xa4 script ;
      end ; 

  (* incremenet all of the offsets by 16 *)
  List.iter (fun o -> let cur = int_of_str_off buff o in
  String.blit (str_of_int (cur+16)) 0 buff o 4) [
  0x64 ; (* actors *) 
    0x6c ; (* info points *) 
    0x70 ; (* spawn *) 
    0x78 ; (* entrances *) 
    0x80 ; (* containers *) 
    0x88 ; (* items *) 
    0x8c ; (* vertices *) 
    0x94 ; (* ambients *) 
    0x98 ; (* variables *) 
    0xb0 ; (* explored bitmap *) 
    0xb8 ; (* doors *) 
    0xc0 ; (* animations *) 
    0xc8 ; (* tiled objects *) 
    0xcc ; (* tiled objects *) 
    0xd0 ; (* rest spawn *) 
    0xd4 ; (* automap *) 
  ] ; 

  (* handle the actors *) 
  let offset = int_of_str_off buff 0x64 in 
  let num = short_of_str_off buff 0x68 in
  for i = 0 to num - 1 do 
    let offset = offset + (i * 272) in 
    patch_resref buff (offset + 0x284 - 0x23c) "DLG" ;
    for j = 1 to 6 do 
      patch_resref buff ((offset + 0x284 - 0x23c) + (j * 8)) "BCS" ;
    done;
    let orig_cre = get_string_of_size buff (offset + 0x2bc - 0x23c) 8 in 
    patch_resref buff (offset + 0x2bc - 0x23c) "CRE" ;

    patch_resref buff (offset + 16 + 0x2bc - 0x23c) "BCS" ;

    write_byte buff (offset + 0x154 - 0x12c) 1 ;  (* visible *)
    write_int buff (offset + 0xbf8 - 0xbcc) 0x07000000 ;  (* really visible *)
    write_int buff (offset + 0xbfc - 0xbcc) 0x0000e408 ;  (* really visible *)

    let bigname = get_string_of_size buff offset 32 in 
    error "ARE" "%s.ARE has actor [%s] [%s]\n" new_r bigname orig_cre ;

    (* fixup the variable *) 
    let variable = convert orig_cre "variable_of_cre" in 
    if variable <> "None" then 
      String.blit (str_to_exact_size variable 16) 0 buff offset 16  ;
  done ;

  (* info points *)
  let num = short_of_str_off buff 0x6a in
  let offset = int_of_str_off buff 0x6c in 
  for i = 0 to num - 1 do
    let offset = offset + (i * (0x23e0 - 0x231c)) in 
    patch_resref buff (offset + 0x2354 - 0x231c) "ARE" ;
    patch_strref buff (offset + 0x2380 - 0x231c) ; 
    patch_diff_short   buff (offset + 0x2384 - 0x231c) ;
    patch_diff_short   buff (offset + 0x2386 - 0x231c) ;
    let script_resref = get_string_of_size buff (offset + 0x2398 - 0x231c) 8 in
    Hashtbl.replace infopoint_bcs_ht script_resref true ;
    patch_resref buff (offset + 0x2398 - 0x231c) "BCS" ;
    patch_resref buff (offset + 0x23d8 - 0x231c) "DLG" ;
    let kind = short_of_str_off buff (offset + 0x35ac - 0x358c) in
    if kind = 2 then begin
      let flag = int_of_str_off buff (offset + 0x35ec - 0x358c) in
      if flag land 0x0200 = 0x0200 then 
        write_int buff (offset + 0x35ec - 0x358c) (flag - 0x0200)
	  (* log_and_print "ERROR: %s.ARE %s = %x\n" r name flag ;  *)
    end 
  done ;

  (* spawn points *)
  let num = int_of_str_off buff 0x74 in
  let offset = int_of_str_off buff 0x70 in 
  for i = 0 to num - 1 do
    let offset = offset + (i * (0x40fc - 0x4034)) in 
    (* handle random spawns! *)
    write_short buff (offset + 0x1340 - 0x12c8) 65537 ; (* spawn time *)
    for j = 0 to 9 do 
      let first = get_string_of_size buff ((offset + 0x4058 - 0x4034)+(j*8)) 8 in
      let first = String.uppercase first in
      (*
	if Hashtbl.mem random_spawn_ht first then begin
        let new_first = convert first "random_spawn" in 
        write_resref buff (((offset + 0x4058 - 0x4034))+(j*8)) new_first 
	end else  *)
      patch_resref buff ((offset + 0x4058 - 0x4034) + (j*8)) "CRE" ;
    done 
  done ;

  (* containers *)
  let num = short_of_str_off buff 0x84 in
  let offset = int_of_str_off buff 0x80 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0x492c - 0x486c)*i) in
    patch_resref buff (offset + (0x60c4 - 0x607c)) "BCS" ;
    patch_resref buff (offset + (0xb0c - 0xa94)) "ITM" ; (* KEY *) 
    patch_diff_short buff (offset + 0x4892 - 0x486c) ; (* lock *) 
    patch_diff_short buff (offset + 0x4898 - 0x486c) ; (* trap *) 
    patch_diff_short buff (offset + 0x489a - 0x486c) ; (* trap *) 
  done ;

  (* items *)
  let num = short_of_str_off buff 0x86 in
  let offset = int_of_str_off buff 0x88 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0x6800 - 0x67ec)*i) in
    patch_resref buff (offset) "ITM" ;
  done;

  (* ambients *)
  let cut_big_ambients = true in 

  let num = short_of_str_off buff 0x92 in
  let offset = int_of_str_off buff 0x94 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0x7dbc - 0x7ce8)*i) in
    let num_sounds = short_of_str_off buff (offset + 0x414c - 0x40cc ) in
    let sounds_to_use = int_of_str_off buff (offset + 0x7520 - 0x74a0 ) in
    let radius = short_of_str_off buff (offset + 0x74c4 - 0x74a0 ) in
    let freq = short_of_str_off buff (offset + 0x74c6 - 0x74a0 ) in
    let volu = short_of_str_off buff (offset + 0x74ce - 0x74a0 ) in
    for j = 0 to 9 do 
      let old = get_string_of_size buff ((offset + 0x7d18 - 0x7ce8) + (j*8)) 8 
      in
      (*
	if String.uppercase old <> "NONE" && old <> "" then begin
        let key = config.source.Load.key in 
        let bif_name, bif_index, file_size = 
        let res = find_resource key old "WAV" in
        let bif = key.biff.(res.bif_index) in
        let filename = Str.global_replace other_path_separators "/" bif.filename in
        (filename,res.other_index,bif.length) in
        let bif = Load.load_bif_in_game config.source bif_name in 
        error "ARE" "%s ambient [%8s.WAV] %6d %s %8d %3d %3d %3d %d %d\n" 
        new_r old 
        bif.Biff.files.(bif_index).Biff.res_size
        bif_name file_size 
        radius freq volu num_sounds sounds_to_use
        ;
	end ; 
       *)
      patch_resref buff ((offset + 0x7d18 - 0x7ce8) + (j*8)) "WAV" ;
    done 
  done ; 

  (* doors *)
  let num = int_of_str_off buff 0xb4 in
  let offset = int_of_str_off buff 0xb8 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0x8948 - 0x8880) * i) in
    patch_resref buff (offset + 0x89a0 - 0x8948) "WAV" ;
    patch_resref buff (offset + 0x89a8 - 0x8948) "WAV" ;

    patch_diff_short buff (offset + 0x89b4 - 0x8948) ;
    patch_diff_short buff (offset + 0x89b6 - 0x8948) ;
    patch_diff_int buff (offset + 0x89d4 - 0x8948) ;

    patch_resref buff (offset + 0x89c0 - 0x8948) "ITM" ;
    patch_resref buff (offset + 0x89c8 - 0x8948) "BCS" ;
    patch_strref buff (offset + 0x89e0 - 0x8948) ;
    patch_strref buff (offset + 0x89fc - 0x8948) ;
    patch_resref buff (offset + 0x8a00 - 0x8948) "DLG" ;
  done ; 

  (* animations *)
  let num = int_of_str_off buff 0xbc in
  let offset = int_of_str_off buff 0xc0 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0xaf2c - 0xaee0) * i) in
    patch_resref buff (offset + 0xaf54 - 0xaf2c) "BAM" ;
  done ; 

  (* rest spawn *)
  let offset = int_of_str_off buff 0xd0 in 
  for i = 0 to 9 do 
    let offset = offset + 0xb878 - 0xb858 + (i * 4) in
    patch_strref buff offset ; 
    let offset = offset + 0xb8a0 - 0xb858 + (i * 8) in
    patch_resref buff offset "CRE" ;
  done ;

  (* automap *)
  let num = int_of_str_off buff 0xd8 in
  let offset = int_of_str_off buff 0xd4 in 
  for i = 0 to num - 1 do
    let offset = offset + ((0xb970 - 0xb93c) * i) in
    patch_strref buff (offset + 0x4) ;
  done ; 

  let descriptor = [
    (* OFF, S, Number,    size *)
    ( 0x64, 4, Some(0x68,2), 272 ) ; (* actors *) 
    ( 0x6c, 4, Some(0x6a,2), (0x730 - 0x66c) ) ; (* info *) 
    ( 0x70, 4, Some(0x74,4), (0x1064 - 0xf9c) ) ; (* spawn *) 
    ( 0x78, 4, Some(0x7c,4), (0x125c - 0x11f4) ) ; (* entr *) 
    ( 0x80, 4, Some(0x84,2), (0x1384 - 0x12c4) ) ; (* cont *) 
    ( 0x88, 4, Some(0x86,2), (0x18 - 0x4) ) ; (* item *) 
    ( 0x8c, 4, Some(0x90,2), 4 ) ; (* vert *) 
    ( 0x94, 4, Some(0x92,2), (0x1aa8 - 0x19d4) ) ; (* amb *) 
    ( 0x98, 4, Some(0x9c,4), 100 ) ; (* var: FIXME *) 
    ( 0xb0, 4, Some(0xac,4), 1   ) ; (* bitmap *) 
    ( 0xb8, 4, Some(0xb4,4), (0x4bfc - 0x4b34) ) ; (* door *) 
    ( 0xc0, 4, Some(0xbc,4), (0x5c08 - 0x5bbc) ) ; (* anim *) 
    ( 0xc8, 4, Some(0xc4,4), 100 ) ; (* tiled: FIXME *) 
    ( 0xcc, 4, None        , (0x1c48 - 0x1bb8) ) ; (* song *) 
    ( 0xd0, 4, None        , (0x1d2c - 0x1c48) ) ; (* rest spawn *) 
    ( 0xd4, 4, Some(0xd8,4), (0xb980 - 0xb94c) ) ; (* automap notes *) 
  ] in

  let spawns = ref [] in
  let num = int_of_str_off buff 0x74 in
  let offset = int_of_str_off buff 0x70 in 
  for i = 0 to num - 1 do
    let offset = offset + (i * (0x40fc - 0x4034)) in 
    (* handle random spawns! *)
    let big_name = get_string_of_size buff (offset) 32 in 
    let x = short_of_str_off buff (offset + 32) in
    let y = short_of_str_off buff (offset + 34) in
    let spawns_here = ref [] in 
    for j = 0 to 9 do 
      let cre_name = get_string_of_size buff 
          ((offset + 0x4058 - 0x4034)+(j*8)) 8 in
      if cre_name <> "None" && cre_name <> "" then 
        spawns_here := (cre_name) :: !spawns_here 
    done ;
    if (List.length !spawns_here > 1) then begin
      (* make up a fake CRE to handle this *) 
      let hack_name = convert_random_spawn_list !spawns_here in 
      spawns := (big_name,x,y,hack_name) :: !spawns
    end else begin (* 0 or 1 valid spawns *) 
      List.iter (fun cre ->
        spawns := (big_name,x,y,cre) :: !spawns
		) !spawns_here 
    end 
  done ;
  write_int buff 0x74 0 ; (* no more spawn points! *) 
  let spawn_buff = Buffer.create ((List.length !spawns) * 272) in
  List.iter (fun (big_name, x, y, cre_name) ->
    (* log_and_print "[%s] %d %d [%s]\n" big_name x y cre_name ;  *)
    let buff = String.make 272 '\000' in 
    String.blit big_name 0 buff 0 (String.length big_name) ;
    write_short buff 32 x ;
    write_short buff 34 y ;
    write_short buff 36 x ;
    write_short buff 38 y ;
    write_byte buff (0x154 - 0x12c) 1 ;  (* visible *)
    write_int buff (0xbf8 - 0xbcc) 0x07000000 ;  (* really visible *)
    write_int buff (0xbfc - 0xbcc) 0x0000e408 ;  (* really visible *)
    write_short buff (0x17a4 - 0x176c) 0xffff;
    write_short buff ((0x17a4 - 0x176c)+2) 0xffff;
    write_short buff (0x17ac - 0x176c) 0xffff;
    write_short buff ((0x17ac - 0x176c)+2) 0xffff;
    write_resref buff (0x17ec - 0x176c) cre_name ;
    Buffer.add_string spawn_buff buff 
	    ) !spawns ;

  let contents = List.map (fun (off, off_size, num_opt, size_per) ->
    let variable_read where size =
      match size with
      | 4 -> int_of_str_off buff where 
      | 2 -> short_of_str_off buff where
      | 1 -> byte_of_str_off buff where
      | _ -> failwith "are variable read"
    in 
    let actual_off = variable_read off off_size in 
    (*
      log_and_print "Actual_off = read 0x%x %d = 0x%x\n" 
      off off_size actual_off ;  
     *)
    let actual_num = match num_opt with
      Some(where,size) -> variable_read where size
    | None -> 1
    in 
    let chunk = 
      if (actual_num * size_per) = 0 then ""
      else try String.sub buff actual_off (actual_num * size_per)
      with _ -> (log_and_print "String.sub |%d| 0x%x %d*%d\n" 
		   (String.length buff) 
		   actual_off
		   actual_num size_per ; exit 1)
    in 
    (off, off_size, num_opt, actual_num, chunk) 
			  ) descriptor in 

  let (off,off_size,num_opt, actual_num, chunk) = List.hd contents in
  let c_t = List.tl contents in
  let contents = (off,off_size,num_opt,actual_num + (List.length !spawns),
		  (chunk ^ (Buffer.contents spawn_buff))) :: c_t in 

  let payload_size = List.fold_left (fun acc
      (off,off_size,num_opt,actual_num,chunk) -> 
	acc + (String.length chunk)) 0 contents in
  let header_size = if config.target_variant = IWD2 then 0x12c 
  else 0x12c - 16 in 
  let total_size = header_size + payload_size in 

  let new_buff = String.make total_size '\000' in  
  (match config.target_variant with
    IWD2 -> String.blit buff 0 new_buff 0 header_size ;
  | _ -> String.blit buff 0 new_buff 0 0x54 ;
      String.blit buff (0x54+16) new_buff 0x54 (header_size - 0x54)
  ) ; 
  let current_offset = ref header_size in 
  List.iter (fun (off,off_size,num_opt,actual_num,chunk) ->
    let variable_write where size what =
      let where = if config.target_variant = BG2 then where - 16 else where
      in 
      match size with
      | 4 -> write_int new_buff where what
      | 2 -> write_short new_buff where what
      | 1 -> write_byte new_buff where what
      | _ -> failwith "are variable write"
    in 
    variable_write off off_size !current_offset ;
    (match num_opt with
      None -> ()
    | Some(where,size) -> variable_write where size actual_num) ;
    let chunk_size = String.length chunk in 
    String.blit chunk 0 new_buff !current_offset chunk_size ;
    current_offset := !current_offset + chunk_size;
	    )  contents ; 

  (* save_in_override "_BIGTEST" "ARE" new_buff ; *)
  save_in_override new_r "ARE" new_buff ;

end 

(***********************************************************************
 * Testing Hackery
 ***********************************************************************)
let eff_list_hack () = begin
  let r = "sw1h04" in 
  let new_r = "_effhack" in 
  let buff = load_source_res r "ITM" in 

  if String.sub buff 0 8 <> "ITM V1  " then begin
    failwith "not a valid ITM V1 file (wrong sig)"
  end ; 

  let i = try read_itm buff with e -> 
    error "ITM" "[%s.ITM] error reading BG2 ITM: %s\n" r 
      (Printexc.to_string e); raise e in

  let spl_buff = load_target_res "effblud1" "SPL" in 
  write_short spl_buff 0xaa 139 ;
  write_byte spl_buff 0xac 2 ;
  write_byte spl_buff 0xad 0 ;
  write_byte spl_buff 0xbc 100 ;

  (try 
    mutate_itm new_r r i 
  with
    e -> error "ITM" "[%s.ITM] error mutating ITM: %s\n" r 
	(Printexc.to_string e); raise e);

  i.i_abil.(0).a_eff <- Array.init 93 (fun num ->
    let num = num + 0 in 
    let res = Printf.sprintf "__EFF%03d" num in
    write_int spl_buff 0xae (debug_string 
			       (Printf.sprintf "AEL Cat %03d" num)) ;
    save_in_override res "SPL" spl_buff ; 
    { empty_eff with e_opcode = target_opcode "Add_effects_list" ;
      e_target = 2 ;
      e_arg2 = num ;
      e_duration = 1; 
      e_prob1 = 100;
      e_resref = res;
    } 
				      )  ;

  let buff = serialize_itm i in
  save_in_override new_r "ITM" buff 
end 

let convert_baldur_bcs () = 
  if !option_bcs_none then 
    ()
  else begin 
    let args = {
      cb_is_cutscene = false ;
      cb_is_infopoint = false ; 
    } in 
    try begin
      let prepend = match config.baldur_bcs_prepend with
        "" -> []
      | prepend_name -> 
          let prepend_buff = load_file prepend_name in 
          let lexbuf = lex_init_from_string prepend_name prepend_buff in 
          Stats.time "parsing .BCS"  
            (fun () -> Bcsparser.bcs_file Bcslexer.initial lexbuf) () 
      in 
      ignore (convert_bcs "BALDUR" "BALDUR" args prepend)
    end with e -> begin
      error "BCS" "Error converting BALDUR.BCS!\n" ;
      raise e
    end 
  end 

(***********************************************************************
 * 2DA Files - CHAPTERS.2DA
 *
 * in IWD2: 
 * guichp0b.mos
 * guichp1b.mos
 * guichp2b.mos
 * ...
 * guichp6b.mos
 *
 * ch01ajon.mos
 * ch02agln.mos
 * ch03amon.mos
 * ch04ashp.mos
 * ch04csah.mos
 * ch05aund.mos
 * ch06adrw.mos
 * ch07asul.mos
 * chapsara.mos
 * chptamke.mos
 * chptmana.mos
 ***********************************************************************)
let convert_chapters () = begin
  let name_filename = config.tra_directory ^ "/chapter-name.tra" in 
  let desc_filename = config.tra_directory ^ "/chapter-desc.tra" in 
  let names = parse_tra name_filename in 
  let desc = parse_tra desc_filename in 
  save_in_override_truly_generic "CHAPTERS" "2DA" 
    [Open_wronly; Open_creat; Open_trunc; Open_text] 
    (fun oc -> 
      Printf.fprintf oc "2DA V1.0\nNONE\n\t0\t1\t2\nTICKS\t100\n" ;
      let i = ref 1 in
      let ok = ref true in 
      while !ok do
	if List.mem_assoc !i names && List.mem_assoc  !i desc then begin
	  let name_strref = convert_tlk_string_to_idx (List.assoc !i names) in
	  let desc_strref = convert_tlk_string_to_idx (List.assoc !i desc) in
	  Printf.fprintf oc "%d\t%d\t%d\t15473\n" !i name_strref desc_strref;
	  incr i; 
	end else ok := false 
      done ;
    )
end 

(* japh: CHPTXT* - These are all different.  We'll just need the strref's
   patched to their updated value. *)
(* Weimer hack: treat all numbers >= 10,000 as strrefs. *) 
let convert_2da_strref filename = begin
  let buff, _ = Load.load_resource "weimorph" config.source true filename "2DA" in  
  let lines = Str.split newline_regexp buff in 
  let res_buff = Buffer.create 4096 in  
  List.iter (fun (line : string) ->
    let words = Str.split_delim blank_regexp line in 
    List.iter (fun word ->
      let new_word = try
        let i = int_of_str word in
        if i >= 10000 (* HACK *) then 
          let i' = convert_strref i in
          Printf.sprintf "%d" i' 
        else word
      with _ -> word
      in 
      Buffer.add_string res_buff new_word ;
      Buffer.add_char res_buff ' ' ;
	      ) words ;
    Buffer.add_char res_buff '\n' ;
	    ) lines; 
  save_in_override filename "2DA" (Buffer.contents res_buff)  
end 

let handle_innate_abilities () =
  begin
    let s = load_target_res "LISTINNT" "2DA" in 
    save_in_override_truly_generic "LISTINNT" "2DA" 
      [Open_wronly; Open_creat; Open_trunc; Open_text] 
      (fun oc ->
	output_string oc s ;
	let i = ref 500 in 
	Hashtbl.iter (fun k v -> 
          Printf.fprintf oc "%d %s\r\n" !i k ;
          incr i 
		     ) innate_ability_ht ;
      ) 
  end 

type rl_cost = Fixed of int
  | Var
  | VarP of int
  | Var5
  | Var10
  | VarImmune
type rl_dur  = Perm
  | Limited of int 
  | Equipped
type disres  = NoRes
  | NoSave
  | TargSelf
  | ProbDirect (* 50% happening = 50% cost *) 
  | CastSpell
  | SummonCreature
  | Damage (* acid, fire, elec, etc. *) 

(* weimer: randomize weapons, roguelike *) 
let rl () = begin

  let spell_lst = [|
    "SPPR105"; (*  [~Entangle~]*)
    "SPPR113"; (*  [~Doom~]*)
    "SPPR302"; (*  [~Call Lightning~]*)
    "SPPR304"; (*  [~Glyph of Warding~]*)
    "SPPR309"; (*  [~Invisibility Purge~]*)
    "SPPR310"; (*  [~Miscast Magic~]*)
    "SPPR311"; (*  [~Rigid Thinking~]*)
    "SPPR313"; (*  [~Holy Smite~]*)
    "SPPR314"; (*  [~Unholy Blight~]*)
    "SPPR319"; (*  [~Summon Insects~]*)
    "SPPR405"; (*  [~Mental Domination~]*)
    "SPPR411"; (*  [~Poison~]*)
    "SPPR416"; (*  [~Cloak of Fear~]*)
    "SPPR503"; (*  [~Flame Strike~]*)
    "SPPR505"; (*  [~True Seeing~]*)
    "SPPR512"; (*  [~Greater Command~]*)
    "SPPR515"; (*  [~Repulse Undead~]*)
    "SPPR517"; (*  [~Insect Plague~]*)
    "SPPR609"; (*  [~False Dawn~]*)
    "SPPR610"; (*  [~Dolorous Decay~]*)
    "SPPR612"; (*  [~Bolt of Glory~]*)
    "SPPR704"; (*  [~Nature's Beauty~]*)
    "SPPR705"; (*  [~Fire Storm~]*)
    "SPPR706"; (*  [~Symbol, Fear~]*)
    "SPPR707"; (*  [~Sunray~]*)
    "SPPR708"; (*  [~Finger of Death~]*)
    "SPPR709"; (*  [~Confusion~]*)
    "SPPR710"; (*  [~Holy Word~]*)
    "SPPR715"; (*  [~Unholy Word~]*)
    "SPPR717"; (*  [~Creeping Doom~]*)
    "SPPR718"; (*  [~Symbol, Stun~]*)
    "SPPR719"; (*  [~Symbol, Death~]*)
    "SPWI106"; (*  [~Blindness~]*)
    "SPWI112"; (*  [~Magic Missile~]*)
    "SPWI118"; (*  [~Chromatic Orb~]*)
    "SPWI119"; (*  [~Larloch's Minor Drain~]*)
    "SPWI125"; (*  [~Spook~]*)
    "SPWI203"; (*  [~Detect Invisibility~]*)
    "SPWI215"; (*  [~Web~]*)
    "SPWI211"; (*  [~Acid Arrow~]*)
    "SPWI221"; (*  [~Ray of Enfeeblement~]*)
    "SPWI223"; (*  [~Deafness~]*)
    "SPWI224"; (*  [~Glitterdust~]*)
    "SPWI302"; (*  [~Remove Magic~]*)
    "SPWI303"; (*  [~Flame Arrow~]*)
    "SPWI304"; (*  [~FireBall~]*)
    "SPWI306"; (*  [~Hold Person~]*)
    "SPWI307"; (*  [~Invisibility 10' Radius~]*)
    "SPWI308"; (*  [~Lightning Bolt~]*)
    "SPWI313"; (*  [~Skull Trap~]*)
    "SPWI316"; (*  [~Dire Charm~]*)
    "SPWI321"; (*  [~Spell Thrust~]*)
    "SPWI322"; (*  [~Detect Illusion~]*)
    "SPWI324"; (*  [~Hold Undead~]*)
    "SPWI326"; (*  [~Dispel Magic~]*)
    "SPWI399"; (*  [~Lightning Bolt~]*)
    "SPWI401"; (*  [~Confusion~]*)
    "SPWI404"; (*  [~Ice Storm~]*)
    "SPWI409"; (*  [~Contagion~]*)
    "SPWI411"; (*  [~Emotion~]*)
    "SPWI412"; (*  [~Greater Malison~]*)
    "SPWI413"; (*  [~Otiluke's Resilient Sphere~]*)
    "SPWI415"; (*  [~Polymorph Other~]*)
    "SPWI419"; (*  [~Secret Word~]*)
    "SPWI421"; (*  [~Teleport Field~]*)
    "SPWI502"; (*  [~Cloud Kill~]*)
    "SPWI503"; (*  [~Cone of Cold~]*)
    "SPWI506"; (*  [~Domination~]*)
    "SPWI507"; (*  [~Hold Monster~]*)
    "SPWI508"; (*  [~Chaos~]*)
    "SPWI513"; (*  [~Breach~]*)
    "SPWI514"; (*  [~Lower Resistance~]*)
    "SPWI523"; (*  [~Sunfire~]*)
    "SPWI604"; (*  [~Flesh to Stone~]*)
    "SPWI605"; (*  [~Death Spell~]*)
    "SPWI608"; (*  [~Pierce Magic~]*)
    "SPWI609"; (*  [~True Sight~]*)
    "SPWI612"; (*  [~Power Word Silence~]*)
    "SPWI614"; (*  [~Death Fog~]*)
    "SPWI615"; (*  [~Chain Lightning~]*)
    "SPWI616"; (*  [~Disintegrate~]*)
    "SPWI704"; (*  [~Ruby Ray of Reversal~]*)
    "SPWI705"; (*  [~Khelben's Warding Whip~]*)
    "SPWI711"; (*  [~Sphere of Chaos~]*)
    "SPWI712"; (*  [~Delayed Blast Fireball~]*)
    "SPWI713"; (*  [~Finger of Death~]*)
    "SPWI714"; (*  [~Prismatic Spray~]*)
    "SPWI715"; (*  [~Power Word, Stun~]*)
    "SPWI720"; (*  [~Control Undead~]*)
    "SPWI805"; (*  [~Pierce Shield~]*)
    "SPWI810"; (*  [~Incendiary Cloud~]*)
    "SPWI811"; (*  [~Symbol, Fear~]*)
    "SPWI812"; (*  [~Abi-Dalzim's Horrid Wilting~]*)
    "SPWI815"; (*  [~Power Word Blind~]*)
    "SPWI816"; (*  [~Symbol, Stun~]*)
    "SPWI817"; (*  [~Symbol, Death~]*)
    "SPWI818"; (*  [~Bigby's Clenched Fist~]*)
    "SPWI903"; (*  [~Spellstrike~]*)
    "SPWI911"; (*  [~Meteor Swarm~]*)
    "SPWI912"; (*  [~Power Word, Kill~]*)
    "SPWI913"; (*  [~Wail of the Banshee~]*)
    "SPWI914"; (*  [~Energy Drain~]*)
    "SPWI918"; (*  [~Bigby's Crushing Hand~]*)
  |] in 

  let immune_lst = [|
    ("Deafness", 1) ;
    ("Feeblemindedness", 1) ;
    ("Fatigue_bonus", 1) ;
    ("Maze", 1) ;
    ("Movementrate_bonus", 1) ;
    ("Disease", 2) ;
    ("Blindness", 2) ;
    ("Teleport_field", 2) ;
    ("Wing_buffet", 2) ;

    ("Power_word__Kill", 3) ;
    ("Power_word__Stun", 3) ;
    ("Sleep", 3) ;
    ("Petrification", 3) ;

    ("Slow", 4) ;
    ("Confusion", 4) ;
    ("Panic", 4) ;
    ("Stun", 4) ;
    ("Kill_target", 4) ;

    ("Charm_creature", 5) ;
    ("Hold_creature_type", 5) ;
    ("Level_drain", 5) ;

(* ? ("Hold_creature", 5) ; *)

    ("Remove_magical_protections", 6) ;
    ("Remove_spell_protections", 6) ;
    ("True_sight", 6) ;
  |] in 

  (* Factors: 
   *
   * Res              1.0
   * No Res           3.0
   * Save Spell +4    0.8
   * Save Spell +0    1.0
   * Save Spell -4    3.0
   * No Save          5.0
   * 33%              0.8
   * 50%              0.9
   * 100%             1.0
   *)

  let attack_lst = [|
    ("Damage",                (VarP(2)), (Var), (Fixed(0x20000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Cold *) 
    ("Damage",                (VarP(3)), (Var), (Fixed(0x80000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Fire *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x40000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Elec *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x10000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Acid *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x400000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Magic *) 
    ("Poison",                VarP(3), (Var), (Fixed(2)), Limited(3), [NoRes;ProbDirect]  (* Per Second *)) ;
    ("Disease",               VarP(3), (Var), Fixed(2), Limited(3), [NoRes;ProbDirect] ) ; (* per second *) 

    ("Damage",                (VarP(2)), (Var), (Fixed(0x20000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Cold *) 
    ("Damage",                (VarP(3)), (Var), (Fixed(0x80000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Fire *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x40000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Elec *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x10000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Acid *) 
    ("Damage",                (VarP(4)), (Var), (Fixed(0x400000)), Perm, [NoRes;NoSave;ProbDirect;Damage]) ; (* Magic *) 
    ("Poison",                VarP(3), (Var), (Fixed(2)), Limited(3), [NoRes;ProbDirect]  (* Per Second *)) ;
    ("Disease",               VarP(3), (Var), Fixed(2), Limited(3), [NoRes;ProbDirect] ) ; (* per second *) 

    ("Feeblemindedness",      Fixed(15), Fixed(0), Fixed(0), Limited(10), [] ) ;
    ("Level_drain",           Fixed(40), Fixed(1), Fixed(0), Perm, [NoSave; NoRes] ); (* 1 lvl *)
    ("Stun",                  Fixed(10), Fixed(0), Fixed(0), Limited(10), [] ) ;
    ("Dispel_effects",        Fixed(10), Fixed(0), Fixed(0), Perm, [NoRes; NoSave] ) ;
    ("Power_word__Stun",      Fixed(9), Fixed(0), Fixed(0), Perm, []) ; (* <= 90 *) 
    ("Kill_target",           Fixed(10), Fixed(0), Fixed(8), Perm, [] ) ;
    ("Power_word__Kill",      Fixed(8), Fixed(0), Fixed(0), Perm,[]);(* <= 60 HP *)
    ("Petrification",         Fixed(10), Fixed(0), Fixed(0), Perm, [] ) ;
    ("Hold_creature_type",    Fixed(9), Fixed(0), Fixed(2), Limited(30), [] ) ;
    ("Sleep",                 Fixed(9), Fixed(0), Fixed(0), Limited(30), [] ) ;

    ("Blindness",             Fixed(9), Fixed(0), Fixed(0), Limited(30), [] ) ;
    ("Fatigue_bonus",         VarP(9), (Var), Fixed(0), Limited(100), [ProbDirect]) ;

    ("Charm_creature",        Fixed(8), (Fixed(0)), (Fixed(3)), Limited(300), [] ) ;
    ("Panic",                 Fixed(8), Fixed(0), Fixed(0), Limited(30), [] ) ;
    ("Slow",                  Fixed(8), Fixed(0), Fixed(0), Limited(30), [] ) ;
    ("Maze",                  Fixed(8), Fixed(0), Fixed(0), Perm, [] ) ;

    ("Confusion",             Fixed(6), Fixed(0), Fixed(0), Limited(30), [] ) ;
(* ("Berserk",               Fixed(5), Fixed(0), Fixed(1), Limited(30), [] ); *)
    ("Movementrate_bonus",    Fixed(5), Fixed(1), Fixed(0), Limited(30), [] ) ; (* := 0 *) 

    ("Silence",               Fixed(4), Fixed(0), Fixed(0), Limited(100), [] ) ;
    ("Deafness",              Fixed(3), Fixed(0), Fixed(0), Limited(300), [] ) ;
    ("Casting_failure",       VarP(5), (Var5), Fixed(0), Limited(100), [NoRes;ProbDirect;NoSave] ) ;

    ("Current_HP_bonus",      VarP(5), (Var), Fixed(0), Perm, [TargSelf; NoRes; NoSave] ) ;
    ("Max_HP_Bonus",          VarP(10), (Var), Fixed(0), Limited(100), [TargSelf; NoRes; NoSave] ) ;

    ("Mirror_image",          VarP(10), (Var), Fixed(0), Limited(300), [TargSelf; NoRes; NoSave] ) ;
    ("Wing_buffet",           Fixed(7), Fixed(20), Fixed(2), Limited(2),[] ) ; 

    ("True_sight",            Fixed(7), Fixed(9), Fixed(5), Perm, [NoRes; NoSave] ) ;
    ("Remove_spell_protections", Fixed(7), Fixed(9), Fixed(3), Perm, [NoRes; NoSave] ) ; (* true sight *) 

    ("Remove_spell_protections", Fixed(7), Fixed(9), Fixed(7), Perm, [NoRes; NoSave] ) ; (* breach1 *) 
    ("Remove_spell_protections", Fixed(7), Fixed(9), Fixed(2), Perm, [NoRes; NoSave] ) ; (* breach2 *) 

    ("Remove_magical_protections", Fixed(7), Fixed(9), Fixed(1), Perm, [NoRes; NoSave] ) ; (* rem 1 *)

    ("Cast_spell", Fixed(15), Fixed(20), Fixed(1), Perm, [ NoRes; NoSave; CastSpell] );
    ("Cast_spell", Fixed(15), Fixed(20), Fixed(1), Perm, [ NoRes; NoSave; CastSpell] );
    ("Cast_spell", Fixed(15), Fixed(20), Fixed(1), Perm, [ NoRes; NoSave; CastSpell] );
    ("Cast_spell", Fixed(15), Fixed(20), Fixed(1), Perm, [ NoRes; NoSave; CastSpell] );
    ("Cast_spell", Fixed(15), Fixed(20), Fixed(1), Perm, [ NoRes; NoSave; CastSpell] );
    ("Summon_creature", Fixed(25), Fixed(0), Fixed(0), Perm, [ NoRes; NoSave;
							       SummonCreature] );
  |] in 
(*
  ("Display_string", ) ;
  ("Cast_spell", ) ;
 *)

(*
 * Human Fighter-9, Grand Mastery (+3/+5), 2d4 sword. STR=DEX=CON=18/00
 * THAC0 = 12 - 3 - 3 = 6. Enemy AC = -4. 
 * DMG = 2d4+6+5 = 16, APR = 3
 *
 * (3 swings / round) * (1 hit / 2 swings) * (16 dmg / hit) = 24 dmg / rnd
 *
 * Cost = 2x extra damage done
 *)

  let equipped_lst = [|
    ("Maximum_damage_each_hit",     Fixed(7), Fixed(20), Fixed(0) ) ; 
    (* +3 on avg -> 28.5 *) 
    ("Strength_bonus",              VarP(4), Var, Fixed(0)) ;
    (* +1 -> 25.5 *) 
    ("Attacks_per_round_bonus",     VarP(16), (Var), (Fixed(0))) ;
    (* +1 -> 32 *)
    ("Double_number_of_attacks",    Fixed(48), Fixed(0), Fixed(1) ) ;
    (* x2 -> 48 *) 
    ("Haste",                       Fixed(16), Fixed(0), Fixed(0)) ;
    (* +1 -> 32 *) 
    ("Base_attack_bonus",           VarP(5), Var, Fixed(0)) ;
    (* +1 -> 26.4 *)
    ("Attack_damage_bonus",         VarP(3), Var, Fixed(0)) ;
    (* +1 -> 25.5 *)
    ("Luck_bonus",                  VarP(8), Var, Fixed(0)) ;
    (* +1 hit, +1 dmg -> 28, FIXME SAVES *) 

    ("Draw_upon_holy_might__non-cumulative", VarP(14), Var, Fixed(0) ) ;
    ("Dexterity_bonus",             VarP(5), Var, Fixed(0)) ;
    ("Constitution_bonus",          VarP(5), Var, Fixed(0)) ;
    (* +1 con = ~ +4.5 HP *) 
    ("Max_HP_bonus",                VarP(4), Var5, Fixed(0)) ;

    ("Bonus_to_AC",                 VarP(5), (Var), (Fixed(0)) ) ;

    ("Bless__non-cumulative",       Fixed(5), Fixed(1), Fixed(0) ) ;
    (* +X attack, +1 vs. fear *) 
    ("Aid__non-cumulative",         Fixed(10), Fixed(1), Fixed(0) ) ;
    (* +X attack, saves -> *)
    ("Chant__non-cumulative",       Fixed(11), Fixed(1), Fixed(0) ) ;
    (* +X attack, +X dmg, +X save *) 

    ("Save_vs_death_bonus",         VarP(1), Var, Fixed(0)) ;
    ("Save_vs_wands_bonus",         VarP(1), Var, Fixed(0)) ;
    ("Save_vs_polymorph_bonus",     VarP(1), Var, Fixed(0)) ;
    ("Save_vs_breath_bonus",        VarP(1), Var, Fixed(0)) ;
    ("Save_vs_spell_bonus",         VarP(1), Var, Fixed(0)) ;

    ("Vocalize",                      Fixed(1), Fixed(0), Fixed(0)) ;
    ("Non-detection",                 Fixed(2), Fixed(0), Fixed(0)) ;
    ("Immunity_to_backstab",          Fixed(2), Fixed(0), Fixed(1) ) ;
    ("Immune_to_timestop",            Fixed(10), Fixed(0), Fixed(1) ) ;
(* ("Increase_attack_speed_factor",  Fixed(2), Fixed(9), Fixed(0) ) ; *)
    ("Increase_casting_speed_factor", Fixed(10), Fixed(1), Fixed(0) ) ;

    ("Charisma_bonus",              VarP(1), Var, Fixed(0) ) ;
    ("Intelligence_bonus",          VarP(1), Var, Fixed(0)) ;
    ("Wisdom_bonus",                VarP(3), Var, Fixed(0)) ;


    ("Regeneration",                VarP(15), (Var), Fixed(2) ) ; (* hp / sec *) 
    ("Immunity_to_spell_level",     Fixed(10), Fixed(1), Fixed(0) ) ; (* lvl 1 *) 
    ("Immunity_to_spell_level",     Fixed(20), Fixed(2), Fixed(0) ) ; (* lvl 2 *) 
    ("Immunity_to_weapons",         Fixed(20), Fixed(0), Fixed(2) ) ; (* non-mag *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(1) ) ; (* ABJ *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(2) ) ; (* CON *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(3) ) ; (* DIV *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(4) ) ; (* ENC *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(5) ) ; (* ILL *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(6) ) ; (* EVO *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(7) ) ; (* NEC *)
    ("Protection_from_spell_school", Fixed(20), Fixed(0), Fixed(8) ) ; (* ALT *)
    ("Spell_type_deflection",        Fixed(30), Fixed(0), Fixed(10) ) ; (* Offensive *) 
    ("Spell_type_deflection",        Fixed(20), Fixed(0), Fixed(11) ) ; (* Disabling *) 

    ("Free_action",                 Fixed(5), Fixed(0), Fixed(0) ) ;

    ("Protection_from_creature_type", Fixed(5), Fixed(9), Fixed(7) ) ; (* summoned demon *) 

    ("Movementrate_bonus",          Fixed(5), Fixed(200), Fixed(2) ) ;
    ("Reduced_damage_from_poison",    Fixed(5), Fixed(100), Fixed(0) ) ;

    ("Acid_resistance_bonus",         VarP(1), Var10, Fixed(0)) ;
    ("Cold_resistance_bonus",         VarP(1), Var10, Fixed(0)) ;
    ("Electricity_resistance_bonus",  VarP(1), Var10, Fixed(0)) ;

    ("Fire_resistance_bonus",         VarP(1), Var5, Fixed(0)) ;
    ("Magic_damage_resistance_bonus", VarP(1), Var5, Fixed(0)) ;

    ("Slashing_resistance_bonus",   VarP(5), Var5, Fixed(0)) ;
    ("Crushing_resistance_bonus",   VarP(5), Var5, Fixed(0)) ;
    ("Piercing_resistance_bonus",   VarP(5), Var5, Fixed(0)) ;
    ("Missile_resistance_bonus",    VarP(5), Var5, Fixed(0)) ;

    ("Magic_resistance_bonus",      VarP(5), Var5, Fixed(0)) ;

    ("Immunity_to_effect", VarImmune, Fixed(0), Var) ; 
    ("Immunity_to_effect", VarImmune, Fixed(0), Var) ; 
    ("Immunity_to_effect", VarImmune, Fixed(0), Var) ; 
    ("Immunity_to_effect", VarImmune, Fixed(0), Var) ; 
    ("Immunity_to_effect", VarImmune, Fixed(0), Var) ; 
(* ("Protection_from_spell", ) ; *)

(* ("Bonus_wizard_spells", ) ; 
   ("Bonus_priest_spells", ) ;*)
(*
  ("Stealth_bonus", ) ;
  ("Lockpicking_bonus", ) ;
  ("Find_traps_bonus", ) ;
  ("Pick_pockets_bonus", ) ;
  ("Backstab_bonus", ) ;
  ("Hide_in_shadow_bonus", ) ;
  ("Detect_illusions_bonus", ) ;
  ("Set_traps_bonus", ) ;
  ("Backstab_every_hit", ) ;
 *)

(* ("Use_EFF_file", ) ;
   ("THAC0_vs._type_bonus", ) ;
   ("Damage_vs._type_bonus", ) ; *)
(* ("Increase_critical_hits", ) ; *)

(* ("Reflect_specified_spell", ) ; *)
(* ("Cast_spell_on_condition", ) ; *)


  |] in

  let cost_of_eff e  poss poss_cost a b =
    let poss_ids = Bcs.ids_of_sym (config.source) "srcop" poss in 
    let poss_opcode = Int32.to_int poss_ids.Ids.i_num in 
    if e.e_opcode = poss_opcode then begin
      match poss_cost with
      | Fixed(i) -> i
      | Var5
      | Var10
      | Var -> failwith "var?" 
      | VarImmune -> failwith "varimmune?" 
      | VarP(i) -> 
          begin 
            let amt = if e.e_opcode = 12 then
              ((e.e_numdice * (1 + e.e_dicesize)) / 2) + e.e_arg1
            else if e.e_opcode = 0 && e.e_arg2 = 16 then
              10 - e.e_arg1 
            else if e.e_opcode = 0 && e.e_arg2 <> 0 then
              0
            else if e.e_opcode = 98 && e.e_arg2 = 3 then
              1 
            else if e.e_opcode = 41 && e.e_arg2 = 1 then
              e.e_arg1 - 16
            else e.e_arg1 
            in 
            let amt = amt * amt in 
            let e_amt = match a with
            | Fixed _ 
            | VarImmune 
            | VarP _ -> failwith "fixed/varp"
            | Var -> amt
            | Var5 -> (amt + 4) / 5
            | Var10 -> (amt + 9) / 10 
            in
            e_amt * i 
          end 
    end else 0 
  in 

  let eval_itm i = begin
    let base = i.i_enchant * 10 in 
    let cost = ref base in 
    Array.iter (fun e -> 
      if (e.e_opcode = 101) then begin (* immunity to effect *) 
        Array.iter (fun (poss,c) ->
          let poss_ids = Bcs.ids_of_sym (config.source) "srcop" poss in 
          let poss_opcode = Int32.to_int poss_ids.Ids.i_num in 
          if (e.e_arg2 = poss_opcode) then 
            cost := !cost + c
		   ) immune_lst 
      end else if (e.e_opcode = 177) then begin
        (* use EFF file *)
        cost := !cost + 3; 
      end else 
	Array.iter (fun (poss,poss_cost,a,b) -> 
          cost := !cost + (cost_of_eff e poss poss_cost a b) ) equipped_lst ; 
	       ) i.i_equipped ; 
    Array.iter (fun abil -> 
      Array.iter (fun e -> 
        if (e.e_opcode = 177) then begin (* use EFF file *)
          if (e.e_resref = "DIE") then begin 
            cost := !cost + 10; 
          end else
            cost := !cost + 3; 
        end else
          let this_c = ref 0 in 
          Array.iter (fun (poss,poss_cost,a,b,time,flags) -> 
            let c = ref (cost_of_eff e poss poss_cost a b) in
            if (!c > 0) then begin
              (if (e.e_disres = 0 || e.e_disres = 3) && 
		not (List.mem NoRes flags) then c := !c * 3) ;
              (if (e.e_savetype = 0) &&
		not (List.mem NoSave flags) then c := !c * 5
              else if (e.e_savebonus < 0) then c := !c * 3
              else if (e.e_savebonus > 0) then c := (!c * 8) / 10;
              ) ;
              let prob = e.e_prob1 - e.e_prob2 in
              if (List.mem ProbDirect flags) then 
		c := !c * prob / 100
              else if (prob >= 100) then ()
              else if (prob >= 50) then c := (!c * 9) / 10
              else c := (!c * 8) / 10
            end ;
            (if (abil.a_type = 3) then c := !c / 2); 
            this_c := max !this_c !c;
		     ) attack_lst ;
          cost := !cost + !this_c;
		 ) abil.a_eff
	       ) i.i_abil ; 
    !cost
  end in

(*
  let files_in_chitin = Key.list_of_key_resources config.source.Load.key in 
  let regexp = Str.regexp_case_fold "sw.h..\\.itm" in 
  let matches = ref [] in 
  List.iter (fun possible ->
  if Str.string_match regexp possible 0 then begin
  matches := (possible) :: !matches
  end 
  ) files_in_chitin ;

  let unsorted = ref [] in 

  List.iter (fun poss -> 
  let name, ext = split poss in 
  let buff = load_source_res name ext in 
  let i = read_itm buff in
  try 
  let cost = eval_itm i in
  unsorted := (cost, poss, (Tlk.pretty_print config.source.Load.dialog i.i_iname)) :: !unsorted ; 
  with e -> 
  log_and_print "Error (%s) on %s\n" (Printexc.to_string e) poss 
  ) !matches ;

  let sorted = List.sort (fun (a,b,c) (a',b',c') -> a - a') !unsorted in

  List.iter (fun (a,b,c) ->
  log_and_print "%3d %s %s\n" a b c 
  ) sorted ;
 *)

  let build_itm i cost is_armor = begin
    i.i_price <- cost * 100 ; 
    (match i.i_cat with
    | 24 (* dart *)
    | 14 (* bullet *) 
    | 5 (* arow *) 
    | 31 (* bolt *) 
      -> i.i_price <- i.i_price / 20 ;
    | _ -> () ) ; 
    i.i_lore <- 0; 
    let points = ref (float_of_int cost) in 
    let spend x = points := !points -. x in
    let can_attack = (Array.length i.i_abil) > 0 &&
      i.i_abil.(0).a_type <> 3 in
    let can_potion = (Array.length i.i_abil) > 0 &&
      i.i_abil.(0).a_type = 3 in 
    let can_equip = 
      (* (not can_potion) && *)
      (i.i_cat != 0) && (* misc *) 
      (i.i_cat != 31) && (* bolt *) 
      (i.i_cat != 5) && (* arow *) 
      (i.i_cat != 14) (* arow *) 
    in 
    while can_attack && !points >= 10.0 && (Random.bool ()) do
      spend 10.0 ;
      i.i_enchant <- i.i_enchant + 1; 
      i.i_abil.(0).a_tohit <- i.i_enchant ; 
      i.i_abil.(0).a_dmgbonus <- i.i_enchant ; 
    done ;
    let how_many poss_cost cost =  
      match poss_cost with
      | Fixed(i) -> 0, (float_of_int i) *. !cost 
      | Var5
      | Var10
      | Var -> failwith "var!?"
      | VarImmune -> 
          let imw,imc = immune_lst.(Random.int (Array.length immune_lst)) in 
          let poss_ids = Bcs.ids_of_sym (config.source) "srcop" imw in 
          let poss_opcode = Int32.to_int poss_ids.Ids.i_num in 
          poss_opcode, (float_of_int imc) 

      | VarP(i) -> 
          let price_per_x = (float_of_int i) *. !cost in
          assert(price_per_x > 0.0) ;
          let x = ref 0 in
          while ((!x+1) * (!x+1) * i) >= (int_of_float !points) do
            incr x ; 
          done ;
          let max_x = min !x 10 in 
          max_x, ((float_of_int (max_x*max_x)) *. price_per_x)
            (*
              let max_x = truncate (!points /. price_per_x) in 
              let max_x = min max_x 10 in 
              max_x, ((float_of_int max_x) *. price_per_x)
             *)
    in 
    let rarity_check (poss_cost : rl_cost) : bool = 
      let x = match poss_cost with
      | Fixed(i) -> i
      | VarP(i) -> i
      | _ -> 50
      in
      (Random.int 100) >= x
    in 

    let buy_or_skip flags x a b prob poss poss_cost price time res save = begin 
      let bad_for_now = 
        (* List.mem CastSpell flags ||  *)
        List.mem SummonCreature flags ||
        (not (rarity_check poss_cost))
      in
      let get_arg a = match a with
      | Fixed(i) -> i
      | Var -> x
      | Var5 -> x * 5 
      | Var10 -> x * 10
      | VarImmune -> failwith "varimmune" 
      | VarP(i) -> failwith "varp" 
      in 
      if (price > 0.0 && price < !points && not bad_for_now) then begin
        spend price ; 
        let new_eff = {
          e_opcode = 
          begin 
            let poss_ids = Bcs.ids_of_sym (config.source) "srcop" poss in 
            let poss_opcode = Int32.to_int poss_ids.Ids.i_num in 
            poss_opcode 
          end ;
          e_target = 
          if (List.mem TargSelf flags) then 1 else 2 ;
          e_power = 0;
          e_arg1 = get_arg a ;
          e_arg2 = get_arg b ;
          e_duration = (match time with 
          | Equipped -> 2
          | Perm -> 1
          | Limited(i) -> 0) ;
          e_disres = if res then 2 else 0 ;
          e_time = (match time with
          | Equipped
          | Perm -> 0
          | Limited(i) -> i) ;
          e_prob1 = prob ;
          e_prob2 = 0 ;
          e_resref = 
          if List.mem CastSpell flags then 
            spell_lst.(Random.int (Array.length spell_lst))
          else "" ;
          e_numdice = 0;
          e_dicesize = 0; 
          e_savetype = if save = None then 0 else 1 ;
          e_savebonus = (match save with
          | None -> 0
          | Some(i) -> i) ; 
        }  in
        Some(new_eff) 
      end else None
    end in 

    if is_armor then begin
      let amt = Random.int (int_of_float (!points /. 3.0)) in 
      let amt = min amt 20 in 
      let price = (float_of_int amt) *. 3.0 in
      match buy_or_skip [TargSelf] (10 - amt) (Var) (Fixed(0x10)) 100 
          "Bonus_to_AC" (Fixed(0)) price (Equipped) (false) (None) with 
      | Some(new_eff) -> 
          i.i_equipped <- Array.append i.i_equipped [| new_eff |] 
      | None -> () 
    end ; 

    let have_eff poss arr = 
      let poss_ids = Bcs.ids_of_sym (config.source) "srcop" poss in 
      let poss_opcode = Int32.to_int poss_ids.Ids.i_num in 
      Array.fold_left (fun acc elt -> 
        acc || 
        (if elt.e_opcode = poss_opcode then begin
          if poss_opcode = 101 (* immune to eff *) 
        || poss_opcode = 12 (* damage *) 
          then false else true
        end else false)
) false arr
    in 

    let trials = ref 0 in 
    while !points > 1.0 && !trials < 5000 do
      incr trials ; 
      if can_attack && (Random.bool ()) then begin
        (* try for an attacking ability *) 
        let x = Random.int (Array.length attack_lst) in 
        let (poss,poss_cost,a,b,time,flags) = attack_lst.(x) in 
        let cost = ref 1.0 in 
        let res = 
          if (List.mem NoRes flags) then false
          else if (Random.bool ()) then (cost := (!cost *. 3.0) ; false)
          else true
        in 
        let save = 
          if (List.mem NoSave flags ) then
            None
          else 
            match Random.int 6 with
            | 0 -> cost := !cost *. 5.0 ; None
            | 1 -> cost := !cost *. 3.0 ; Some(-4)
            | 2 -> cost := !cost *. 0.8 ; Some(4) 
            | _ -> Some(0)
        in 
        let prob =
          match Random.int 5 with
          | 0 -> cost := !cost *. 0.8 ; 33
          | 1 -> cost := !cost *. 0.9 ; 50
          | _ -> 100
        in 
        let x, price = how_many poss_cost cost in 

        if not (have_eff poss i.i_abil.(0).a_eff) then 
          match buy_or_skip flags x a b prob poss poss_cost price time res save with
          | Some(new_eff) -> 
              i.i_abil.(0).a_eff <- Array.append i.i_abil.(0).a_eff [| new_eff |] 
          | None -> () 
      end else if can_potion && (Random.bool ()) then begin
        (* magic ability *) 

      end else if can_equip then begin
        (* equipped ability *) 
        let x = Random.int (Array.length equipped_lst) in 
        let (poss,poss_cost,a,b) = equipped_lst.(x) in 
        let res = false in
        let save = None in 
        let time = Equipped in 
        let cost = ref 1.0 in 
        let x, price = how_many poss_cost cost in 
        if not (have_eff poss i.i_equipped) then 
          match buy_or_skip [TargSelf] x a b 100 poss poss_cost price time res save with
          | Some(new_eff) -> 
              i.i_equipped <- Array.append i.i_equipped [| new_eff |] 
          | None -> () 
      end 
    done 
  end in

  let describe_rl general_name i = begin 
    (* returns a string describing the ITM *) 
    let r = "RL" in
    let b = Buffer.create 80 in 
    Printf.bprintf b "%s properties:\n" general_name ;
    Printf.bprintf b "Weight: %d\n" i.i_weight ; 
    if (i.i_lore <> 0) then Printf.bprintf b "Lore to identify: %d\n" i.i_lore ; 
    if (i.i_price <> 0) then Printf.bprintf b "Price: %d\n" i.i_price ; 
    let is_weapon = (List.exists (fun a -> a.a_type <> 3) 
                       (Array.to_list i.i_abil)) in
    let is_two_handed = i.i_flags land 2 = 2 in 
    if is_two_handed then Printf.bprintf b "Hands required: Two\n" ;
    if is_weapon then begin 
      Printf.bprintf b "Enchantment: +%d\n" i.i_enchant ; 
    end else begin 
      () 
    end ;
    if Array.length i.i_equipped > 0 then begin
      Printf.bprintf b "\nEquipped effects:\n" ; 
      Array.iter (fun e -> describe_eff 2 r e b) i.i_equipped ; 
    end ; 
    Array.iter (fun a ->
      Printf.bprintf b "\n%s ability:\n" (match a.a_type with
        1 -> "Melee weapon" 
      | 2 -> "Ranged weapon" 
      | 3 -> "Magical"
      | 4 -> "Missile Launcher"
      | 0 -> "Default" 
      | i -> (log_and_print "ERROR: ITM: %s ability type %d" r i ; exit 1)) ; 
      (if a.a_range <> 1 then Printf.bprintf b "  Range: %d\n" a.a_range) ; 
      (if a.a_type <> 3 then begin 
        Printf.bprintf b "  Weapon Speed: %d\n" a.a_speed ; 
        Printf.bprintf b "  Attack Bonus: +%d\n" a.a_tohit ; 
      end ) ; 
      let invalid a = a = 0 || a > 1000 in 
      if (invalid a.a_numdice & invalid a.a_dmgbonus) then begin
        if a.a_type <> 3 then Printf.bprintf b "  Base Damage: None\n" ;
      end else begin 
        Printf.bprintf b "  Base Damage: %s\n" 
          (print_xdy a.a_numdice a.a_dicesize a.a_dmgbonus) ;
        Printf.bprintf b "  Damage Type: %s\n" (match a.a_dmgtype with
          0 -> "None"
        | 1 -> "Piercing"
        | 2 -> "Bludgeoning" 
        | 3 -> "Slashing"
        | 4 -> "Missile (piercing)"
        | 5 -> "Non-lethal" 
        | i -> (log_and_print "WARNING: ITM: %s.ITM has damage type %d\n" r i ;
                a.a_dmgtype <- 3;
                "Slashing"))
      end ; 
      (if a.a_type <> 3 then Printf.bprintf b "  Strength Damage Bonus: %s\n"
          (match a.a_allowstr with 
	  | 0 -> "None" 
	  | _ -> "Yes"
	  )) ;
      if Array.length a.a_eff > 0 then begin 
	(if a.a_type = 3 then 
          Printf.bprintf b "  Magical effects:\n" 
	else 
          Printf.bprintf b "  Striking effects:\n") ;
	Array.iter (fun e -> describe_eff 4 r e b) a.a_eff ; 
      end 
	       ) i.i_abil ; 
    Buffer.contents b 
  end in 

  Random.self_init () ; 

  override := "iwg2/rl" ; 

  let files_in_chitin = Key.list_of_key_resources config.source.Load.key in 
  let files_in_override = 
    let dh = Case_ins.unix_opendir "override" in
    let lst = ref [] in 
    (try 
      while true do 
        let next = Unix.readdir dh in
        if ((Case_ins.unix_stat ("override/" ^ next)).Unix.st_kind = 
            Unix.S_REG) then 
          lst := (String.uppercase next) :: !lst
      done 
    with End_of_file -> () );
    !lst
  in 
  let regexp = Str.regexp_case_fold ".*\\.itm" in 
  let matches = ref [] in 
  List.iter (fun possible ->
    if Str.string_match regexp possible 0 then begin
      matches := (possible) :: !matches
    end 
	    ) (files_in_chitin @ files_in_override);
  let sorted = List.sort compare !matches in 
  let rec uniqify lst = match lst with
  | [] -> []
  | a :: (b :: tl as foo) when a = b -> uniqify foo
  | hd :: tl -> hd :: (uniqify tl)
  in 
  let matches = uniqify sorted in 

  let fout = Case_ins.perv_open_out "Setup-RL.tp2" in 

  Printf.fprintf fout "BACKUP ~iwg2/backup~\n" ; 
  Printf.fprintf fout "AUTHOR ~weimer~\n" ; 
  Printf.fprintf fout "BEGIN ~Roguelike Random Equipment~\n" ; 

  let rl_idx = ref 0 in 

  List.iter (fun m -> 
    try 
      let base, ext = split m in 
      let buff = load_source_res base "ITM" in 
      let i = read_itm buff in
      let cost = eval_itm i in 
      if (cost > 10) && (i.i_cat != 9) && 
        (i.i_cat != 11) (* scroll *) && 
        (i.i_cat != 35) (* wand *) && 
        (i.i_cat != 28) (* hand-to-hand-weapons *) && 
        (i.i_flags land 0x40 = 0x40) && (* magical *) 
        (cost < 300) then begin
          let is_armor = i.i_cat = 2 in 

          if (i.i_cat = 0) then begin
            if (Array.length i.i_abil) > 0 &&
              (i.i_abil.(0).a_type = 1) then begin
		i.i_cat <- 16; (* daggers *) 
		i.i_inv <- "DD";
		i.i_icon <- "IDAGG01" ;
		i.i_gicon <- "GDAGG01" ;
		i.i_cicon <- "CDAGG01" ; 
		i.i_abil.(0).a_icon <- "IDAGG01" ; 
              end ; 
          end ; 

          i.i_equipped <- [| |] ; 
          i.i_enchant <- 1 ; 
          (* if (i.i_flags land 0x0c) <> 0x0c then raise Not_found ;  *)
          (* i.i_flags <- i.i_flags lor 0x0c ; (* drop, display *)  *)
          i.i_flags <- i.i_flags lor 0x40 ; (* magical *) 
          i.i_prof <- 96; (* dagger *) 
          if (Array.length i.i_abil) > 0 then begin 
            i.i_abil <- [| i.i_abil.(0) |] ;
            if Array.length i.i_abil.(0).a_eff = 0 &&
              i.i_abil.(0).a_type = 0 then begin
		i.i_abil <- [| |]; 
              end else  
              i.i_abil.(0).a_eff <- [| |] ; 
          end ;

          build_itm i cost is_armor ; 
          let arr = random_melee.(1) in 
          let general_name = arr.(Random.int (Array.length arr)) in 
          let auto_desc = describe_rl general_name i in

          (* randomize up the icon: later *) 

          let rl_name = Printf.sprintf "RL%04d" !rl_idx in 
          incr rl_idx ; 
          (* if (!rl_idx > 5) then exit 1 ;  *)

          let buff = serialize_itm i in 
          save_in_override rl_name "ITM" buff ;
          Printf.fprintf fout "COPY ~iwg2/rl/%s.ITM~ ~override/%s~\n"
            rl_name m ;
          Printf.fprintf fout "\tSAY 0x0c ~%s~\n" general_name ; 
          Printf.fprintf fout "\tSAY 0x54 \"%s\n(was %s, %s, cost %d)\"\n\n" 
            auto_desc 
            (Tlk.pretty_print config.source.Load.dialog i.i_iname)
            (base)
            cost ; 
	end 
    with _ -> () 

(*
  let finished = ref false in
  while not !finished do 
  let icon_idx = (Random.int 77) + 1 in 
  let iic = Printf.sprintf "I%s%02d" icon icon_idx in 
  let cic = Printf.sprintf "C%s%02d" icon icon_idx in 
  i.i_icon <- iic ; 
  i.i_cicon <- cic ; 
  if (Array.length i.i_abil > 0) then 
  i.i_abil.(0).a_icon <- iic ; 
  try 
  let _ = load_source_res iic "BAM" in
  finished := true
  with _ -> () 
  done ; 

  let name = Printf.sprintf "RL%03d" idx in 
 *)
	    ) matches ; 

  () 

(*
  while true do begin 
  let str = read_line () in
  let buff = load_source_res str "ITM" in
  let i = read_itm buff in
  eval_itm i
  end done 
 *)
end 

let main () = begin

  log_and_print "\t\tIWG2 (version %d, weidu %s)\n" iwg2_version version ; 

  let auto_list = [ "iwg2" ; "iwg1" ] in 

  let cfg_file = ref (
    let rec try_it lst = match lst with
    | [] -> "no configuration file specified"
    | hd :: tl -> 
	begin
          let iwg2_regexp = Str.regexp_case_fold hd in (* legacy support *) 
          try 
            let argv0_base, argv0_ext = split (String.uppercase 
						 (Case_ins.filename_basename Sys.argv.(0))) in 
            let _ = Str.search_forward iwg2_regexp argv0_base 0 in
            (hd ^ "/configuration")
          with Not_found -> try_it tl 
	end 
    in try_it auto_list )
  in 

  let option_do_rl = ref false in 

  let usageMsg = Printf.sprintf "\nusage: IWG2 [options] [specific-resource-files-to-convert]\n\nOptions:\n" in
  let argDescr = [
    "--cfg", Myarg.String (fun s -> cfg_file := s),"X\tread global configuration from file X"; 
    "--bcs-debug", Myarg.Set option_bcs_debug, "\tIncluding debugging information in BCS files (ON by default)" ; 
    "--bcs-none", Myarg.Set option_bcs_none, "\tConvert all BCSs to None.BCS" ; 
    "--cre-keg", Myarg.Set option_cre_keg, "\tConvert all CREs to 10KEG.CRE" ;
    "--cre-fighter", Myarg.Set option_cre_fighter, "\tConvert all CREs classes to fighter" ;
    "--cre-no-feats", Myarg.Set option_cre_no_feats, "\tDrop all feats when converting CREs" ;
    "--cre-no-items", Myarg.Set option_cre_no_items, "\tDrop all items when converting CREs" ;
    "--cre-no-spells", Myarg.Set option_cre_no_spells, "\tDrop all memorized spells when converting CREs" ;
    "--no-cre-in-name", Myarg.Clear option_cre_in_name, "\tPut CRE in creature name" ;
    "--animate-keg", Myarg.Set option_animate_keg, "\tConvert all animations to Keg 1" ;
    "--damage-reduction", Myarg.Set option_damage_reduction, "\tArmor reduces damage, not 'armor class'" ;
    "--log", Myarg.String (fun s -> init_log Version.version s),"X\tlog output and details to X" ;
    "--rl", Myarg.Set option_do_rl, "\t---" ; 
  ] in 

  let give_help () = Myarg.usage argDescr usageMsg ; exit 1 in 
  let handleArg str = 
    res_to_process := (split (String.uppercase str)) :: !res_to_process 
  in 

  Myarg.parse argDescr handleArg usageMsg  ;

  log_and_print "\n*** Reading Global Configuration ***\n" ; 

  let read_rule_file filename = 
    begin
      let inchan = Case_ins.perv_open_in filename in 
      let lexbuf : Lexing.lexbuf = lex_init filename inchan in
      try 
        let result = Stats.time "parsing rules"  
            (fun () -> Iwgparser.iwg_rule_file Iwglexer.initial lexbuf) () in
        pop_context () ;
        close_in inchan ; 
        log_and_print "[%s] parsed (%d rules)\n" 
          filename (List.length result); 
        result 
      with e -> 
        log_and_print "ERROR: parsing [%s]: %s\n" filename 
          (Printexc.to_string e) ; exit 1 
    end
  in 

  let cfg_rules = read_rule_file !cfg_file in

  input_error_to_stdout := false ; 

  (* parse the global configuraiton *) 
  List.iter (fun rule -> match rule with
  | Replace("source",[p]) -> 
      Load.game_paths := [ p ] ; config.source <- Load.load_game () ;
      Load.saved_game := Some(config.source) 
  | Replace("target",[p]) -> 
      Load.game_paths := [ p ] ; config.target <- Load.load_game () ;
      let buff = load_target_res "CHARBASE" "CRE" in 
      charbase_buff := buff ; 
      Dc.cur_index := (Array.length (config.target).Load.dialog)
  | Replace("source.variant",[v]) -> 
      config.source_variant <- (variant_of_string v) 
  | Replace("target.variant",[v]) -> 
      config.target_variant <- (variant_of_string v) 

  | Replace("use.as.target.override",[v]) -> 
      (config.target).Load.override_path_list <- v ::
        (config.target).Load.override_path_list ; 
      override := v

  | Replace("copy.to.target.override",[v]) -> 
      iterdir v (fun filename ->
        let dst = (config.target).Load.game_path ^ "/override/" ^ 
          (Case_ins.filename_basename filename) in
        if file_exists filename then copy_one_file  filename dst 
		)
  | Replace("copy.to.source.override",[v]) -> 
      iterdir v (fun filename ->
        let dst = (config.source).Load.game_path ^ "/override/" ^ 
          (Case_ins.filename_basename filename) in
        if file_exists filename then copy_one_file  filename dst 
		)

  | Replace("error.files.in",[s]) -> 
      Util.error_chn_base := s 

  | Replace("delete.files.in",[s]) -> 
      let s_d_h = try Case_ins.unix_opendir s 
      with _ -> failwith ("cannot open directory " ^ s) in 
      begin try 
        while true do
          let s' = Unix.readdir s_d_h in
          let filename = (s ^ "/" ^ s') in 
          try Case_ins.unix_unlink filename with _ -> () 
        done
      with e -> (Unix.closedir s_d_h ) end 

  | Replace("parse.rules.in",[s]) -> 
      log_and_print "*** Reading Conversion Rules in [%s] ***\n" s ; 
      iterdir s (fun filename -> 
        if (Case_ins.filename_basename filename).[0] <> '.' then 
          process_rules (read_rule_file filename)
		) ; 

  | Replace("random.spawn.cre",[f]) -> 
      config.random_spawn_cre <- load_file f
  | Replace("bringer.cre",[f]) -> 
      config.bringer_cre <- load_file f

  | Replace("blank.script.bcs",[f]) -> config.blank_script_bcs <- f

  | Replace("premade.npc.directory",[f]) -> config.premade_npc_directory <- f

  | Replace("tra.directory",[f]) -> config.tra_directory <- f

  | Replace("convert.2da.strref",lst) -> 
      List.iter (fun f -> convert_2da_strref f) lst 

  | Replace("convert.baldur.bcs.prepend",[f]) -> 
      config.baldur_bcs_prepend <- f 

  | Replace(x,_) 
  | ReplaceWithConverted(x,_) 
    -> log_and_print "ERROR: unknown global configuration [%s]\n" x ;
      exit 1
	    ) cfg_rules ; 

  if (!option_do_rl) then begin
    rl () 
  end else begin 

    let key_type = Key.key_of_ext true "ARE" in 
    Array.iter (fun r ->
      if r.Key.res_type = key_type then 
	ignore (rename r.Key.res_name "ARE") 
	       ) (config.source).Load.key.Key.resource ; 

    process_automatic_rules () ; 

    log_and_print "\n*** Converting ***\n\n" ; 

    (match config.source_variant , config.target_variant with 
    | BG2, IWD2 -> 
	begin 
          convert_chapters () ;
          convert_spawngrp () ; 
          (* 
             convert_interdia () ; 
             convert_pdialog () ; 
             --> now handled in GAM conversion 
           *)
          (try Case_ins.unix_mkdir "Sounds/A_NPC" 511 with _ -> () ) 
	end 
    | BG1, BG2 -> 
	log_and_print "Japh (or whatnot) must do something here.\n" 

    | _ -> () (* unknown conversion *) 
    ) ; 

    if (!res_to_process <> []) then begin 
      List.iter (fun (r,e) ->
	let _ = convert r e in
	()
		) !res_to_process
    end else begin 
      let _ = convert "BALDUR" "GAM" in 
      let _ = convert "WORLDMAP" "WMP" in 
      convert_baldur_bcs () ; 
      List.iter (fun (r,e) -> let _ = convert r e in ()) !convert_at_end ;
      (match config.source_variant , config.target_variant with 
      | BG2, IWD2 -> handle_innate_abilities () 
      | _ -> ()
      ) ; 

      ()
    end ; 
  end ;

  Stats.time "DIALOG.TLK" (fun () -> 
    (* make sure we add all those strings! *)
    if not (Queue.is_empty Dc.strings_to_add) then begin
      let dc_lse_strapp_list = Dc.strings_to_add in 
      Load.append_strings (config.target) dc_lse_strapp_list 
    end ;

    (* Emit DIALOG.TLK *)
    let output_dialog = ref (Some("dialog.tlk")) in 
    (match !output_dialog, (config.target).Load.dialog_mod with
      Some(path), true -> 
	let outchan = open_for_writing path true in 
	Tlk.save_tlk path (config.target).Load.dialog outchan 
    | _, _ -> ()) ; 
			  ) () ; 

  Stats.time "CHITIN.KEY" (fun () -> 
    let new_key = process_add_to_key () in 

    let oc = Case_ins.perv_open_out_bin "chitin.key" in 
    Key.save_key new_key oc ;
			  ) () 
    (* Load.saved_game := Some(game) ;  *)
end 

;;

(try 
  Stats.time "main" main () 
with e -> 
  log_and_print "\nERROR: %s\n" (Printexc.to_string e) ) 

;;

let did = ref 0 in 
let total = Array.length (config.source).Load.key.Key.resource in 
if total <> 0 then begin 
  Hashtbl.iter (fun k elt -> 
    log_and_print "Converted %s: %d\n" k elt ;
    did := !did + elt ;
	       ) convert_count ;
  log_and_print "Converted %d/%d = %d%%\n" !did total (!did * 100 / total) 
end 
;;

(match !Util.log_channel with
  Some(o) -> Stats.print o "\n\t\tWeiMorph Timings\n" 
| None -> Stats.print stdout "\n\t\tWeiMorph Timings\n" ) 

;; 

(match !Util.log_channel with
  Some(o) -> close_out o 
| None -> () ) 

;;

Util.log_channel := None 

;;

exit 0 
;;
