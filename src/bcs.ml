(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.bcs.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)
open Util
open Ids

type script = condition_response list
and condition_response = condition * response list
and condition = trigger list
and trigger = {
    mutable trigger_id : Int32.t ;
    mutable negated : bool ;
    mutable t_1 : Int32.t ;
    mutable t_2 : Int32.t ;
    mutable t_3 : string ;
    mutable t_4 : string ;
    mutable t_5 : object_param ;
    mutable t_coord : point_param ;
    unknown : Int32.t;  (* 3rd int parameter, iwd2  *)
  } 
and response = int * action list 
and action = {
    mutable action_id : Int32.t;
    mutable a_1 : object_param ; (* ActionOverride(a_1, ...) *)
    mutable a_2 : object_param ;
    mutable a_3 : object_param ;
    mutable a_4 : Int32.t ;
    a_5 : point_param; 
    a_6 : Int32.t ;
    a_7 : Int32.t ;
    mutable a_8 : string ;
    a_9 : string ;
  } 
and object_param = {
    mutable o_ea : Int32.t ;
    mutable o_faction : Int32.t ; (* PST *) 
    mutable o_team : Int32.t ; (* PST *)
    mutable o_general : Int32.t ;
    mutable o_race : Int32.t ;
    mutable o_subrace : Int32.t ; (* IWD2 *) 
    mutable o_class : Int32.t ;
    mutable o_specific : Int32.t ;
    mutable o_gender : Int32.t ;
    mutable o_alignment : Int32.t ;
    mutable o_identifiers : Int32.t ;
    mutable o_unknown1 : Int32.t ;
    mutable o_unknown2 : Int32.t ;
    mutable o_unknown3 : Int32.t ;
    mutable o_unknown4 : Int32.t ;
    mutable o_name : string ;
    mutable o_rect : rect_param ; 
    mutable o_iwd2_1 : Int32.t ;
    mutable o_iwd2_2 : Int32.t ;
  } 
and point_param = Int32.t * Int32.t 
and rect_param = Int32.t * Int32.t * Int32.t * Int32.t
let empty_rect () = (-1l, -1l, -1l, -1l)

let empty_object_param () = {
  o_ea = 0l;
  o_faction = 0l;
  o_team = 0l;
  o_general = 0l;
  o_race = 0l;
  o_subrace = 0l;
  o_class = 0l;
  o_specific = 0l;
  o_gender = 0l;
  o_alignment = 0l;
  o_identifiers = 0l;
  o_unknown1 = 0l;
  o_unknown2 = 0l;
  o_unknown3 = 0l;
  o_unknown4 = 0l;
  o_name = "";
  o_rect = empty_rect () ; 
  o_iwd2_1 = 0l ;
  o_iwd2_2 = 0l ; 
} 

let empty_action () = { 
  action_id = 0l ;
  a_1 = empty_object_param () ; 
  a_2 = empty_object_param () ; 
  a_3 = empty_object_param () ; 
  a_4 = 0l  ; 
  a_5 = (0l,0l) ; 
  a_6 = 0l  ; 
  a_7 = 0l  ; 
  a_8 = "";
  a_9 = "";
} 

let empty_trigger () = { 
  trigger_id = 0l ;
  negated = false ;
  t_coord = (0l,0l) ; 
  t_1 = 0l ;
  t_2 = 0l ;
  t_3 = "" ;
  t_4 = "" ;
  t_5 = empty_object_param () ;
  unknown = 0l ;
}

let ids_file_of_object_position ss i =
  match ss, i with
  | Load.IWD2,0 -> "EA"
  | Load.IWD2,1 -> "GENERAL"
  | Load.IWD2,2 -> "RACE"
  | Load.IWD2,3 -> "CLASS"
  | Load.IWD2,4 -> "SPECIFIC"
  | Load.IWD2,5 -> "GENDER"
  | Load.IWD2,6 -> "ALIGNMNT"
  | Load.IWD2,7 -> "SUBRACE"
  | Load.IWD2,8 -> "AVCLASS"
  | Load.IWD2,9 -> "CLASSMSK"

  | Load.BG1,0 -> "EA"
  | Load.BG1,1 -> "GENERAL"
  | Load.BG1,2 -> "RACE"
  | Load.BG1,3 -> "CLASS"
  | Load.BG1,4 -> "SPECIFIC"
  | Load.BG1,5 -> "GENDER"
  | Load.BG1,6 -> "ALIGN"

  | Load.BG2,0 -> "EA"
  | Load.BG2,1 -> "GENERAL"
  | Load.BG2,2 -> "RACE"
  | Load.BG2,3 -> "CLASS"
  | Load.BG2,4 -> "SPECIFIC"
  | Load.BG2,5 -> "GENDER"
  | Load.BG2,6 -> "ALIGN"

  | Load.IWD1,0 -> "EA"
  | Load.IWD1,1 -> "GENERAL"
  | Load.IWD1,2 -> "RACE"
  | Load.IWD1,3 -> "CLASS"
  | Load.IWD1,4 -> "SPECIFIC"
  | Load.IWD1,5 -> "GENDER"
  | Load.IWD1,6 -> "ALIGN"

  | Load.PST,0 -> "EA"
  | Load.PST,1 -> "FACTION"
  | Load.PST,2 -> "TEAM"
  | Load.PST,3 -> "GENERAL"
  | Load.PST,4 -> "RACE"
  | Load.PST,5 -> "CLASS"
  | Load.PST,6 -> "SPECIFIC"
  | Load.PST,7 -> "GENDER"
  | Load.PST,8 -> "ALIGN"

  | _,i -> failwith (Printf.sprintf "Don't know what to do with the %dth specifier" i )

type ids_map = { (* represents one IDS file *)
    from_int : (Int32.t, ids) Hashtbl.t ;
    from_sym : (string, ids) Hashtbl.t ;
    from_uppercase_sym : (string, ids) Hashtbl.t ;
  } 

let non_empty_object o =
  o.o_ea <> 0l ||
  o.o_faction <> 0l ||
  o.o_team <> 0l || 
  o.o_general <> 0l ||
  o.o_race <> 0l ||
  o.o_class <> 0l ||
  o.o_specific <> 0l ||
  o.o_gender <> 0l ||
  o.o_alignment <> 0l ||
  o.o_identifiers <> 0l ||
  o.o_unknown1 <> 0l ||
  o.o_unknown2 <> 0l ||
  o.o_unknown3 <> 0l ||
  o.o_unknown4 <> 0l ||
  o.o_name <> ""

type how_to_save_bcs = 
  | Save_BCS_OC of out_channel
  | Save_BCS_Buffer of Buffer.t

let check_file_presence_trigger string script_style ids game first_or_second_string =
  if Modder.get "MISSING_RESREF" = Modder.None then ()
  else (* if script_style = Load.BG then *)
    (
     if string <> "" then begin
       let ext = match ids with
       | 0x0001l (* Acquired(S:Resref.) *)
       | 0x0009l (* Unusable(S:Resref.) *)
       | 0x4042l (* PartyHasItem(S:Resref.) *)
       | 0x4061l (* HasItem(S:Item.,O:) *)
       | 0x4075l (* Contains(S:,O:) *)
       | 0x4077l (* NumItems(S:,I) *)
       | 0x4078l (* NumItemsLT(S:,I) *)
       | 0x4079l (* NumItemsGT(S:,I) *)
       | 0x407Al (* NumItemsParty(S:,I) *)
       | 0x407Bl (* NumItemsPartyLT(S:,I) *)
       | 0x407Cl (* NumItemsPartyGT(S:,I) *)
       | 0x407Fl (* HasItemEquiped(S:) *)
       | 0x40A9l (* PartyHasItemIdentified(S:) *)
       | 0x40C2l (* HasItemRealEquiped(S:) *)
         -> "itm"

       | 0x407El (* AreaCheck(S:) *)
       | 0x40D4l (* AreaCheck(S:) *)
         -> "are"

       | 0x4031l (* HaveSpellRes *)
         -> "spl"

       | _ -> ""
       in

       if ext <> "" then begin
         if not (try
           Load.skip_next_load_error := true;
           let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true string ext in
           (String.length buff > 0) with | Invalid_argument "String.create" -> true
	 | _ -> false )
         then (Modder.handle_deb "MISSING_RESREF"
		 (Printf.sprintf "POSSIBLE ERROR: file %s.%s is not found in trigger %d.\n" string ext (Int32.to_int ids)))

       end
     end
    ) ;
  ()

let check_file_presence_action string script_style ids game first_or_second_string =
  if Modder.get "MISSING_RESREF" = Modder.None then ()
  else (* if script_style = Load.BG then *)
    (
     if string <> "" then begin
       let ext = match ids with
       | 9l   (* DropItem *)
       | 11l  (* EquipItem *)
       | 14l  (* GetItem *)
       | 15l  (* GiveItem *)
       | 34l  (* UseItem *)
       | 82l  (* CreateItem *)
       | 97l  (* UseItemPoint *)
       | 116l (* TakePartyItem *)
       | 140l (* GiveItemCreate *)
       | 169l (* DestroyItem *)
       | 188l (* TakeItemPartyAll *)
       | 193l (* TakePartyItemRange *)
       | 204l (* TakePartyItemNum *)
       | 257l (* PickUpItem *)
       | 278l (* GiveItemReplace *)
       | 292l (* DisplayStringOverHeadOwner *)
         -> [ "itm" ]

       | 91l  (* LeaveArea *)
       | 108l (* EscapeAreaMove *)
       | 110l (* LeaveAreaLUA *)
       | 170l (* RevealAreaOnMap *)
       | 185l (* SetMasterArea *) (* Remember to check wether it's BCS or ARE *)
       | 189l (* LeaveAreaLUAPanic *)
       | 197l (* MoveAreaGlobal *)
       | 251l (* HideAreaOnMap *)
       | 264l (* CopyGroundPilesTo *)
       | 277l (* EscapeAreaObjectMove *)
       | 310l (* MoveGlobalsTo *)
       | 347l (* SetEncounterProbability *)
         -> [ "are" ]

       | 225l (* MoveBetweenAreas *)
       | 350l (* LeaveAreaLuaEntry *)
       | 351l when first_or_second_string = 1 (* LeaveAreaLuaEntryPanic *)
         -> [ "are" ]

       | 279l (* AddSpecialAbility *)
       | 31l  (* SpellRES *)
       | 95l  (* SpellPointRES *)
       | 113l (* ForceSpellRES *)
       | 114l (* ForceSpellPointRES *)
       | 160l (* ApplySpellRES *)
       | 181l (* ReallyForceSpellRES *)
       | 191l (* SpellNoDecRES *)
       | 192l (* SpellPointNoDecRES *)
       | 240l (* ReallyForceSpellDeadRES *)
       | 318l (* ForceSpellRangeRES *)
       | 319l (* ForceSpellPointRangeRES *)
       | 337l (* ReallyForceSpellPointRES *)
         -> [ "spl" ]

       | 7l   (* CreateCreature *)
       | 194l (* ChangeAnimation (takes a cre) *)
       | 219l (* ChangeAnimationNoEffect (see above) *)
       | 228l (* CreateCreatureImpassable *)
       | 231l (* CreateCreatureDoor *)
       | 232l (* CreateCreatureObjectDoor *)
       | 233l (* CreateCreatureObjectOffScreen *)
       | 238l (* CreateCreatureOffscren *)
       | 252l (* CreateCreatureObjectOffset *)
       | 295l (* CreateCreatureCopyPoint *)
       | 323l (* CreateCreatureImpassableAllowOverlap *)
         -> [ "cre" ]

       | 250l (* CreateCreatureObjectCopy *)
       | 227l when first_or_second_string = 1 (* CreateCreatureObject *)
         -> [ "cre" ]

       | 26l (* PlaySound *)
         -> [ "wav" ]

       | 60l  (* ChangeAIScript *)
       | 120l (* StartCutScene *)
         -> [ "bcs" ]

       | 137l (* StartDialog(ue) *)
       | 138l (* SetDialog(ue) *)
       | 266l (* StartDialog(ue)Interrupt *)
       | 293l (* StartDialogueOverride *)
       | 294l (* StartDialogueOverrideInterrupt *)
       | 334l (* StartDialogueNoName *)
         -> [ "dlg" ]

       | 150l (* StartStore *)
         -> [ "sto" ]

       | 161l (* IncrementChapter *)
       | 199l (* TextScreen *)
       | 220l (* TakeItemPartyList *)
       | 226l (* TakeItemPartyListNum *)
         -> [ "2da" ]

       | 167l (* StartMovie *)
         -> [ "mve" ]

       | 272l (* CreateVisualEffectPoint *)
       | 273l (* CreateVisualEffectObject *)
         -> [ "vvc"; "bam" ]

       | 341l (* StaticPalette *) (* Ask wether it's really a bmp file *)
         -> [ "bmp" ]

       | 225l (* MoveBetweenAreasEffect *)
       | 227l (* CreateCreatureObjectEffect *)
       | 250l when first_or_second_string = 2 (* CreateCreatureObjectCopyEffect *)
         -> [ "vvc"; "bam" ]

       | _ -> []
       in

       if ext <> [] then begin
         let result = ref false in
         let put_or = ref false in
         List.iter (fun ext_s -> 
           try
(*             log_and_print "Looking for %s.%s\n" string ext_s ; *)
             Load.skip_next_load_error := true;
             let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true string ext_s in
             result := !result or ( String.length buff > 0 ) ;
           with | Invalid_argument "String.create" -> result := true
	 | _ -> ()
		   ) ext ;
         if not !result then ( Modder.handle_deb "MISSING_RESREF"
				 (Printf.sprintf "POSSIBLE ERROR: file %s.%s is not found in action %d.\n" string (List.fold_left
														     (fun acc this -> acc ^ (if not !put_or then (put_or:=true ; this) else ( "/" ^ this))) "" ext)
				    (Int32.to_int ids)))

       end
     end
    ) ;
  ()

let save_bcs game how bcs =
  Stats.time "marshal BCS" (fun () ->
    let bcs_printf fmt = 
      let k result = match how with
      | Save_BCS_OC(oc) -> output_string oc result
      | Save_BCS_Buffer(b) -> Buffer.add_string b result
      in 
      Printf.kprintf k fmt 
    in 
    let rec save_bcs game bcs =
      bcs_printf "SC\n" ;
      List.iter (save_cr game) bcs ;
      bcs_printf "SC\n"
    and save_cr game (c,rl) =
      bcs_printf "CR\nCO\n" ;
      List.iter (save_t game ) c ;
      bcs_printf "CO\nRS\n" ;
      List.iter (save_r game ) rl ;
      bcs_printf "RS\nCR\n"
    and save_t game t =
      check_file_presence_trigger t.t_3 game.Load.script_style t.trigger_id game 1 ;
      check_file_presence_trigger t.t_4 game.Load.script_style t.trigger_id game 2 ;
      begin
	match game.Load.script_style with
	| Load.PST ->
	    let x,y = t.t_coord in 
	    bcs_printf "TR\n%ld %ld %d %ld %ld [%ld,%ld] \"%s\" \"%s\" "
              t.trigger_id t.t_1 (if t.negated then 1 else 0)
              t.t_2 t.unknown x y t.t_3 t.t_4 ;
	| Load.BG1
	| Load.BG2
	| Load.IWD1
	| Load.IWD2 ->
	    (* IWD2 NearLocation(Myself,111,222,333)
	     * -> TR\n16553 111 0 222 333 "" ""
	     *)
	    bcs_printf "TR\n%ld %ld %d %ld %ld \"%s\" \"%s\" "
              t.trigger_id t.t_1 (if t.negated then 1 else 0)
              t.t_2 t.unknown t.t_3 t.t_4 ;
	| Load.NONE -> failwith "No scripting style specified."
      end ;
      save_obj game t.t_5 ;
      bcs_printf "TR\n" 
    and save_r game (w,al) =
      bcs_printf "RE\n%d" w ;
      List.iter (save_a game) al ;
      bcs_printf "RE\n" ;
    and save_a game a =
      check_file_presence_action a.a_8 game.Load.script_style a.action_id game 1 ;
      check_file_presence_action a.a_9 game.Load.script_style a.action_id game 2 ;
      bcs_printf "AC\n%ld" a.action_id;
      save_obj game a.a_1 ;
      save_obj game a.a_2 ;
      save_obj game  a.a_3 ;
      let p1,p2 = a.a_5 in 
      bcs_printf "%ld %ld %ld %ld %ld\"%s\" \"%s\" AC\n"
	a.a_4 p1 p2 a.a_6 a.a_7 a.a_8 a.a_9 
    and save_obj game obj =
      match game.Load.script_style with 
      | Load.IWD2 -> 
	  let w,x,y,z = obj.o_rect in
	  bcs_printf "OB\n%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld [%ld.%ld.%ld.%ld] \"%s\" %ld %ld OB\n"
	    obj.o_ea
	    obj.o_general
	    obj.o_race
	    obj.o_class (* NO CLUE JONES! THIS DOES NOT WORK *) 
	    obj.o_specific
	    obj.o_gender
	    obj.o_alignment
	    obj.o_subrace (* NO CLUE JONES! THIS DOES NOT WORK *) 
	    obj.o_identifiers obj.o_unknown1 obj.o_unknown2
	    obj.o_unknown3 obj.o_unknown4 
	    w x y z 
	    obj.o_name
	    obj.o_iwd2_1 obj.o_iwd2_2 
      | Load.BG1
      | Load.BG2 ->
	  bcs_printf "OB\n%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld \"%s\"OB\n"
	    obj.o_ea obj.o_general obj.o_race obj.o_class
	    obj.o_specific obj.o_gender obj.o_alignment 
	    obj.o_identifiers obj.o_unknown1 obj.o_unknown2
	    obj.o_unknown3 obj.o_unknown4 obj.o_name
      | Load.IWD1 ->
	  let w,x,y,z = obj.o_rect in
	  bcs_printf "OB\n%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld [%ld.%ld.%ld.%ld] \"%s\"OB\n"
	    obj.o_ea obj.o_general obj.o_race obj.o_class
	    obj.o_specific obj.o_gender obj.o_alignment
	    obj.o_identifiers obj.o_unknown1 obj.o_unknown2
	    obj.o_unknown3 obj.o_unknown4 w x y z
	    obj.o_name
      | Load.PST ->
	  let w,x,y,z = obj.o_rect in
	  bcs_printf "OB\n%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld [%ld.%ld.%ld.%ld] \"%s\"OB\n"
	    obj.o_ea obj.o_faction obj.o_team obj.o_general obj.o_race obj.o_class
	    obj.o_specific obj.o_gender obj.o_alignment
	    obj.o_identifiers obj.o_unknown1 obj.o_unknown2
	    obj.o_unknown3 obj.o_unknown4
	    w x y z
	    obj.o_name
      | Load.NONE -> failwith "No scripting style specified."
    in
    save_bcs game bcs 
			   ) () 

let all_games_ids_state = Hashtbl.create 3 

(* let ids_state : (string, ids_map) Hashtbl.t = Hashtbl.create 511  *)

let make_ids_map il = 
  let from_int = Hashtbl.create 511 in
  let from_sym = Hashtbl.create 511 in
  let from_uppercase_sym = Hashtbl.create 511 in
  List.iter (fun ids ->
    (* log_and_print "IDS: %ld %s\n" ids.i_num ids.i_name ;  *)
    Hashtbl.add from_int ids.i_num ids ;
    Hashtbl.add from_sym ids.i_name ids ;
    Hashtbl.add from_uppercase_sym (String.uppercase ids.i_name) ids ;
	    ) il ;
  { from_int = from_int ;
    from_sym = from_sym ;
    from_uppercase_sym = from_uppercase_sym; } 

let clear_ids_map game = 
  try
    let ids_state = Hashtbl.find all_games_ids_state game.Load.game_path in
    Hashtbl.clear ids_state ;
    log_or_print "[*.IDS] forgotten\n"  
  with e ->  ()
      (*
	log_only "[*.IDS] in [%s]\n" game.Load.game_path 
       *)

let get_ids_map game ids_filename =
  let ids_filename = String.uppercase ids_filename in 
  try
    let ids_state = Hashtbl.find all_games_ids_state game.Load.game_path in
    Hashtbl.find ids_state ids_filename
  with e -> begin
    let buff,path = Load.load_resource "get_ids_map" game true ids_filename "IDS" in 
    let where_newline = String.index buff '\n' in
    let before = Str.string_before buff where_newline in
    let ok = Str.string_match (Str.regexp "[0-9][xX]?[0-9]* .*") before 0 in
    let buff = if ok then buff else Str.string_after buff where_newline in
    if !debug_ocaml then log_and_print "%s.ids: the first line is %sOK\n" ids_filename (if ok then "" else "not ");
    let lexbuf = lex_init_from_string ids_filename buff in
    let lexer = if (((game.Load.script_style = Load.IWD1 || game.Load.script_style = Load.IWD2) &&
                     ids_filename <> "ACTION" &&
                     ids_filename <> "TRIGGER" ) ||
		     (game.Load.script_style = Load.PST && ids_filename = "ANIMATE")) then
      Idslexer2.initial
    else Idslexer.initial
    in
    let result = Stats.time "parsing .ids files" (fun () ->
      Idsparser.ids_file lexer lexbuf) () in
    pop_context () ; 
    log_or_print_modder "[%s.IDS] parsed\n" ids_filename ;
    let ids_map = make_ids_map result in 
    let my_ids_state = try 
      Hashtbl.find all_games_ids_state game.Load.game_path 
    with Not_found -> Hashtbl.create 511 
    in 
    Hashtbl.add my_ids_state ids_filename ids_map ;
    Hashtbl.add all_games_ids_state game.Load.game_path my_ids_state ;
    ids_map
  end 

let five_quotes_string s =
  let find chars str = List.exists (fun char ->
    let x = try ignore (String.index str char) ; true with _ -> false in
    x
				   ) chars in
  if find ['\"';',';'.';'(';')'] s then
	     "\"\"\"\"\"" ^ s ^ "\"\"\"\"\"" else s

let five_quotes res = {res with i_name = five_quotes_string res.i_name}

let ids_of_int game ids_file i =
  let ids_file = String.uppercase ids_file in
  let ids_map : ids_map = get_ids_map game ids_file in
  let res = (Hashtbl.find ids_map.from_int i) in
  five_quotes res


let ids_of_sym game ids_file sym =
  let ids_file = String.uppercase ids_file in
  let ids_map : ids_map = get_ids_map game ids_file in
  try
    (Hashtbl.find ids_map.from_sym sym)
  with Not_found ->
    let a = (Hashtbl.find ids_map.from_uppercase_sym (String.uppercase sym)) in
(*    let msg = Printf.sprintf "[%s] should be [%s] (note case)\n"
      sym a.i_name in
      (try input_error "PARSE" msg with _ -> () ) ; *)
    a

let every_ids_of_int game ids_file i =
  let ids_file = String.uppercase ids_file in
  let ids_map : ids_map = get_ids_map game ids_file in
  List.map five_quotes (Hashtbl.find_all ids_map.from_int i)

let int_of_sym game ids_file sym =
  let ids_file = String.uppercase ids_file in 
  let ids = ids_of_sym game ids_file sym in
  ids.i_num 

let sym_of_int game ids_file i =
  let ids_file = String.uppercase ids_file in 
  try
    let ids_map : ids_map = get_ids_map game (String.uppercase ids_file ) in
    five_quotes_string ((Hashtbl.find ids_map.from_int i).i_name)
  with _ ->
    (* log_or_print "WARNING: %d (0x%x) not found in %s.IDS\n" i i ids_file ; *)
    Printf.sprintf "%ld" i

let is_concat_string ss ids =
  match ss, ids.i_num with
  | _, 0x400Fl (*Global(S:Name.,S:Area.,I:Value.)*)
  | _, 0x4034l (*GlobalGT(S:Name.,S:Area.,I:Value.)*)
  | _, 0x4035l (*GlobalLT(S:Name.,S:Area.,I:Value.)*)
  | _, 30l (*SetGlobal(S:Name.,S:Area.,I:Value.)*)
  | _, 115l (*SetGlobalTimer(S:Name.,S:Area.,I:Time.GTimes)*)
  | _, 109l (*IncrementGlobal(S:Name.,S:Area.,I:Value.)*)
  | Load.BG1, 246l (*CreateCreatureAtLocation(S:GLOBAL.,S:Area.,S:ResRef.)*)
  | Load.BG2, 246l (*CreateCreatureAtLocation(S:GLOBAL.,S:Area.,S:ResRef.)*)
  | _, 256l (*CreateItemGlobal(S:Global.,S:Area.,S:ResRef.)*)
  | _, 268l (* RealSetGlobalTimer(S:Name*,S:Area*,I:Time*GTimes) *)
  | Load.BG1, 297l (* MoveToSavedLocation(S:*,S:* ) *)
  | Load.BG2, 297l (* MoveToSavedLocation(S:*,S:* ) *)
  | _, 335l (* SetTokenGlobal(S:GLOBAL.,S:Area.,S:Token.) *)
  | Load.PST, 227l (* GlobalBAND(S:Name*,S:Area*,I:Value *)
  | Load.PST, 228l (* GlobalBOR(S:Name*,S:Area*,I:Value *)
  | Load.PST, 229l (* GlobalSHR(S:Name*,S:Area*,I:Value*,I:ShiftIn*Boolean) *)
  | Load.PST, 230l (* GlobalSHL(S:Name*,S:Area*,I:Value*,I:ShiftIn*Boolean) *)
  | Load.PST, 231l (* GlobalMAX(S:Name*,S:Area*,I:Value *)
  | Load.PST, 232l (* GlobalMIN(S:Name*,S:Area*,I:Value *)
  | Load.PST, 244l (* BitSet(S:Name*,S:Area*,I:Bit*Bits) *)
  | Load.PST, 0x407Fl (* BitCheck(S:Name*,S:Area*,I:Bit*Bits) *)
  | Load.PST, 0x4081l (* BitCheckExact(S:Name*,S:Area*,I:Bit*Bits) *)
  | Load.PST, 245l (* BitClear(S:Name*,S:Area*,I:Bit*Bits) *)
  | Load.PST, 141l (* GivePartyGoldGlobal(S:Name*,S:Area *)
  | Load.PST, 165l (* AddexperiencePartyGlobal(S:Name*,S:Area *)
  | Load.PST, 260l (* GlobalXOR(S:Name*,S:Area*,I:Value *)
  | Load.PST, 0x4080l (* GlobalBAND(S:Name*,S:Area*,I:Value *)
  | Load.PST, 0x4095l (* Xor(S:Name*,S:Area*,I:Value*)
  | Load.PST, 0x409Cl (* StuffGlobalRandom(S:Name*,S:Area*,I:Range*)
  | Load.IWD2, 247l   (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD2, 0x40A5l (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD2, 306l    (* SetGlobalRandom(S:Name*,S:Area*,I:Min*,I:Max*)
  | Load.IWD2, 307l    (* SetGlobalTimerRandom(S:Name*,S:Area*,I:Min*,I:Max*)
  | Load.IWD2, 308l (*SetGlobalTimerOnce(S:Name*,S:Area*,I:Time*GTimes) *)
  | Load.IWD1, 141l (* GivePartyGoldGlobal(S:Name*,S:Area) *)
  | Load.IWD1, 165l (* AddexperiencePartyGlobal(S:Name*,S:Area) *)
  | Load.IWD1, 247l   (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD1, 0x40A5l (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
    -> 1
  | Load.PST, 233l (* GlobalSetGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 234l (* GlobalAddGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 235l (* GlobalSubGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 236l (* GlobalANDGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 237l (* GlobalORGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 238l (* GlobalBANDGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 239l (* GlobalBORGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 240l (*GlobalSHRGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:ShiftIn*Boolean)*)
  | Load.PST, 241l (*GlobalSHLGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:ShiftIn*Boolean)*)
  | Load.PST, 242l (* GlobalMAXGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 243l (* GlobalMINGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2 *)
  | Load.PST, 261l (*  GlobalXORGlobal(S:Name*,S:Area*,S:Name2*,S:Area2 *)
  | Load.PST, 0x4082l (* GlobalEqualsGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4083l (* GlobalLTGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4084l (* GlobalGTGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4085l (* GlobalANDGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4086l (* GlobalORGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4087l (* GlobalBANDGlobal(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.PST, 0x4088l (* GlobalBANDGlobalExact(S:Name1*,S:Area1*,S:Name2*,S:Area2*)
  | Load.IWD1, 243l   (* IncrementGlobalOnce(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:Val*)
  | Load.IWD2, 243l   (* IncrementGlobalOnce(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:Val*)
  | Load.PST, 202l    (* IncrementGlobalOnce(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:Val*)
  | Load.IWD2, 248l   (* GlobalBitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD2, 0x40A6l (* GlobalBitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD1, 248l   (* GlobalBitGlobal(S:String1*,S:String2*,S:String3*,S:String4*,I:Mode*BitMode) *)
  | Load.IWD1, 0x40A6l (* GlobalBitGlobal(S:String1*,S:String2*,S:String3*,S:String4*,I:Mode*BitMode) *)
    -> 2

  | Load.IWD2, 289l   (* SpellCastEffect(O:Source*,S:Voice*,S:Sound1*,S:Sound2*,I:Animation*sceffect,I:Speed*,I:Sequence*Sequence) *)
    -> 3

  | Load.NONE,_ -> failwith "No scripting style specified."
  | _ -> 0



let split6 s =
  if String.length s >= 6 then
    begin
      let (name,scope) = (String.sub s 6 ((String.length s) - 6) ,
			  String.sub s 0 6 ) in
      (* if name.[0] = ':' then (String.sub name 1 ((String.length name) -1),scope) else (name,scope) *)
	  (name,scope)
    end
  else
    "",s

let rec best_ids_of_trigger game c =
  let ids = every_ids_of_int game "TRIGGER" c.trigger_id in 
  if ids = [] then begin
    log_and_print "Cannot resolve trigger 0x%lx\n" c.trigger_id ;
    raise Not_found
  end else begin
    let rec proc lst = match lst with
    | [] -> (try List.hd ids with e -> 
	if c.trigger_id < 0x4000l then begin
	  c.trigger_id <- Int32.add c.trigger_id (0x4000l) ;
	  best_ids_of_trigger game c ;
	end else begin
	  log_and_print "Cannot resolve trigger 0x%lx\n" c.trigger_id ; raise e
	end)
    | ids :: tl -> 
	let string_formal_count = List.fold_left  
	    (fun acc elt  -> acc + 
              if (elt.arg_kind = Arg_String) then 1 else 0) 0 ids.i_args in
	if (string_formal_count > 0 && c.t_3 = "") ||
	(string_formal_count > 1 && c.t_4 = "") || 
	(string_formal_count = 0 && c.t_3 <> "") ||
	(string_formal_count = 0 && c.t_4 <> "") then
	  proc tl
	else begin
	  ids 
	end 
    in 
    proc ids
  end

let rec best_ids_of_action game a = 
  let ids = every_ids_of_int game "ACTION" a.action_id in 
  if ids = [] then begin
    log_and_print "Cannot resolve action 0x%lx\n" a.action_id ;
    raise Not_found
  end else begin
    let rec proc lst = match lst with
    | [] -> (try List.hd ids with e -> 
	if a.action_id < 0x4000l then begin
	  a.action_id <- Int32.add a.action_id (0x4000l);
	  best_ids_of_action game a ;
	end else begin 
	  log_and_print "Cannot resolve action %ld\n" a.action_id ; raise e
	end)
    | ids :: tl -> 
	let string_formal_count = List.fold_left  
	    (fun acc elt  -> acc + 
              if (elt.arg_kind = Arg_String) then 1 else 0) 0 ids.i_args in
	if (string_formal_count > 0 && a.a_8 = "") ||
	(string_formal_count > 1 && a.a_9 = "") || 
	(string_formal_count = 0 && a.a_8 <> "") ||
	(string_formal_count = 0 && a.a_9 <> "") then
	  proc tl
	else begin
	  ids 
	end 
    in 
    proc ids
  end

type aarg =
    Act_Integer of Int32.t
  | Act_String of string
  | Act_Object of object_param 
  | Act_Point of point_param 

type print_what =
  | BCS_Print_Script of script
  | BCS_Print_ActionList of action list 
  | BCS_Print_TriggerList of trigger list

let trigger_to_arg_list ss t ids game =
  match is_concat_string ss ids with
  | 3 (* FIXME *)
  | 2 (* FIXME *)
  | 1 ->
      let a,b = split6 t.t_3 in
      [ (Arg_Integer,(Act_Integer t.t_1)) ;
	(Arg_Integer,(Act_Integer t.t_2)) ;
	(Arg_Integer,(Act_Integer t.unknown)) ;
	(Arg_Point,(Act_Point t.t_coord)) ;
	(Arg_String,(Act_String a));
	(Arg_String,(Act_String b));
	(Arg_String,(Act_String t.t_4));
	(Arg_Object,(Act_Object t.t_5)) ; ]
  | _ ->
      [ (Arg_Integer,(Act_Integer t.t_1)) ;
	(Arg_Integer,(Act_Integer t.t_2)) ;
	(Arg_Integer,(Act_Integer t.unknown)) ;
	(Arg_Point,(Act_Point t.t_coord)) ;
	(Arg_String,(Act_String t.t_3)) ;
	(Arg_String,(Act_String t.t_4)) ;
	(Arg_Object,(Act_Object t.t_5)) ; ]

let action_to_arg_list ss a ids =
  match is_concat_string ss ids with
  | 1 -> 
      let aa,b = split6 a.a_8 in
      [ (Arg_Object,(Act_Object a.a_2)) ;
	(Arg_Object,(Act_Object a.a_3)) ;
	(* (Arg_Object,(Act_Object a.a_1)) ; *)
	(Arg_Integer,(Act_Integer a.a_4)) ;
	(Arg_Point,(Act_Point a.a_5)) ;
	(Arg_Integer,(Act_Integer a.a_6)) ;
	(Arg_Integer,(Act_Integer a.a_7)) ;
	(Arg_String,(Act_String aa)) ;
	(Arg_String,(Act_String b)) ;
	(Arg_String,(Act_String a.a_9)) ; ]
  | 2 -> 
      let aa,b = split6 a.a_8 in
      let aa',b' = split6 a.a_9 in
      [ (Arg_Object,(Act_Object a.a_2)) ;
	(Arg_Object,(Act_Object a.a_3)) ;
	(* (Arg_Object,(Act_Object a.a_1)) ; *)
	(Arg_Integer,(Act_Integer a.a_4)) ;
	(Arg_Point,(Act_Point a.a_5)) ;
	(Arg_Integer,(Act_Integer a.a_6)) ;
	(Arg_Integer,(Act_Integer a.a_7)) ;
	(Arg_String,(Act_String aa)) ;
	(Arg_String,(Act_String b)) ;
	(Arg_String,(Act_String aa')) ;
	(Arg_String,(Act_String b')) ]
  | 3 ->
      [ (Arg_Object,(Act_Object a.a_2)) ;
	(Arg_Object,(Act_Object a.a_3)) ;
	(* (Arg_Object,(Act_Object a.a_1)) ; *)
	(Arg_Integer,(Act_Integer a.a_4)) ;
	(Arg_Point,(Act_Point a.a_5)) ;
	(Arg_Integer,(Act_Integer a.a_6)) ;
	(Arg_Integer,(Act_Integer a.a_7)) ;
	(Arg_String,(Act_String a.a_8)) ;
	(Arg_String,(Act_String a.a_9)) ;
	(Arg_String,(Act_String "")) ; ]
  | _ ->
      [ (Arg_Object,(Act_Object a.a_2)) ;
	(Arg_Object,(Act_Object a.a_3)) ;
	(* (Arg_Object,(Act_Object a.a_1)) ; *)
	(Arg_Integer,(Act_Integer a.a_4)) ;
	(Arg_Point,(Act_Point a.a_5)) ;
	(Arg_Integer,(Act_Integer a.a_6)) ;
	(Arg_Integer,(Act_Integer a.a_7)) ;
	(Arg_String,(Act_String a.a_8)) ;
	(Arg_String,(Act_String a.a_9)) ; ]

let get_first_of_type argl tau =
  if List.mem_assoc tau !argl then begin 
    let res = List.assoc tau !argl in
    argl := List.remove_assoc tau !argl ;
    res
  end else 
    failwith "cannot find an argument of the right type" 

let formal_arg_is_strref arg = 
  let up = String.uppercase arg.arg_comment in
  up = "STRREF" || up = "STRINGREF"

let print_script_text game how what comments strhandle =
  let bcs_printf fmt =
    let k result = match how with
    | Save_BCS_OC(oc) -> output_string oc result
    | Save_BCS_Buffer(b) -> Buffer.add_string b result
    in
    Printf.kprintf k fmt
  in
  let rec print_cr (tl,rl) =
    bcs_printf "IF\n" ;
    let or_count = ref 0 in
    List.iter (fun t ->
      let indent = 2 + if !or_count > 0 then (decr or_count ; 2) else 0 in
      bcs_printf "%.*s" indent " " ;
      or_count := !or_count + (print_trigger t) ;
	      ) tl ;
    bcs_printf "THEN\n" ;
    List.iter (fun (w,al) -> 
      bcs_printf "  RESPONSE #%d\n" w ;
      List.iter (fun a -> 
        bcs_printf "    " ;
        print_action a ;
		) al 
	      ) rl ;
    bcs_printf "END\n\n" 
  and print_obj obj =
    (* if obj.o_name <> "" then
       bcs_printf "\"%s\"" obj.o_name
       else *) begin

	 let ilist = [ obj.o_unknown4 ; obj.o_unknown3 ; obj.o_unknown2 ;
		       obj.o_unknown1 ; obj.o_identifiers ; ] 
	 in 

	 let dotlst = match game.Load.script_style with
	 | Load.IWD2 -> [
             obj.o_ea , "EA" ;
             obj.o_general , "GENERAL" ;
             obj.o_race , "RACE" ;
             obj.o_class , "CLASS" ;
             obj.o_specific , "SPECIFIC" ;
             obj.o_gender , "GENDER" ;
             obj.o_alignment , "ALIGNMNT" ;
             obj.o_subrace , "SUBRACE" ;
             obj.o_iwd2_1 , "AVCLASS" ;
             obj.o_iwd2_2 , "CLASSMSK"
	   ] 
	 | Load.BG1
	 | Load.BG2
	 | Load.IWD1 -> [
             obj.o_ea , "EA" ;
             obj.o_general , "GENERAL" ;
             obj.o_race , "RACE" ;
             obj.o_class , "CLASS" ;
             obj.o_specific , "SPECIFIC" ;
             obj.o_gender , "GENDER" ;
             obj.o_alignment , "ALIGN" ] 
	 | Load.PST -> [
             obj.o_ea , "EA" ;
             obj.o_faction , "FACTION" ;
             obj.o_team , "TEAM" ;
             obj.o_general , "GENERAL" ;
             obj.o_race , "RACE" ;
             obj.o_class , "CLASS" ;
             obj.o_specific , "SPECIFIC" ;
             obj.o_gender , "GENDER" ;
             obj.o_alignment , "ALIGN" ] 
	 | Load.NONE -> failwith "No scripting style specified."
	 in

	 let rec all_zeros lst = match lst with
	 | [] -> true
	 | (v,s) :: tl -> if v = 0l then all_zeros tl else false
	 in 

	 let zero_dotlst = all_zeros dotlst in 

	 let rec proc_dot lst = match lst with
	 | [] -> ()
	 | (v,s) :: tl -> 
             let str = if v = 0l then "0" else 
             try sym_of_int game s v with _ -> Printf.sprintf "%ld" v in
             if all_zeros tl then
               bcs_printf "%s" str
             else begin
               bcs_printf "%s." str ;
               proc_dot tl
             end
	 in

	 let get_rect rect =
	   if (game.Load.script_style = Load.IWD2 ||game.Load.script_style = Load.PST || game.Load.script_style = Load.IWD1) && rect <> (-1l,-1l,-1l,-1l) then begin
	     let (a,b,c,d) = rect in
	     Printf.sprintf "[%ld.%ld.%ld.%ld]" a b c d;
	   end else ""
	 in
	 let rec proc lst lp rp anything = match lst with
	 | [] ->
             begin
               if all_zeros dotlst then begin
		 if not anything && obj.o_name = "" then bcs_printf "[ANYONE]%s" (get_rect obj.o_rect)
		 else (if obj.o_name <> "" then bcs_printf "%s\"%s\"%s%s" lp obj.o_name (get_rect obj.o_rect) rp
		 else bcs_printf "%s" (get_rect obj.o_rect))
               end else begin
		 bcs_printf"%s[" lp ;
		 proc_dot dotlst ;
		 bcs_printf "]%s%s" (get_rect obj.o_rect) rp;
               end;
             end
	 | hd :: tl ->
             if hd = 0l then
               proc tl lp rp anything
             else begin
               let str = sym_of_int game "OBJECT" hd in
               bcs_printf "%s%s" lp str ;
               proc tl "(" ")" true ;
               bcs_printf "%s" rp ;
             end
	 in
	 proc ilist "" "" false ;
       end
  and print_arg_list argl ids =
    let commas = ref ((List.length ids.i_args) - 1) in
    List.iter (fun a ->
      begin
	match get_first_of_type argl a.arg_kind with
	| (Act_Object obj) -> print_obj obj
	| (Act_String s) -> bcs_printf "\"%s\"" s
	| (Act_Integer i) -> begin
            if a.arg_file = "" then begin
              match strhandle with
		Some(f) when formal_arg_is_strref a -> begin
                  let str = f i in
                  bcs_printf "%s" str
                end
              | _ -> bcs_printf "%ld" i
            end else begin
              let str = sym_of_int game a.arg_file i in
              bcs_printf "%s" str
            end
        end
	| (Act_Point (x,y)) -> bcs_printf "[%ld.%ld]" x y
      end ;
      if !commas > 0 then begin
        bcs_printf "," ;
        decr commas
      end
	      ) ids.i_args ;
  and print_action a =
    if non_empty_object a.a_1 then begin
      bcs_printf "ActionOverride(" ;
      print_obj a.a_1 ;
      bcs_printf "," ;
    end ;
    let ids = best_ids_of_action game a in
    bcs_printf "%s(" ids.i_name ;
    let argl = ref (action_to_arg_list game.Load.script_style a ids) in
    print_arg_list argl ids ;
    bcs_printf ")" ;
    if non_empty_object a.a_1 then begin bcs_printf ")" end ;
    if comments then begin
      (* look for strref / stringref parameters *)
      let contains_strref = List.exists formal_arg_is_strref ids.i_args in
      if contains_strref then begin
        bcs_printf "  // %s"
          (Tlk.one_line_pretty_print game.Load.dialog (Int32.to_int a.a_4))
      end ;
      match ids.i_name with
      | "GiveItemCreate"
      | "GiveItem"
      | "CreateItem"
      | "DestroyItem"
      | "UseItem"
	-> bcs_printf "  // %s" (name_of_res a.a_8 "ITM" 0xc)
      | "CreateCreature"
      | "CutSceneId"
      | "CreateCreatureOffScreen"
      | "CreateCreatureObject"
      | "CreateCreatureObjectDoor"
      | "CreateCreatureObjectOffScreen"
	-> bcs_printf "  // %s" (name_of_res a.a_8 "CRE" 0xc)
      | "AddSpecialAbility"
      | "ForceSpellRES"
      | "ReallyForceSpellRES"
      | "SpellRES"
      | "ApplySpellRES"
	-> bcs_printf "  // %s" (name_of_res a.a_8 "SPL" 0x8)
      | "MarkSpellAndObject"
	->
	  for i = 0 to (String.length a.a_8 / 4) - 1 do
            let str =
              try
		let sub = String.sub a.a_8 (i*4) 4 in
		let ids = ids_of_int game "SPELL" (Int32.of_string sub) in
		ids.i_name
              with _ -> "???"
            in
            bcs_printf "\n      // %s" str
	  done

      | _ -> ()
    end ;
    bcs_printf "\n"
  and print_trigger t =
    if (t.negated) then bcs_printf "!" ;
    let ids = best_ids_of_trigger game t in
    bcs_printf "%s(" ids.i_name ;
    let argl = ref (trigger_to_arg_list game.Load.script_style t ids game) in
    print_arg_list argl ids ;
    bcs_printf ")" ;
    if comments then begin
      match ids.i_name with
      | "PartyHasItem"
      | "HasItem"
	-> bcs_printf "  // %s" (name_of_res t.t_3 "ITM" 0xc)
      | "Dead"
	-> bcs_printf "  // %s" (name_of_res t.t_3 "CRE" 0xc)
      | _ ->
          if t.t_5.o_name <> "" then
            bcs_printf "  // %s" (name_of_res t.t_5.o_name "CRE" 0xc)
    end ;
    bcs_printf "\n" ;
    if String.uppercase ids.i_name = "OR" then
      (Int32.to_int t.t_1)
    else 0
  and name_of_res r ext offset =
    try
      Load.skip_next_load_error := true ;
      let buff,path = Load.load_resource "printing BAF" game true r ext in
      Load.skip_next_load_error := false ;
      if String.length buff >= offset + 4 then
        (Tlk.one_line_pretty_print game.Load.dialog
           ((int_of_str_off buff offset )))
      else r
    with _ -> r
  in
  match what with
  | BCS_Print_Script(s) -> List.iter print_cr s
  | BCS_Print_ActionList(al) -> List.iter print_action al
  | BCS_Print_TriggerList(tl) -> List.iter
	(fun t -> ignore (print_trigger t)) tl
