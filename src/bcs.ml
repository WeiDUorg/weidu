(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.bcs.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)
open BatteriesInit
open Hashtblinit
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
    (if string <> "" then begin
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
        | _ -> false)
        then (Modder.handle_deb "MISSING_RESREF"
                (Printf.sprintf "POSSIBLE ERROR: file %s.%s is not found in trigger %d.\n" string ext (Int32.to_int ids)))
      end
    end) ;
  ()

let check_ids_presence_trigger value file script_style ids game =
  if Modder.get "MISSING_RESREF" = Modder.None then ()
  else (* if script_style = Load.BG then *)
    (let ext = match ids with
    | 0x4031l (* HaveSpellRes *)
      -> "spl"

    | _ -> ""
    in

    if ext <> "" then begin
      if value = 0l && file = ""
      then (Modder.handle_deb "MISSING_RESREF"
              (Printf.sprintf "POSSIBLE ERROR: spell '0' called for trigger %d.\n" (Int32.to_int ids)))
    end) ;
  ()

let check_file_presence_action string script_style ids game first_or_second_string =
  if Modder.get "MISSING_RESREF" = Modder.None then ()
  else (* if script_style = Load.BG then *)
    (if string <> "" then begin
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
        -> if (Load.eep ()) then
          [ "wbm" ] else [ "mve" ]

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
            Load.skip_next_load_error := true;
            let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true string ext_s in
            result := !result || ( String.length buff > 0 ) ;
          with | Invalid_argument "String.create" -> result := true
        | _ -> ()) ext ;
        if not !result then
          (Modder.handle_deb "MISSING_RESREF"
             (Printf.sprintf "POSSIBLE ERROR: file %s.%s is not found in action %d.\n" string
                (List.fold_left (fun acc this ->
                  acc ^ (if not !put_or then (put_or:=true ; this) else ( "/" ^ this))) "" ext)
                (Int32.to_int ids)))
      end
    end) ;
  ()

let check_ids_presence_action value file script_style ids game =
  if Modder.get "MISSING_RESREF" = Modder.None then ()
  else (* if script_style = Load.BG then *)
    (let ext = match ids with
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
      -> "spl"
    | _ -> ""
    in

    if ext <> "" then begin
      if value = 0l && file = ""
      then (Modder.handle_deb "MISSING_RESREF"
              (Printf.sprintf "POSSIBLE ERROR: spell '0' called for action %d.\n" (Int32.to_int ids)))
     end) ;
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
      check_ids_presence_trigger t.t_1 t.t_3 game.Load.script_style t.trigger_id game;
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
      check_ids_presence_action a.a_4 a.a_8 game.Load.script_style a.action_id game;
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
            obj.o_subrace
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
    save_bcs game bcs) ()

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
    Hashtbl.add from_uppercase_sym (String.uppercase ids.i_name) ids) il ;
  { from_int = from_int ;
    from_sym = from_sym ;
    from_uppercase_sym = from_uppercase_sym ; }

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
                     (game.Load.script_style = Load.PST && ids_filename = "ANIMATE") ||
                     (game.Load.script_style = Load.BG2 && ids_filename = "MISSILE")) then
      Idslexer2.initial
    else Idslexer.initial
    in
    let result = (try
      Stats.time "parsing .ids files" (fun () ->
        Idsparser.ids_file lexer lexbuf) ()
    with e -> log_and_print "WARNING: error parsing %s.IDS: %s\n"
        ids_filename (printexc_to_string e) ; raise e) in
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
    x) chars in
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
  | Load.BG2, 364l (*SetGlobalRandom(S:Variable.,S:Area.,I:Count.,I:Size.)*)
  | Load.BG2, 377l (*SetGlobalTimerRandom(S:Name*,S:Area*,I:Min*GTimes,I:Max*GTimes)*)
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
  | Load.IWD2, 308l (*SetGlobalTimerOnce(S:Name*,S:Area*,I:Time*GTimes) *)
  | Load.IWD1, 141l (* GivePartyGoldGlobal(S:Name*,S:Area) *)
  | Load.IWD1, 165l (* AddexperiencePartyGlobal(S:Name*,S:Area) *)
    -> (1, "")

  | Load.IWD2, 306l    (* SetGlobalRandom(S:Name*,S:Area*,I:Min*,I:Max*)
  | Load.IWD2, 307l    (* SetGlobalTimerRandom(S:Name*,S:Area*,I:Min*,I:Max*)
  | Load.IWD2, 247l   (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD2, 0x40A5l (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD1, 247l   (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD1, 0x40A5l (* BitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
    -> (1, ":")

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
    -> (2, "")

  (* EE special: IncrementGlobalOnce(S:Name1*,S:Area1*,S:Name2*,S:Area2*,I:Val*)
  | Load.BG2, 446l when ids.i_name <> "IncrementGlobalOnceEx"
    -> (2, "")

  | Load.IWD2, 248l   (* GlobalBitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD2, 0x40A6l (* GlobalBitGlobal(S:String1*,S:String2*,I:Value,I:Mode*BitMode) *)
  | Load.IWD1, 248l   (* GlobalBitGlobal(S:String1*,S:String2*,S:String3*,S:String4*,I:Mode*BitMode) *)
  | Load.IWD1, 0x40A6l (* GlobalBitGlobal(S:String1*,S:String2*,S:String3*,S:String4*,I:Mode*BitMode) *)
    -> (2, ":")

  | Load.IWD2, 289l   (* SpellCastEffect(O:Source*,S:Voice*,S:Sound1*,S:Sound2*,I:Animation*sceffect,I:Speed*,I:Sequence*Sequence) *)
    -> (3, ":")

  | Load.NONE,_ -> failwith "No scripting style specified."
  | _ -> (0, "")



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

let split_colon s =
  let splat = Str.split (Str.regexp ":") s in
  (match splat with
  | l :: r :: rest -> (r,l)
  | _ -> (s,""))

let rec best_ids_of_trigger game c =
  let ids = every_ids_of_int game "TRIGGER" c.trigger_id in
  if ids = [] then begin
    log_and_print "ERROR: cannot resolve trigger 0x%lx\n" c.trigger_id ;
    raise Not_found
  end else begin
    let rec proc lst = match lst with
    | [] -> (try List.hd ids with e ->
        if c.trigger_id < 0x4000l then begin
          c.trigger_id <- Int32.add c.trigger_id (0x4000l) ;
          best_ids_of_trigger game c ;
        end else begin
          log_and_print "ERROR: cannot resolve trigger 0x%lx\n" c.trigger_id ; raise e
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
    log_and_print "ERROR: cannot resolve action %ld\n" a.action_id ;
    raise Not_found
  end else begin
    let rec proc lst = match lst with
    | [] -> (try List.hd ids with e ->
        if a.action_id < 0x4000l then begin
          a.action_id <- Int32.add a.action_id (0x4000l);
          best_ids_of_action game a ;
        end else begin
          log_and_print "ERROR: cannot resolve action %ld\n" a.action_id ; raise e
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
  | Act_Integer of Int32.t
  | Act_String of string
  | Act_Object of object_param
  | Act_Point of point_param

type print_what =
  | BCS_Print_Script of script
  | BCS_Print_ActionList of action list
  | BCS_Print_TriggerList of trigger list * bool

let trigger_to_arg_list ss t ids game =
  match is_concat_string ss ids with
  | (3, interposer) (* FIXME *)
  | (2, interposer) ->
      let a,b = if interposer <> "" then split_colon t.t_3
      else split6 t.t_3 in
      let a',b' = if interposer <> "" then split_colon t.t_4
      else split6 t.t_4 in
      [ (Arg_Integer,(Act_Integer t.t_1)) ;
        (Arg_Integer,(Act_Integer t.t_2)) ;
        (Arg_Integer,(Act_Integer t.unknown)) ;
        (Arg_Point,(Act_Point t.t_coord)) ;
        (Arg_String,(Act_String a)) ;
        (Arg_String,(Act_String b)) ;
        (Arg_String,(Act_String a')) ;
        (Arg_String,(Act_String b')) ;
        (Arg_Object,(Act_Object t.t_5)) ; ]
  | (1, interposer) ->
      let a,b = if interposer <> "" then split_colon t.t_3
          else split6 t.t_3 in
      [ (Arg_Integer,(Act_Integer t.t_1)) ;
        (Arg_Integer,(Act_Integer t.t_2)) ;
        (Arg_Integer,(Act_Integer t.unknown)) ;
        (Arg_Point,(Act_Point t.t_coord)) ;
        (Arg_String,(Act_String a));
        (Arg_String,(Act_String b));
        (Arg_String,(Act_String t.t_4));
        (Arg_Object,(Act_Object t.t_5)) ; ]
  | (_, _) ->
      [ (Arg_Integer,(Act_Integer t.t_1)) ;
        (Arg_Integer,(Act_Integer t.t_2)) ;
        (Arg_Integer,(Act_Integer t.unknown)) ;
        (Arg_Point,(Act_Point t.t_coord)) ;
        (Arg_String,(Act_String t.t_3)) ;
        (Arg_String,(Act_String t.t_4)) ;
        (Arg_Object,(Act_Object t.t_5)) ; ]

let action_to_arg_list ss a ids =
  match is_concat_string ss ids with
  | (1, interposer) ->
      let aa, b = if interposer <> "" then split_colon a.a_8
      else split6 a.a_8 in
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
  | (2, interposer) ->
      let aa,b = if interposer <> "" then split_colon a.a_8
      else split6 a.a_8 in
      let aa',b' = if interposer <> "" then split_colon a.a_9
      else split6 a.a_9 in
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
  | (3, interposer) ->
      let aa,b = split_colon a.a_9 in
      [ (Arg_Object,(Act_Object a.a_2)) ;
        (Arg_Object,(Act_Object a.a_3)) ;
        (* (Arg_Object,(Act_Object a.a_1)) ; *)
        (Arg_Integer,(Act_Integer a.a_4)) ;
        (Arg_Point,(Act_Point a.a_5)) ;
        (Arg_Integer,(Act_Integer a.a_6)) ;
        (Arg_Integer,(Act_Integer a.a_7)) ;
        (Arg_String,(Act_String a.a_8)) ;
        (Arg_String,(Act_String aa)) ;
        (Arg_String,(Act_String b)) ; ]
  | (_, _) ->
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
    print_trigger_list tl false or_count;
    bcs_printf "THEN\n" ;
    List.iter (fun (w,al) ->
      bcs_printf "  RESPONSE #%d\n" w ;
      List.iter (fun a ->
        bcs_printf "    " ;
        print_action a) al) rl ;
    bcs_printf "END\n\n"
  and print_obj obj =
    (* if obj.o_name <> "" then
       bcs_printf "\"%s\"" obj.o_name
       else *) begin

         let ilist = [ obj.o_unknown4 ; obj.o_unknown3 ; obj.o_unknown2 ;
                       obj.o_unknown1 ; obj.o_identifiers ; ]
         in

         let dotlst = match game.Load.script_style with
         | Load.IWD2 ->
             let subrace = Int32.logor (Int32.shift_left obj.o_race 16)
                 obj.o_subrace in
             [
             obj.o_ea , "EA" ;
             obj.o_general , "GENERAL" ;
             obj.o_race , "RACE" ;
             obj.o_class , "CLASS" ;
             obj.o_specific , "SPECIFIC" ;
             obj.o_gender , "GENDER" ;
             obj.o_alignment , "ALIGNMNT" ;
             subrace , "SUBRACE" ;
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
           if (game.Load.script_style = Load.IWD2 ||
           game.Load.script_style = Load.PST ||
           game.Load.script_style = Load.IWD1) &&
             rect <> (-1l,-1l,-1l,-1l) then begin
             let (a,b,c,d) = rect in
             Printf.sprintf "[%ld.%ld.%ld.%ld]" a b c d;
           end else ""
         in
         let rec proc lst lp rp anything = match lst with
         | [] ->
             begin
               if all_zeros dotlst then begin
                 if not anything && obj.o_name = "" then
                   bcs_printf "[ANYONE]%s" (get_rect obj.o_rect)
                 else (if obj.o_name <> "" then
                   bcs_printf "%s\"%s\"%s%s" lp obj.o_name (get_rect obj.o_rect) rp
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
      end) ids.i_args ;
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
          (Tlk.one_line_pretty_print (Load.get_active_dialog game) (Int32.to_int a.a_4))
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
  and print_trigger_list tr compiling_to_dlg or_count =
    match tr with
    | t :: [] when not compiling_to_dlg && String.lowercase ((best_ids_of_trigger game t).i_name) = "nexttriggerobject" ->
      failwith "NextTriggerObject() without a next trigger (broken TriggerOverride)"
    | t :: t1 :: tl when not compiling_to_dlg && String.lowercase ((best_ids_of_trigger game t).i_name) = "nexttriggerobject" ->
      let indent = 2 + if !or_count > 0 then (decr or_count ; 2) else 0 in
      bcs_printf "%*s" indent " " ;
      bcs_printf "%sTriggerOverride(" (if t1.negated then "!" else "");
      print_obj t.t_5;
      bcs_printf ",";
      if print_trigger {t1 with negated = false} false > 0 then
        failwith "OR() cannot be used inside NextTriggerObject / TriggerOverride";
      bcs_printf ")";
      if comments then begin
        print_trigger_comment game t1
      end ;
      bcs_printf "\n" ;
      print_trigger_list tl compiling_to_dlg or_count
    | t :: tl ->
      let indent = 2 + if !or_count > 0 then (decr or_count ; 2) else 0 in
      bcs_printf "%*s" indent " " ;
      or_count := !or_count + (print_trigger t comments) ;
      bcs_printf "\n";
      print_trigger_list tl compiling_to_dlg or_count
    | [] -> ()
  and print_trigger t comments =
    if (t.negated) then bcs_printf "!" ;
    let ids = best_ids_of_trigger game t in
    bcs_printf "%s(" ids.i_name ;
    let argl = ref (trigger_to_arg_list game.Load.script_style t ids game) in
    print_arg_list argl ids ;
    bcs_printf ")" ;
    if comments then begin
      print_trigger_comment game t ;
    end ;
    if String.uppercase ids.i_name = "OR" then
      (Int32.to_int t.t_1)
    else 0
  and print_trigger_comment game t =
    let ids = best_ids_of_trigger game t in
    (match ids.i_name with
    | "PartyHasItem"
    | "HasItem"
      -> bcs_printf "  // %s" (name_of_res t.t_3 "ITM" 0xc)
    | "Dead"
      -> bcs_printf "  // %s" (name_of_res t.t_3 "CRE" 0xc)
    | _ ->
        if t.t_5.o_name <> "" then
          bcs_printf "  // %s" (name_of_res t.t_5.o_name "CRE" 0xc))
  and name_of_res r ext offset =
    try
      Load.skip_next_load_error := true ;
      let buff,path = Load.load_resource "printing BAF" game true r ext in
      Load.skip_next_load_error := false ;
      if String.length buff >= offset + 4 then
        (Tlk.one_line_pretty_print (Load.get_active_dialog game)
           ((int_of_str_off buff offset )))
      else r
    with _ -> r
  in
  match what with
  | BCS_Print_Script(s) -> List.iter print_cr s
  | BCS_Print_ActionList(al) -> List.iter print_action al
  | BCS_Print_TriggerList(tl,dlg) -> let or_count = ref 0 in print_trigger_list tl dlg or_count

let parse_al : (string -> action list) ref =  ref (fun al -> failwith "Internal: Bcs.parse_al is not loaded")

let is_invalid_for_ict1 action =
  let ans = match (Load.the_game()).Load.script_style, Int32.to_int action.action_id with
  | _, 0   (* NoAction() *)
  | _, 2   (* AddWayPoint(P:WayPoint) *)
  | _, 3   (* Attack(O:Target) *)
  | _, 5   (* BackStab(O:Target) *)
  | _, 8   (* Dialogue(O:Object) *)
  | _, 9   (* DropItem(S:Object,P:Location) *)
  | _, 10  (* Enemy() *)
  | _, 11  (* EquipItem(S:Object) *)
  | _, 13  (* FindTraps() *)
  | _, 14  (* GetItem(S:Object,O:Target) *)
  | _, 15  (* GiveItem(S:Object,O:Target) *)
  | _, 16  (* GiveOrder(O:Object,I:Order) *)
  | _, 17  (* Help() *)
  | _, 18  (* Hide() *)
  | _, 19  (* JoinParty() *)
  | _, 20  (* LayHands(O:Target) *)
  | _, 21  (* LeaveParty() *)
  | _, 22  (* MoveToObject(O:Target) *)
  | _, 23  (* MoveToPoint(P:Point) *)
  | _, 24  (* Panic() *)
  | _, 25  (* PickPockets(O:Target) *)
  | _, 27  (* ProtectPoint(P:Target,I:Range) *)
  | _, 28  (* RemoveTraps(O:Trap) *)
  | _, 29  (* RunAwayFrom(O:Creature,I:Time) *)
  | _, 31  (* Spell(O:Target,I:SpellSpell) *)
  | _, 33  (* Turn() *)
  | _, 34  (* UseItem(S:Object,O:Target) *)
  | _, 36  (* Continue() *)
  | _, 37  (* FollowPath() *)
  | _, 38  (* Swing() *)
  | _, 39  (* Recoil() *)
  | _, 40  (* PlayDead(I:Time) *)
  | _, 47  (* Formation(O:Leader,P:Offset) *)
  | _, 48  (* JumpToPoint(P:Target) *)
  | _, 60  (* ChangeAIScript(S:ScriptFile,I:LevelScrlev) *)
  | _, 61  (* StartTimer(I:ID,I:Time) *)
  | _, 62  (* SendTrigger(O:Target,I:TriggerNum) *)
  | _, 69  (* VEquip(I:item) *)
  | _, 82  (* CreateItem(S:ResRef,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 84  (* Face(I:Direction) *)
  | _, 85  (* RandomWalk() *)
  | _, 86  (* SetInterrupt(I:StateBoolean) *)
  | _, 87  (* ProtectObject(O:Target,I:Range) *)
  | _, 88  (* Leader(P:Point) *)
  | _, 89  (* Follow(P:Point) *)
  | _, 90  (* MoveToPointNoRecticle(P:Point) *)
  | _, 91  (* LeaveArea(S:Area,P:Point,I:Face) *)
  | _, 92  (* SelectWeaponAbility(I:WeaponNumSlots,I:AbilityNum) *)
  | _, 94  (* GroupAttack(O:Target) *)
  | _, 95  (* SpellPoint(P:Target,I:SpellSpell) *)
  | _, 97  (* UseItemPoint(S:Item,P:Target,I:Ability) *)
  | _, 98  (* AttackNoSound(O:Target) *)
  | _, 100 (* RandomFly() *)
  | _, 102 (* MoraleSet(O:Target,I:Morale) *)
  | _, 103 (* MoraleInc(O:Target,I:Morale) *)
  | _, 104 (* MoraleDec(O:Target,I:Morale) *)
  | _, 105 (* AttackOneRound(O:Target) *)
  | _, 106 (* Shout(I:IDSHOUTIDS) *)
  | _, 107 (* MoveToOffset(P:Offset) *)
  | _, 108 (* EscapeArea() *)
  | _, 110 (* LeaveAreaLUA(S:Area,S:Parchment,P:Point,I:Face) *)
  | _, 111 (* DestroySelf() *)
  | _, 112 (* UseContainer() *)
  | _, 113 (* ForceSpell(O:Target,I:SpellSpell) *)
  | _, 114 (* ForceSpellPoint(P:Target,I:SpellSpell) *)
  | _, 116 (* TakePartyItem(S:Item) *)
  | _, 117 (* TakePartyGold(I:Amount) *)
  | _, 118 (* GivePartyGold(I:Amount) *)
  | _, 119 (* DropInventory() *)
  | _, 124 (* Berserk() *)
  | _, 128 (* AnkhegEmerge() *)
  | _, 129 (* AnkhegHide() *)
  | _, 130 (* RandomTurn() *)
  | _, 131 (* Kill(O:Object) *)
  | _, 134 (* AttackReevaluate(O:Target,I:ReevaluationPeriod) *)
  | _, 137 (* StartDialog(S:DialogFile,O:Target) *)
  | _, 138 (* SetDialogue(S:DialogFile) *)
  | _, 139 (* PlayerDialogue(O:Target) *)
  | _, 141 (* GivePartyGoldGlobal(S:Name,S:Area) *)
  | _, 146 (* Polymorph(I:AnimationTypeAnimate) *)
  | _, 147 (* RemoveSpell(I:SpellSpell) *)
  | _, 149 (* EquipMostDamagingMelee() *)
  | _, 152 (* ChangeAIType(O:Object) *)
  | _, 160 (* ApplySpell(O:Target,I:SpellSpell) *)
  | _, 166 (* SetNumTimesTalkedTo(I:Num) *)
  | _, 168 (* Interact(O:Object) *)
  | _, 169 (* DestroyItem(S:ResRef) *)
  | _, 171 (* GiveGoldForce(I:Amount) *)
  | _, 174 (* EquipRanged() *)
  | _, 175 (* SetLeavePartyDialogFile() *)
  | _, 176 (* EscapeAreaDestroy(I:Delay) *)
  | _, 179 (* DialogInterrupt(I:StateBoolean) *)
  | _, 180 (* MoveToObjectFollow(O:Object) *)
  | _, 181 (* ReallyForceSpell(O:Target,I:SpellSpell) *)
  | _, 182 (* MakeUnselectable(I:Time) *)
  | _, 184 (* RunAwayFromNoInterrupt(O:Creature,I:Time) *)
  | _, 188 (* TakePartyItemAll(S:Item) *)
  | _, 189 (* LeaveAreaLUAPanic(S:Area,S:Parchment,P:Point,I:Face) *)
  | _, 191 (* SpellNoDec(O:Target,I:SpellSpell) *)
  | _, 192 (* SpellPointNoDec(P:Target,I:SpellSpell) *)
  | _, 193 (* TakePartyItemRange(S:Item) *)
  | _, 194 (* ChangeAnimation(S:ResRef) *)
  | _, 198 (* StartDialogueNoSet(O:Object) *)
  | _, 200 (* RandomWalkContinuous() *)
  | _, 204 (* TakePartyItemNum(S:ResRef,I:Num) *)
  | _, 207 (* MoveToPointNoInterrupt(P:Point) *)
  | _, 208 (* MoveToObjectNoInterrupt(O:Object) *)
  | _, 212 (* GlobalShout(I:ID) *)
  | _, 215 (* FollowObjectFormation(O:Object,I:Formation,I:Position) *)
  | _, 216 (* AddFamiliar() *)
  | _, 217 (* RemoveFamiliar() *)
  | _, 219 (* ChangeAnimationNoEffect(S:ResRef) *)
  | _, 220 (* TakeItemListParty(S:ResRef) *)
  | _, 221 (* SetMoraleAI(I:MoraleMoraleAI) *)
  | _, 222 (* IncMoraleAI(I:Morale) *)
  | _, 223 (* DestroyAllEquipment() *)
  | _, 224 (* GivePartyAllEquipment() *)
  | _, 225 (* MoveBetweenAreas(S:Area,P:Location,I:Face) *)
  | _, 226 (* TakeItemListPartyNum(S:ResRef,I:Num) *)
  | _, 229 (* FaceObject(O:Object) *)
  | _, 239 (* MoveToCenterOfScreen(I:NotInterruptableFor) *)
  | _, 240 (* ReallyForceSpellDead(O:Target,I:SpellSpell) *)
  | _, 242 (* Ally() *)
  | _, 244 (* SaveLocation(S:Area,S:Global,P:Point) *)
  | _, 245 (* SaveObjectLocation(S:Area,S:Global,O:Object) *)
  | _, 247 (* SetToken(S:Token,I:STRREF) *)
  | _, 248 (* SetTokenObject(S:Token,O:Object) *)
  | _, 257 (* PickUpItem(S:ResRef) *)
  | _, 258 (* FillSlot(I:SlotSLOTS) *)
  | _, 260 (* DestroyGold(I:Gold) *)
  | _, 261 (* SetHomeLocation(P:Point) *)
  | _, 265 (* DialogueForceInterrupt(O:Object) *)
  | _, 266 (* StartDialogInterrupt(S:DialogFile,O:Target) *)
  | _, 267 (* StartDialogNoSetInterrupt(O:Object) *)
  | _, 270 (* PolymorphCopy(O:Object) *)
  | _, 274 (* AddKit(I:KitKIT) *)
  | _, 276 (* EscapeAreaNoSee() *)
  | _, 277 (* EscapeAreaObjectMove(S:ResRef,O:Object,I:X,I:Y,I:Face) *)
  | _, 279 (* AddSpecialAbility(S:ResRef) *)
  | _, 280 (* DestroyAllDestructableEquipment() *)
  | _, 281 (* RemovePaladinHood() *)
  | _, 282 (* RemoveRangerHood() *)
  | _, 283 (* RegainPaladinHood() *)
  | _, 284 (* RegainRangerHood() *)
  | _, 285 (* PolymorphCopyBase(O:Object) *)
  | _, 288 (* SetName(I:STRREF) *)
  | _, 289 (* AddSuperKit(I:KitKIT) *)
  | _, 290 (* PlayDeadInterruptable(I:Time) *)
  | _, 293 (* StartDialogOverride(S:DialogFile,O:Target) *)
  | _, 294 (* StartDialogOverrideInterrupt(S:DialogFile,O:Target) *)
  | _, 296 (* BattleSong() *)
  | _, 297 (* MoveToSavedLocationn(S:GLOBAL,S:Area) *)
  | _, 306 (* ApplyDamagePercent(O:Object,I:Amount,I:TypeDMGTYPE) *)
  | _, 318 (* ForceSpellRange(O:Target,I:SpellSpell) *)
  | _, 319 (* ForceSpellPointRange(P:Target,I:SpellSpell) *)
  | _, 324 (* SetBeenInPartyFlags() *)
  | _, 325 (* GoToStartScreen() *)
  | _, 334 (* StartDialogNoName(S:DialogFile,O:Target) *)
  | _, 336 (* MakeGlobal() *)
  | _, 337 (* ReallyForceSpellPoint(P:Target,I:SpellSpell) *)
  | _, 339 (* SwingOnce() *)
  | _, 345 (* SetSequence(I:SequenceSEQ) *)
  | _, 348 (* SetupWish(I:Column,I:Count) *)
  | _, 350 (* LeaveAreaLUAEntry(S:Area,S:Entry,P:Point,I:Face) *)
  | _, 351 (* LeaveAreaLUAPanicEntry(S:Area,S:Entry,P:Point,I:Face) *)
    -> true
  | _, 50  (* MoveViewObject(O:Target,I:ScrollSpeedScroll) *)
  | _, 68  (* CallLightning(O:Target) *)
  | _, 125 (* Deactivate(O:Object) *)
  | _, 126 (* Activate(O:Object) *)
  | _, 127 (* CutSceneId(O:Object) *)
  | _, 132 (* VerbalConstant(O:Object,I:ConstantsoundOff) *)
  | _, 133 (* ClearActions(O:Object) *)
  | _, 140 (* GiveItemCreate(S:ResRef,O:Object,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 151 (* DisplayString(O:Object,I:StrRef) *)
  | _, 153 (* ChangeEnemyAlly(O:Object,I:ValueEA) *)
  | _, 154 (* ChangeGeneral(O:Object,I:ValueGeneral) *)
  | _, 155 (* ChangeRace(O:Object,I:ValueRace) *)
  | _, 156 (* ChangeClass(O:Object,I:ValueClass) *)
  | _, 157 (* ChangeSpecifics(O:Object,I:ValueSpecific) *)
  | _, 158 (* ChangeGender(O:Object,I:ValueGender) *)
  | _, 159 (* ChangeAlignment(O:Object,I:ValueAlign) *)
  | _, 197 (* MoveGlobal(S:Area,O:Object,P:Point) *)
  | _, 227 (* CreateCreatureObject(S:ResRef,O:Object,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 232 (* CreateCreatureObjectDoor(S:ResRef,O:Object,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 233 (* CreateCreatureObjectOffScreen(S:ResRef,O:Object,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 234 (* MoveGlobalObjectOffScreen(O:Object,O:Target) *)
  | _, 241 (* Calm(O:Object) *)
  | _, 249 (* SetGabber(O:Object) *)
  | _, 250 (* CreateCreatureObjectCopy(S:ResRef,O:Object,I:Usage1,I:Usage2,I:Usage3) *)
  | _, 252 (* CreateCreatureObjectOffset(S:ResRef,O:Object,P:Offset) *)
  | _, 259 (* AddXPObject(O:Object,I:XP) *)
  | _, 262 (* DisplayStringNoName(O:Object,I:StrRef) *)
  | _, 269 (* DisplayStringHead(O:Object,I:StrRef) *)
  | _, 271 (* VerbalConstantHead(O:Object,I:ConstantsoundOff) *)
  | _, 273 (* CreateVisualEffectObject(S:DialogFile,O:Target) *)
  | _, 278 (* TakeItemReplace(S:Give,S:Take,O:Object) *)
  | _, 291 (* MoveGlobalObject(O:Object,O:Target) *)
  | _, 295 (* CreateCreatureCopyPoint(S:ResRef,O:Object,P:Dest) *)
  | _, 298 (* ApplyDamage(O:Object,I:Amount,I:TypeDMGTYPE) *)
  | _, 311 (* DisplayStringWait(O:Object,I:StrRef) *)
  | _, 320 (* SetPlayerSound(O:Object,I:STRREF,I:SlotNumSNDSLOT) *)
  | _, 322 (* FakeEffectExpiryCheck(O:Object,I:Ticks) *)
  | _, 342 (* DisplayStringHeadDead(O:Object,I:StrRef) *)
  | _, 346 (* DisplayStringNoNameHead(O:Object,I:StrRef) *)
  | _, 349 (* SetupWishObject(O:Creature,I:Count) *)
    -> let check ob = ob.o_unknown4 = 1l || ob.o_unknown3 = 1l || ob.o_unknown2 = 1l || ob.o_unknown1 = 1l || ob.o_identifiers = 1l in
    List.exists check [action.a_1; action.a_2; action.a_3]
  | _, 30  (* SetGlobal(S:Name,S:Area,I:Value) *)
  | _, 109 (* IncrementGlobal(S:Name,S:Area,I:Value) *)
  | _, 115 (* SetGlobalTimer(S:Name,S:Area,I:TimeGTimes) *)
  | _, 256 (* CreateItemGlobal(S:Global,S:Area,S:ResRef) *)
  | _, 268 (* RealSetGlobalTimer(S:Name,S:Area,I:TimeGTimes) *)
  | _, 335 (* SetTokenGlobal(S:GLOBAL,S:Area,S:Token) *)
    -> let check s = String.uppercase (snd (split6 s)) = "LOCALS" in
    List.exists check [action.a_8; action.a_9]
  | _ -> false in
  ans && action.a_1.o_name = ""

let is_invalid_for_ict2 action =
  match (Load.the_game()).Load.script_style, Int32.to_int action.action_id with
  | _, 96  (* Rest() *)
  | _, 120 (* StartCutScene() *)
  | _, 121 (* StartCutSCeneMode() *)
  | _, 150 (* StartStore(S:Store*,O:Target *)
  | _, 167 (* StartMovie *)
  | _, 338 (* SetCutSceneLite *)
    -> true
  | _ -> false

let invalid_for_ict (al: string) which =
  let al = !parse_al al in
  let mismatch = List.filter (match which with
  | "ICT1" -> is_invalid_for_ict1
  | "ICT2" -> is_invalid_for_ict2
  | _ -> failwith "invalid_for_ict internal type mismatch") al in
  if mismatch = [] then "" else begin
    let buffer = Buffer.create 500 in
    print_script_text (Load.the_game()) (Save_BCS_Buffer buffer) (BCS_Print_ActionList mismatch) false None;
    let ans = Buffer.contents buffer in
    String.sub ans 0 (String.length ans - 1)
  end
