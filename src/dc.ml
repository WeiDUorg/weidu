(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   9 March 06. All changes for this file are listed in
   diffs/src.dc.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

open BatteriesInit
open Hashtblinit
open Util

let notChanged = ref false
;;

type action =
  | Create of Dlg.dlg
  | Append of string * bool * (Dlg.state list)
  | Append_Early of string * bool * (Dlg.state list)
  | Extend_Bottom of string * (string list) * int * (Dlg.transition list)
  | Extend_Top of string * (string list) * int * (Dlg.transition list)
  | Replace of string * (Dlg.state list)
  | Replace_Say of string * string * (Dlg.tlk_string)
  | Add_Trans_Trigger of string * (string list)* string * (string list) * d_when list
  | Add_Trans_Action
    of string       (* filename *)
        * (string list) (* state labels *)
        * (int list)    (* transition #s, empty = all *)
        * string        (* new action text *)
        * d_when list
  | Alter_Trans of string * string list * int list * (string * alter_trans_feature) list
  | Replace_Trans_Action of string * string list * int list * string * string * d_when list
  | Replace_Trans_Trigger of string * string list * int list * string * string * d_when list
  | Replace_State_Trigger of string * (string list)* string * d_when list
  | Add_State_Trigger of string * (string list)* string * d_when list
  | Set_Weight of string * string * int
  | Chain of chain_info
(* bool = "use regexp for filenames" *)
  | Replace_Action_Text of (string list) * string * string * bool * d_when list
  | Replace_Trigger_Text of string * string * string * bool * d_when list
  | Chain3 of chain3_info

and d_when =
  | W_If of string
  | W_Unless of string

and alter_trans_feature =
  | Alter_Trans_String of string
  | Alter_Trans_Lse of Dlg.tlk_string

and chain_info = {
    entry_file : string;
    entry_label : string;
    dialogue : (string * Dlg.tlk_string) list ; (* SPEAKER + WORDS *)
    exit_file : string;
    exit_label : string;
  }

and chain3_info = {
    mutable c3_entry_condition : string option;
    mutable c3_entry_weight : Dlg.weight;
    c3_entry_file : string;
    c3_iffileexists : bool;
    c3_entry_label : string;
    c3_dialogue : c3_dialogue_unit list ;
    mutable c3_exit_trans : Dlg.transition array ;
    c3_variable : string option ;
    c3_keep_first_do_with_first_speaker : bool ;
    (* if 'true', do what Rastor suggests below: *)
    (* Rastor: Currently, I_C_T carries over both the DO actions as well as
       the GOTO/EXTERN/REPLY commands.  Unfortunately, this can cause some
       problems if you want to interject into the ends of dialogues where an NPC
       leaves (causing your new character to EscapeArea() instead) as well as a
       few other issues that you can view here:
       http://forums.pocketplane.net/index.php?topic=16835.0.

       I propose an INTERJECT_COPY_TRANS2 command which would be identical to
       I_C_T in format, but would only carry over the GOTO/EXTERN/REPLYs in the
       original dialogue but not carry over the DOs. *)
    c3_append_all : bool ;
    (* If true,
       INTERJECT_COPY_TRANS IMOEN2 0 TestInterject
       == MINSCJ IF ~IsValidForPartyDialog("Minsc")~ THEN ~Hey Imoen!~
       == JAHEIRAJ IF ~IsValidForPartyDialog("Jaheira")~ THEN ~Yo, Imoen, wassup?~
       == KORGANJ IF ~IsValidForPartyDialog("Korgan")~ THEN ~Och, aye.~
       == IMOEN2 IF ~~ THEN ~You're all nuts.~
       END
       is parsed as
       INTERJECT_COPY_TRANS IMOEN2 0 TestInterject3
       == KORGANJ IF ~IsValidForPartyDialog("Korgan")~ THEN ~Och, aye.~
       == IMOEN2 IF ~~ THEN ~You're all nuts.~
       END

       INTERJECT_COPY_TRANS IMOEN2 0 TestInterject2
       == JAHEIRAJ IF ~IsValidForPartyDialog("Jaheira")~ THEN ~Yo, Imoen, wassup?~
       == KORGANJ IF ~IsValidForPartyDialog("Korgan")~ THEN ~Och, aye.~
       == IMOEN2 IF ~~ THEN ~You're all nuts.~
       END

       INTERJECT_COPY_TRANS IMOEN2 0 TestInterject
       == MINSCJ IF ~IsValidForPartyDialog("Minsc")~ THEN ~Hey Imoen!~
       == JAHEIRAJ IF ~IsValidForPartyDialog("Jaheira")~ THEN ~Yo, Imoen, wassup?~
       == KORGANJ IF ~IsValidForPartyDialog("Korgan")~ THEN ~Och, aye.~
       == IMOEN2 IF ~~ THEN ~You're all nuts.~
       END
       for easier multiple-comments in the same ICT block.
     *)
  }

and c3_dialogue_unit = {
    c3du_speaker : string ;
    c3du_condition : string option ;
    mutable c3du_action : string option;
    c3du_say : Dlg.tlk_string ;
    c3du_id : int ;
    c3du_ifexists : bool ;
  }

let d_file_exists game file =
  let f = file ^ ".DLG" in
  let old_allow_missing = !Load.allow_missing in
  Load.allow_missing := [] ;
  let res =
    (try
      let a,b = split f in
      Load.skip_next_load_error := true;
      let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true a b in
      (String.length buff > 0)
    with _ -> false ) in
  Load.allow_missing := old_allow_missing ;
  res

let c3du_counter = ref 0

let get_c3du_counter () =
  let ans = !c3du_counter in
  incr c3du_counter ;
  ans

let strings_to_add = ref (Queue.create ()) (* LSEs stored in order-of-creation *)
let strings_added_ht = Hashtbl.create 255
let strings_to_add_ht = Hashtbl.create 32767
let cur_index = ref 0

let chain_counter = ref 0

let chain_label () =
  incr chain_counter ;
  Printf.sprintf "!chain_%d" !chain_counter

let available_dlgs = Hashtbl.create 31

let d_action_list = ref ([] : (string * action) list)

let trans_strings = ref [ Hashtbl.create 511 ]

let add_trans_strings (tsl : (int * Dlg.tlk_string) list ) =
  let tbl = List.hd !trans_strings in
  List.iter (fun (id,lse) -> Hashtbl.add tbl id (lse,true)) tsl

let push_trans () =
  if !debug_ocaml then log_and_print "Push_Trans()\n";
  trans_strings := (Hashtbl.create 511) :: !trans_strings

let push_copy_trans () =
  if !debug_ocaml then log_and_print "Push_Copy_Trans()\n";
  let now = match !trans_strings with
  | hd :: tl -> hd
  | [] -> Hashtbl.create 511
  in
  trans_strings := (Hashtbl.copy now) :: !trans_strings

let push_copy_trans_modder () =
  if !debug_ocaml then log_and_print "Push_Copy_Trans_Modder(): ";
  match Modder.get "SETUP_TRA" with
  | Modder.None -> push_copy_trans ()
  | Modder.Warn -> begin
      if !debug_ocaml then log_and_print "Modder algorithm.\n";
      let now = match !trans_strings with
      | hd :: tl -> hd
      | [] -> Hashtbl.create 511
      in
      let now = Hashtbl.copy now in
      Hashtbl.iter (fun id (lse,safe) ->
        Hashtbl.replace now id (lse,false)
                   ) now;
      trans_strings := now :: !trans_strings
  end
  | Modder.Fail -> push_trans ()

let pop_trans () =
  if !debug_ocaml then log_and_print "Pop_Trans()\n";
  trans_strings := List.tl !trans_strings

let rec resolve_tlk_string_internal can_create warn_mess game ts =
  match ts with
    Dlg.Local_String(lse) ->
      let lse = {lse_male = Var.get_string lse.lse_male;
                 lse_male_sound = Var.get_string lse.lse_male_sound;
                 lse_female = Var.get_string lse.lse_female;
                 lse_female_sound = Var.get_string lse.lse_female_sound;
               } in
      let index =
        Stats.time "find local string" (fun () ->
          try
            Tlk.find_string_fast lse (Load.get_active_dialog game)
              (Load.get_active_dialogf_opt game) game.Load.dialog_search ;
          with
            Not_found ->
              Stats.time "strings to add" (fun () ->
                if Hashtbl.mem strings_to_add_ht lse then begin
                  Hashtbl.find strings_to_add_ht lse
                end else begin
                  if not can_create then raise Not_found else begin
                    let index = !cur_index in
                    Queue.add (lse) !strings_to_add ;
                    Hashtbl.add strings_to_add_ht lse index ;
                    Hashtbl.add strings_added_ht index lse ;
                    incr cur_index ;
                    index
                  end
                end ) ()) () in
      Dlg.TLK_Index(index)
  | Dlg.Trans_String(Dlg.Int(idx)) -> begin
      try
        let (new_ts,safe) = Hashtbl.find (List.hd !trans_strings) idx in
        if not safe && Modder.get "SETUP_TRA" = Modder.Warn then (
          (try assert false with Assert_failure(file,line,col) -> set_errors file line);
          log_and_print "WARNING: @%i taken from setup.tra.\n" idx
         );
        resolve_tlk_string_internal can_create warn_mess game new_ts
      with Not_found ->
        if warn_mess then log_and_print "ERROR: No translation provided for @%d\n" idx ;
        raise Not_found
  end
  | Dlg.Trans_String(Dlg.String(s)) ->
      resolve_tlk_string_internal can_create warn_mess game (Dlg.Trans_String(Dlg.Int(Int32.to_int(Var.get_int32_extended s))))
  | _ -> ts

let resolve_tlk_string = resolve_tlk_string_internal true (* will add the new string if absent from the tlk *)
    true (* warning message if @xx is missing from the tra *)

let ok_to_resolve_strings_while_loading = ref None
let doing_traify = ref false
let resolve_string_while_loading ts =
  match !ok_to_resolve_strings_while_loading with
  | Some(game) -> resolve_tlk_string game ts
  | None -> ts

let rec single_string_of_tlk_string game ts =
  match ts with
  | Dlg.Local_String(lse) -> lse.lse_male
  | Dlg.Trans_String(Dlg.Int(idx)) -> begin
      try
        let (new_ts,safe) = Hashtbl.find (List.hd !trans_strings) idx in
        single_string_of_tlk_string game new_ts
      with Not_found ->
        if !eval_pe_warn then log_and_print "ERROR: No translation provided for @%d\n" idx ;
        raise Not_found
  end
  | Dlg.Trans_String(Dlg.String(s)) ->
      single_string_of_tlk_string game (Dlg.Trans_String(Dlg.Int(Int32.to_int(Var.get_int32_extended s))))
  | Dlg.TLK_Index(idx) ->
      begin
        try
          (Load.get_active_dialog game).(idx).Tlk.text
        with _ -> Printf.sprintf "<Invalid Strref %d>" idx
      end

let rec single_string_of_tlk_string_safe game ts =
  match ts with
  | Dlg.Local_String(lse) -> lse.lse_male
  | Dlg.Trans_String(Dlg.Int(idx)) -> begin
      try
        let (new_ts,safe) = Hashtbl.find (List.hd !trans_strings) idx in
        single_string_of_tlk_string game new_ts
      with Not_found ->
        log_and_print "ERROR: No translation provided for @%d\n" idx ;
        log_and_print "Continuing despite error.\n";
        (try assert false with Assert_failure(file,line,col) -> set_errors file line);
        "UNDEFINED STRING:   @" ^ string_of_int idx
  end
  | Dlg.Trans_String(Dlg.String(s)) ->
      single_string_of_tlk_string_safe game (Dlg.Trans_String(Dlg.Int(Int32.to_int(Var.get_int32_extended s))))
  | Dlg.TLK_Index(idx) ->
      Tlk.pretty_print (Load.get_active_dialog game) idx

let pretty_print_no_quote tlk i female sound =
  let alt = Array.length tlk in
  if i < alt then
    Tlk.pretty_print_q tlk i false sound
  else begin
    try
      let lse = Hashtbl.find strings_added_ht i in
      match female,sound with
      | false,false -> lse.lse_male
      | true ,false -> lse.lse_female
      | false,true  -> lse.lse_male_sound
      | true ,true  -> lse.lse_female_sound
    with Not_found -> Tlk.pretty_print_q tlk i false sound
  end

let set_string (g : Load.game) (i :int) (ts : Dlg.tlk_string)
    (allow_strref : bool) : unit =
  let rec process ts = match ts with
  | Dlg.TLK_Index(i) ->
      if (allow_strref) then begin
        if (i < 0 || i > !cur_index) then begin
          log_and_print "SET_STRING %d out of range 0 -- %d\n" i (!cur_index) ;
        end ;
        if (i < Array.length (Load.get_active_dialog g)) then
          (((Load.get_active_dialog g).(i)) ,
           (match Load.get_active_dialogf_opt g with
           | None -> (Load.get_active_dialog g).(i)
           | Some(t) -> t.(i)))
        else (Tlk.lse_to_tlk_string (Hashtbl.find strings_added_ht i))
      end else failwith "SET_STRING does not allow #strrefs"
  | Dlg.Local_String(lse) -> Tlk.lse_to_tlk_string lse
  | Dlg.Trans_String(Dlg.String(s)) ->
      process (Dlg.Trans_String(Dlg.Int(Int32.to_int(Var.get_int32_extended s))))
  | Dlg.Trans_String(Dlg.Int idx) ->
      begin
        try
          let (new_lse,_) = Hashtbl.find (List.hd !trans_strings) idx in
          process new_lse
        with Not_found ->
          log_and_print "ERROR: No translation provided for @%d\n" idx ;
          raise Not_found
      end
  in
  let m, f = process ts in
  let m = {m with Tlk.text = Var.get_string m.Tlk.text;
           Tlk.sound_name = Var.get_string m.Tlk.sound_name} in
  let f = {f with Tlk.text = Var.get_string f.Tlk.text;
           Tlk.sound_name = Var.get_string f.Tlk.sound_name} in
  let dialog = Load.get_active_dialog g in
  if (i < 0 || i > !cur_index) then begin
    log_and_print "SET_STRING %d out of range 0 -- %d\n" i (!cur_index) ;
    failwith "SET_STRING out of range"
  end ;
  (*
    log_or_print "SET_STRING #%d to %s\n" i (Tlk.short_print m 18);
   *)
  if (i < Array.length dialog) then begin
    (match Load.get_active_dialogf_opt g with
    | Some(a) -> g.Load.str_sets <- (i,dialog.(i),a.(i)) ::
        g.Load.str_sets ;
        a.(i) <- f ;
        (Load.get_active_dialogs g).Load.dialogf_mod <- true
    | None -> g.Load.str_sets <- (i,dialog.(i),dialog.(i))
        :: g.Load.str_sets) ;
    dialog.(i) <- m ;
  end else if (i < !cur_index) then begin (* Array.length dialog < !cur_index *)
    let orig_lse = Hashtbl.find strings_added_ht i in
    let orig_tlk = Tlk.lse_to_tlk_string orig_lse in
    let new_lse = {lse_male = m.Tlk.text ; lse_male_sound = m.Tlk.sound_name ;
                   lse_female = f.Tlk.text ; lse_female_sound = f.Tlk.sound_name} in
    (match Load.get_active_dialogf_opt g with
    | Some(a) -> g.Load.str_sets <- (i,fst orig_tlk, snd orig_tlk) ::
        g.Load.str_sets ;
    | None -> g.Load.str_sets <- (i,fst orig_tlk, snd orig_tlk)
        :: g.Load.str_sets) ;
    Hashtbl.remove strings_added_ht i;
    Hashtbl.remove strings_to_add_ht orig_lse;
    Hashtbl.add strings_added_ht i new_lse;
    Hashtbl.add strings_to_add_ht new_lse i;
    let newQueue = Queue.create () in
    while not (Queue.is_empty !strings_to_add) do
      let elt = Queue.take !strings_to_add in
      if (elt != orig_lse) then
        Queue.add elt newQueue
      else
        Queue.add new_lse newQueue
    done;
    strings_to_add := newQueue;
  end else begin (* i = !cur_index *)
    let lse = {lse_male = m.Tlk.text; lse_male_sound = m.Tlk.sound_name;
               lse_female = f.Tlk.text; lse_female_sound = f.Tlk.sound_name} in
    Queue.add lse !strings_to_add ;
    Hashtbl.add strings_to_add_ht lse i ;
    Hashtbl.add strings_added_ht i lse ;
    incr cur_index ;
  end ;
  (Load.get_active_dialogs g).Load.dialog_mod <- true ;
  ()

let set_string_while_loading forced_index ts =
  match !ok_to_resolve_strings_while_loading with
  | Some(game) -> set_string game forced_index ts true
  | None -> failwith "cannot set strings at this point: no game loaded"


let resolve_tlk_string_opt game tso = match tso with
| Some(ts) -> Some(resolve_tlk_string game ts)
| None -> None

let test_trans out game =
  let max = Array.length (Load.get_active_dialog game) in
  Hashtbl.iter (fun id (tlk_string,_) ->
    let ref = match resolve_tlk_string game tlk_string with
    | Dlg.TLK_Index(idx) -> idx
    | _ -> 999999
    in
    if (ref < max) then begin
      out (Printf.sprintf  "@%d is #%d\n" id ref)
    end) (List.hd !trans_strings)

exception FoundState of int

let resolve_label (file,label) game =
  try
    let dlg =
      try Hashtbl.find available_dlgs file
      with _ -> begin
        if Modder.get "MISSING_EXTERN" = Modder.None then () else
        begin
          try
            Load.skip_next_load_error := true;
            ignore(Load.load_resource "just_a_check" game true file "DLG")
          with _ ->
            Modder.handle_deb "MISSING_EXTERN" (Printf.sprintf "WARNING: couldn't find the file %s.DLG for EXTERN\n" file);
        end;
        let index =
          try
            int_of_string label
          with _ ->
            log_and_print "ERROR: Cannot resolve external symbolic label [%s] for DLG [%s]\n" label file ; failwith "cannot resolve label"
        in
        raise (FoundState index)
      end
    in
    let found_count = ref 0 in
    let found = ref 0 in
    Array.iteri (fun i s ->
      if s.Dlg.symbolic_label = label then begin
        incr found_count ;
        found := i;
      end) dlg.Dlg.state ;
    (if (!found_count > 1) then begin
      log_and_print "ERROR: internal label [%s] appears %d times in processed DLG [%s]\n" label !found_count file ;
      failwith "cannot resolve label"
    end else if (!found_count = 1) then
      raise (FoundState(!found))) ;
    try
      let answer = int_of_string label in
      log_and_print "WARNING: internal label [%s] not found in processed DLG [%s]\n" label file ;
      (try assert false with Assert_failure(file,line,col) -> set_errors file line);
      answer
    with _ -> begin
      log_and_print "ERROR: Cannot resolve internal symbolic label [%s] for DLG [%s]\nKnown labels:" label file ;
      Array.iteri (fun i s ->
        log_and_print " %s" s.Dlg.symbolic_label) dlg.Dlg.state ;
      log_and_print "\n" ; failwith "cannot resolve label"
    end
  with FoundState(i) -> i

let resolve_symbolic_labels d game =
  Array.iteri (fun i s ->
    Array.iteri (fun j t ->
      match t.Dlg.next with
      | Dlg.Symbolic(name,label,_) ->
          let idx = resolve_label (name,label) game in
          t.Dlg.next <- Dlg.Absolute(name,idx)
      | _ -> ()) s.Dlg.trans) d.Dlg.state

let resolve_strings game d =
  Array.iteri (fun i s ->
    try
      s.Dlg.resp_str <- resolve_tlk_string game s.Dlg.resp_str ;
      Array.iteri (fun j t ->
        t.Dlg.trans_str <- resolve_tlk_string_opt game t.Dlg.trans_str ;
        t.Dlg.journal_str <- match t.Dlg.journal_str with
        | Some(a,str) -> Some(a,resolve_tlk_string game str)
        | None -> None) s.Dlg.trans
    with e ->
      log_and_print "ERROR: cannot resolve strings in %s\n" d.Dlg.name ;
      raise e) d.Dlg.state

let locate_dlg game name =
  if Hashtbl.mem available_dlgs name then
    Hashtbl.find available_dlgs name
  else begin
    let buff, final_path = Load.load_resource "copy_trans" game true name "DLG" in
    let dlg = Dlg.load_dlg name buff in
    dlg
  end

let make_available for_what game name unsafe =
  if Hashtbl.mem available_dlgs name then
    ()
  else begin
    try
      if unsafe then Load.skip_next_load_error := true ;
      let buff, final_path = Load.load_resource for_what game true name "DLG" in
      let dlg = Dlg.load_dlg name buff in
      Hashtbl.add available_dlgs name dlg
    with e -> begin
      if !debug_ocaml then log_and_print "File %s is missing.\n" name ;
      if not unsafe then raise e;
    end
  end

let clear_state () =
  d_action_list := [] ;
  Hashtbl.clear available_dlgs

let preprocess_append_early game a = match a with
| Append_Early(n,unsafe,sl) ->
    make_available "APPEND_EARLY" game n unsafe ;
    let dlg = Hashtbl.find available_dlgs n in
    dlg.Dlg.state <- Array.append dlg.Dlg.state
        (Array.of_list sl)
| _ -> ()

let preprocess_action1 game a = match a with
| Create(d) ->
    if Hashtbl.mem available_dlgs d.Dlg.name then begin
      log_and_print "BEGIN %s when %s is already loaded/created\n" d.Dlg.name d.Dlg.name;
      log_and_print "(you should say BEGIN %s exactly once and before APPEND %s, etc.)\n" d.Dlg.name d.Dlg.name ;
      failwith "invalid D file"
    end ;
    log_or_print "Adding %s to internal list of available DLGs\n" d.Dlg.name ;
    Hashtbl.add available_dlgs d.Dlg.name d
| _ -> ()

let action_to_str a = match a with
| Create _ -> "CREATE"
| Append _ -> "APPEND"
| Append_Early _ -> "APPEND_EARLY"
| Extend_Bottom _ -> "EXTEND_BOTTOM"
| Extend_Top _ -> "EXTEND_TOP"
| Replace _ -> "REPLACE"
| Replace_Say _ -> "REPLACE_SAY"
| Add_Trans_Trigger _ -> "ADD_TRANS_TRIGGER"
| Add_Trans_Action  _ -> "ADD_TRANS_ACTION"
| Alter_Trans _ -> "ALTER_TRANS"
| Replace_Trans_Action _ -> "REPLACE_TRANS_ACTION"
| Replace_Trans_Trigger _ -> "REPLACE_TRANS_TRIGGER"
| Replace_State_Trigger _ -> "REPLACE_STATE_TRIGGER"
| Add_State_Trigger _ -> "ADD_STATE_TRIGGER"
| Set_Weight _ -> "SET_WEIGHT"
| Chain _ -> "CHAIN"
| Replace_Action_Text _ -> "REPLACE_ACTION_TEXT"
| Replace_Trigger_Text _ -> "REPLACE_TRIGGER_TEXT"
| Chain3 _ -> "CHAIN3"

let preprocess_action2 game a = match a with
| Create(d) -> ()

| Append(n,unsafe,_)
| Append_Early(n,unsafe,_)
  -> make_available (action_to_str a) game n unsafe
| Extend_Top(n,_,_,_)
| Extend_Bottom(n,_,_,_)
| Replace(n,_)
| Replace_Say(n,_,_)
| Add_State_Trigger(n,_,_,_)
| Replace_State_Trigger(n,_,_,_)
| Add_Trans_Trigger(n,_,_,_,_)
| Add_Trans_Action(n,_,_,_,_)
| Replace_Trans_Action(n,_,_,_,_,_)
| Replace_Trans_Trigger(n,_,_,_,_,_)
| Set_Weight(n,_,_)
| Replace_Trigger_Text(n,_,_,false,_)
  -> make_available (action_to_str a) game n false
| Replace_Action_Text(nl,_,_,false,_)
  -> List.iter (fun n -> make_available (action_to_str a) game n false) nl
| Alter_Trans (n,_,_,l) ->
    make_available (action_to_str a) game n false;
    List.iter (fun (b,c) -> if String.uppercase b = "EPILOGUE" then begin
      match c with
      | Alter_Trans_String c -> begin
          let parts = Str.split (Str.regexp " ") c in
          (match parts with
          | [b;c;d] -> if b = "EXTERN" then make_available (action_to_str a) game c false;
          | _ -> ()) end
      | Alter_Trans_Lse c -> failwith "LSE on EPILOGUE in ALTER_TRANS"
    end
              ) l
| Replace_Action_Text(_,_,_,true,_)
| Replace_Trigger_Text(_,_,_,true,_) -> ()
| Chain(ci) ->
    let s = action_to_str a in
    make_available s game ci.entry_file false ;
    List.iter (fun (speaker,says) -> make_available s game speaker false )
      ci.dialogue
| Chain3(ci) ->
    let s = action_to_str a in
    make_available s game ci.c3_entry_file ci.c3_iffileexists ;
    List.iter (fun c3du -> make_available s game c3du.c3du_speaker c3du.c3du_ifexists)
      ci.c3_dialogue

let append_state n state =
  let dlg = Hashtbl.find available_dlgs n in
  dlg.Dlg.state <- Array.append dlg.Dlg.state [| state |]

let passes d_when str =
  List.fold_left (fun acc elt -> match elt with
  | W_If s -> begin
      let my_regexp = Str.regexp_case_fold (Var.get_string s) in
      try let _ = Str.search_forward my_regexp str 0 in
      true
      with _ ->
        false
  end
  | W_Unless s -> begin
      let my_regexp = Str.regexp_case_fold (s) in
      try let _ = Str.search_forward my_regexp str 0 in
      false
      with _ ->
        true
  end)  true d_when

let rec process_action game a = match a with
| Create(d) -> ()

| Set_Weight(n,s,w) ->
    let dlg = Hashtbl.find available_dlgs n in
    let num = resolve_label (n,s) game in
    dlg.Dlg.state.(num).Dlg.state_trigger_weight <- Dlg.Offset(w)

| Replace_Action_Text(nl,s_from,s_to,use_regexp,d_when) ->
    let r = Str.regexp_case_fold (Var.get_string s_from) in
    let s_to = Var.get_string s_to in
    let process dlg =
      Array.iter (fun state ->
        Array.iter (fun trans ->
          (match trans.Dlg.action with
            Some(trans_str) ->
              if passes d_when trans_str then begin
                trans.Dlg.action <- Some(Str.global_replace r s_to trans_str);
              end
          | None -> ())) state.Dlg.trans) dlg.Dlg.state
    in
    if use_regexp then begin
      let regexp_list = List.map (fun n -> Str.regexp_case_fold n) nl in
      let matches = Key.search_key_resources game.Load.key true
          (fun poss ->
            let b,e = split (String.uppercase poss) in
            let ans = e = "DLG" &&
              (List.exists (fun regexp -> Str.string_match regexp b 0)
                 regexp_list) in
            if ans then make_available (action_to_str a) game b false;
            ans)
      in
      List.iter (fun n -> process (Hashtbl.find available_dlgs (fst (split n)))) matches
    end else
      List.iter (fun n ->
        let dlg = Hashtbl.find available_dlgs n in
        process dlg) nl

| Replace_Trigger_Text(n,s_from,s_to,use_regexp,d_when) ->
    let process dlg =
      let r = Str.regexp_case_fold s_from in
      Array.iter (fun state ->
        if passes d_when state.Dlg.state_trigger then begin
          if !debug_ocaml then log_and_print "~%s~\n" state.Dlg.state_trigger;
          state.Dlg.state_trigger <- Str.global_replace r s_to
              state.Dlg.state_trigger ;
        end;
        Array.iter (fun trans ->
          match trans.Dlg.trans_trigger with
            Some(trans_str) ->
              if passes d_when trans_str then begin
                if !debug_ocaml then log_and_print "~%s~\n" trans_str;
                trans.Dlg.trans_trigger <- Some(Str.global_replace r s_to trans_str);
              end
          | None -> ()) state.Dlg.trans) dlg.Dlg.state
    in
    if (use_regexp) then begin
      let regexp = Str.regexp_case_fold n in
      let matches = Key.search_key_resources game.Load.key true
          (fun poss ->
            let b,e = split (String.uppercase poss) in
            let ans = e = "DLG" &&
              Str.string_match regexp b 0 in
            if ans then make_available (action_to_str a) game b false;
            ans)
      in
      List.iter (fun n -> process (Hashtbl.find available_dlgs (fst (split n)))) matches
    end else process (Hashtbl.find available_dlgs n)

| Append(n,unsafe,sl) ->
    let dlg = Hashtbl.find available_dlgs n in
    dlg.Dlg.state <- Array.append dlg.Dlg.state
        (Array.of_list sl)
| Append_Early(n,unsafe,sl) -> () (* done earlier! *)

| Extend_Top(n,sl,0,tl) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      if (num >= 0 && num < Array.length dlg.Dlg.state) then
        dlg.Dlg.state.(num).Dlg.trans <- Array.append
            (Array.of_list tl) dlg.Dlg.state.(num).Dlg.trans
      else
        log_or_print
          "WARNING: EXTEND_TOP state #%d out of range 0-%d, SKIPPED\n"
          num (Array.length dlg.Dlg.state)) sl

| Extend_Top(n,sl,place,tl) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      if (num >= 0 && num < Array.length dlg.Dlg.state) then begin
        let s = dlg.Dlg.state.(num) in
        let tlen = Array.length s.Dlg.trans in
        let place =
          if place < 0 then
            failwith "EXTEND_TOP #position must be non-negative"
          else if place >= tlen then begin
            log_or_print
              "WARNING: EXTEND_TOP #position %d out of range 0-%d\n"
              place tlen ;
            tlen - 1
          end else place
        in
        (* insert these transitions just after already-extans transition
         * #place *)
        let before = Array.sub s.Dlg.trans 0 place in
        let after = Array.sub s.Dlg.trans (place) (tlen - place) in
        let result = Array.concat [ before ; Array.of_list tl ; after ] in
        dlg.Dlg.state.(num).Dlg.trans <- result
      end else
        log_or_print
          "WARNING: EXTEND_TOP state #%d out of range 0-%d, SKIPPED\n"
          num (Array.length dlg.Dlg.state)) sl

| Extend_Bottom(n,sl,0,tl) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      if (num >= 0 && num < Array.length dlg.Dlg.state) then
        dlg.Dlg.state.(num).Dlg.trans <- Array.append
            dlg.Dlg.state.(num).Dlg.trans (Array.of_list tl)
      else
        log_or_print
          "WARNING: EXTEND_BOTTOM state #%d out of range 0-%d, SKIPPED\n"
          num (Array.length dlg.Dlg.state)) sl

| Extend_Bottom(n,sl,place,tl) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s->
      let num = resolve_label (n,s) game in
      if (num >= 0 && num < Array.length dlg.Dlg.state) then begin
        let s = dlg.Dlg.state.(num) in
        let tlen = Array.length s.Dlg.trans in
        let place =
          if place < 0 then
            failwith "EXTEND_BOTTOM #position must be non-negative"
          else if place >= tlen then begin
            log_or_print
              "WARNING: EXTEND_BOTTOM #position %d out of range 0-%d\n"
              place tlen ;
            tlen - 1
          end else place
        in
        let place = tlen - place in
        (* insert these transitions just before already-extant transition
         * #place *)
        let before = Array.sub s.Dlg.trans 0 place in
        let after = Array.sub s.Dlg.trans (place) (tlen - place) in
        let result = Array.concat [ before ; Array.of_list tl ; after ] in
        dlg.Dlg.state.(num).Dlg.trans <- result
      end else
        log_or_print
          "WARNING: EXTEND_BOTTOM state #%d out of range 0-%d, SKIPPED\n"
          num (Array.length dlg.Dlg.state)) sl

| Replace_Say(n,l,s) ->
    let dlg = Hashtbl.find available_dlgs n in
    let num = resolve_label (n,l) game in
    dlg.Dlg.state.(num).Dlg.resp_str <- s

| Replace(n,new_s_list) ->
    let dlg = Hashtbl.find available_dlgs n in
    let trigger_ord = ref 0 in
    let trigger_unord = ref 0 in
    Array.iteri (fun i s ->
      if (s.Dlg.state_trigger <> "") then begin
        ( match s.Dlg.state_trigger_weight, !trigger_ord with
        | Dlg.Offset(a),b -> if (a != b) then incr trigger_unord
        | _,_ -> ());
        incr trigger_ord;
      end) dlg.Dlg.state ;
    List.iter (fun new_s ->
      let num = resolve_label (n,new_s.Dlg.symbolic_label) game in
      if (num >= 0 && num < Array.length dlg.Dlg.state) then begin
        let old_wgt = dlg.Dlg.state.(num).Dlg.state_trigger_weight in
        let new_wgt = new_s.Dlg.state_trigger_weight in begin
          ( match !trigger_unord, old_wgt, new_wgt with
          | 0,_,Dlg.Not_Specified -> ()
          | 0,Dlg.Offset(a),Dlg.Offset(b) -> log_or_print "WARNING: REPLACE ignoring specified WEIGHT for state %d (%d).  DLG has trivial weights, using weight from DLG (%d).  Use SET_WEIGHT if you want to change state weights.\n" num b a
          | c,Dlg.Offset(a),Dlg.Not_Specified -> log_or_print "WARNING: REPLACE specifies no WEIGHT for state %d and DLG uses non-trivial weights.  Using weight from DLG (%d). [%d]\n" num a c
          | c,Dlg.Offset(a),Dlg.Offset(b) -> if (a != b) then log_or_print "WARNING: REPLACE ignoring specified WEIGHT for state %d (%d).  Using weight from DLG (%d).  Use SET_WEIGHT if you want to change state weights. [%d]\n" num b a c
          | _,_,_ -> ());
          dlg.Dlg.state.(num) <- new_s;
          dlg.Dlg.state.(num).Dlg.state_trigger_weight <- old_wgt
        end
      end
      else
        log_or_print "WARNING: REPLACE %d out of range 0-%d\n"
          num (Array.length dlg.Dlg.state)) new_s_list
| Replace_State_Trigger(n,sl,new_t,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      if passes d_when dlg.Dlg.state.(num).Dlg.state_trigger then
        dlg.Dlg.state.(num).Dlg.state_trigger <- new_t) sl
| Add_State_Trigger(n,sl,new_t,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      if passes d_when dlg.Dlg.state.(num).Dlg.state_trigger then
        dlg.Dlg.state.(num).Dlg.state_trigger <- new_t ^ "\r\n" ^
          dlg.Dlg.state.(num).Dlg.state_trigger) sl

| Add_Trans_Action(n,state_labels,transition_indices,action_text,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun state_label ->
      let num = resolve_label (n,state_label) game in
      let do_it trans =
        match trans.Dlg.action with
        | None -> if passes d_when "" then trans.Dlg.action <- Some(action_text)
        | Some(str) -> if passes d_when str then
            trans.Dlg.action <- Some(action_text ^ "\r\n" ^ str);
      in
      match transition_indices with
      | [] -> (* do them all *)
          Array.iter do_it dlg.Dlg.state.(num).Dlg.trans
      | lst ->
          List.iter (fun i ->
            if (i >= 0 &&
                i < Array.length dlg.Dlg.state.(num).Dlg.trans) then begin
                  let trans = dlg.Dlg.state.(num).Dlg.trans.(i) in
                  do_it trans
                end) lst) state_labels;

| Alter_Trans(n,state_labels,transition_indices,changes) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun state_label ->
      let num = resolve_label (n,state_label) game in
      let do_it trans =
        List.iter (fun (what,into) ->
          let get_lse s =
            match s with
            | Alter_Trans_String s -> begin
                if s = "" then
                  (Dlg.Local_String({ lse_male = s ; lse_male_sound = "";
                                      lse_female = s; lse_female_sound = ""; })) else match s.[0] with
                                      | '@' -> failwith "Unresolved local_string in alter_trans (internal)"
                                      | '#' -> Dlg.TLK_Index (int_of_string(Str.string_after s 1))
                                      | _ -> Dlg.Local_String({ lse_male = s ; lse_male_sound = "";
                                                                lse_female = s; lse_female_sound = ""; })
            end
            | Alter_Trans_Lse s -> s
          in
          let some_or_none prod into = match into with
          | "" -> None
          | _ -> Some(prod)
          in
          let get_next s =
            let parts = Str.split (Str.regexp " ") s in
            match parts with
            | [a;b;c] -> Dlg.Symbolic(String.uppercase b,c,false)
            | [a;b] -> Dlg.Symbolic(String.uppercase n,b,false)
            | [a] -> Dlg.Exit
            | _ -> failwith "unknown transition in ALTER_STRING"
          in
          let intos = match into with
          | Alter_Trans_Lse s -> single_string_of_tlk_string game s
          | Alter_Trans_String s -> s
          in
          match String.uppercase what with
          | "TRIGGER" -> trans.Dlg.trans_trigger <- some_or_none intos intos
          | "ACTION" -> trans.Dlg.action <- some_or_none intos intos
          | "REPLY" -> trans.Dlg.trans_str <- some_or_none (get_lse(into)) intos
          | "JOURNAL" -> trans.Dlg.journal_str <- some_or_none (Dlg.Normal_Journal,get_lse(into)) intos
          | "SOLVED_JOURNAL" -> trans.Dlg.journal_str <- some_or_none (Dlg.Solved_Journal,get_lse(into)) intos
          | "UNSOLVED_JOURNAL"->trans.Dlg.journal_str<- some_or_none (Dlg.Unsolved_Journal,get_lse(into)) intos
          | "EPILOGUE" -> trans.Dlg.next <- get_next(intos)
          | "FLAGS" -> trans.Dlg.unknown_flags <- int_of_string(intos)
          | _ -> failwith(Printf.sprintf "DCUnknown ALTER_TRANS command: %s" what)) changes;
      in
      match transition_indices with
      | [] -> (* do them all *)
          Array.iter do_it dlg.Dlg.state.(num).Dlg.trans
      | lst ->
          List.iter (fun i ->
            if (i >= 0 &&
                i < Array.length dlg.Dlg.state.(num).Dlg.trans) then begin
                  let trans = dlg.Dlg.state.(num).Dlg.trans.(i) in
                  do_it trans
                end) lst) state_labels;

| Replace_Trans_Action(n,state_labels,transition_indices,oldt,newt,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun state_label ->
      let num = resolve_label (n,state_label) game in
      let do_it trans =
        match trans.Dlg.action with
        | None -> ()
        | Some(str) -> if passes d_when str then
            trans.Dlg.action <- Some(Str.global_replace
                                       (Str.regexp_case_fold oldt) newt str);
      in
      match transition_indices with
      | [] -> (* do them all *)
          Array.iter do_it dlg.Dlg.state.(num).Dlg.trans
      | lst ->
          List.iter (fun i ->
            if (i >= 0 &&
                i < Array.length dlg.Dlg.state.(num).Dlg.trans) then begin
                  let trans = dlg.Dlg.state.(num).Dlg.trans.(i) in
                  do_it trans
                end) lst) state_labels;

| Replace_Trans_Trigger(n,state_labels,transition_indices,oldt,newt,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun state_label ->
      let num = resolve_label (n,state_label) game in
      let do_it trans =
        match trans.Dlg.trans_trigger with
        | None -> ()
        | Some(str) -> if passes d_when str then
            trans.Dlg.trans_trigger <- Some(Str.global_replace
                                              (Str.regexp_case_fold oldt) newt str);
      in
      match transition_indices with
      | [] -> (* do them all *)
          Array.iter do_it dlg.Dlg.state.(num).Dlg.trans
      | lst ->
          List.iter (fun i ->
            if (i >= 0 &&
                i < Array.length dlg.Dlg.state.(num).Dlg.trans) then begin
                  let trans = dlg.Dlg.state.(num).Dlg.trans.(i) in
                  do_it trans
                end) lst) state_labels;

| Add_Trans_Trigger(n,sl,new_t,t_list,d_when) ->
    let dlg = Hashtbl.find available_dlgs n in
    List.iter (fun s ->
      let num = resolve_label (n,s) game in
      let do_it trans =
        match trans.Dlg.trans_trigger with
        | None -> if passes d_when "" then trans.Dlg.trans_trigger <- Some(new_t )
        | Some(str) -> if passes d_when str then trans.Dlg.trans_trigger <- Some(new_t ^ "\r\n" ^ str)
      in
      match t_list with
      | [] ->
          Array.iter do_it dlg.Dlg.state.(num).Dlg.trans
      | tl ->
          List.iter (fun trans_idx ->
            let i = my_int_of_string trans_idx in
            let trans = dlg.Dlg.state.(num).Dlg.trans.(i) in
            do_it trans) tl) sl
| Chain(ci) -> begin
    let rec process_chain cl in_label = match cl with
    | (f1,s1) :: (f2,s2) :: tl ->
        let new_label = chain_label () in
        let new_state = Dlg.make_state s1 in_label f2 new_label in
        append_state f1 new_state ;
        process_chain ((f2,s2)::tl) new_label
    | (f1,s1) :: [] -> let new_state = Dlg.make_state s1
          in_label ci.exit_file ci.exit_label in
      append_state f1 new_state
    | [] -> ()
    in
    process_chain ci.dialogue ci.entry_label
end

| Chain3(ci) -> begin
    if not ci.c3_append_all then begin
      (* expunge the failing IF_FILE_EXISTS lines *)
      let ci = {ci with c3_dialogue = (
                let new_dialogue = ref [] in
                List.iter (fun x -> if not x.c3du_ifexists then
                  new_dialogue := x :: !new_dialogue
                else begin
                  let f = x.c3du_speaker ^ ".DLG" in
                  let old_allow_missing = !Load.allow_missing in
                  Load.allow_missing := [] ;
                  let res =
                    (try
                      let a,b = split f in
                      Load.skip_next_load_error := true;
                      let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true a b in
                      (String.length buff > 0)
                    with _ -> false ) in
                  Load.allow_missing := old_allow_missing ;
                  if res then new_dialogue := x :: !new_dialogue ;
                end) ci.c3_dialogue ;
                List.rev (!new_dialogue)) ;
              } in
      let label_of_ci = Hashtbl.create 255 in
      List.iter (fun c3du ->
        Hashtbl.add label_of_ci c3du (c3du.c3du_speaker,(chain_label ()))
                ) ci.c3_dialogue ;
      (match ci.c3_variable with
      | None -> Hashtbl.replace label_of_ci (List.hd ci.c3_dialogue)
            (ci.c3_entry_file,ci.c3_entry_label) ;
      | Some _ -> ()) ;
      let final_trans = Array.to_list ci.c3_exit_trans in
      let keep = ci.c3_keep_first_do_with_first_speaker in
      (* I_C_T4: make a backup of original first action *)
      let backup_action = match final_trans with
      | [] -> None
      | hd :: tl -> hd.Dlg.action
      in
      (*
        log_or_print "DEBUG: I_C_T: %b (%d)\n" keep
        (List.length final_trans);
       *)
      let first_actions, final_trans = match final_trans, keep with
      | hd :: tl ,true -> begin
          let orig_act = hd.Dlg.action in
          let rec process lst = match lst with
          | [] -> ()
          | hd' :: tl' ->
              let action_compare a1 a2 = match a1,a2 with
              | None, None -> None
              | Some(a1),Some(a2) ->
                  let aa1 = Str.global_replace (Str.regexp "[ \t\n\r]+") "" a1 in
                  let aa2 = Str.global_replace (Str.regexp "[ \t\n\r]+") "" a2 in
                  if String.uppercase aa1 = String.uppercase aa2 then None else begin try
                    let a2_init = Str.string_before a2 (Str.search_forward one_newline_or_cr_regexp a2 0) in
                    let a2_after = Str.global_replace (Str.regexp "[ \t\n\r]+") "" (Str.string_after a2 (Str.search_forward one_newline_or_cr_regexp a2 0)) in
                    if Str.string_match (Str.regexp "SetGlobal(\"[^\"]*\",\"GLOBAL\",1)") a2_init 0 then begin
                      if a2_after = aa1 then Some a2_init else Some ""
                    end else Some ""
                  with _ -> Some "" end
              | _ -> Some ""
              in
              let diff = action_compare orig_act hd'.Dlg.action in
              if diff <> None then begin
                let ps s_opt = match s_opt with
                | Some(s) -> s
                | None -> "\"\""
                in
                Modder.handle_msg "ICT2_ACTIONS"
                  (Printf.sprintf
                     "WARNING: I_C_T2: the interjection point (%s %s) has multiple exit transitions that have different actions!\nFirst Action: ~%s~\nOther Action: ~%s~\n"
                     ci.c3_entry_file ci.c3_entry_label (ps orig_act) (ps hd'.Dlg.action));
                if ps diff <> "" then begin
                  let g s = try
                    ignore (Str.string_match (Str.regexp "SetGlobal(\"\\([^\"]*\\)\",\"GLOBAL\",1)") (ps hd'.Dlg.action) 0);
                    Str.matched_group 1 (ps hd'.Dlg.action);
                  with _ -> ""
                  in
                  Modder.handle_msg "ICT2_ACTIONS" (Printf.sprintf "It is possible (but not guaranteed) that the difference is caused by another mod interjecting here with the following ICT variable: ~%s~.\n" (g diff));
                end else begin
                  Modder.handle_deb "ICT2_ACTIONS" ""
                end
              end ;
              hd'.Dlg.action <- None ;
              process tl'
          in
          process (hd :: tl) ;
          orig_act, (hd :: tl)
      end ;
      | x,false -> None, x
      | _,true -> failwith "ERROR: CHAIN3: 'keep first do with first speaker' requires that you have at least one exit transition"
      in

      if keep && Modder.enabled "ICT2_ACTIONS" then begin
        let first_actions = match first_actions with None -> "" | Some x -> x in
        let mismatch = Bcs.invalid_for_ict first_actions "ICT2" in
        if mismatch <> "" then
          Modder.handle_deb "ICT2_ACTIONS" (Printf.sprintf "WARNING: ICT2/ICT4: The interjection point (%s %s) has actions that must be the last in the dialogue: \"%s\".\nUse ICT1/3%s.\n" ci.c3_entry_file ci.c3_entry_label mismatch (if Bcs.invalid_for_ict first_actions "ICT1" <> "" then " and a throwback" else ""));
      end;

      let combine_some s1 s2 = match s1, s2 with
      | Some(s1), Some(s2) -> Some(s1 ^ "\n" ^ s2)
      | Some(s1), None -> Some(s1)
      | None,Some(s2) -> Some(s2)
      | None,None -> None
      in

      let rec possible_trans action cl = match cl with
      | [] ->
          List.rev (
          List.map (fun t ->
            { t with Dlg.action = combine_some t.Dlg.action action }) final_trans
         ) (* will be reversed again inside process_chain *)
      | ci :: tl -> begin
          let a,b = Hashtbl.find label_of_ci ci in
          let trans = { Dlg.unknown_flags = 0;
                        Dlg.trans_str = None ;
                        Dlg.journal_str = None ;
                        Dlg.trans_trigger = ci.c3du_condition ;
                        Dlg.action = action ;
                        Dlg.next = Dlg.Symbolic(a,b,false) } in
          trans :: (match ci.c3du_condition with
          | None -> []
          | Some _ -> (possible_trans action tl))
      end
      in
      let rec process_chain cl condition weight = match cl with
      | ci :: tl ->
          let (this_file,this_label) = Hashtbl.find label_of_ci ci in
          let trans = List.rev (possible_trans ci.c3du_action tl) in
          let trans = Array.of_list trans in
          let new_state = Dlg.make_state_trans ci.c3du_say this_label trans in
          (match condition with
            Some(s) -> new_state.Dlg.state_trigger <- s;
              new_state.Dlg.state_trigger_weight <- weight ;
          | None -> ()) ;
          append_state ci.c3du_speaker new_state ;
          process_chain tl None (Dlg.Not_Specified)
      | [] -> ()
      in
      process_chain ci.c3_dialogue ci.c3_entry_condition ci.c3_entry_weight;
      if keep then begin
        (* I_C_T4: restore original first action *)
        match Array.to_list ci.c3_exit_trans with
        | [] -> ()
        | hd :: tl -> hd.Dlg.action <- backup_action ; ()
      end ;
      begin
        match ci.c3_variable with
        | None -> ()
        | Some(var) ->
            begin
              let fst = List.hd ci.c3_dialogue in
              let file,label = Hashtbl.find label_of_ci fst in
              let trans = Dlg.make_trans_of_next (Dlg.Symbolic(file,label,false)) in
              trans.Dlg.trans_trigger <- Some
                  (Printf.sprintf "Global(\"%s\",\"GLOBAL\",0)%s" var
                     (match fst.c3du_condition with Some(s) -> (" " ^ s) | None -> ""));
              let a1 = Some
                  (Printf.sprintf "SetGlobal(\"%s\",\"GLOBAL\",1)" var) in
              trans.Dlg.action <- combine_some a1 first_actions ;
              process_action game (Extend_Bottom(ci.c3_entry_file,
                                                 [ci.c3_entry_label],0,[trans]))
            end
      end
    end
    else begin
      let the_list = ref ci.c3_dialogue in
      let list_to_process = ref [] in
      while List.length !the_list > 0 do
        list_to_process := !the_list :: !list_to_process ;
        if !debug_ocaml then log_and_print "%d appended has have %d entries\n" (List.length ci.c3_dialogue - List.length !list_to_process) (List.length !the_list) ;
        the_list := List.tl !the_list ;
      done ;
      List.iter (fun x ->
        if !debug_ocaml then log_and_print "Processing the one long %d\n" (List.length x) ;
        process_action game (Chain3({
                                    ci with c3_dialogue = x ; c3_append_all = false;
                                   })))
        !list_to_process ;
    end
end

let pctta late game tl keep = (* process_copy_trans__trans_array *)
  (* given a transition, return the list of transition it expands to *)
  try
    let process t =
      let doit f s safe =
        let d = locate_dlg game f in
        let i = resolve_label (f,s) game in
        if i < 0 || i >= Array.length d.Dlg.state then begin
          log_and_print
            "ERROR: COPY_TRANS %s state #%d out of range 0-%d, SKIPPED\n"
            f i (Array.length d.Dlg.state) ;
          failwith "COPY_TRANS out of range"
        end ;
        let lst = Array.to_list (d.Dlg.state.(i).Dlg.trans) in
        let ans = List.map Dlg.duplicate_trans lst in
        if not safe && not keep && Modder.enabled "ICT2_ACTIONS" then begin
          let al = List.map (fun tr -> match tr.Dlg.action with None -> "" | Some x -> x) ans in
          let mismatch = Bcs.invalid_for_ict (String.concat " " al) "ICT1" in
          if mismatch <> "" then begin
            Modder.handle_msg "ICT2_ACTIONS" (Printf.sprintf "WARNING: COPY_TRANS: the chosen point (%s %s) has actions that must be left with the original speaker: \"%s\"\n" f s mismatch) ;
            Modder.handle_msg "ICT2_ACTIONS" "If this is plain COPY_TRANS(_LATE), make sure that the last speaker is the same dialogue file as the one you're COPY_TRANSing from, then add SAFE after COPY_TRANS.\n";
            Modder.handle_deb "ICT2_ACTIONS" (Printf.sprintf "If this is INTERJECT_COPY_TRANS: %s.\n" (if Bcs.invalid_for_ict (String.concat " " al) "ICT2" <> "" then " add a throwback line and add SAFE after the ICT" else "use ICT2/4 instead"));
          end;
        end;
        ans
      in
      if not late then
        match t.Dlg.next with
        | Dlg.Copy(f,s,safe) -> doit f s safe
        | _ -> [t]
      else
        match t.Dlg.next with
        | Dlg.Copy_Late(f,s,safe) -> doit f s safe
        | _ -> [t] ;
    in
    let expanded_trans_a = Array.map process tl in
    let expanded_trans_list = Array.to_list expanded_trans_a in
    let expanded_trans_list = List.flatten expanded_trans_list in
    let expanded_trans_a = Array.of_list expanded_trans_list in
    expanded_trans_a
  with s ->
    log_and_print "ERROR: Cannot process COPY_TRANS (%s)\n" (printexc_to_string s) ;
    raise s

let process_copy_trans late game a = match a with
| Create(d) ->
    Array.iter (fun s ->
      s.Dlg.trans <- pctta late game s.Dlg.trans false) d.Dlg.state ;
    Create(d)
| Append(s,unsafe,sl) ->
    List.iter (fun s -> s.Dlg.trans <- pctta late game s.Dlg.trans false) sl;
    Append(s,unsafe,sl)
| Extend_Top(a,b,pos,tl) ->
    Extend_Top(a,b,pos,Array.to_list (pctta late game (Array.of_list tl) false))
| Extend_Bottom(a,b,pos,tl) ->
    Extend_Bottom(a,b,pos,Array.to_list (pctta late game (Array.of_list tl) false))
| Replace(n,sl) ->
    List.iter (fun s -> s.Dlg.trans <- pctta late game s.Dlg.trans false) sl ;
    Replace(n,sl)
| Append_Early(_,_,_)
| Replace_Say(_,_,_)
| Add_State_Trigger(_,_,_,_)
| Replace_State_Trigger(_,_,_,_)
| Add_Trans_Trigger(_,_,_,_,_)
| Add_Trans_Action(_,_,_,_,_)
| Alter_Trans(_,_,_,_)
| Replace_Trans_Action(_,_,_,_,_,_)
| Replace_Trans_Trigger(_,_,_,_,_,_)
| Set_Weight(_,_,_)
| Replace_Action_Text(_,_,_,_,_)
| Replace_Trigger_Text(_,_,_,_,_)
| Chain(_) -> a
| Chain3(c) -> c.c3_exit_trans <- pctta late game c.c3_exit_trans c.c3_keep_first_do_with_first_speaker; a

let postprocess_dlg game name d =
  (*      try  *)
  Stats.time "resolve strings" (resolve_strings game) d ;
  Stats.time "resolve labels" (resolve_symbolic_labels) d game
    (* with s ->
       log_and_print "ERROR: Cannot postprocess %s\n" name ;
       raise s *)

let prife game tl = (* PRocess_If_File_Exists *)
  (* given a transition, return the list of transition it expands to *)
  try
    let process t =
      match t.Dlg.next with
      | Dlg.Symbolic(a,b,true) ->
          let f = a ^ ".DLG" in
          let old_allow_missing = !Load.allow_missing in
          Load.allow_missing := [] ;
          let res =
            (try
              let a,b = split f in
              Load.skip_next_load_error := true;
              let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true a b in
              (String.length buff > 0)
            with _ -> false ) in
          Load.allow_missing := old_allow_missing ;
          if res then [t] else []
      | _ -> [t]
    in
    let expanded_trans_a = Array.map process tl in
    let expanded_trans_list = Array.to_list expanded_trans_a in
    let expanded_trans_list = List.flatten expanded_trans_list in
    let expanded_trans_a = Array.of_list expanded_trans_list in
    expanded_trans_a
  with s ->
    log_and_print "ERROR: Cannot process IF_FILE_EXISTS\n" ;
    raise s

let preprocess_ife game a =
  match a with
  | Create(d) ->
      Array.iter (fun s ->
        s.Dlg.trans <- prife game s.Dlg.trans) d.Dlg.state ;
      Create(d)
  | Append_Early(s,unsafe,sl) ->
      List.iter (fun s -> s.Dlg.trans <- prife game s.Dlg.trans) sl ;
      Append_Early(s,unsafe,sl)
  | Append(s,unsafe,sl) ->
      List.iter (fun s -> s.Dlg.trans <- prife game s.Dlg.trans) sl ;
      Append(s,unsafe,sl)
  | Extend_Top(a,b,pos,tl) ->
      Extend_Top(a,b,pos,Array.to_list (prife game (Array.of_list tl)))
  | Extend_Bottom(a,b,pos,tl) ->
      Extend_Bottom(a,b,pos,Array.to_list (prife game (Array.of_list tl)))
  | Replace(n,sl) ->
      List.iter (fun s -> s.Dlg.trans <- prife game s.Dlg.trans) sl ;
      Replace(n,sl)
  | Replace_Say(_,_,_)
  | Add_State_Trigger(_,_,_,_)
  | Replace_State_Trigger(_,_,_,_)
  | Add_Trans_Trigger(_,_,_,_,_)
  | Add_Trans_Action(_,_,_,_,_)
  | Alter_Trans(_,_,_,_)
  | Replace_Trans_Action(_,_,_,_,_,_)
  | Replace_Trans_Trigger(_,_,_,_,_,_)
  | Set_Weight(_,_,_)
  | Replace_Action_Text(_,_,_,_,_)
  | Replace_Trigger_Text(_,_,_,_,_)
  | Chain(_) -> a
  | Chain3(c) -> c.c3_exit_trans <- prife game c.c3_exit_trans ; a

let preprocess_ife_state game a =
  match a with
  | Append_Early(s,unsafe,sl)
  | Append(s,unsafe,sl) ->
      if unsafe && not (d_file_exists game s) then None else Some(a)
  | Chain3(c) -> if c.c3_iffileexists && not (d_file_exists game c.c3_entry_file) then None else Some(a)
  | Extend_Top(_,_,_,_)
  | Extend_Bottom(_,_,_,_)
  | Replace(_,_)
  | Create(_)
  | Replace_Say(_,_,_)
  | Add_State_Trigger(_,_,_,_)
  | Replace_State_Trigger(_,_,_,_)
  | Add_Trans_Trigger(_,_,_,_,_)
  | Add_Trans_Action(_,_,_,_,_)
  | Alter_Trans(_,_,_,_)
  | Replace_Trans_Action(_,_,_,_,_,_)
  | Replace_Trans_Trigger(_,_,_,_,_,_)
  | Set_Weight(_,_,_)
  | Replace_Action_Text(_,_,_,_,_)
  | Replace_Trigger_Text(_,_,_,_,_)
  | Chain(_) -> Some(a)

let dc game lst =
  let what = ref "" in
  let where = ref "" in
  try
    what := "preprocess IF_FILE_EXISTS" ;
    let result = ref [] in
    List.iter (fun (w,a) ->
      where := w;
      let this_one = (preprocess_ife_state game a) in
      match this_one with
      | None -> ()
      | Some(x) -> result := (w,x) :: !result) lst ;
    let lst = List.rev !result in
    let lst = List.map (fun (w,a) -> where := w; (w,preprocess_ife game a)) lst in
    (* IF_FILE_EXISTS somewhere *)
    what := "preprocessing CREATE" ;
    List.iter (fun (w,a) -> where := w; preprocess_action1 game a) lst ;
    (* BEGIN *)
    what := "preprocessing APPEND_EARLY" ;
    List.iter (fun (w,a) -> where := w; preprocess_append_early game a) lst ;
    (* APPEND_EARLY *)
    List.iter (fun (w,a) -> where := w;  preprocess_action2 game a) lst ;
    (* make_available *)
    what := "processing COPY_TRANS" ;
    let lst =
      List.map (fun (w,a) -> where := w ; (w,process_copy_trans false game a)) lst in
    what := "processing .D actions" ;
    List.iter (fun (w,a) -> where := w; process_action game a) lst ;
    what := "processing COPY_TRANS_LATE" ;
    let lst =
      List.map (fun (w,a) -> where := w ; (w,process_copy_trans true game a)) lst in
    what := "postprocessing" ;
    Hashtbl.iter (fun n d -> where := n; postprocess_dlg game n d)
      (* resolve_string & resolve_label *)
      available_dlgs ;
    ()
  with e ->
    log_and_print "ERROR: %s [%s]: %s\n" !what !where
      (printexc_to_string e);
    raise e
