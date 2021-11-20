(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Infinity Engine [DLG] *)
open BatteriesInit
open Hashtblinit
open Util

let emit_from = ref true
let comments = ref true
let emit_text = ref false

let emit_some_states_only = ref false
let emit_these_states = (Hashtbl.create 1 : (int, unit) Hashtbl.t)

type string_or_int =
  | Int of int
  | String of string

type tlk_string =
  | TLK_Index of int
  | Local_String of local_string_entry
  | Trans_String of string_or_int

(*** Store Local Strings ***)
let local_string_ht = ref (None : tlk_string list option)

let tlk_ify ts =
  match ts with
  | TLK_Index(i) -> i
  | Local_String(ls) -> failwith "tlk_ify: ls"
  | Trans_String(i) -> failwith "tlk_ify: ts"

type state = {
    mutable resp_str : tlk_string ;
    mutable trans : transition array ;
    mutable state_trigger : string ;
    mutable state_trigger_weight : weight ;
    mutable symbolic_label : string ;
  }

and weight =
  | Not_Specified
  | Offset of int

and journal_type =
  | Normal_Journal
  | Solved_Journal
  | Unsolved_Journal

and transition = {
    mutable unknown_flags : int ;
    mutable trans_str : tlk_string option ;
    mutable journal_str : (journal_type * tlk_string) option ;
    mutable trans_trigger : string option ;
    mutable action : string option ;
    mutable next : trans_next ;
  }

and trans_next =
  | Exit
  | Absolute of string * int
  | Symbolic of string * string * bool (* file * label * if_exists *)
  | Copy of string * string * bool  (* copy all transitions from here; is the copy safe? *)
  | Copy_Late of string * string * bool


and dlg = {
    mutable name : string ;
    mutable state : state array ;
    mutable dlg_flags : int ;
  }

let duplicate_trans t =
  {
   unknown_flags = t.unknown_flags ;
   trans_str = t.trans_str ;
   journal_str = t.journal_str ;
   trans_trigger = t.trans_trigger ;
   action = t.action ;
   next = t.next ;
 }

let any_cr_lf = (Str.regexp ")[\r\n\t ]+")

let convert_raw_text s =
  Str.global_replace any_cr_lf ")\r\n" s

let convert_raw_text_out s =
  let s = Var.get_string s in
  Str.global_replace any_cr_lf ")\n" s

let from_ht = Hashtbl.create 511

let load_dlg name buff = begin
  let result = Stats.time "unmarshal DLG" (fun () ->
    let name = let a,b = split_resref name in String.uppercase a in
    try
      if String.sub buff 0 8 <> "DLG V1.0" then begin
        failwith "not a valid DLG file (wrong sig)"
      end ;
      if String.length buff < 52 then begin
        log_and_print "WARNING: %s is not a valid DLG file (too small)\n" name ;
        { name = name ;
          state = [| |] ;
          dlg_flags = 0 ;
        }
      end else begin
        let num_states = int_of_str_off buff 8 in
        let offset_state_table = int_of_str_off buff 12 in
        let num_trans = int_of_str_off buff 16 in
        let offset_trans_table = int_of_str_off buff 20 in
        let offset_state_trig_table = int_of_str_off buff 24 in
        let num_state_trig = int_of_str_off buff 28 in
        let offset_trans_trig_table = int_of_str_off buff 32 in
        let num_trans_trig = int_of_str_off buff 36 in
        let offset_action_table = int_of_str_off buff 40 in
        let num_actions = int_of_str_off buff 44 in
        let flags = if offset_state_table = 48 then 0 else int_of_str_off buff 48 in
        let get_trigger i off =
          if i < 0 then ""
          else begin
            let o = int_of_str_off buff (((off) + (i * 8)) + 0) in
            let l = int_of_str_off buff (((off) + (i * 8)) + 4) in
            try
              String.sub buff o l
            with _ ->
              log_or_print "WARNING: %s has an invalid trigger/action (%d), defaulting to \"\"\n" name i ; ""
          end
        in
        let get_state_trigger i = get_trigger i offset_state_trig_table in
        let get_trans_trigger i = get_trigger i offset_trans_trig_table in
        let get_action i = get_trigger i offset_action_table in
        {
         name = name ;
         state = Array.init num_states (fun i ->
           try
             let off = offset_state_table + (i * 16) in
             let str_index = int_of_str_off buff off in
             let trans_index = int_of_str_off buff (off+4) in
             let trans_num = int_of_str_off buff (off+8) in
             let trigger_index = int_of_str_off buff (off+12) in
             (* process the transitions *)
             let make_trans t = begin
               let off = offset_trans_table + ((trans_index + t) * 32) in
               let flags = int_of_str_off buff off in
               let trans_string = int_of_str_off buff (off + 4) in
               let journal_string = int_of_str_off buff (off + 8) in
               let index_trigger = int_of_str_off buff (off + 12) in
               let index_action = int_of_str_off buff (off + 16) in
               let next_dlg = get_string_of_size buff (off + 20) 8 in
               let index_state = int_of_str_off buff (off + 28) in
               let journal_type,flags =
                 if flags land 0x40 = 0x40 then
                   Unsolved_Journal , flags - 0x40
                 else if flags land 0x100 = 0x100 then
                   Solved_Journal, flags - 0x100
                 else Normal_Journal, flags
               in
               ({
                unknown_flags = (flags lsr 5) lsl 5 ;
                trans_str = if flags land 1 <> 0
                then Some(TLK_Index(trans_string)) else None ;
                journal_str = if flags land 16 <> 0
                then Some(journal_type, TLK_Index(journal_string)) else None ;
                trans_trigger = if flags land 2 <> 0
                then Some(dos2unix (get_trans_trigger index_trigger)) else None ;
                action = if flags land 4 <> 0
                then Some(dos2unix (get_action index_action)) else None ;
                next = if flags land 8 <> 0 then Exit
                else
                  let dest = String.uppercase next_dlg in
                  (* create "from: " lists *)
                  if !emit_from then begin
                    Hashtbl.add from_ht (dest, index_state)
                      (String.uppercase name,i,t) ;
                  end ;
                  (Absolute(dest,index_state)) ;
              } , off)
             end in
             let strig = get_state_trigger trigger_index in
             let trans_weight_array = Array.init trans_num make_trans in
             Array.stable_sort (fun (_,w1) (_,w2) -> w1 - w2)
               trans_weight_array ;
             {
              resp_str = (TLK_Index(str_index)) ;
              trans = Array.map (fun (elt,w) -> elt) trans_weight_array ;
              state_trigger = dos2unix strig ;
              state_trigger_weight = (match strig with
                "" -> Not_Specified
              | _ -> Offset(trigger_index)) ;
              symbolic_label = string_of_int i ;
            }
           with e -> log_and_print "ERROR: problem loading state %d: %s\n" i
               (printexc_to_string e) ; raise e) ;
         dlg_flags = flags ;
       } end
    with _ ->
      { name = name ;
        state = [| |] ;
        dlg_flags = 0 ;
      }) () in
  log_or_print "[%s.DLG] loaded\n" name ;
  result
end


(*******************************************************************)
let letter_regexp = Str.regexp_case_fold "[A-Z0-9]"

let save_dlg dlg outbuff = begin
  Stats.time "marshal DLG" (fun () ->

    let num_state = Array.length dlg.state in

    let header_buff = Buffer.create 52 in
    let state_buff = Buffer.create (num_state * 16) in
    let trans_buff = Buffer.create (num_state * 32) in
    let state_trig_buff = Buffer.create (num_state * 8) in
    let trans_trig_buff = Buffer.create (num_state * 8) in
    let action_buff = Buffer.create (num_state * 8) in
    let string_buff = Buffer.create (num_state * 50) in

    let cur_trans = ref 0 in
    let cur_state_trig = ref 0 in
    let cur_trans_trig = ref 0 in
    let cur_action = ref 0 in

    let num_states = Array.length dlg.state in
    let num_trans = Array.fold_left (fun acc elt ->
      acc + (Array.length elt.trans)) 0 dlg.state in
    let num_strig = Array.fold_left (fun acc elt ->
      acc + if elt.state_trigger = "" then 0 else 1) 0 dlg.state in
    let num_ttrig = Array.fold_left (fun acc elt ->
      acc + (Array.fold_left (fun acc elt -> acc +
        if elt.trans_trigger = None then 0 else 1) 0 elt.trans)) 0 dlg.state in
    let num_action = Array.fold_left (fun acc elt ->
      acc + (Array.fold_left (fun acc elt -> acc +
        if elt.action = None then 0 else 1) 0 elt.trans)) 0 dlg.state in

    let offset_state_table = 52 in
    let offset_trans_table = offset_state_table + (num_states * 16) in
    let offset_strig_table = offset_trans_table + (num_trans * 32) in
    let offset_ttrig_table = offset_strig_table + (num_strig * 8) in
    let offset_action_table = offset_ttrig_table + (num_ttrig * 8) in
    let end_of_action_table = offset_action_table + (num_action * 8) in
    let start_of_strings = end_of_action_table in

    let add_trig tbuff str =
      let str = convert_raw_text str in
      Buffer.add_string tbuff (str_of_int (start_of_strings + (Buffer.length string_buff))) ;
      Buffer.add_string tbuff (str_of_int (String.length str)) ;
      Buffer.add_string string_buff str
    in

    (* add the state triggers in Weighted order *)
    let state_trigger_of_state =
      Hashtbl.create ((Array.length dlg.state) * 2) in

    (* sort the states by state trigger weight *)
    let sorted_states = Array.copy dlg.state in

    Array.iteri (fun i s ->
      s.symbolic_label <- string_of_int i  (* tie breaker *)
                ) sorted_states ;

    Array.stable_sort
      (fun s1 s2 ->
        match s1.state_trigger_weight, s2.state_trigger_weight with
        | Offset(a),Not_Specified -> -999999
        | Not_Specified,Offset(b) ->  999999
        | Offset(a),Offset(b) -> a - b
        | Not_Specified,Not_Specified -> begin
            match s1.state_trigger, s2.state_trigger with
            | "","" -> 0
            | "",_ ->  999999
            | _,"" -> -999999
            | a,b -> (int_of_string s1.symbolic_label) -
                  (int_of_string s2.symbolic_label)
        end) sorted_states ;

    Array.iteri (fun i s ->
      if (s.state_trigger <> "") then begin
        Hashtbl.add state_trigger_of_state
          (int_of_string s.symbolic_label) !cur_state_trig ;
        add_trig state_trig_buff s.state_trigger ;
        incr cur_state_trig ;
      end) sorted_states ;

    let transition_trigger_queue = Queue.create () in
    let transition_action_queue = Queue.create () in

    Array.iteri (fun i s ->
      Buffer.add_string state_buff (str_of_int (tlk_ify s.resp_str)) ;
      Buffer.add_string state_buff (str_of_int !cur_trans) ;
      Buffer.add_string state_buff (str_of_int (Array.length s.trans)) ;

      if (s.state_trigger = "") then
        Buffer.add_string state_buff "\255\255\255\255"
      else begin
        let index = Hashtbl.find state_trigger_of_state i in
        Buffer.add_string state_buff (str_of_int index) ;
      end ;

      (* now emit transitions *)
      Array.iter (fun t ->
        (* compute flags *)
        let flags = ref t.unknown_flags in
        if (t.trans_str <> None) then flags := !flags lor 1 ;
        if (t.trans_trigger <> None) then flags := !flags lor 2 ;
        (match t.action with
        | None -> ()
        | Some(s) when
            (try let _ = Str.search_forward letter_regexp s 0 in
            false
            with Not_found -> true) -> ()
        | Some(real_action) -> flags := !flags lor 4) ;
        (* if (t.action <> None) then flags := !flags lor 4 ; *)
        if (t.next = Exit) then flags := !flags lor 8 ;
        if (t.journal_str <> None) then flags := !flags lor 16 ;
        (match t.journal_str with
        | Some(Solved_Journal,_) -> flags := !flags lor 0x100
        | Some(Unsolved_Journal,_) -> flags := !flags lor 0x40
        | _ -> ()) ;

        Buffer.add_string trans_buff (str_of_int !flags) ;
        (match t.trans_str with
        | None -> Buffer.add_string trans_buff "\255\255\255\255"
        | Some(s) -> Buffer.add_string trans_buff (str_of_int (tlk_ify s))) ;
        (match t.journal_str with
        | None -> Buffer.add_string trans_buff "\000\000\000\000"
        | Some(_,s) -> Buffer.add_string trans_buff (str_of_int (tlk_ify s))) ;
        (match t.trans_trigger with
        | None -> Buffer.add_string trans_buff "\000\000\000\000"
        | Some(s) -> Buffer.add_string trans_buff (str_of_int !cur_trans_trig) ;
            Queue.add s transition_trigger_queue ;
            incr cur_trans_trig ) ;
        (match t.action with
        | None -> Buffer.add_string trans_buff "\000\000\000\000"
        | Some(s) -> Buffer.add_string trans_buff (str_of_int !cur_action) ;
            Queue.add s transition_action_queue ;
            incr cur_action ) ;
        (match t.next with
        | Exit -> Buffer.add_string trans_buff
              "\000\000\000\000\000\000\000\000\000\000\000\000"
        | Copy _
        | Copy_Late _ -> failwith "unresolved COPY_TRANS"
        | Symbolic(s1,s2,b) -> failwith "emit_dlg symbolic transition"
        | Absolute(s,i) -> Buffer.add_string trans_buff (str_to_exact_size s 8) ;
            Buffer.add_string trans_buff (str_of_int i)) ;

        incr cur_trans) s.trans) dlg.state ;

    Queue.iter (fun string -> add_trig trans_trig_buff string) transition_trigger_queue ;
    Queue.iter (fun string -> add_trig action_buff string) transition_action_queue ;

    (*
     * Layout Order:
     * Header
     * States
     * Trans
     * State Trig
     * Trans Trig
     * Actions
     * Raw Text Strings
     *)
    Buffer.add_string header_buff "DLG V1.0" ;
    Buffer.add_string header_buff (str_of_int num_state) ;
    Buffer.add_string header_buff (str_of_int offset_state_table) ;

    Buffer.add_string header_buff (str_of_int !cur_trans) ;
    Buffer.add_string header_buff (str_of_int offset_trans_table) ;

    Buffer.add_string header_buff (str_of_int offset_strig_table) ;
    Buffer.add_string header_buff (str_of_int !cur_state_trig) ;

    Buffer.add_string header_buff (str_of_int offset_ttrig_table) ;
    Buffer.add_string header_buff (str_of_int !cur_trans_trig) ;

    Buffer.add_string header_buff (str_of_int offset_action_table) ;
    Buffer.add_string header_buff (str_of_int !cur_action) ;

    Buffer.add_string header_buff (str_of_int dlg.dlg_flags) ;

    if num_trans <> !cur_trans then failwith "trans count mismatch" ;
    if num_strig <> !cur_state_trig then failwith "strig count mismatch" ;
    if num_ttrig <> !cur_trans_trig then failwith "ttrig count mismatch" ;
    if num_action <> !cur_action then failwith "action count mismatch" ;

    Buffer.add_buffer outbuff header_buff ;
    Buffer.add_buffer outbuff state_buff ;
    Buffer.add_buffer outbuff trans_buff ;
    Buffer.add_buffer outbuff state_trig_buff ;
    Buffer.add_buffer outbuff trans_trig_buff ;
    Buffer.add_buffer outbuff action_buff ;
    Buffer.add_buffer outbuff string_buff ;

    log_or_print "[%s.DLG] saved\t%d states, %d trans, %d strig, %d ttrig, %d actions\n"
      dlg.name (Array.length dlg.state) !cur_trans !cur_state_trig
      !cur_trans_trig !cur_action) () ;
end

(*
 * Code for emitting ".D" files
 *)

let os foo = match foo with
| Some(s) -> convert_raw_text_out s
| None -> ""

let print_flags f = match f with
| 0 -> ""
| i -> Printf.sprintf " FLAGS %d" i

let trans_count = ref 0

let tc_of_tlk_index = Hashtbl.create 511

let use_trans_ref = ref false

let reprint_trigger : (string -> string) ref = ref (fun s -> failwith "Dlg.reprint_trigger not implemented")

let emit_d dlg out_name dt dft o ot only_state reprint_d_action
    transitive toplevel =
  begin

    Stats.time "emit D" (fun () ->

      (match ot , !comments with
      | Some(ot) , true -> Printf.fprintf ot "// %s translation file\n" dlg.name
      | _ , _ -> ()) ;

      let ts foo = match foo with
      | TLK_Index i -> begin
          let male = Tlk.pretty_print dt i in
          let female = Tlk.pretty_print_opt dft i in
          (match ot with (* translation file *)
          | Some(ot) -> begin
              let tc =
                if Hashtbl.mem tc_of_tlk_index male then
                  Hashtbl.find tc_of_tlk_index male
                else begin
                  incr trans_count ;
                  Hashtbl.add tc_of_tlk_index male !trans_count ;
                  Printf.fprintf ot "@%-3d = " !trans_count ;
                  if !use_trans_ref then
                    Printf.fprintf ot "#%d /* " i ;
                  (if (female = "" || male = female) then
                    Printf.fprintf ot "%s" male
                  else
                    Printf.fprintf ot "%s %s" male female) ;
                  if !use_trans_ref then
                    Printf.fprintf ot " */" ;
                  Printf.fprintf ot "\n" ;
                  !trans_count
                end
              in
              (if not !comments then
                Printf.sprintf "@%d" tc
              else
                Printf.sprintf "@%d /* %s #%d */" tc male i) ;
          end
          | None -> begin
              let male_female =
                if female = "" || male = female then male
                else male ^ " " ^ female
              in
              (match !comments, !emit_text with
              | true, true -> Printf.sprintf "%s /* #%d */" male_female i
              | false, true -> Printf.sprintf "%s" male_female
              | false, false -> Printf.sprintf "#%d" i
              | true, false -> Printf.sprintf "#%d /* %s */" i male_female)
          end)
      end
      | Local_String _ -> failwith "emit_d:ts: lse"
      | Trans_String _ -> failwith "emit_d:ts: ts"
      in
      let print_action so = match so with
      | Some(s) ->
          begin
            match ot with (* translation file *)
            | Some(ot) -> begin
                let old_comments = !comments in
                comments := false ;
                let str = reprint_d_action s
                    ((fun i -> ts (TLK_Index(Int32.to_int i)))) in
                comments := old_comments ;
                Printf.sprintf " DO ~%s~" str
              end
            | None -> Printf.sprintf " DO ~%s~" (convert_raw_text_out s)
          end
      | None -> ""
      in

      let print_reply tso = match tso with
      | Some(foo) -> Printf.sprintf " REPLY %s" (ts foo)
      | None -> ""
      in
      let print_journal tso = match tso with
      | Some(Normal_Journal,foo) -> Printf.sprintf " JOURNAL %s" (ts foo)
      | Some(Solved_Journal,foo) -> Printf.sprintf " SOLVED_JOURNAL %s" (ts foo)
      | Some(Unsolved_Journal,foo) -> Printf.sprintf " UNSOLVED_JOURNAL %s" (ts foo)
      | None -> ""
      in

      let worklist = ref [] in
      let processed_states = Hashtbl.create 50 in

      let print_goto no = match no with
      | Absolute(str,i) when str = dlg.name -> Printf.sprintf " GOTO %d" i
      | Absolute(str,i) ->
          begin
            (if transitive && not (Hashtbl.mem processed_states (str,i)) &&
              not (List.mem (str,i) !worklist) then
              worklist := (str,i) :: !worklist) ;
            Printf.sprintf " EXTERN ~%s~ %d" str i ;
          end
      | Symbolic(s1,s2,b) -> failwith "emit_d : symbolic next"
      | Copy _
      | Copy_Late _ -> failwith "emit_d : unresolved COPY_TRANS"
      | Exit -> " EXIT"
      in
      let checked_after state_num weight_num =
        let result = ref [] in
        for i = state_num + 1 to (Array.length dlg.state) - 1 do
          match dlg.state.(i).state_trigger,dlg.state.(i).state_trigger_weight with
          | s, Offset(w) when w < weight_num && s <> "" ->
              result := i :: !result
          | _ -> ()
        done ;
        List.rev !result
      in

      let trivial_weighting = (* true if there is only trivial weighting *)
        let so_far = ref 0 in
        let answer = ref true in
        Array.iter (fun s ->
          match s.state_trigger_weight with
          |  Offset(i) -> if i < !so_far then answer := false
          else so_far := i
          | _ -> ()) dlg.state ;
        !answer
      in

      let print_weight o state_num s w = match s,w with
      | "",_ -> ()
      | _,Not_Specified -> ()
      | _,Offset(i) ->
          let sl = checked_after state_num i in
          Printf.bprintf o "WEIGHT #%d " i ;
          if (sl <> []) && !comments then begin
            Printf.bprintf o "/* Triggers after states #:" ;
            List.iter (fun sn -> Printf.bprintf o " %d" sn) sl ;
            Printf.bprintf o " even though they appear after this state */\n" ;
          end
      in
      if only_state = None then begin
        (if (dlg.dlg_flags = 0) then
          Printf.bprintf o "BEGIN ~%s~\n" dlg.name
        else if !comments then
          Printf.bprintf o "BEGIN ~%s~ %d // non-zero flags may indicate non-pausing dialogue\n" dlg.name dlg.dlg_flags
        else
          Printf.bprintf o "BEGIN ~%s~ %d\n" dlg.name dlg.dlg_flags) ;

        if !comments && not trivial_weighting then begin
          Printf.bprintf o "//////////////////////////////////////////////////\n// WARNING: this file contains non-trivial WEIGHTs\n//////////////////////////////////////////////////\n" ;
        end ;
      end ;

      let print_state this_dlg_name i s str = begin
        if (only_state = None || only_state = Some(i)) &&
          ((!emit_some_states_only = false) ||
          (Hashtbl.mem emit_these_states i) ) &&
          ((toplevel = false) || (s.state_trigger <> ""))
        then
          try
            Printf.bprintf o "\n%sIF " (if str = "" || not !comments then "" else "/* ") ;
            if not trivial_weighting then
              print_weight o i s.state_trigger s.state_trigger_weight ;
            Printf.bprintf o "~%s~ THEN BEGIN %s%d"
              (convert_raw_text_out (!reprint_trigger s.state_trigger))
              str i ;

            if !emit_from && !comments then begin
              Printf.bprintf o " // from:" ;
              (* print "from:" list *)
              let already_ht = Hashtbl.create 255 in
              List.iter (fun (n,s,t) ->
                if Hashtbl.mem already_ht (n,s,t) then
                  ()
                else begin
                  Hashtbl.add already_ht (n,s,t) true ;
                  if n = this_dlg_name then
                    Printf.bprintf o " %d.%d" s t
                  else
                    Printf.bprintf o " %s:%d.%d" n s t
                end) (Hashtbl.find_all from_ht (this_dlg_name,i)) ;
            end ;

            Printf.bprintf o "\n  SAY %s\n" (ts s.resp_str) ;

            (* print transitions *)
            Array.iteri (fun j t ->
              try
                Printf.bprintf o "  IF ~%s~ THEN%s%s%s%s%s\n"
                  (os (match t.trans_trigger with None -> None | Some x -> Some (!reprint_trigger x)))
                  (print_reply t.trans_str)
                  (print_action t.action)
                  (print_journal t.journal_str)
                  (print_flags t.unknown_flags)
                  (print_goto t.next)
              with e -> log_and_print "ERROR: problem emitting trans %d: %s\n" j
                  (printexc_to_string e) ; raise e) s.trans ;
            Printf.bprintf o "END%s\n" (if str = "" || not !comments then "" else " */") ;
          with e -> log_and_print "ERROR: problem emitting state %d: %s\n" i
              (printexc_to_string e) ; raise e
      end in

      Array.iteri (fun i s ->
        print_state dlg.name i s "" ;
        while !worklist <> [] do
          try
            let (file,i) = List.hd !worklist in
            worklist := List.tl !worklist ;
            let buff,path = Load.load_resource dlg.name (Load.the_game ()) true file "DLG" in
            let dlg = load_dlg file buff in
            print_state file i (dlg.state.(i)) (file ^ " ") ;
            Hashtbl.add processed_states (file,i) true ;
          with _ -> ()
        done) dlg.state ;

      if (only_state = None) then begin
        log_or_print "[%s] created from [%s.DLG]\n" out_name dlg.name ;
      end ;
      match ot with
      | Some(ot) -> close_out ot
      | None -> ()) () ;
  end

let dlg_compare o d1 d2 dt dft reprint_d_action =
  let oldlength = Array.length d2.state in
  if Array.length d1.state > Array.length d2.state then begin
    Printf.bprintf o "//////////////////////////////////////////////////\n// NOTICE: initial dialogue has more states than the resulting one.\n" ;
    Printf.bprintf o "// State %d and following don't exist in the resulting file.\n" (Array.length d2.state) ;
    Printf.bprintf o "// They're overwritten with a blank state because I can't delete them.\n//////////////////////////////////////////////////\n"
  end ;
  Printf.bprintf o "REPLACE ~%s~\n\n" d1.name ;
  let d2 = {d2 with state = if Array.length d1.state > Array.length d2.state then begin
    Array.init (Array.length d1.state) (fun i ->
      if i < (Array.length d2.state) then d2.state.(i)
      else { d1.state.(i) with
             state_trigger = "False()\n" ^ d1.state.(i).state_trigger ;
             symbolic_label = (Printf.sprintf "state_that_should_not_exist_%d" i) ;
           })
  end else d2.state} in
  for i = 0 to (Array.length d1.state) - 1 do
    let s1 = d1.state.(i) in
    let s2 = d2.state.(i) in
    if oldlength = i then
      Printf.bprintf o "//////////////////////////////////////////////////\n// NOTICE: states that should be deleted start from here.\n//////////////////////////////////////////////////\n" ;
    if s1 = s2 then
      ()
    else
      emit_d d2 d2.name dt dft o (None) (Some(i)) reprint_d_action false false
  done ;
  Printf.bprintf o "\nEND\n" ;
  Printf.bprintf o "APPEND ~%s~\n\n" d1.name ;
  for i = (Array.length d1.state) to (Array.length d2.state) - 1 do
    let s1 = d2.state.(i) in
    emit_d d2 d2.name dt dft o (None) (Some(i)) reprint_d_action false false
  done ;
  Printf.bprintf o "\nEND\n"

let make_state_trans says called trans =
  {
   resp_str = says ;
   trans = trans ;
   state_trigger = "" ;
   state_trigger_weight = Not_Specified ;
   symbolic_label = called ;
 }

let make_state says called goes_to_file goes_to_label =
  let trans =
    {
     unknown_flags = 0 ;
     trans_str = None ;
     journal_str = None ;
     trans_trigger = None ;
     action = None ;
     next = (Symbolic(goes_to_file,goes_to_label,false))
   }
  in
  {
   resp_str = says ;
   trans = [| trans |] ;
   state_trigger = "" ;
   state_trigger_weight = Not_Specified ;
   symbolic_label = called ;
 }

let make_trans_of_next next = {
  unknown_flags = 0 ;
  trans_str = None ;
  journal_str = None ;
  trans_trigger = None ;
  action = None ;
  next = next ; }

let sort_list_for_traify list =
  let sounded, unsounded = List.partition (fun (index, ls) ->
    ls.lse_male_sound <> "" || ls.lse_female_sound <> "") list in
  List.append sounded unsounded

let lse_of_ts ts =
  (match ts with
  | Local_String(ls) -> ls
  | _ -> failwith "Internal error: ts is not an lse")

let make_list_for_traify traify_num =
  let local_strings = (match !local_string_ht with
  | Some (l) -> l
  | None -> failwith "Internal error: no local strings for traify") in
  let counter = ref !traify_num in
  let enumerated = List.map (fun ts ->
    let tuple = (!counter, (lse_of_ts ts)) in
    incr counter ;
    tuple) (deduplicate (List.rev local_strings)) in
  sort_list_for_traify enumerated
