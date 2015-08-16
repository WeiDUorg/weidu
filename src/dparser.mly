%{

open Util
open Load

(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.dparser.mly.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(*** input handle ***)

let current_unit = ref None

(*** Error handling ***)
let parse_error = Util.parse_error

let get_current_unit () = match !current_unit with
  Some(s) -> s
| None -> parse_error "No current unit (use BEGIN)" 

type trans_opt_type =
    Trans_Reply of Dlg.tlk_string option
  | Trans_Do of string option
  | Trans_Journal of (Dlg.journal_type * Dlg.tlk_string) option
  | Trans_Flags of int

let extra_actions = ref []

let verify_trigger_list s =
  if not !Dc.doing_traify then begin
    let con = the_context () in
    let filename = (Printf.sprintf "trigger list near line %d, column %d of %s"
		      con.line con.col con.filename) in
    let lexbuf = lex_init_from_internal_string filename s in
    let lexbuf = Lexing.from_string (String.copy s) in
    let result = try
      let res = Bafparser.trigger_list Baflexer.initial lexbuf in
      let buff = Buffer.create (String.length s) in
      Bcs.print_script_text (the_game()) (Bcs.Save_BCS_Buffer(buff))
        (Bcs.BCS_Print_TriggerList(res,true)) false None ;
      let result = Buffer.contents buff in
      result
    with e ->
      log_and_print "WARNING: cannot verify trigger ~%s~: %s\n" s
        (printexc_to_string e) ;
      (try assert false with Assert_failure(file,line,col) -> set_errors file line);
      s
    in
    pop_context () ;
    result
  end else
    s

let verify_action_list s =
  if not !Dc.doing_traify then begin
    let con = the_context () in
    let filename = (Printf.sprintf "action list near line %d, column %d of %s"
		      con.line con.col con.filename) in
    let lexbuf = lex_init_from_internal_string filename s in
    let result = try
      let res = Bafparser.action_list Baflexer.initial lexbuf in
      let buff = Buffer.create (String.length s) in
      Bcs.print_script_text (the_game()) (Bcs.Save_BCS_Buffer(buff))
        (Bcs.BCS_Print_ActionList(res)) false None ;
      Buffer.contents buff
    with e ->
      log_and_print "WARNING: cannot verify action ~%s~: %s\n" s
        (printexc_to_string e) ;
      (try assert false with Assert_failure(file,line,col) -> set_errors file line);
      s
    in
    pop_context () ;
    (*
      (if result = "" then begin
      let warn = con.warn_only in
      con.warn_only <- true ;
      (try parse_error "Empty Action (may confuse some utilities, like NI)"
      with _ -> () ) ;
      con.warn_only <- warn
      end) ;
     *)
    result
  end else
    s

    %}

  %token ADD_STATE_TRIGGER
  %token ADD_TRANS_TRIGGER
  %token ADD_TRANS_ACTION
  %token ALTER_TRANS
  %token APPEND
  %token APPEND_EARLY
  %token APPENDI
  %token AT
  %token BEGIN
  %token BRANCH
  %token CHAIN2
  %token CHAIN3
  %token COPY_TRANS
  %token COPY_TRANS_LATE
  %token DO
  %token END
  %token EQUALS
  %token EQUALSEQUALS
  %token EXIT
  %token EXTEND_BOTTOM
  %token EXTEND_BOTTOM_REGEXP
  %token EXTEND_TOP
  %token EXTEND_TOP_REGEXP
  %token EXTERN
  %token FLAGS
  %token GOTO
  %token IF
  %token IF_FILE_EXISTS
  %token INTERJECT
  %token INTERJECT_COPY_TRANS
  %token INTERJECT_COPY_TRANS2
  %token INTERJECT_COPY_TRANS3
  %token INTERJECT_COPY_TRANS4
  %token JOURNAL SOLVED_JOURNAL UNSOLVED_JOURNAL
  %token LPAREN
  %token PLUS
  %token REPLACE
  %token REPLACE_ACTION_TEXT
  %token REPLACE_ACTION_TEXT_PROCESS
  %token REPLACE_ACTION_TEXT_REGEXP
  %token REPLACE_ACTION_TEXT_PROCESS_REGEXP
  %token REPLACE_SAY
  %token REPLACE_STATE_TRIGGER
  %token REPLACE_TRANS_ACTION
  %token REPLACE_TRANS_TRIGGER
  %token REPLACE_TRIGGER_TEXT
  %token REPLACE_TRIGGER_TEXT_REGEXP
  %token REPLY
  %token RPAREN
  %token SAFE
  %token SAY
  %token SET_WEIGHT
  %token STRING_CONCAT
  %token THEN
  %token UNLESS
  %token WEIGHT

  %token EOF

  %token <string> SOUND STRING
  %token <string * string> INLINED_FILE
  %token <int> STRING_REF TRANS_REF FORCED_STRING_REF

  %left EQUALS EQUALSEQUALS
  %left PLUS 

  /* Non-terminals informations */
%start d_file tra_file log_file unsetstr_file args_file tlk_path_file

  %type <(string * int)> begin_prologue
  %type <(string * bool)> append_prologue
  %type <(string)> replace_prologue
  %type <(bool * string * (string list) * int)> extend_prologue
  %type <(string * bool * string * string * bool * bool)> interject_prologue
  %type <(string * bool * string * string * bool * bool * bool)>interject_copy_trans_prologue
  %type <Dc.action list> d_file action_list
  %type <Dc.action> action
  %type <Dlg.state list> state_list state
  %type <Dlg.transition list> transition_list
  %type <Dlg.transition> transition
  %type <Load.str_set_record list> unsetstr_file
  %type <string list> args_file
  %type <string * (string option)> tlk_path_file

  %type <Dlg.weight> weight

  %type <(string * Dlg.tlk_string) list> chain2_list

  %type <((string option) * Dlg.tlk_string * (string option)) list> chain3_list
  %type <string option> optional_condition

  %type <Dlg.tlk_string> lse
  %type <Dlg.tlk_string list> say_list
  %type <string> sound_opt
  %type <Dlg.trans_next> next

  %type <trans_opt_type> trans_opt
  %type <trans_opt_type list> trans_opt_list

  %type <(int * Dlg.tlk_string) list> tra_file

  %type <string list> string_list
  %type <(string * Dc.alter_trans_feature) list> alter_trans_list
  %type <int list> int_list
  %type <string list> upper_string_list
  %type <int> hash_int_option

  %type <(string * int * int * (string option)) list> log_file installed_mod_list

  %type <string option * string> appendi_prologue
  %%

d_file : action_list   { let answer = $1 @ !extra_actions in
extra_actions := [] ;
answer }
  ;

action_list :           { [] }
| action action_list    { $1 :: $2 }
    ;

begin_prologue :
  BEGIN STRING             { current_unit := Some(String.uppercase $2); (String.uppercase $2,0) }
| BEGIN STRING STRING       { current_unit := Some(String.uppercase $2); (String.uppercase $2,my_int_of_string $3) }
    ;

append_prologue : APPEND STRING       
  { current_unit := Some(String.uppercase $2) ;
    (String.uppercase $2,false) } ;
| APPEND IF_FILE_EXISTS STRING
    { current_unit := Some(String.uppercase $3) ;
      (String.uppercase $3,true) } ;

append_safe_prologue : APPEND_EARLY STRING
  { current_unit := Some(String.uppercase $2) ;
    (String.uppercase $2,false) } ;
| APPEND_EARLY IF_FILE_EXISTS STRING
    { current_unit := Some(String.uppercase $3) ;
      (String.uppercase $3,true) } ;


string_list :            { [] }
| STRING string_list  { $1 :: $2 } 
    ;

alter_trans_list :            { [] }
| STRING STRING alter_trans_list  {
  let handle f =
    let con = the_context () in
    let filename = (Printf.sprintf "ALTER_TRANS stuff list near line %d, column %d of %s"
		      con.line con.col con.filename) in
    let result = begin try
      ($1, f $2) :: $3
    with e ->
      log_and_print "WARNING: can not verify %s parameter for %s" $1 filename;
      (try assert false with Assert_failure(file,line,col) -> set_errors file line);
      ($1, Dc.Alter_Trans_String $2) :: $3
    end
    in
    result
  in
  match String.uppercase $1 with
  | "TRIGGER" -> handle (fun s -> Dc.Alter_Trans_String (verify_trigger_list s))
  | "ACTION" -> handle (fun s -> Dc.Alter_Trans_String (verify_action_list s))
  | "REPLY" | "JOURNAL" | "SOLVED_JOURNAL" | "UNSOLVED_JOURNAL" ->
      handle (fun s ->
	if s = "" then Dc.Alter_Trans_String "" else
	match s.[0] with
	| '@' -> Dc.Alter_Trans_Lse (Dc.resolve_string_while_loading (Dlg.Trans_String (Dlg.String(Str.string_after s 1))))
	| '#' -> Dc.Alter_Trans_String ("#" ^ string_of_int(int_of_string(Str.string_after s 1)))
	| _ -> Dc.Alter_Trans_String s
	     )
  | "EPILOGUE" -> handle (fun s ->
      let parts = Str.split (Str.regexp " ") s in
      if (match parts with
      | [a;b;c] -> a <> "EXTERN"
      | [a;b] -> a <> "GOTO" && a <> "+"
      | [a] -> a <> "EXIT"
      | _ -> true) then failwith "Wrong EPILOGUE for ALTER_TRANS";
      Dc.Alter_Trans_String s
			 )
  | "FLAGS" -> handle (fun s ->  Dc.Alter_Trans_String (string_of_int (int_of_string s)))
  | _ -> failwith(Printf.sprintf "Unknown ALTER_TRANS command: %s" $1)
}
    ;

int_list:   { [] }
| STRING int_list  { (my_int_of_string $1) :: $2 }
    ;

upper_string_list :            { [] }
| STRING upper_string_list  { (String.uppercase $1) :: $2 }
    ;

when_list : { [] }
| IF STRING when_list { Dc.W_If $2 :: $3 }	
| UNLESS STRING when_list { Dc.W_Unless $2 :: $3 }	
	
extend_prologue :
  EXTEND_TOP STRING string_list hash_int_option 
  { current_unit := Some(String.uppercase $2) ;
    (true,String.uppercase $2,$3,$4) }
| EXTEND_BOTTOM STRING string_list hash_int_option
    { current_unit := Some(String.uppercase $2) ;
      (false,String.uppercase $2,$3,$4) }
    ; 

hash_int_option :             { 0 }
| STRING_REF                  { $1 } 
    ;

replace_prologue : REPLACE STRING 
  { current_unit := Some(String.uppercase $2); (String.uppercase $2) }
  ; 
replace_state_trigger_prologue : REPLACE_STATE_TRIGGER STRING STRING STRING 
  string_list
  { current_unit := Some(String.uppercase $2); 
    (String.uppercase $2,$3 :: $5,$4) }
  ; 

opt_do_string_list :    { [] }
| DO string_list        { $2 }
    ;

add_trans_trigger_prologue : ADD_TRANS_TRIGGER STRING STRING STRING string_list
  opt_do_string_list 
  { current_unit := Some(String.uppercase $2); 
    (String.uppercase $2,$3 :: $5,$4,$6) }
  ; 
add_state_trigger_prologue : ADD_STATE_TRIGGER STRING STRING STRING string_list
  { current_unit := Some(String.uppercase $2); 
    (String.uppercase $2,$3 :: $5,$4) 
  }
  ;

chain3_prologue : CHAIN3 optional_weighted_condition STRING STRING
  { current_unit := Some(String.uppercase $3); ($2,String.uppercase $3,$4,false) }
| CHAIN3 optional_weighted_condition IF_FILE_EXISTS STRING STRING
    { current_unit := Some(String.uppercase $4); ($2,String.uppercase $4,$5,true) }
    ;

interject_prologue : INTERJECT STRING STRING STRING
  { current_unit := Some(String.uppercase $2); (String.uppercase $2,false,$3,$4,false,false) }
  ;
| INTERJECT IF_FILE_EXISTS STRING STRING STRING
  { current_unit := Some(String.uppercase $3); (String.uppercase $3,true,$4,$5,false,false) }
  ;

interject_copy_trans_prologue :
  /* first boolean = enable don't copy actions a la ICT2; second boolean: add all transitions a la
  ICT3 */
  INTERJECT_COPY_TRANS optional_safe STRING STRING STRING
  { current_unit := Some(String.uppercase $3); (String.uppercase $3,false,$4,$5,false,false,$2) }
| INTERJECT_COPY_TRANS2 optional_safe STRING STRING STRING
    { current_unit := Some(String.uppercase $3); (String.uppercase $3,false,$4,$5,true,false,$2) }
| INTERJECT_COPY_TRANS3 optional_safe STRING STRING STRING
    { current_unit := Some(String.uppercase $3); (String.uppercase $3,false,$4,$5,false,true,$2) }
| INTERJECT_COPY_TRANS4 optional_safe STRING STRING STRING
    { current_unit := Some(String.uppercase $3); (String.uppercase $3,false,$4,$5,true,true,$2) }
    ;
| INTERJECT_COPY_TRANS optional_safe IF_FILE_EXISTS STRING STRING STRING
  { current_unit := Some(String.uppercase $4); (String.uppercase $4,true,$5,$6,false,false,$2) }
| INTERJECT_COPY_TRANS2 optional_safe IF_FILE_EXISTS STRING STRING STRING
    { current_unit := Some(String.uppercase $4); (String.uppercase $4,true,$5,$6,true,false,$2) }
| INTERJECT_COPY_TRANS3 optional_safe IF_FILE_EXISTS STRING STRING STRING
    { current_unit := Some(String.uppercase $4); (String.uppercase $4,true,$5,$6,false,true,$2) }
| INTERJECT_COPY_TRANS4 optional_safe IF_FILE_EXISTS STRING STRING STRING
    { current_unit := Some(String.uppercase $4); (String.uppercase $4,true,$5,$6,true,true,$2) }
    ;

  action : 
    begin_prologue state_list { let name,flags = $1 in Dc.Create(
    { Dlg.name = name ;
      Dlg.state = Array.of_list $2 ;
      Dlg.dlg_flags = flags ; })}
| append_prologue state_list END 
    {let a,b=$1 in current_unit := None ; Dc.Append( a,b , $2 ) }
| append_safe_prologue state_list END
    { let a,b=$1 in current_unit := None ; Dc.Append_Early( a,b , $2 ) }
| extend_prologue transition_list END 
    { let top,ext_unit,ext_label,number = $1 in
    current_unit := None ; 
    if top then 
      Dc.Extend_Top( String.uppercase ext_unit, ext_label, number, $2 )
    else
      Dc.Extend_Bottom( String.uppercase ext_unit, ext_label, number, $2 ) 
    } 
| replace_prologue state_list END 
    { current_unit := None ;
      Dc.Replace($1, $2) }
| REPLACE_SAY STRING STRING lse
    { Dc.Replace_Say(String.uppercase $2,$3,$4) } 
| replace_state_trigger_prologue when_list
    { let f,s,t = $1 in 
    let verified_t = verify_trigger_list t in 
    current_unit := None ; 
    Dc.Replace_State_Trigger(f,s,verified_t,$2) } 
| add_state_trigger_prologue when_list
    { let f,s,t = $1 in 
    let verified_t = verify_trigger_list t in 
    current_unit := None ; 
    Dc.Add_State_Trigger(f,s,verified_t,$2) } 
| add_trans_trigger_prologue  when_list
    { let f,s,t,tl = $1 in 
    let verified_t = verify_trigger_list t in
    current_unit := None ; 
    Dc.Add_Trans_Trigger(f,s,verified_t,tl,$2) } 
| ADD_TRANS_ACTION STRING BEGIN string_list END BEGIN int_list END STRING when_list
    { current_unit := Some(String.uppercase $2);
      let verified_a = verify_action_list $9 in
      current_unit := None ; 
      Dc.Add_Trans_Action(String.uppercase $2,$4,$7,verified_a,$10) }
| ALTER_TRANS STRING BEGIN string_list END BEGIN int_list END BEGIN alter_trans_list END when_list
    { Dc.Alter_Trans(String.uppercase $2,$4,$7,$10) }
| REPLACE_TRANS_ACTION STRING BEGIN string_list END BEGIN int_list END STRING STRING when_list
    { current_unit := Some(String.uppercase $2);
      current_unit := None ; 
      Dc.Replace_Trans_Action(String.uppercase $2,$4,$7,$9,$10,$11) }
| REPLACE_TRANS_TRIGGER STRING BEGIN string_list END BEGIN int_list END STRING STRING when_list
    { current_unit := Some(String.uppercase $2);
      current_unit := None ;
      Dc.Replace_Trans_Trigger(String.uppercase $2,$4,$7,$9,$10,$11) }
| SET_WEIGHT STRING STRING STRING_REF
    { Dc.Set_Weight(String.uppercase $2,$3,$4) }
| chain3_prologue chain3_list compound_chain3_list chain3_epilogue
    { let (entry_weight,entry_cond),file,state,ifexist = $1 in
    let first_part = List.map (fun (cond,says,action) ->
      {
       Dc.c3du_speaker = file ;
       Dc.c3du_condition = cond ;
       Dc.c3du_action = action ;
       Dc.c3du_say = says ;
       Dc.c3du_id = Dc.get_c3du_counter () ;
       Dc.c3du_ifexists = false ;
     } ) $2 in
    Dc.Chain3
      {
       Dc.c3_entry_condition = entry_cond;
       Dc.c3_entry_weight = entry_weight;
       Dc.c3_iffileexists = ifexist;
       Dc.c3_entry_file = file ;
       Dc.c3_entry_label = state ;
       Dc.c3_dialogue = first_part @ $3 ;
       Dc.c3_variable = None ;
       Dc.c3_exit_trans = $4 ;
       Dc.c3_keep_first_do_with_first_speaker = false;
       Dc.c3_append_all = false ;
     }
    }
| interject_prologue compound_chain3_list chain3_epilogue
    { let file,iffileexists,label,var,keep_do,append_all = $1 in
    Dc.Chain3
      {
       Dc.c3_entry_condition = None;
       Dc.c3_entry_weight = Dlg.Not_Specified;
       Dc.c3_entry_file = file ;
       Dc.c3_iffileexists = iffileexists ;
       Dc.c3_entry_label = label ;
       Dc.c3_dialogue = $2 ;
       Dc.c3_variable = Some(var) ;
       Dc.c3_exit_trans = $3 ;
       Dc.c3_keep_first_do_with_first_speaker = keep_do;
       Dc.c3_append_all = append_all ;
     }
    }
| interject_copy_trans_prologue compound_chain3_list END
    { let file,iffileexists,label,var,keep_do,append_all,safe = $1 in
	let trans = [| Dlg.make_trans_of_next (Dlg.Copy(file,label,safe)) |] in
    Dc.Chain3
      {
       Dc.c3_entry_condition = None;
       Dc.c3_entry_weight = Dlg.Not_Specified;
       Dc.c3_entry_file = file ;
       Dc.c3_iffileexists = iffileexists;
       Dc.c3_entry_label = label ;
       Dc.c3_dialogue = $2 ;
       Dc.c3_variable = Some(var) ;
       Dc.c3_exit_trans = trans ;
       Dc.c3_keep_first_do_with_first_speaker = keep_do;
       Dc.c3_append_all = append_all ;
     }
    }
| REPLACE_TRIGGER_TEXT STRING STRING STRING when_list
    { Dc.Replace_Trigger_Text(String.uppercase $2,$3,$4,false,$5) }
| REPLACE_TRIGGER_TEXT_REGEXP STRING STRING STRING when_list
    { Dc.Replace_Trigger_Text(String.uppercase $2,$3,$4,true,$5) }
| REPLACE_ACTION_TEXT STRING STRING STRING upper_string_list when_list
    { Dc.Replace_Action_Text(String.uppercase $2 :: $5,$3,$4,false,$6) }
| REPLACE_ACTION_TEXT_PROCESS STRING STRING STRING upper_string_list when_list
    { Dc.Replace_Action_Text(String.uppercase $2 :: $5,$3,verify_action_list $4,false,$6) }
| REPLACE_ACTION_TEXT_REGEXP STRING STRING STRING upper_string_list when_list
    { Dc.Replace_Action_Text(String.uppercase $2 :: $5,$3,$4,true,$6) }
| REPLACE_ACTION_TEXT_PROCESS_REGEXP STRING STRING STRING upper_string_list when_list
    { Dc.Replace_Action_Text(String.uppercase $2 :: $5,$3,verify_action_list $4,true,$6) }
    ;

  chain3_list : optional_condition lse optional_action     { [($1,$2,$3)] }
| optional_condition lse optional_action EQUALS chain3_list 
    { ($1,$2,$3) :: (List.map (fun (a,b,c) -> 
      match $1,a with
      | _,None -> ($1,b,c)
      | None,_ -> (a,b,c)
      | Some(first),Some(second) -> (Some(first ^ "\n" ^ second),b,c)
			      ) $5) }
    ;

  optional_action :       { None }
| DO STRING             { let verified_action = verify_action_list $2 in 
  Some(verified_action) } 
    ; 

  optional_safe:
   { false }
| SAFE { true }
;
	
  chain3_epilogue: 
| END STRING STRING
    { let next = Dlg.Symbolic(String.uppercase $2,$3,false) in
    [| Dlg.make_trans_of_next next |] }
| EXTERN STRING STRING
    { let next = Dlg.Symbolic(String.uppercase $2,$3,false) in
    [| Dlg.make_trans_of_next next |] }
| COPY_TRANS optional_safe STRING STRING
    { let next = Dlg.Copy(String.uppercase $3,$4,$2) in
	let trans = [| Dlg.make_trans_of_next next |] in
	trans }
| COPY_TRANS_LATE optional_safe STRING STRING
    { let next = Dlg.Copy_Late(String.uppercase $3,$4,$2) in
	let trans = [| Dlg.make_trans_of_next next |] in
	trans }
| EXIT
    { let next = Dlg.Exit in
    [| Dlg.make_trans_of_next next |] }
| END transition_list                { Array.of_list $2 }
    ; 

  optional_weighted_condition :    { ((Dlg.Not_Specified),(None)) }
| IF weight STRING THEN { ($2,(Some(verify_trigger_list $3))) }
    ; 

  optional_condition :
| IF STRING optional_then  { Some(verify_trigger_list $2) }
| { None }
    ; 

  compound_chain3_list :                  { [] }
| EQUALSEQUALS STRING chain3_list compound_chain3_list
    { let this_speaker = String.uppercase $2 in
    let first_part = List.map (fun (cond,says,action) ->
      { Dc.c3du_speaker = this_speaker ;
        Dc.c3du_condition = cond ;
        Dc.c3du_action = action ;
        Dc.c3du_say = says ;
        Dc.c3du_id = Dc.get_c3du_counter () ;
        Dc.c3du_ifexists = false ;
      }) $3 in
    first_part @ $4
    }
| EQUALSEQUALS IF_FILE_EXISTS STRING chain3_list compound_chain3_list
    { let this_speaker = String.uppercase $3 in
    let first_part = List.map (fun (cond,says,action) ->
      { Dc.c3du_speaker = this_speaker ;
        Dc.c3du_condition = cond ;
        Dc.c3du_action = action ;
        Dc.c3du_say = says ;
        Dc.c3du_id = Dc.get_c3du_counter () ;
        Dc.c3du_ifexists = true ;
      }) $4 in
    first_part @ $5
    }
| BRANCH STRING BEGIN compound_chain3_list END compound_chain3_list
    {
     List.map (fun x ->
       {
        x with
        Dc.c3du_condition = match x.Dc.c3du_condition with
        | None -> Some ($2)
        | Some(y) -> Some($2 ^ " " ^ y)
      }
	      ) $4 @ $6
   }
    ;



  state_list :            { [] }
| state state_list      { $1 @ $2 } 
    ;

  weight :                { Dlg.Not_Specified }
| WEIGHT STRING_REF     { Dlg.Offset($2) }
    ;

  optional_then: { () }
| THEN { () }
    ; 
  optional_begin: { () }
| BEGIN { () }
    ; 

  state_trigger : 
    STRING { if $1 = "" then "" else verify_trigger_list $1 } 
    ; 

  state :
    IF weight state_trigger optional_then optional_begin STRING 
    SAY say_list
    transition_list
    END {
  let state_trigger = $3 in 
  if List.length $8 = 1 then
    [{ Dlg.resp_str = List.hd $8 ;
       Dlg.trans = Array.of_list $9 ;
       Dlg.state_trigger = state_trigger ;
       Dlg.state_trigger_weight = $2 ; 
       Dlg.symbolic_label = $6 ; }]
  else begin
    let rec process_say_list sl in_label in_trig in_weight = match sl with
      s1 :: s2 :: tl -> 
        let new_label = Dc.chain_label () in
        let dest_file = get_current_unit () in 
        let new_state = Dlg.make_state s1 in_label dest_file new_label in
        new_state.Dlg.state_trigger_weight <- in_weight ; 
        new_state.Dlg.state_trigger <- in_trig ;
        new_state :: (process_say_list (s2::tl) new_label "" Dlg.Not_Specified)
    | s1 :: [] -> [
        let state = Dlg.make_state_trans s1 in_label (Array.of_list $9)
        in state.Dlg.state_trigger_weight <- in_weight ;
        state ] 
    | [] -> [] 
    in 
    process_say_list $8 $6 $3 $2
  end
} 
    /* backwards compat: internal append */
| appendi_prologue state_list END
    {
     let old_unit, append_name = $1 in
     extra_actions := (Dc.Append( append_name,false, $2 )) :: !extra_actions ;
     current_unit := old_unit ;
     [] 
   }
    /* backwards compat: internal 2-person chain */
| CHAIN2 STRING STRING chain2_list END STRING STRING
    { let rec convert cl extern_guy = match cl with
      [] -> []
    | (code,s) :: tl -> 
        let file = if extern_guy then $2 else get_current_unit () in
        let extern_guy = if code = "==" then not extern_guy else extern_guy in
        (file,s) :: convert tl extern_guy
    in 
    extra_actions := (Dc.Chain(
		      { Dc.entry_file = String.uppercase $2 ;
			Dc.entry_label = $3 ;
			Dc.dialogue = convert $4 true;
			Dc.exit_file = String.uppercase $6 ;
			Dc.exit_label = $7 ;
		      } )) :: !extra_actions ;
    []  
    } 
    ;

  appendi_prologue : APPENDI STRING 
    { let what = String.uppercase $2 in  
    let old = !current_unit in current_unit := Some(what); (old, what) } 
    ; 

  chain2_list : lse               { [("",$1)] } 
| lse EQUALS chain2_list        { ("",$1) :: $3 }
| lse EQUALSEQUALS chain2_list  { ("==",$1) :: $3 }
    ; 

  say_list : lse                  { [$1] } 
| lse EQUALS say_list           { $1 :: $3 }
    ; 

  transition_list :            { [] }
| transition transition_list { $1 :: $2 }
    ;

  trans_opt :
| REPLY lse      { Trans_Reply(Some($2)) } 
| DO STRING      { let verified_action = verify_action_list $2 in
  Trans_Do(Some(verified_action)) }
| JOURNAL lse    { Trans_Journal(Some(Dlg.Normal_Journal,$2)) }
| SOLVED_JOURNAL lse    { Trans_Journal(Some(Dlg.Solved_Journal,$2)) }
| UNSOLVED_JOURNAL lse    { Trans_Journal(Some(Dlg.Unsolved_Journal,$2)) }
| FLAGS STRING   { Trans_Flags(my_int_of_string $2)}
    ; 

  trans_opt_list :                { [] }
| trans_opt trans_opt_list      { $1 :: $2 } 
    ;

  trans_trigger : STRING { if $1 = "" then None else
  Some(verify_trigger_list $1) } 
    ; 

  optional_string : STRING { $1 }
| { "" }
    ;

  transition :
    IF trans_trigger optional_then trans_opt_list next
    { let result = {
      Dlg.trans_trigger = $2 ;
      Dlg.trans_str = None ;
      Dlg.action = None ;
      Dlg.journal_str = None;
      Dlg.unknown_flags = 0 ;
      Dlg.next = $5 ;
    } in
    List.iter (fun elt -> match elt with
    | Trans_Reply(r) -> (if (result.Dlg.trans_str <> None) && !debug_modder then
        try parse_error "You may only have one REPLY per transition.  Recovering."
        with _ -> ()) ; result.Dlg.trans_str <- r ;
    | Trans_Do(r) ->(if (result.Dlg.action <> None) && !debug_modder then
        try
          parse_error "You may only have one DO per transition.  Recovering."
        with _ -> ()) ;
        let r = try Some(
          value_of_option r ^ value_of_option result.Dlg.action
         ) with _ -> r in
        result.Dlg.action <- r ;
    | Trans_Journal(r) ->
        (if (result.Dlg.journal_str <> None) && !debug_modder then
          try parse_error "You may only have one JOURNAL per transition.  Recovering." with _ -> ()) ;
        result.Dlg.journal_str <- r;
    | Trans_Flags(r) -> result.Dlg.unknown_flags <- r;
	      ) $4 ;
    result
    }
| PLUS optional_string PLUS lse trans_opt_list next
    { let result = {
      Dlg.trans_trigger =
      if $2 = "" then None
      else Some(verify_trigger_list $2) ;
      Dlg.trans_str = Some($4) ;
      Dlg.action = None ;
      Dlg.journal_str = None;
      Dlg.unknown_flags = 0 ;
      Dlg.next = $6 ;
    } in
    List.iter (fun elt -> match elt with
    | Trans_Reply(r) -> (if (result.Dlg.trans_str <> None) && !debug_modder then
        try parse_error "You may only have one REPLY per transition.  Recovering."
        with _ -> ()) ; result.Dlg.trans_str <- r ;
    | Trans_Do(r) ->(if (result.Dlg.action <> None) && !debug_modder then
        try
          parse_error "You may only have one DO per transition.  Recovering."
        with _ -> ()) ;
        let r = try Some(
          value_of_option r ^ value_of_option result.Dlg.action
         ) with _ -> r in
        result.Dlg.action <- r ;
    | Trans_Journal(r) ->
        (if (result.Dlg.journal_str <> None) && !debug_modder then
          try parse_error "You may only have one JOURNAL per transition.  Recovering." with _ -> ()) ;
        result.Dlg.journal_str <- r;
    | Trans_Flags(r) -> result.Dlg.unknown_flags <- r;
	      ) $5 ;
    result
    }
| COPY_TRANS optional_safe STRING STRING
    { 
	{
      Dlg.trans_trigger = None ;
      Dlg.trans_str = None ;
      Dlg.action = None ;
      Dlg.journal_str = None;
      Dlg.unknown_flags = 0 ;
      Dlg.next = Dlg.Copy(String.uppercase $3,$4,$2) ;
    }
    }
| COPY_TRANS_LATE optional_safe STRING STRING
    {
	{
      Dlg.trans_trigger = None ;
      Dlg.trans_str = None ;
      Dlg.action = None ;
      Dlg.journal_str = None;
      Dlg.unknown_flags = 0 ;
      Dlg.next = Dlg.Copy_Late(String.uppercase $3,$4,$2) ;
    }
    }
    ;

  next : GOTO STRING     { Dlg.Symbolic(get_current_unit () ,$2,false) }
| PLUS STRING     { Dlg.Symbolic(get_current_unit () ,$2,false) }
| EXTERN STRING STRING { Dlg.Symbolic(String.uppercase $2,$3,false) }
| EXTERN IF_FILE_EXISTS STRING STRING { Dlg.Symbolic(String.uppercase $3,$4,true) }
| EXIT                 { Dlg.Exit }
    ;

  lse_string:     STRING { $1 }
| STRING STRING_CONCAT lse_string { $1 ^ $3 }
    ;

  lse : lse_string sound_opt
    { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2;
				      lse_female = $1; lse_female_sound = $2;}) in
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l ) then begin
        Dlg.local_string_ht := Some(result :: l )
      end
    | _ -> () ) ;
    result
    }
| lse_string sound_opt lse_string sound_opt
    { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2;
				      lse_female = $3; lse_female_sound = $4; }) in
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l) then
        Dlg.local_string_ht := Some(result :: l)
    | _ -> () ) ;
    result
    }
| STRING_REF { Dlg.TLK_Index($1) }
| TRANS_REF  { Dc.resolve_string_while_loading (Dlg.Trans_String(Dlg.Int $1)) }
| LPAREN AT STRING RPAREN  { Dc.resolve_string_while_loading (Dlg.Trans_String(Dlg.String $3)) }
| FORCED_STRING_REF lse
    { let _ = Dc.set_string_while_loading $1 $2 in Dlg.TLK_Index($1) }
    ;

  sound_opt :             { "" }
| SOUND                 { $1 }
    ;

  tra_file :              { [] }
| TRANS_REF EQUALS lse tra_file { ($1,$3) :: $4 }
    ;

  log_file : installed_mod_list { $1 }
    ;

  installed_mod_list :          { [] }
| STRING STRING_REF STRING_REF installed_mod_list
    { ($1,$2,$3,None) :: $4 }
| STRING STRING_REF STRING_REF STRING installed_mod_list
    { ($1,$2,$3,Some($4)) :: $5 }
    ;

  unsetstr_file :   { [] }
  | STRING_REF STRING SOUND STRING_REF STRING_REF STRING_REF STRING SOUND STRING_REF STRING_REF STRING_REF unsetstr_file
    { ($1,{Tlk.text=$2;Tlk.sound_name=$3;Tlk.flags=$4;Tlk.volume=$5;Tlk.pitch=$6},{Tlk.text=$7;Tlk.sound_name=$8;Tlk.flags=$9;Tlk.volume=$10;Tlk.pitch=$11}) :: $12 }
    ;

  args_file : { [] }
  | STRING args_file { $1 :: $2 }
    ;

  tlk_path_file :
    STRING STRING tlk_path_file { ($1, Some($2)) }
  | STRING tlk_path_file { ($1, None) }
  | { "dialog.tlk", None }
    ;
  %%



