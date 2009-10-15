%{
open Util
open Ids
open Bcs
open Load

(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.bafparser.mly.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)


(*** input handle ***)

(*** Error handling ***)

type bafarg = 
  BA_Symbol of string
| BA_Integer of Int32.t
| BA_String of string
| BA_Bracket of bracket_arg list
| BA_Compound of string * bafarg
| BA_Rect of bafarg * (Int32.t * Int32.t * Int32.t * Int32.t) option
and bracket_arg =
  Bracket_Integer of Int32.t
| Bracket_Symbol of string

let get_next_of_type argl tau = 
  try 
    let res = List.assoc tau !argl in
    argl := List.remove_assoc tau !argl ;
    res
  with e -> begin
          match tau with
          | Arg_Integer -> Act_Integer Int32.zero
          | Arg_String -> Act_String ""
          | Arg_Object -> Act_Object (empty_object_param ())
          | Arg_Point -> Act_Point(Int32.zero,Int32.zero)
          | Arg_Action -> failwith "arg_action"
          end

let get_next_int argl =
  match (get_next_of_type argl Arg_Integer) with
    Act_Integer i -> i
  | _ -> failwith "get_next_int"
let get_next_string argl =
  match (get_next_of_type argl Arg_String) with
    Act_String i -> i
  | _ -> failwith "get_next_string"
let get_next_object argl =
  match (get_next_of_type argl Arg_Object) with
    Act_Object i -> i
  | _ -> failwith "get_next_object"
let get_next_point argl =
  match (get_next_of_type argl Arg_Point) with
    Act_Point i -> i
  | _ -> failwith "get_next_point"

let valid_var_area s =
  let s = String.uppercase s in
  let ans = (s = "GLOBAL" || s = "LOCALS" || s = "KAPUTZ" || s = "MYAREA" || s = "" ||
  ( Str.string_match (Str.regexp "AR[0-9][0-9][0-9][0-9]") s 0) || file_exists_in_game (the_game ()) (s ^ ".ARE")) in
  ans

let fixup_concat argl =
  let s1 = get_next_string argl in
  let s2 = get_next_string argl in
  if not (valid_var_area s2) then Modder.handle_deb "AREA_VARIABLES" (Printf.sprintf "This expression has a typo in the second part: Global(\"%s\",\"%s\")\n" s1 s2);
  argl := (Arg_String,Act_String(s2^s1)) :: !argl

let fixup_concat2 argl =
  let s1 = get_next_string argl in
  let s2 = get_next_string argl in
  if not (valid_var_area s2) then Modder.handle_deb "AREA_VARIABLES" (Printf.sprintf "This expression has a typo in the second part: *Global*(\"%s\",\"%s\")\n" s1 s2);
  let s3 = get_next_string argl in
  let s4 = get_next_string argl in
  if not (valid_var_area s4) then Modder.handle_deb "AREA_VARIABLES" (Printf.sprintf "This expression has a typo in the second part: *Global*(\"%s\",\"%s\")\n" s3 s4);
  argl := (Arg_String,Act_String(s2^s1)) ::
          (Arg_String,Act_String(s4^s3)) :: !argl

let assign_bracket x pos obj ss =
  match ss, pos with
  | Load.IWD2,0 -> obj.o_ea <- x
  | Load.IWD2,1 -> obj.o_general <- x
  | Load.IWD2,2 -> obj.o_race <- x
  | Load.IWD2,3 -> obj.o_subrace <- x
  | Load.IWD2,4 -> obj.o_specific <- x
  | Load.IWD2,5 -> obj.o_gender <- x
  | Load.IWD2,6 -> obj.o_alignment <- x
  | Load.IWD2,7 -> obj.o_class <- x
  | Load.IWD2,8 -> obj.o_iwd2_1 <- x
  | Load.IWD2,9 -> obj.o_iwd2_2 <- x

  | Load.IWD1,0 -> obj.o_ea <- x
  | Load.IWD1,1 -> obj.o_general <- x
  | Load.IWD1,2 -> obj.o_race <- x
  | Load.IWD1,3 -> obj.o_class <- x
  | Load.IWD1,4 -> obj.o_specific <- x
  | Load.IWD1,5 -> obj.o_gender <- x
  | Load.IWD1,6 -> obj.o_alignment <- x

  | Load.BG1,0 -> obj.o_ea <- x
  | Load.BG1,1 -> obj.o_general <- x
  | Load.BG1,2 -> obj.o_race <- x
  | Load.BG1,3 -> obj.o_class <- x
  | Load.BG1,4 -> obj.o_specific <- x
  | Load.BG1,5 -> obj.o_gender <- x
  | Load.BG1,6 -> obj.o_alignment <- x

  | Load.BG2,0 -> obj.o_ea <- x
  | Load.BG2,1 -> obj.o_general <- x
  | Load.BG2,2 -> obj.o_race <- x
  | Load.BG2,3 -> obj.o_class <- x
  | Load.BG2,4 -> obj.o_specific <- x
  | Load.BG2,5 -> obj.o_gender <- x
  | Load.BG2,6 -> obj.o_alignment <- x

  | Load.PST,0 -> obj.o_ea <- x
  | Load.PST,1 -> obj.o_faction <- x
  | Load.PST,2 -> obj.o_team <- x
  | Load.PST,3 -> obj.o_general <- x
  | Load.PST,4 -> obj.o_race <- x
  | Load.PST,5 -> obj.o_class <- x
  | Load.PST,6 -> obj.o_specific <- x
  | Load.PST,7 -> obj.o_gender <- x
  | Load.PST,8 -> obj.o_alignment <- x

  | _ -> failwith (Printf.sprintf "assign %d to unknown bracket position %d" (Int32.to_int x) pos)

let assign_compound x pos obj = match pos with
  | 0 -> obj.o_unknown4 <- x
  | 1 -> obj.o_unknown3 <- x
  | 2 -> obj.o_unknown2 <- x
  | 3 -> obj.o_unknown1 <- x
  | 4 -> obj.o_identifiers <- x
  | _ -> failwith "assign to unknown compound position" 

let rec handle_bracket_list al i obj = 
  let ss = (the_game ()).Load.script_style in
  match al,i with
  | [],_ -> ()
  | (Bracket_Integer(x)::tl),i -> assign_bracket x i obj ss ;
      handle_bracket_list tl (i+1) obj
  | (Bracket_Symbol(s)::tl),i ->
      begin
        try
          let x = int_of_sym (the_game ())
            (ids_file_of_object_position ss i) s in
          assign_bracket x i obj ss ;
          handle_bracket_list tl (i+1) obj
        with Not_found ->
          parse_error (Printf.sprintf
            "unknown object %s specifier [%s]"
            (ids_file_of_object_position ss i) s )
      end

let rec handle_compound_or_bracket obj cpos_ref arg = match arg with
  BA_Rect (arg,rect) ->
			handle_compound_or_bracket obj cpos_ref arg;
			(match rect with
			| None -> ()
			| Some(x) -> obj.o_rect <- x);
  | BA_Bracket l -> 
      handle_bracket_list l 0 obj  
  | BA_Compound (s,next_arg) -> 
      begin 
        let x = int_of_sym (the_game ()) "OBJECT" s in 
        handle_compound_or_bracket obj (cpos_ref) next_arg ;
        assign_compound x !cpos_ref obj ;
        decr cpos_ref 
      end 
  | BA_Symbol s -> 
        obj.o_identifiers <- int_of_sym (the_game ()) "OBJECT" s ;
        decr cpos_ref

  | BA_Integer i -> parse_error (Printf.sprintf "expecting compound object specifier or [bracket.object.specifier], got integer %ld" i)
  | BA_String s -> 
        obj.o_name <- s 
  (* parse_error (Printf.sprintf "expecting compound object specifier or [bracket.object.specifier], got string \"%s\"" s) *)

let rec verify_arg_list name al fl = match (al,fl) with 
    [],[] -> []
  | al,[] ->
  ( try parse_error (Printf.sprintf "Too many arguments to [%s]. Recovering." name) 
    with _ -> [] ) 
  | [],fl -> 
  ( try parse_error (Printf.sprintf "Not enough arguments to [%s]. Recovering." name)
  with _ -> [] )
  | (ah::at),(fh::ft) -> 
      begin 
      let ah' = match ah with
      |	BA_Rect(f,s) -> f
      |	_ -> ah
      in
      let act = (match ah',fh.arg_kind with
      | (BA_Integer i), Arg_Integer -> Act_Integer(i)


      | (BA_Bracket([(Bracket_Symbol s)])), Arg_Integer
      | (BA_String s), Arg_Integer 
      | (BA_Symbol s), Arg_Integer when fh.arg_file <> "" -> 
          begin
            try
              Act_Integer(int_of_sym (the_game ()) fh.arg_file s)
            with _ -> 
              begin
                try
                  Act_Integer(Int32.of_string s)
                with _ -> begin
                (try parse_error (Printf.sprintf 
                  "[%s] argument [%s] not found in [%s.IDS]" name s fh.arg_file)
                 with _ -> ()) ; Act_Integer(Int32.zero) 
                  end
              end 
          end

      | (BA_String s), Arg_Integer ->
            begin
              try Act_Integer(Int32.of_string s)
              with _ -> Act_Integer(Int32.zero) 
            end 

      | (BA_String s), Arg_String -> Act_String(s) 

      | (BA_String s), Arg_Object ->
        let tmp = empty_object_param () in
        tmp.o_name <- s; 
        Act_Object(tmp)

      | (BA_Bracket _), Arg_Object 
      | (BA_Symbol _), Arg_Object 
      | (BA_Compound _), Arg_Object -> 
        let obj = empty_object_param () in 
        handle_compound_or_bracket obj (ref 4) ah ;
        Act_Object(obj) 

      | (BA_Bracket([Bracket_Integer(x);Bracket_Integer(y)])), 
            Arg_Point -> Act_Point(x,y)

      | (BA_Symbol s), Arg_String ->
        begin
          try parse_error (Printf.sprintf "Type mismatch in \"%s\" argument of [%s].\n\tExpecting type \"%s\". Recovering." fh.arg_comment name (print_arg_kind fh.arg_kind))
          with _ -> Act_String(s)
        end

      | _, _ -> 
        parse_error (Printf.sprintf "Type mismatch in \"%s\" argument of [%s].\n\tExpecting type \"%s\"." fh.arg_comment name (print_arg_kind fh.arg_kind))
      ) in
      (fh.arg_kind,act) :: (verify_arg_list name at ft)
      end 

%}

%token LPAREN RPAREN LBRACKET RBRACKET PERIOD COMMA
%token ACTIONOVERRIDE
%token NOT EOF
%token IF THEN END RESPONSE EOF ANYONE
%nonassoc NOT 
%token <string> STRING SYMBOL TILDE_STRING
%token <Int32.t> INTEGER TRANS_REF

%type <Bcs.script> baf_file
%type <Bcs.trigger list> trigger_list
%type <Bcs.action list> action_list
%start baf_file trigger_list action_list

%%

baf_file : ifblock_list         { $1 }
;

ifblock_list :                  { [] }
| ifblock ifblock_list          { $1 :: $2 }
;

ifblock : IF trigger_list THEN response_list END { ($2,$4) } 
; 

trigger_list :                  { [] }
| trigger trigger_list          { $1 :: $2 }
;

trigger : opt_not SYMBOL LPAREN arg_list RPAREN 
  {
    let ids = 
      try ids_of_sym (the_game ()) "TRIGGER" $2 
      with _ -> 
      parse_error (Printf.sprintf "[%s] not found in TRIGGER.IDS" $2)
    in 
    let act_list = ref (verify_arg_list $2 $4 ids.i_args) in 
    let ss = (the_game ()).Load.script_style in 
    (match is_concat_string ss ids with
    | 1 -> fixup_concat act_list 
    | 2 -> fixup_concat2 act_list
    | _ -> () ) ;
    let t1 = get_next_int act_list in
    let t2 = get_next_int act_list in 
    let t3 = get_next_string act_list in
    let t4 = get_next_string act_list in
    let t5 = get_next_object act_list in
    let c = get_next_point act_list in 
    let unknown = get_next_int act_list in 
    { trigger_id = ids.i_num ; negated = $1 ;
      t_1 = t1; t_2 = t2; unknown = unknown ; t_3 = t3; t_4 = t4; t_5 = t5; 
      t_coord = c ; (* FIXME! *) 
    } 
  } 
;

arg_list :  { [] }
| arg comma_before_arg_list { $1 :: $2 }
; 

comma_before_arg_list :   { [] }
| COMMA arg comma_before_arg_list { $2 :: $3 } 
; 

sound_opt :                { "" }
| LBRACKET SYMBOL RBRACKET { $2 } 
; 

rect_opt :																															 { None }
| LBRACKET INTEGER PERIOD INTEGER PERIOD INTEGER PERIOD INTEGER RBRACKET { Some($2,$4,$6,$8) }
;

arg : | SYMBOL opt_arg rect_opt { BA_Rect((match $2 with
                    None -> BA_Symbol($1) 
                  | Some(a) -> BA_Compound($1,a)),$3) }
| INTEGER rect_opt { BA_Rect(BA_Integer($1),$2) }
| STRING rect_opt { BA_Rect(BA_String($1),$2) }
| TILDE_STRING sound_opt 
  { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2; 
                    lse_female = $1; lse_female_sound = $2;}) in 
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l ) then
          Dlg.local_string_ht := Some(result :: l )
    | _ -> () ) ;
    match Dc.resolve_string_while_loading  result with
      Dlg.TLK_Index(i) -> BA_Integer(Int32.of_int i)
    | _ -> 
      if !Dc.doing_traify then BA_Integer(Int32.zero) else 
      parse_error (Printf.sprintf "unable to resolve TLK string ~%s~" $1)
  }
| TILDE_STRING sound_opt TILDE_STRING sound_opt 
  { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2; 
          lse_female = $3; lse_female_sound = $4; }) in 
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l) then
        Dlg.local_string_ht := Some(result :: l)
    | _ -> () ) ;
    match Dc.resolve_string_while_loading result with
      Dlg.TLK_Index(i) -> BA_Integer(Int32.of_int i)
    | _ -> 
      if !Dc.doing_traify then BA_Integer(Int32.zero) else 
      parse_error (Printf.sprintf "unable to resolve TLK string ~%s~" $1)
  }
| TRANS_REF 
  { match Dc.resolve_string_while_loading  (Dlg.Trans_String(Int32.to_int $1))
    with Dlg.TLK_Index(i) -> BA_Integer(Int32.of_int i)
    | _ -> if not !Dc.doing_traify then parse_error (Printf.sprintf "unable to resolve translation string @%ld" $1) else BA_Integer(Int32.of_int 1)
  }
| LBRACKET ba_arg_list RBRACKET rect_opt { BA_Rect(BA_Bracket($2),$4) }
;

opt_arg :       { None }
| LPAREN RPAREN { Some(BA_Symbol("Myself")) } 
| LPAREN arg RPAREN { Some($2) } 
; 

ba_arg_list :   { [] }
| ba_arg period_before_ba_arg_list { $1 :: $2 }
;

period_before_ba_arg_list :   { [] }
| PERIOD ba_arg period_before_ba_arg_list { $2 :: $3 }
; 

ba_arg : INTEGER { Bracket_Integer($1) }
| ANYONE        { Bracket_Integer(Int32.zero) } 
| SYMBOL        { Bracket_Symbol($1) } 
;

opt_not :   { false }
| NOT       { true }
; 

response_list :   { [] }
| response response_list { $1 :: $2 }
;

response : RESPONSE INTEGER action_list { ((Int32.to_int $2),$3) }
;

action_list :     { [] }
| complex_action action_list { $1 :: $2 } 
;

not_actually_optional_rparen : { }
| RPAREN { }
; 

complex_action : action { $1 }
| ACTIONOVERRIDE LPAREN arg COMMA action not_actually_optional_rparen 
  { let base_action = $5 in 
    let act_list = ref (verify_arg_list "ActionOverride" [$3] 
      [ { arg_kind = Arg_Object ; arg_comment = "Actor" ; arg_file = "" } ])
    in 
    let actor = get_next_object act_list in
    base_action.a_1 <- actor ;
    base_action 
  } 
; 

action : SYMBOL LPAREN arg_list RPAREN
  {
    let ids = 
      try ids_of_sym (the_game ()) "ACTION" $1
      with _ -> 
      parse_error (Printf.sprintf "[%s] not found in ACTION.IDS" $1)
    in 
    let act_list = ref (verify_arg_list $1 $3 ids.i_args) in 
    let ss = (the_game ()).Load.script_style in 
    (match is_concat_string ss ids with
    | 1 -> fixup_concat act_list ; 
    | 2 -> fixup_concat2 act_list ; 
    | _ -> () ) ; 
    let a1 = empty_object_param() in 
    let a2 = get_next_object act_list in
    let a3 = get_next_object act_list in
    let a4 = get_next_int act_list in
    let a5 = get_next_point act_list in
    let a6 = get_next_int act_list in
    let a7 = get_next_int act_list in
    let a8 = get_next_string act_list in
    let a9 = get_next_string act_list in
    {
      action_id = ids.i_num ;
      a_1 = a1; a_2 = a2; a_3 = a3; a_4 = a4; a_5 = a5;
      a_6 = a6; a_7 = a7; a_8 = a8; a_9 = a9; 
    } 
  } 
;
