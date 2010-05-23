%{
open Util
open Bcs

(*** input handle ***)

(*** Error handling ***)
let parse_error = Util.parse_error

    %}

  %token SC CR CO TR RS RE AC OB
  %token LPAREN RPAREN LBRACKET RBRACKET PERIOD COMMA
  %token NOT EOF
  %nonassoc NOT
  %token <string> STRING
  %token <Int32.t> INTEGER

  %type <Bcs.script> bcs_file
  %start bcs_file

  %%

bcs_file : SC c_r_list SC { $2 }
| { [] } 
    ;

c_r_list :              { [] } 
| c_r c_r_list          { $1 :: $2 } 
    ;

c_r : CR cond RS resp_list RS CR { ($2,$4) } 
  ; 

cond : CO trig_list CO { $2 }
  ;

trig_list :             { [] }
| trig trig_list        { $1 :: $2 }
    ;

trig_opt_args :         { (0l,0l,0l,"","",(0l,0l)) }
| INTEGER INTEGER INTEGER STRING STRING { ($1,$2,$3,$4,$5,(0l,0l)) } 
| INTEGER INTEGER INTEGER LBRACKET INTEGER COMMA INTEGER RBRACKET STRING STRING
    { ($1, $2, $3, $9, $10, ($5,$7)) } 
    ;       

  trig : TR INTEGER INTEGER trig_opt_args obj_param TR {
  let b,c,d,e,f,(g) = $4 in 
  { trigger_id = $2 ;
    t_1 = $3 ; 
    negated = (b = 1l) ;
    t_2 = c ;
    unknown = d ;
    t_3 = e ;
    t_4 = f ;
    t_5 = $5 ;
    t_coord = g ; 
  } } 
    ;

  resp_list :             { [] }
| resp resp_list        { $1 :: $2 } 
    ;

  resp : RE INTEGER action_list RE { ((Int32.to_int $2),$3) } 
    ; 

  action_list :           { [] }
| action action_list    { $1 :: $2 }
    ;

  optional_string : STRING        { Some($1) }
|                               { None } 
    ;

  not_really_optional_int : INTEGER   { $1 }
|                               { 
  try 
    parse_error "non-optional ACTION parameter omitted, defaulting to 0"
  with _ -> 0l } 

    action : AC INTEGER obj_param obj_param obj_param INTEGER
    INTEGER INTEGER INTEGER not_really_optional_int 
    optional_string optional_string AC {
  { action_id = $2 ;
    a_1 = $3 ;
    a_2 = $4 ;
    a_3 = $5 ;
    a_4 = $6 ;
    a_5 = ($7,$8) ;
    a_6 = $9 ;
    a_7 = $10 ;
    a_8 = (match $11 with Some(s) -> s | None -> $4.o_name) ;
    a_9 = (match $12 with Some(s) -> s | None -> $5.o_name) ;
  } } 
    ; 

  optional_rect :        { empty_rect () } 
| LBRACKET INTEGER PERIOD INTEGER PERIOD INTEGER PERIOD INTEGER RBRACKET 
    { ( $2, $4, $6, $8) } 
    ; 

  int_list :      { [] }
| INTEGER int_list { $1 :: $2 }
    ; 

  obj_param : OB int_list optional_rect STRING int_list OB {
  let a = Array.of_list $2 in 
  let b = Array.of_list $5 in 
  let len = List.length $2 in 
  match len with 
  | 12 -> begin (* BG2 object *) 
      { o_ea = a.(0) ;
	o_faction = 0l ;
	o_team = 0l ; 
	o_general = a.(1) ;
	o_race = a.(2) ;
	o_subrace = 0l ; 
	o_class = a.(3) ;
	o_specific = a.(4) ;
	o_gender = a.(5) ;
	o_alignment = a.(6) ;
	o_identifiers = a.(7);
	o_unknown1 = a.(8) ;
	o_unknown2 = a.(9) ;
	o_unknown3 = a.(10) ;
	o_unknown4 = a.(11) ;
	o_name = $4 ;
	o_rect = $3 ; 
	o_iwd2_1 = 0l;
	o_iwd2_2 = 0l;
      }
  end 
  | 13 (* IWD2 object *) -> 
      { o_ea = a.(0) ;
	o_faction = 0l ;
	o_team = 0l ; 
	o_general = a.(1) ;
	o_race = a.(2) ;
	o_class = a.(3) ;
	o_specific = a.(4) ;
	o_gender = a.(5) ;
	o_alignment = a.(6) ;
	o_subrace = (a.(7));
	o_identifiers = a.(8);
	o_unknown1 = a.(9) ;
	o_unknown2 = a.(10) ;
	o_unknown3 = a.(11) ;
	o_unknown4 = a.(12) ;
	o_name = $4 ;
	o_rect = $3 ;
	o_iwd2_1 = b.(0) ;
	o_iwd2_2 = b.(1) ;
      }
  | 14 -> (* PST *)
      { o_ea = a.(0) ;
	o_faction = (a.(1)) ;
	o_team = (a.(2)) ; 
	o_general = a.(3) ;
	o_race = a.(4) ;
	o_subrace = 0l ; 
	o_class = a.(5) ;
	o_specific = a.(6) ;
	o_gender = a.(7) ;
	o_alignment = a.(8) ;
	o_identifiers = a.(9);
	o_unknown1 = a.(10) ;
	o_unknown2 = a.(11) ;
	o_unknown3 = a.(12) ;
	o_unknown4 = a.(13) ;
	o_name = $4 ;
	o_rect = $3 ; 
	o_iwd2_1 = 0l ;
	o_iwd2_2 = 0l ;
      }  
  | x -> parse_error (Printf.sprintf "OB list has %d entries" x)
} 

