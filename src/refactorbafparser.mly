%{
open Util
open Refactorbaf

let or_counter = ref 0

let build_or = ref []

let handle_or is_not s1 s2 =
  let s,trig_ov = if String.lowercase s1 = "triggeroverride" then begin
    let s2 = String.sub s2 1 (String.length s2 - 2) in
    let i = String.index s2 ',' in
    Str.string_after s2 (i + 1),Some (Str.string_before s2 i)
  end else s1 ^ s2,None
  in
  let res = if is_not then NotTrigger(s,trig_ov) else Trigger(s,trig_ov) in
  if !or_counter = 0 then Some res else begin
    build_or := res :: !build_or ;
    decr or_counter ;
    if !or_counter = 0 then Some (Or (List.rev !build_or)) else None
  end
%}

  %token NOT EOF
  %token IF THEN EOF OR
  %nonassoc NOT
  %token <string> STRING

  %type <string> baf_file multistring
  %type <Refactorbaf.trigger list> trigger_list
  %start baf_file trigger_list

  %%

baf_file  : { "" }
| ifblock baf_file { $1 ^ " " ^ $2 }

ifblock :
| IF trigger_list THEN multistring { "IF " ^ Refactorbaf.print_tl
                                               (Refactorbaf.refactor ($2)) ^
                                     " THEN " ^ $4 }

trigger_list :
| trigger_list_i { if !or_counter != 0 then
    (or_counter := 0; parse_error "Unfinished OR()"); List.rev $1 }

trigger_list_i : { [] }
| trigger_list_i not STRING STRING { match handle_or $2 $3 $4 with
  | Some i -> i :: $1 | None -> $1 }
| trigger_list_i OR STRING { if !or_counter > 0 then
    parse_error "Can't nest OR()" ; or_counter := int_of_string (strip $3) ;
                             build_or := []; $1 }

multistring : { "" }
| STRING multistring { $1^ " " ^ $2 }

not :
| { false }
| NOT { true }
