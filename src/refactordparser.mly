%{
open Util

let refactor s = Tlk.weidu_quotify (Refactorbaf.print_tl (Refactorbaf.refactor (!Refactorbaf.parse_triggers s)))

%}

  %token IF
  %token WEIGHT

  %token EOF

  %token <string> STRING DELIM_STRING

  /* Non-terminals informations */
%start d_file

  %type <string> d_file

  %%

d_file :  { "" }
|	STRING d_file { $1 ^ " " ^ $2 }
|	DELIM_STRING d_file { Tlk.weidu_quotify $1 ^ " " ^ $2 }
| 	IF DELIM_STRING d_file { "IF " ^ refactor $2 ^ " " ^ $3 }
| 	IF WEIGHT STRING DELIM_STRING d_file { "IF WEIGHT " ^ $3 ^ " " ^ refactor $4 ^ " " ^ $5 }
%%



