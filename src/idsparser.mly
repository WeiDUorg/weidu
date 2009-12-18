%{
open Util
open Ids

(*** input handle ***)

(*** Error handling ***)
let parse_error = Util.parse_error

    %}

  %token LPAREN RPAREN COLON STAR COMMA EOF
  %nonassoc NOT
  %token <string> STRING
  %token <Int32.t> INTEGER

  %type <Ids.ids list> ids_file
  %start ids_file

  %%

ids_file : ids_list { $1 } 
| STRING ids_list { $2 } 
    ;

ids_list :  { [] }
| ids ids_list { $1 :: $2 }
    ;

ids : INTEGER concat_string_list opt_args  { { i_num = $1; i_name = $2; i_args = $3; }}
  ;

opt_args :  { [] } 
| LPAREN arg_list RPAREN { $2 } 
    ;

arg_list :  { [] }
| arg arg_list { $1 :: $2 }
    ;

arg : str_or_int COLON concat_string_list 
  opt_star concat_string_list
  opt_comma
  { { arg_kind = (match $1 with
  | "O" -> Arg_Object
  | "A" -> Arg_Action
  | "P" -> Arg_Point
  | "S" -> Arg_String
  | "I" -> Arg_Integer
  | _ -> parse_error (Printf.sprintf "unknown argument type [%s]" $1) );
      arg_comment = $3 ;
      arg_file = $5 ;
    } } 
  ; 

str_or_int : STRING     { $1 }
| INTEGER               { "O" }
    ; 

opt_comma :  { () } 
| COMMA { () } 
    ; 

opt_star :  { () } 
| STAR { () } 
    ; 

concat_string_list :  { "" }
| STRING concat_string_list { $1 ^ $2 } 
    ; 

