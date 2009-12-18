%{
open Util

(*** input handle ***)

(*** Error handling ***)

  %}

  %token LPAREN RPAREN LBRACKET RBRACKET PERIOD COMMA
  %token LBRACE RBRACE
  %token HASH 
  %token NOT EOF
  %nonassoc NOT 
  %token <string> TILDE_STRING
  %token <Int32.t> INTEGER 

  %type <Iwgrule.rule list> iwg_rule_file
  %start iwg_rule_file

  %%

iwg_rule_file :                 { [] }
| iwg_rule iwg_rule_file        { $1 :: $2 }         
    ; 

iwg_rule : 
| TILDE_STRING LBRACKET ts_list RBRACKET { Iwgrule.Replace($1,$3) }
| TILDE_STRING LBRACE ts_list RBRACE { Iwgrule.ReplaceWithConverted($1,$3) }
    ; 

  ts_list :               { [] }
| TILDE_STRING ts_list  { $1 :: $2 }
    ; 
