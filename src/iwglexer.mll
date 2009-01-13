{
open Util
open Iwgparser

(*
** Keyword hashtable
*)

let lexicon = Hashtbl.create 211
let _ = List.iter 
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [ ("PERIOD" , PERIOD )
    ]
} 

let blank = [' ' '\012' '\r']

rule initial = parse 	
  "/*"  { adj lexbuf ; let _ = comment lexbuf in initial lexbuf}
| "//"  { adj lexbuf ; endline lexbuf }
| blank	{ adj lexbuf ; initial lexbuf}
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '('   { adj lexbuf ; LPAREN } 
| ')'   { adj lexbuf ; RPAREN } 
| '!'   { adj lexbuf ; NOT } 
| '#'   { adj lexbuf ; HASH } 
| ','   { adj lexbuf ; COMMA } 
| '.'   { adj lexbuf ; PERIOD } 
| '['   { adj lexbuf ; LBRACKET } 
| ']'   { adj lexbuf ; RBRACKET } 
| '{'   { adj lexbuf ; LBRACE } 
| '}'   { adj lexbuf ; RBRACE } 
| eof   { EOF }
| '~'[^'~']*'~' 
| '%'[^'%']*'%' { str_adj lexbuf ; TILDE_STRING(strip (Lexing.lexeme lexbuf))}
and comment = parse 	
      "*/"	{ adj lexbuf ; () }
|     '\n'      { newline (); comment lexbuf }
|     "/*"      { adj lexbuf ; let _ = comment lexbuf in comment lexbuf } 
|     eof       { lex_error "unterminated comment" } 
|     _ 	{ adj lexbuf ; comment lexbuf }
and endline = parse 
        '\n' 			{ newline (); initial lexbuf}
|	_			{ adj lexbuf ; endline lexbuf}
|       eof                     { EOF }
