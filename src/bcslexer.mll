{
open Util
open Bcsparser

(*
** Keyword hashtable
*)

let lexicon = Hashtbl.create 211
let _ = List.iter 
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [ 
      ("SC", SC) ;
      ("CR", CR) ;
      ("CO", CO) ;
      ("TR", TR) ;
      ("RS", RS) ;
      ("RE", RE) ;
      ("AC", AC) ;
      ("OB", OB) ;
    ] 
} 

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let letter = ['a' - 'z' 'A'-'Z']

let blank = [' ' '\012' '\r']

rule initial = parse 	
  "/*"  { adj lexbuf ; let _ = comment lexbuf in initial lexbuf}
| "//"  { adj lexbuf ; endline lexbuf }
| blank	{ adj lexbuf ; initial lexbuf}
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '('   { adj lexbuf ; LPAREN } 
| ')'   { adj lexbuf ; RPAREN } 
| '['   { adj lexbuf ; LBRACKET } 
| ']'   { adj lexbuf ; RBRACKET } 
| '!'   { adj lexbuf ; NOT } 
| '.'   { adj lexbuf ; PERIOD } 
| ','   { adj lexbuf ; COMMA } 
| "~"[^'~']*"~"  
| "%"[^'%']*"%"  
| '"'[^'"']*'"'  { str_adj lexbuf ; STRING(strip (Lexing.lexeme lexbuf)) } 
| "~~~~~" { adj lexbuf ; let buf = Buffer.create 255 in widestring buf lexbuf } 
| ['A'-'Z''a'-'z']['0'-'9''A'-'Z''a'-'z''#''_''-''.']* { 
    adj lexbuf ; try Hashtbl.find lexicon (Lexing.lexeme lexbuf) 
    with _ -> lex_error (Printf.sprintf "invalid keyword [%s]" 
    (Lexing.lexeme lexbuf)) } 
| ['-']?['0'-'9']+ { adj lexbuf ;
      let str = Lexing.lexeme lexbuf in
      INTEGER((Int32.of_string str)) }
| eof   { EOF }
| _	{ lex_error (Printf.sprintf "invalid character [%s]" (Lexing.lexeme
lexbuf)) }
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
and widestring buf = parse
|     "~~~~~"    { adj lexbuf ; STRING(Buffer.contents buf) }
|     eof        { lex_error "unterminated ~~~~~ string" }
|     '\n'       { newline (); Buffer.add_char buf '\n';widestring buf lexbuf}
|     _          { adj lexbuf ; let str = Lexing.lexeme lexbuf in
                   Buffer.add_string buf str ; widestring buf  lexbuf } 
