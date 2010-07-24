{
 open Util
open Refactorbafparser

let string_of lb = (Lexing.lexeme lb)

let lexicon = Hashtbl.create 211
let _ = List.iter 
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [ 
      ("IF", IF) ;
      ("THEN", THEN) ;
      ("OR", OR) ;
    ]
}

let blank = [' ' '\012' '\r']

    rule initial = parse
| [^'('')''0'-'9''A'-'Z''a'-'z''#''_''-''!']? '!' { adj lexbuf; NOT }
| blank { adj lexbuf; initial lexbuf }
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '('   { adj lexbuf ; let buf = Buffer.create 255 in
  lparen buf lexbuf }
| eof   { EOF }
| [^ ' ' '\012' '\r' '\t' '\n' '!' '(']* { adj lexbuf; let str = string_of lexbuf in
			if Hashtbl.mem lexicon str then Hashtbl.find lexicon str else STRING(str) }

and lparen buf = parse
|     ')'        { adj lexbuf ; let res = "(" ^ Buffer.contents buf ^ ")" in STRING(res) }
|     '('        { adj lexbuf ;
					let str = let buf = Buffer.create 255 in match (lparen buf lexbuf) with 
						| STRING s -> s
						| _ -> lex_error "Wrong state"
					in
					Buffer.add_string buf str ; 
					lparen buf lexbuf }
|     '\n'       { newline (); Buffer.add_char buf '\n'; lparen buf lexbuf}
|     _          { adj lexbuf ; let str = string_of lexbuf in
  Buffer.add_string buf str ; lparen buf lexbuf } 
