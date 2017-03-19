{
open Util
open Refactordparser

let string_of lb = (Lexing.lexeme lb)

let lexicon = Hashtbl.create 211
let _ = List.iter
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [("IF", IF) ;
     ("WEIGHT", WEIGHT)]
}

let blank = [' ' '\012' '\r']

    rule initial = parse
| "/*" { adj lexbuf ; let _ = comment lexbuf in initial lexbuf }
| "//" { adj lexbuf ; endline lexbuf }
| blank { adj lexbuf ; initial lexbuf }
| '\t' { tab () ; initial lexbuf }
| '\n' { newline () ; initial lexbuf }
| "~"[^'~']*"~"
| '"'[^'"']*'"'
| '%'[^'%']*'%' { str_adj lexbuf ; DELIM_STRING(strip (string_of lexbuf)) }
| [^ ' ' '\012' '\r' '\t' '\n' '!' '(' '"' '%' '~']*
                                       { adj lexbuf ; try
                                         Hashtbl.find lexicon (string_of lexbuf)
                                       with _ -> STRING(string_of lexbuf) }
| "~~~~~" { adj lexbuf ; let buf = Buffer.create 255 in widestring buf lexbuf }
| eof { EOF }

and comment = parse
| "*/" { adj lexbuf ; () }
| '\n' { newline () ; comment lexbuf }
| "/*" { adj lexbuf ; let _ = comment lexbuf in comment lexbuf }
| eof { lex_error "unterminated comment" }
| _ { adj lexbuf ; comment lexbuf }
and widestring buf = parse
| "~~~~~" { adj lexbuf ; DELIM_STRING(Buffer.contents buf) }
| eof { lex_error "unterminated ~~~~~ string" }
| '\n' { newline () ; Buffer.add_char buf '\n' ; widestring buf lexbuf }
| _ { adj lexbuf ; let str = string_of lexbuf in
  Buffer.add_string buf str ; widestring buf lexbuf }
and endline = parse
| '\n' { newline () ; initial lexbuf }
| _ { adj lexbuf ; endline lexbuf }
| eof { EOF }
