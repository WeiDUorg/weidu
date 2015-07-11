{
 open Util
open Bafparser

  (* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
     starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.baflexer.mll.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(*
 ** Keyword hashtable
 *)

let string_of lb = (Lexing.lexeme lb)

let lexicon = Hashtbl.create 211
let _ = List.iter
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [
     ("AT",AT);
     ("IF", IF) ;
     ("THEN", THEN) ;
     ("END", END) ;
     ("RESPONSE", RESPONSE) ;
     ("ANYONE", ANYONE) ;
     ("ACTIONOVERRIDE", ACTIONOVERRIDE) ;
     ("TRIGGEROVERRIDE", TRIGGEROVERRIDE) ;
   ]
}

let blank = [' ' '\012' '\r']

    rule initial = parse
    "/*"  { adj lexbuf ; let _ = comment lexbuf in initial lexbuf}
| "//"  { adj lexbuf ; endline lexbuf }
| [^'('')''0'-'9''A'-'Z''a'-'z''#''_''-''!']? '!' { adj lexbuf; NOT }
| blank { adj lexbuf ; initial lexbuf }
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '('   { adj lexbuf ; LPAREN }
| ')'   { adj lexbuf ; RPAREN }
| '#'[' ''\t']*['0'-'9']+   { adj lexbuf ;
                              let str = string_of lexbuf in
                              let i = ref 1 in
                              while (String.sub str !i 1) = " " || (String.sub str !i 1) = "\t" do
                                i := !i + 1;
                              done ;
                              let str = String.sub str !i ((String.length str) - !i) in
                              ( INTEGER(Int32.of_string str)) }
| ','   { adj lexbuf ; COMMA }
| '.'   { adj lexbuf ; PERIOD }
| '['   { adj lexbuf ; LBRACKET }
| ']'   { adj lexbuf ; RBRACKET }
| eof   { EOF }
| '~'[^'~']*'~'
| '%'[^'%']*'%' { str_adj lexbuf ; TILDE_STRING(strip (string_of lexbuf))}
| '"'[^'"']*'"'  { str_adj lexbuf ; STRING(strip (string_of lexbuf)) }
| "~~~~~" { adj lexbuf ; let buf = Buffer.create 255 in
  widestring buf lexbuf }
| "\"\"\"\"\"" {adj lexbuf ; let buf = Buffer.create 255 in
  wide_symbol buf lexbuf }
| '@'['0'-'9']+ { adj lexbuf ;
                  let str = string_of lexbuf in
                  let str = String.sub str 1 ((String.length str) - 1) in
                  TRANS_REF((Int32.of_string str)) }
| ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''#''_''-''!']*
| ['#']['A'-'Z''a'-'z''_''!''-']['0'-'9''A'-'Z''a'-'z''#''_''-''!']* {
  adj lexbuf ;
  try Hashtbl.find lexicon (string_of lexbuf)
  with _ -> begin
    try Hashtbl.find lexicon (String.uppercase (string_of lexbuf))
    with _ -> SYMBOL(string_of lexbuf)
  end }
| ['-']?("0x")?['0'-'9']+ { adj lexbuf ;
                            let str = string_of lexbuf in
                            INTEGER((Int32.of_string str)) }
and comment = parse
    "*/"        { adj lexbuf ; () }
|     '\n'      { newline (); comment lexbuf }
|     "/*"      { adj lexbuf ; let _ = comment lexbuf in comment lexbuf }
|     eof       { lex_error "unterminated comment" }
|     _         { adj lexbuf ; comment lexbuf }
and endline = parse
    '\n'                        { newline (); initial lexbuf}
|       _                       { adj lexbuf ; endline lexbuf}
|       eof                     { EOF }
and widestring buf = parse
|     "~~~~~"    { adj lexbuf ; STRING(Buffer.contents buf) }
|     eof        { lex_error "unterminated ~~~~~ string" }
|     '\n'       { newline (); Buffer.add_char buf '\n';widestring buf lexbuf}
|     _          { adj lexbuf ; let str = string_of lexbuf in
  Buffer.add_string buf str ; widestring buf lexbuf }
and wide_symbol buf = parse
|     "\"\"\"\"\""    { adj lexbuf ; SYMBOL(Buffer.contents buf) }
|     eof        { lex_error "unterminated ~~~~~ string" }
|     '\n'       { newline (); Buffer.add_char buf '\n';wide_symbol buf lexbuf}
|     _          { adj lexbuf ; let str = string_of lexbuf in
  Buffer.add_string buf str ; wide_symbol buf lexbuf }
