{
 open Util
open Idsparser

(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.idlexer2.mll.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(* this one is very forgiving about spaces, commas, that kind of thing *)
let space_regexp = Str.regexp "[ ]+$"

}

let blank = [' ' '\012' '\r']

rule initial = parse
| "/*"  { adj lexbuf ; let _ = comment lexbuf in initial lexbuf}
| "//"  { adj lexbuf ; endline lexbuf }
| blank { adj lexbuf ; initial lexbuf}
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '('   { adj lexbuf ; LPAREN }
| ')'   { adj lexbuf ; RPAREN }
| '*'   { adj lexbuf ; STAR }
| ','   { adj lexbuf ; COMMA }
| ':'   { adj lexbuf ; COLON }
| ['1'-'9']*['_''A'-'Z''a'-'z''#''(''*']['_' '0'-'9' 'A'-'Z' 'a'-'z' '#' '-' '.' '!' ' ' ',' '(' ')' '/' '\'''?' '+']* {
  adj lexbuf ;
  let big_str = Lexing.lexeme lexbuf in
  let str = Str.global_replace space_regexp "" big_str in
  STRING((str)) }
| ['-']?['0'-'9']+ { adj lexbuf ;
                     let str = Lexing.lexeme lexbuf in
                     INTEGER((Int32.of_string str)) }
| "0x"['0'-'9''a'-'f''A'-'F']+ { adj lexbuf ;
                                 let str = Lexing.lexeme lexbuf in
                                 INTEGER((Int32.of_string str)) }
| eof   { EOF }
| _     { lex_error (Printf.sprintf "invalid character [%s]" (Lexing.lexeme
                                                                lexbuf)) }
and comment = parse
| "*/"      { adj lexbuf ; () }
| '\n'      { newline (); comment lexbuf }
| "/*"      { adj lexbuf ; let _ = comment lexbuf in comment lexbuf }
| eof       { lex_error "unterminated comment" }
| _         { adj lexbuf ; comment lexbuf }
and endline = parse
| '\n'      { newline (); initial lexbuf}
| _         { adj lexbuf ; endline lexbuf}
| eof       { EOF }
