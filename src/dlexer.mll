{

open Util
open Dparser

(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.dlexer.mll.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

(*
** Keyword hashtable
*)

let string_of lb = (Lexing.lexeme lb)

let lexicon = Hashtbl.create 211
let _ = List.iter
    (fun (key, token) -> Hashtbl.add lexicon key token)
    [
      ("ADD_STATE_TRIGGER", ADD_STATE_TRIGGER) ;
      ("ADD_TRANS_ACTION", ADD_TRANS_ACTION) ;
      ("ADD_TRANS_TRIGGER", ADD_TRANS_TRIGGER) ;
      ("ALTER_TRANS", ALTER_TRANS) ;
      ("APPEND", APPEND);
      ("APPENDI", APPENDI);
      ("APPEND_EARLY", APPEND_EARLY);
      ("A_S_T", ADD_STATE_TRIGGER) ;
      ("A_T_T", ADD_TRANS_TRIGGER) ;
      ("BEGIN", BEGIN) ;
      ("BRANCH", BRANCH) ;
      ("CHAIN", CHAIN3);
      ("CHAIN2", CHAIN2);
      ("CHAIN3", CHAIN3);
      ("COPY_TRANS", COPY_TRANS);
      ("COPY_TRANS_LATE", COPY_TRANS_LATE);
      ("DO", DO) ;
      ("END", END) ;
      ("EXTEND",EXTEND_BOTTOM) ;
      ("EXTEND_BOTTOM", EXTEND_BOTTOM) ;
      ("EXTEND_BOTTOM_REGEXP", EXTEND_BOTTOM_REGEXP);
      ("EXTEND_TOP", EXTEND_TOP) ;
      ("EXTEND_TOP_REGEXP", EXTEND_TOP_REGEXP);
      ("EXTERN", EXTERN) ;
      ("EXIT", EXIT) ;
      ("FLAGS", FLAGS);
      ("GOTO", GOTO) ;
      ("IF", IF) ;
      ("IF_FILE_EXISTS", IF_FILE_EXISTS) ;
      ("INTERJECT", INTERJECT);
      ("INTERJECT_COPY_TRANS", INTERJECT_COPY_TRANS);
      ("INTERJECT_COPY_TRANS2", INTERJECT_COPY_TRANS2);
      ("INTERJECT_COPY_TRANS3", INTERJECT_COPY_TRANS3);
      ("INTERJECT_COPY_TRANS4", INTERJECT_COPY_TRANS4);
      ("I_C_T", INTERJECT_COPY_TRANS);
      ("I_C_T2", INTERJECT_COPY_TRANS2);
      ("I_C_T3", INTERJECT_COPY_TRANS3);
      ("I_C_T4", INTERJECT_COPY_TRANS4);
      ("JOURNAL", JOURNAL) ;
      ("REPLACE", REPLACE) ;
      ("REPLACE_ACTION_TEXT", REPLACE_ACTION_TEXT) ;
      ("REPLACE_ACTION_TEXT_PROCESS", REPLACE_ACTION_TEXT_PROCESS) ;
      ("REPLACE_ACTION_TEXT_PROCESS_REGEXP", REPLACE_ACTION_TEXT_PROCESS_REGEXP) ;
      ("REPLACE_ACTION_TEXT_REGEXP", REPLACE_ACTION_TEXT_REGEXP) ;
      ("REPLACE_SAY", REPLACE_SAY) ;
      ("REPLACE_STATE_TRIGGER", REPLACE_STATE_TRIGGER) ;
      ("REPLACE_TRIGGER_TEXT", REPLACE_TRIGGER_TEXT) ;
      ("REPLACE_TRIGGER_TEXT_REGEXP", REPLACE_TRIGGER_TEXT_REGEXP) ;
      ("REPLACE_TRANS_ACTION", REPLACE_TRANS_ACTION) ;
      ("REPLACE_TRANS_TRIGGER", REPLACE_TRANS_TRIGGER) ;
      ("REPLY", REPLY) ;
      ("R_A_T", REPLACE_ACTION_TEXT) ;
      ("R_A_T_P_R", REPLACE_ACTION_TEXT_PROCESS_REGEXP) ;
      ("R_S_T", REPLACE_STATE_TRIGGER) ;
      ("R_T_T", REPLACE_TRIGGER_TEXT) ;
      ("SAY", SAY) ;
      ("SET_WEIGHT", SET_WEIGHT) ;
      ("SOLVED_JOURNAL", SOLVED_JOURNAL) ;
      ("THEN", THEN) ;
      ("UNSOLVED_JOURNAL", UNSOLVED_JOURNAL) ;
      ("WEIGHT", WEIGHT) ;
    ]

(*
** Buffer processor
*)
 



} 

let decdigit = ['0'-'9']
let letter = ['a' - 'z' 'A'-'Z']

let blank = [' ' '\012' '\r']

rule initial = parse
  "/*"  { adj lexbuf ; let _ = comment lexbuf in initial lexbuf}
| "//"  { adj lexbuf ; endline lexbuf }
| blank	{ adj lexbuf ; initial lexbuf}
| '\t'  { tab (); initial lexbuf }
| '\n'  { newline (); initial lexbuf }
| '='   { adj lexbuf ; EQUALS }
| '+'   { adj lexbuf ; PLUS }
| '^'   { adj lexbuf ; STRING_CONCAT }

| "=="  { adj lexbuf ; EQUALSEQUALS }
| "["[^']']*"]" { str_adj lexbuf ; SOUND(strip (string_of lexbuf)) }
| "~"[^'~']*"~"
| '"'[^'"']*'"'
| '%'[^'%']*'%'  { str_adj lexbuf ; STRING(strip (string_of lexbuf)) }
| ['0'-'9''A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''#''_''-''.']* {
    adj lexbuf ; try Hashtbl.find lexicon (string_of lexbuf)
    with _ -> STRING(string_of lexbuf) }
| '#'['-']?['0'-'9']+ { adj lexbuf ;
      let str = string_of lexbuf in
      let str = String.sub str 1 ((String.length str) - 1) in
      STRING_REF((int_of_string str)) }
| '!'['-']?['0'-'9']+ { adj lexbuf ;
      let str = string_of lexbuf in
      let str = String.sub str 1 ((String.length str) - 1) in
      FORCED_STRING_REF((int_of_string str)) }
| '@'['-']?['0'-'9']+ { adj lexbuf ;
      let str = string_of lexbuf in
      let str = String.sub str 1 ((String.length str) - 1) in
      TRANS_REF((int_of_string str)) }
| "~~~~~" { adj lexbuf ; let buf = Buffer.create 255 in widestring buf lexbuf }
| eof   { EOF }
| _	{ lex_error (Printf.sprintf "invalid character [%s]" (string_of
lexbuf)) }
and comment = parse
      "*/"	{ adj lexbuf ; () }
|     '\n'      { newline (); comment lexbuf }
|     "/*"      { adj lexbuf ; let _ = comment lexbuf in comment lexbuf }
|     eof       { lex_error "unterminated comment" }
|     _ 	{ adj lexbuf ; comment lexbuf }
and widestring buf = parse
|     "~~~~~"    { adj lexbuf ; STRING(Buffer.contents buf) }
|     eof        { lex_error "unterminated ~~~~~ string" }
|     '\n'       { newline (); Buffer.add_char buf '\n';widestring buf lexbuf}
|     _          { adj lexbuf ; let str = string_of lexbuf in
                   Buffer.add_string buf str ; widestring buf lexbuf }
and endline = parse
        '\n' 			{ newline (); initial lexbuf}
|	_			{ adj lexbuf ; endline lexbuf}
|       eof                     { EOF }
