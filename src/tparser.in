open Lexerint      (* tLexerInterface *)
open Lrparse       (* parse *)
open Glr           (* tGLR, makeGLR, glrParse *)
open Useract       (* tSemanticValue *)
open Parsetables   (* tParseTables *)
open Useract       (* tUserActions *)
open Tlexer
open Trealparser
open Util

class tLexer =
  object (self)
    inherit tLexerInterface

    val mutable lexbuf:Lexing.lexbuf =
      (Lexing.from_string "") ;

    method set_lex l =
      begin
        lexbuf <- l
      end

    method set_tok l =
      begin
        tokType <- l
      end

    method getToken() : unit =
      begin
        (* read from stdin *)
        let t = (initial lexbuf) in
        tokType <- tokenKind t;
(*      token (string) SOUND;
        token (string) STRING;
        token (string * string) INLINED_FILE;
        token (int) STRING_REF;
        token (int) TRANS_REF;
        token (int) FORCED_STRING_REF; *)

        (* break the tToken apart into a kind and an sval; perhaps
         * this belongs in lexer.mll too? *)
        match t with
        | SOUND(i) -> sval <- Obj.repr i
        | STRING(i) -> sval <- Obj.repr i
        | INLINED_FILE(i) -> sval <- Obj.repr i
        | STRING_REF(i) -> sval <- Obj.repr i
        | TRANS_REF(i) -> sval <- Obj.repr i
        | FORCED_STRING_REF(i) -> sval <- Obj.repr i
        | _ -> sval <- Obj.repr ()
      end

    method tokenDesc() : string =
      begin
        let kindDesc:string = (self#tokenKindDesc tokType) in
        if (tokType = 1) then (
          kindDesc ^ "(" ^ (string_of_int (self#getIntSval())) ^ ")"
         )
        else
          kindDesc
      end

    method tokenKindDesc (kind:int) : string =
      begin
        (tokenKindDesc kind)
      end
  end

let realTables = ref None

let parse start lexbuf =
  let decomp_tables = match !realTables with
  | Some x -> x
  | None ->
      let ans = build_table arithParseTables in
      realTables := Some ans;
      ans
  in
  let (tables:tParseTables), (actions:tUserActions) =
    (decomp_tables, arithUserActions)
  in
  let lex:tLexer = (new tLexer ) in
  lex#set_tok (tokenKind start);
  lex#set_lex lexbuf;
  let lex:tLexerInterface = (lex :> tLexerInterface) in
  let (sval:tSemanticValue) =
    (* use GLR *)
    let glr:tGLR = (makeGLR tables actions) in
    let treeTop: tSemanticValue ref = ref cNULL_SVAL in
    let parse_result = (glrParse glr lex) treeTop in
    if not parse_result then parse_error "GLR parse error" ;
    !treeTop
  in
  let (cu:Tp.wrapper) =
    ((Obj.obj sval) : Tp.wrapper) in
  cu


let parse_tp2_file filename =
  let tp_file lexbuf =
    let cu = parse START_FROM_TP lexbuf in
    match cu with
    | Tp.Start_From_Tp(a) -> a
    | _ -> failwith "Elkhound internal"
  in
  try
    parse_file false filename "Parsing TP2 files"
      (Toldparser.tp_file Toldlexer.initial)
  with _ ->
    parse_file true filename "Parsing TP2 files" tp_file

let parse_tpa_file filename =
  let tpa_file lexbuf =
    let cu = parse START_FROM_TPA lexbuf in
    match cu with
    | Tp.Start_From_Tpa(a) -> a
    | _ -> failwith "Elkhound internal"
  in
  try
    parse_file false filename "Parsing TPA files"
      (Toldparser.tph_file Toldlexer.initial)
  with _ ->
    parse_file true filename "Parsing TPA files" tpa_file

let parse_tpp_file filename =
  let tpp_file lexbuf =
    let cu = parse START_FROM_TPP lexbuf in
    match cu with
    | Tp.Start_From_Tpp(a) -> a
    | _ -> failwith "Elkhound internal"
  in
  try
    parse_file false filename "Parsing TPP files"
      (Toldparser.tpp_file Toldlexer.initial)
  with _ ->
    parse_file true filename "Parsing TPP files" tpp_file
