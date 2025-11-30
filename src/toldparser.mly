%{

open Util
open Load

(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.dparser.mly.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)
  %}

  %token QUESTION COLON SEMICOLON
  %token ACTION_BASH_FOR
  %token ACTION_DEFINE_ARRAY
  %token ACTION_FOR_EACH
  %token ACTION_PHP_EACH
  %token ACTION_IF
  %token ACTION_READLN
  %token AS
  %token PATCH_IF
  %token ADD_CRE_ITEM
  %token ADD_KIT
  %token ADD_KNOWN_SPELL
  %token ADD_MEMORIZED_SPELL
  %token ADD_MAP_NOTE
  %token ADD_MUSIC
  %token ADD_PROJECTILE
  %token ADD_SPELL
  %token ADD_STORE_ITEM
  %token AFTER
  %token ALLOW_MISSING
  %token ALWAYS
  %token AND OR NOT ELSE LPAREN RPAREN
  %token APPEND
  %token APPEND_FILE
  %token APPEND_FILE_EVALUATE
  %token APPEND_OUTER
  %token APPEND_COL
  %token ASK_EVERY_COMPONENT
  %token NO_IF_EVAL_BUG
  %token AT_EXIT
  %token AT_INTERACTIVE_EXIT
  %token AT_INTERACTIVE_NOW
  %token AT_INTERACTIVE_UNINSTALL
  %token AT_NOW
  %token BIFF
  %token SET_2DA_ENTRY
  %token COUNT_2DA_ROWS
  %token COUNT_2DA_COLS
  %token SET_2DA_ENTRY_LATER
  %token SET_2DA_ENTRIES_NOW
  %token READ_2DA_ENTRIES_NOW
  %token READ_2DA_ENTRY
  %token READ_2DA_ENTRY_FORMER
  %token AT_UNINSTALL
  %token AUTHOR
  %token AUTO_TRA
  %token BACKUP
  %token BEFORE
  %token BEGIN
  %token BUT_ONLY_IF_IT_CHANGES
  %token CASE_INSENSITIVE
  %token CASE_SENSITIVE
  %token CLEAR_IDS_MAP
  %token CLEAR_MEMORY
  %token COMPILE
  %token COMPILE_BAF_TO_BCS
  %token COMPILE_D_TO_DLG
  %token COPY_ALL_GAM_FILES
  %token COUNT_REGEXP_INSTANCES
  %token DECOMPILE_BCS_TO_BAF
  %token DECOMPILE_DLG_TO_D
  %token DEFINE_ACTION_MACRO
  %token DEFINE_PATCH_MACRO
  %token DOLLARS
  %token COPY
  %token COPY_KIT
  %token COPY_LARGE
  %token GLOB
  %token EQUIP
  %token TWOHANDED
  %token COPY_EXISTING
  %token COPY_EXISTING_REGEXP
  %token COPY_RANDOM
  %token DELETE_BYTES
  %token DEPRECATED
  %token DESIGNATED
  %token EDIT_SAV_FILE
  %token END
  %token EQUALS
  %token EQUALSEQUALS
  %token NOTEQUALS
  %token EQUALSGREATER
  %token EVALUATE_REGEXP
  %token EXACT_MATCH
  %token EXTEND_BOTTOM
  %token EXTEND_BOTTOM_REGEXP
  %token EXTEND_MOS
  %token EXTEND_TOP
  %token EXTEND_TOP_REGEXP
  %token EVALUATE_BUFFER
  %token FAIL
  %token FILE_CONTAINS
  %token FILE_CONTAINS_EVALUATED
  %token FILE_IS_IN_COMPRESSED_BIF
  %token FILE_MD5
  %token FILE_EXISTS
  %token FILE_EXISTS_IN_GAME
  %token FILE_SIZE
  %token FIRST
  %token FORBID_FILE
  %token FORCED_SUBCOMPONENT
  %token FOR
  %token FROM
  %token GAME_IS
  %token GET_STRREF
  %token GET_DIRECTORY_ARRAY
  %token GET_FILE_ARRAY
  %token GROUP
  %token GT GTE LT LTE
  %token BAND BOR BXOR
  %token BLSL BASR BLSR BNOT
  %token SET_IDS_SYMBOL_OF_INT
  %token IDS_OF_SYMBOL
  %token IF
  %token SET
  %token IF_EVAL
  %token IF_FILE_EXISTS
  %token IF_SIZE_IS
  %token IN
  %token INCLUDE
  %token PATCH_INCLUDE
  %token INNER_ACTION
  %token INNER_PATCH
  %token INNER_PATCH_FILE
  %token INNER_PATCH_SAVE
  %token INSERT_2DA_ROW
  %token INSERT_BYTES
  %token INSERT_FILE
  %token IS_AN_INT
  %token LANGUAGE
  %token LAUNCH_ACTION_MACRO
  %token LAUNCH_PATCH_MACRO
  %token LAST
  %token LOAD
  %token LOAD_TRA
  %token LOCAL_SET
  %token LOCAL_SPRINT
  %token LOCAL_TEXT_SPRINT
  %token MENU_STYLE
  %token MKDIR
  %token MOD_IS_INSTALLED
  %token MODDER
  %token NO_LOG_RECORD
  %token NULL
  %token OUTER_FOR
  %token OUTER_PATCH
  %token OUTER_PATCH_SAVE
  %token OUTER_SET
  %token OUTER_SPRINT
  %token OUTER_TEXT_SPRINT
  %token OUTER_WHILE
  %token PATCH_BASH_FOR
  %token PATCH_CLEAR_ARRAY
  %token PATCH_DEFINE_ARRAY
  %token DEFINE_ASSOCIATIVE_ARRAY
  %token PATCH_PHP_EACH
  %token PATCH_FOR_EACH
  %token PATCH_GAM
  %token PATCH_READLN
  %token PATCH_SILENT
  %token PATCH_VERBOSE
  %token TIMESTIMES
  %token PLUS MINUS TIMES DIVIDE
  %token PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS
  %token OR_EQUALS AND_EQUALS BLSL_EQUALS BLSR_EQUALS BXOR_EQUALS
  %token PRINT
  %token PRETTY_PRINT_2DA
  %token SPRINT
  %token SNPRINT
  %token SPACES
  %token QUOTE
  %token PATCH_PRINT
  %token RANDOM_SEED PATCH_RANDOM_SEED
  %token RANDOM
  %token READ_ASCII
  %token READ_STRREF
  %token READ_BYTE
  %token READ_SBYTE
  %token READ_SSHORT
  %token READ_LONG
  %token READ_SHORT
  %token REINCLUDE
  %token PATCH_REINCLUDE
  %token REMOVE_CRE_ITEM
  %token REMOVE_KNOWN_SPELL
  %token REMOVE_MEMORIZED_SPELL
  %token REMOVE_STORE_ITEM
  %token REPLACE
  %token REPLACE_BCS_BLOCK
  %token REPLACE_BCS_BLOCK_REGEXP
  %token REPLACE_CRE_ITEM
  %token APPLY_BCS_PATCH
  %token APPLY_BCS_PATCH_OR_COPY
  %token REPLACE_TEXTUALLY
  %token REPLACE_EVALUATE
  %token REQUIRE_FILE
  %token REQUIRE_PREDICATE
  %token REQUIRE_COMPONENT
  %token INSTALL_BY_DEFAULT
  %token FORBID_COMPONENT
  %token SAY
  %token SAY_EVALUATED
  %token SILENT
  %token SUBCOMPONENT
  %token SCRIPT_STYLE
  %token STATE_WHICH_SAYS
  %token STRING
  %token STRING_CONCAT
  %token STRING_SET
  %token STRING_SET_EVALUATE
  %token STRING_SET_RANGE
  %token STRING_COMPARE
  %token STRING_COMPARE_CASE
  %token STRING_EQUAL
  %token STRING_EQUAL_CASE
  %token STRING_MATCHES_REGEXP
  %token STRING_CONTAINS_REGEXP
  %token THEN
  %token TEXT_SPRINT
  %token TO_LOWER
  %token TO_UPPER
  %token UNINSTALL
  %token UNLESS
  %token USING
  %token VARIABLE_IS_SET
  %token VERBOSE
  %token WRITE_ASCII
  %token WRITE_ASCII_TERMINATED
  %token WRITE_EVALUATED_ASCII
  %token WRITE_BYTE
  %token WRITE_FILE
  %token WRITE_LONG
  %token WRITE_SHORT
  %token WHILE

  %token ITM_V10_HEADERS
  %token ITM_V10_GEN_EFFECTS
  %token WMP_AREAS
  %token WMP_LINKS

  %token ITM_V10_HEAD_EFFECTS

  %token EOF

  %token <string> SOUND STRING
  %token <string * string> INLINED_FILE
  %token <int> STRING_REF TRANS_REF FORCED_STRING_REF

  %right QUESTION COLON
  %left OR
  %left AND
  %left BOR
  %left BXOR
  %left BAND
  %left EQUALS EQUALSEQUALS NOTEQUALS STRING_COMPARE STRING_COMPARE_CASE STRING_EQUAL STRING_EQUAL_CASE STRING_MATCHES_REGEXP STRING_CONTAINS_REGEXP
  %left GT GTE LT LTE
  %left BLSL BLSR BASR
  %left PLUS MINUS
  %left TIMES DIVIDE
  %left TIMESTIMES
  %right NOT BNOT

  /* Non-terminals informations */
%start tp_file tph_file tpp_file

  %type <Dlg.tlk_string> lse
  %type <string> sound_opt

  %type <Tp.tp_file> tp_file
  %type <Tp.tp_lang list> tp_lang_list
  %type <Tp.tp_flag list> tp_flag_list
  %type <Tp.tp_mod list> tp_mod_list
  %type <Tp.tp_action list> tp_action_list
  %type <Tp.tp_action> tp_action
  %type <Tp.tp_constraint list> tp_when_list
  %type <(string * string) list> str_str_list
  %type <Tp.tp_patch list> tp_patch_list
  %type <string list> string_list
  %type <string list> upper_string_list

  %type <Tp.tp_action list> tph_file
  %type <Tp.tp_patch list>  tpp_file

  %%
optional_evaluate :
| { false }
| EVALUATE_BUFFER { true }
    ;

  optional_then: { () }
| THEN { () }
    ;

  lse_string:     STRING { $1 }
| STRING STRING_CONCAT lse_string { $1 ^ $3 }
    ;

  lse : lse_string sound_opt
    { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2;
				      lse_female = $1; lse_female_sound = $2;}) in
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l ) then begin
        Dlg.local_string_ht := Some(result :: l )
      end
    | _ -> () ) ;
    result
    }
| lse_string sound_opt lse_string sound_opt
    { let result = Dlg.Local_String({ lse_male = $1 ; lse_male_sound = $2;
				      lse_female = $3; lse_female_sound = $4; }) in
    (match !Dlg.local_string_ht with
      Some(l) -> if not (List.mem result l) then
        Dlg.local_string_ht := Some(result :: l)
    | _ -> () ) ;
    result
    }
| STRING_REF { Dlg.TLK_Index($1) }
| TRANS_REF  { Dc.resolve_string_while_loading (Dlg.Trans_String(Dlg.Int $1)) }
| FORCED_STRING_REF lse
    { let _ = Dc.set_string_while_loading $1 $2 in Dlg.TLK_Index($1) }
    ;

  sound_opt :             { "" }
| SOUND                 { $1 }
    ;

  tp_file :
    BACKUP STRING AUTHOR STRING tp_flag_list tp_lang_list tp_mod_list
    { { Tp.tp_filename = $2 ;
	Tp.backup = (Arch.backslash_to_slash $2);
	Tp.author = $4;
	Tp.flags = $5 ;
	Tp.languages = $6 ;
	Tp.module_list = $7 ;
	Tp.is_auto_eval_string = List.mem Tp.Auto_Eval_Strings $5;
      } }
    ;

  tp_flag_list :                  { [] }
| AUTO_TRA STRING tp_flag_list         { (Tp.Auto_Tra($2)) :: $3 }
| NO_IF_EVAL_BUG tp_flag_list          { Tp.TP_No_If_Eval () :: $2 }
| MENU_STYLE STRING tp_flag_list       { (Tp.Menu_Style($2)) :: $3 }
| ASK_EVERY_COMPONENT tp_flag_list     { (Tp.Ask_Every_Component) :: $2 }
| ALWAYS tp_action_list END tp_flag_list   { (Tp.Always($2)) :: $4 }
| DEFINE_ACTION_MACRO STRING BEGIN tp_local_declaration_list tp_action_list END tp_flag_list
    { (Tp.Define_Action_Macro($2,$4,$5)) :: $7 }
| DEFINE_PATCH_MACRO STRING BEGIN tp_local_declaration_list tp_patch_list END tp_flag_list
    { (Tp.Define_Patch_Macro($2,$4,$5)) :: $7 }
| LOAD string_list tp_flag_list { (Tp.Load_Macro($2)) :: $3 }
| MODDER tp_flag_list { Modder.set_modder [] ; $2 }
| ALLOW_MISSING upper_string_list tp_flag_list
    { Tp.Allow_Missing($2) :: $3 }
| SCRIPT_STYLE STRING tp_flag_list
    { let n = match (String.uppercase $2) with
    | "BG"
    | "BG2" -> Load.BG2
    | "BG1" -> BG1
    | "PST" -> Load.PST
    | "IWD"
    | "IWD1" -> Load.IWD1
    | "IWD2" -> Load.IWD2
    | _ -> parse_error "unknown SCRIPT_STYLE"
    in
    Tp.Script_Style(n) :: $3 }
    ;

  upper_string_list :            { [] }
| STRING upper_string_list  { (String.uppercase $1) :: $2 }
    ;

  tp_lang_list :   { [] }
| LANGUAGE STRING STRING string_list tp_lang_list
    { { Tp.lang_name = $2 ;
	Tp.lang_dir_name = $3 ;
	Tp.lang_tra_files = $4 ;
      } :: $5 }
    ;

  tp_mod_list :         { [] }
| BEGIN lse
    tp_mod_flag_list
    tp_action_list tp_mod_list
    { { Tp.mod_name = $2 ; Tp.mod_parts = $4 ;
	Tp.mod_flags = $3 } :: $5 }
    ;

  tp_mod_flag_list :    { [] }
| SUBCOMPONENT lse tp_mod_flag_list
    { Tp.TPM_SubComponents($2,Tp.Pred_True,false) :: $3 }
| FORCED_SUBCOMPONENT lse tp_mod_flag_list
    { Tp.TPM_SubComponents($2,Tp.Pred_True,true) :: $3 }
| SUBCOMPONENT lse patch_exp tp_mod_flag_list
    { Tp.TPM_SubComponents($2,$3,false) :: $4 }
| FORCED_SUBCOMPONENT lse patch_exp tp_mod_flag_list
    { Tp.TPM_SubComponents($2,$3,true) :: $4 }
| NO_LOG_RECORD tp_mod_flag_list { Tp.TPM_NotInLog :: $2 }
| DEPRECATED lse tp_mod_flag_list { Tp.TPM_Deprecated($2) :: $3 }
| DESIGNATED STRING tp_mod_flag_list { Tp.TPM_Designated(my_int_of_string $2) :: $3 }
| REQUIRE_COMPONENT STRING STRING lse tp_mod_flag_list
    { Tp.TPM_RequireComponent($2,(my_int_of_string $3),$4) :: $5 }
| INSTALL_BY_DEFAULT tp_mod_flag_list
    { Tp.TPM_InstallByDefault :: $2 }
| FORBID_COMPONENT STRING STRING lse tp_mod_flag_list
    { Tp.TPM_ForbidComponent($2,(my_int_of_string $3),$4) :: $5 }
| REQUIRE_PREDICATE patch_exp lse tp_mod_flag_list { Tp.TPM_RequirePredicate($2,$3) :: $4 }
| GROUP lse tp_mod_flag_list { Tp.TPM_Group($2,Tp.Pred_True) :: $3 }
    ;

  tp_action_list :            { [] }
| tp_action tp_action_list  { $1 :: $2 }
    ;

  optional_using :                { [] }
| USING string_list             { $2 }
    ;

  optional_backup : { 0 }
| PLUS            { 1 }
| MINUS           { 2 }
    ;

  optional_plus : { true }
| PLUS          { false }
    ;

  optional_equip :    { false }
| EQUIP             { true }
    ;

  optional_2h :       { true }
| TWOHANDED         { false }
    ;

  optional_glob :     { false }
| GLOB              { true }
    ;

  string_ref_or_pe :
| STRING_REF { Tp.PE_String(Tp.PE_LiteralString(string_of_int $1)) }
| LPAREN patch_exp RPAREN  { $2 }
    ;
  str_reg_list : { [] }
|STRING optional_match_exact STRING str_reg_list {($1,$2,$3)::$4}

    tp_action :
| ACTION_BASH_FOR str_reg_list BEGIN tp_action_list END
    { Tp.TP_ActionBashFor($2, $4) }
| ACTION_FOR_EACH patch_STRING_right IN string_list BEGIN tp_action_list END
    { Tp.TP_Action_For_Each($2,$4,$6) }
| ACTION_PHP_EACH patch_STRING_left AS patch_STRING_left EQUALSGREATER
    patch_STRING_left BEGIN tp_action_list END
    { Tp.TP_ActionPHPEach($2,$4,$6,$8) }
| CLEAR_MEMORY {Tp.TP_ClearMemory  }
| CLEAR_IDS_MAP { Tp.TP_Clear_Ids_Map }
| COPY optional_backup optional_glob str_str_list tp_patch_list tp_when_list
    { Tp.TP_Copy(
      { Tp.copy_get_existing = false;
        Tp.copy_use_regexp = false;
        Tp.copy_use_glob = $3 ;
        Tp.copy_file_list = $4 ;
        Tp.copy_patch_list = $5 ;
        Tp.copy_constraint_list = $6 ;
        Tp.copy_backup = not ($2 = 1) ;
        Tp.copy_at_end = false ;
        Tp.copy_save_inlined = ($2 = 2) ;
      } ) }
| COPY_ALL_GAM_FILES tp_patch_list tp_when_list
    { Tp.TP_CopyAllGamFiles($2, $3) ;
    }
| COPY_LARGE optional_plus optional_glob str_str_list
    { Tp.TP_CopyLarge(
      { Tp.copy_large_use_glob = $3;
        Tp.copy_large_file_list = $4 ;
        Tp.copy_large_backup = $2 ;
      } ) }
| COPY_EXISTING optional_backup str_str_list tp_patch_list tp_when_list
    { Tp.TP_Copy(
      { Tp.copy_get_existing = true;
        Tp.copy_use_regexp = false;
        Tp.copy_use_glob = false;
        Tp.copy_file_list = $3 ;
        Tp.copy_patch_list = $4 ;
        Tp.copy_constraint_list = $5 ;
        Tp.copy_backup = not ($2 = 1) ;
        Tp.copy_at_end = false ;
        Tp.copy_save_inlined = ($2 = 2) ;
      } ) }
| COPY_EXISTING_REGEXP optional_backup optional_glob str_str_list tp_patch_list tp_when_list
    { Tp.TP_Copy(
      { Tp.copy_get_existing = true;
        Tp.copy_use_regexp = true;
        Tp.copy_use_glob = true;
        Tp.copy_file_list = $4 ;
        Tp.copy_patch_list = $5 ;
        Tp.copy_constraint_list = $6 ;
        Tp.copy_backup = not ($2 = 1) ;
        Tp.copy_at_end = false ;
        Tp.copy_save_inlined = ($2 = 2) ;
      } ) }
| COPY_RANDOM copy_random_string_list tp_patch_list tp_when_list { Tp.TP_CopyRandom($2,$3,$4) }
| COPY_RANDOM STRING string_list tp_patch_list tp_when_list { Tp.TP_CopyRandom([$2::$3],$4,$5) }
| RANDOM_SEED patch_exp
    {
     Tp.TP_RandomSeed($2)
   }
| COMPILE optional_evaluate string_list optional_using { Tp.TP_Compile($2,$3,[],$4) }
| AT_NOW STRING { Tp.TP_At_Now(None,$2,false) }
| AT_INTERACTIVE_NOW STRING { Tp.TP_At_Now(None,$2,false) }
| BIFF STRING BEGIN str_reg_list END { Tp.TP_Biff($2,$4) }
| INLINED_FILE { Tp.TP_Inlined_File($1) }
| DEFINE_ACTION_MACRO STRING BEGIN tp_local_declaration_list tp_action_list END
    { Tp.TP_Define_Action_Macro($2,$4,$5) }
| DEFINE_PATCH_MACRO STRING BEGIN tp_local_declaration_list tp_patch_list END
    { Tp.TP_Define_Patch_Macro($2,$4,$5) }
| LAUNCH_ACTION_MACRO STRING { Tp.TP_Launch_Action_Macro ($2) }
| INCLUDE string_list { Tp.TP_Include($2) }
| REINCLUDE string_list { Tp.TP_Reinclude($2) }
| LOAD_TRA string_list { Tp.TP_Load_Tra($2) }
| MKDIR string_list { Tp.TP_Mkdir($2) }
| OUTER_FOR LPAREN tp_patch_list SEMICOLON patch_exp SEMICOLON tp_patch_list
    RPAREN BEGIN tp_action_list END { Tp.TP_Outer_For($3,$5,$7,$10) }
| OUTER_PATCH STRING BEGIN tp_patch_list END { Tp.TP_Outer_Inner_Buff($2,$4) }
| OUTER_PATCH_SAVE patch_STRING_left STRING BEGIN tp_patch_list END { Tp.TP_Outer_Inner_Buff_Save($2,$3,$5) }
| OUTER_SET patch_STRING_left EQUALS patch_exp { Tp.TP_Outer_Set($2,$4) }
| OUTER_SPRINT patch_STRING_left dlg_or_patch_STRING_right { Tp.TP_Outer_Sprint($2,$3) }
| OUTER_TEXT_SPRINT patch_STRING_left patch_STRING_right { Tp.TP_Outer_Text_Sprint($2,$3) }
| OUTER_WHILE patch_exp BEGIN tp_action_list END { Tp.TP_Outer_While($2,$4) }
| ACTION_READLN patch_STRING_left { Tp.TP_Action_ReadLN($2) }
| REQUIRE_FILE STRING lse { Tp.TP_Require_File($2,$3) }
| FORBID_FILE STRING lse { Tp.TP_Forbid_File($2,$3) }
| APPEND STRING STRING tp_when_list { Tp.TP_Append($2,$3,$4,true,false,0) }
| APPEND_OUTER STRING STRING tp_when_list { Tp.TP_Append($2,$3,$4,false,false,0) }
| APPEND_COL STRING STRING tp_when_list { Tp.TP_Append_Col($2,$3,Tp.get_pe_int "0",$4,true,0) }
| APPEND_COL STRING STRING  STRING tp_when_list { Tp.TP_Append_Col($2,$3,(Tp.get_pe_int $4),$5,true,0) }
| EXTEND_TOP STRING STRING tp_patch_list optional_using
    { Tp.TP_Extend_Top(false,$2,$3,$4,$5) }
| EXTEND_BOTTOM STRING STRING tp_patch_list optional_using
    { Tp.TP_Extend_Bottom(false,$2,$3,$4,$5) }
| EXTEND_TOP_REGEXP STRING STRING tp_patch_list optional_using
    { Tp.TP_Extend_Top(true,$2,$3,$4,$5) }
| EXTEND_BOTTOM_REGEXP STRING STRING tp_patch_list optional_using
    { Tp.TP_Extend_Bottom(true,$2,$3,$4,$5) }
| AT_EXIT STRING { Tp.TP_At_Exit($2,false) }
| AT_INTERACTIVE_EXIT STRING { Tp.TP_At_Interactive_Exit($2,false) }
| AT_INTERACTIVE_UNINSTALL STRING { Tp.TP_At_Interactive_Uninstall($2,false) }
| AT_UNINSTALL STRING { Tp.TP_At_Uninstall($2,false) }
| ADD_MUSIC STRING STRING { Tp.TP_Add_Music(
			    { Tp.music_name = $2;
			      Tp.music_file = $3; } ) }
| ADD_SPELL STRING patch_exp patch_exp STRING tp_patch_list tp_when_list
{ if $7 <> [] then log_and_print_modder "\n\nWARNING: ignoring non-empty constraint list on ADD_SPELL\n\n";  Tp.TP_Add_Spell($2,$3,$4,$5,$6,None,None) }
| ADD_PROJECTILE STRING {
  Tp.TP_Add_Projectile( { Tp.pro_file = $2;  Tp.missile_ids_name = Case_ins.filename_chop_extension (Case_ins.filename_basename $2)} ) }
| SILENT { Tp.TP_Silent }
| STRING_SET string_lse_list { Tp.TP_String_Set($2,None) }
| STRING_SET string_lse_list USING STRING { Tp.TP_String_Set($2,Some($4)) }
| STRING_SET_EVALUATE pe_lse_list { Tp.TP_String_Set_Evaluate($2,None) }
| STRING_SET_EVALUATE pe_lse_list USING STRING { Tp.TP_String_Set_Evaluate($2,Some($4)) }
| STRING_SET_RANGE string_ref_or_pe string_ref_or_pe USING STRING
    { Tp.TP_String_Set_Range($2,$3,$5) }
| ACTION_DEFINE_ARRAY patch_STRING_left BEGIN string_list END
    { Tp.TP_ActionDefineArray($2,$4) }
| UNINSTALL STRING patch_exp { Tp.TP_Uninstall_Now($2,$3) }
| VERBOSE { Tp.TP_Verbose }
| COPY_KIT STRING STRING LPAREN str_str_list RPAREN { Tp.TP_CopyKit($2,$3,$5) }
| ADD_KIT
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    STRING
    SAY lse
    SAY lse
    SAY lse
    { Tp.TP_Add_Kit(
      { Tp.kit_name = $2 ;
	Tp.clasweap = $3 ;
	Tp.weapprof = $4 ;
	Tp.abclasrq = $5 ;
	Tp.abclsmod = $6 ;
	Tp.abdcdsrq = $7 ;
	Tp.abdcscrq = $8 ;
	Tp.alignmnt = $9 ;
	Tp.dualclas = $10 ;
	Tp.ability_file = $11 ;
	Tp.include_in = $12 ;
	Tp.unused_class = $13 ;
	Tp.tob_abbr = $14;
	Tp.tob_start =
	(let lst = Str.split (Str.regexp "[ \t]+") $15 in
	List.map (fun elt -> if elt = "$" then "" else elt) lst) ;
	Tp.lower = $17 ;
	Tp.mixed = $19 ;
	Tp.help = $21 ;
      })}
| FAIL lse       { Tp.TP_Fail($2) }
| PRINT lse       { Tp.TP_Print($2) }
| ACTION_IF patch_exp THEN BEGIN tp_action_list END { Tp.TP_If($2,$5,[]) }
| ACTION_IF patch_exp THEN BEGIN tp_action_list END ELSE
    BEGIN tp_action_list END { Tp.TP_If($2,$5,$9) }
    ;

  string_lse_list :               { [] }
| STRING lse string_lse_list    { ($1,$2) :: $3 }
    ;

  pe_lse_list :               { [] }
| patch_exp lse pe_lse_list    { ($1,$2) :: $3 }
    ;

  /*
  tp_predicate : LPAREN patch_exp RPAREN { $2 }
| patch_exp AND patch_exp { Tp.Pred_And($1,$3) }
| patch_exp OR patch_exp { Tp.Pred_Or($1,$3) }
| NOT patch_exp { Tp.Pred_Not($2) }
| LPAREN patch_exp RPAREN { Tp.Pred_Expr($2) }
| FILE_EXISTS STRING { Tp.Pred_File_Exists($2) }
| FILE_MD5 STRING STRING { Tp.Pred_File_MD5($2,$3) }
| FILE_EXISTS_IN_GAME STRING { Tp.Pred_File_Exists_In_Game($2) }
| FILE_SIZE STRING STRING { Tp.Pred_File_Size($2,my_int_of_string $3) }
| FILE_SIZE STRING STRING { Tp.Pred_File_Size($2,my_int_of_string $3) }
| FILE_CONTAINS STRING STRING { Tp.Pred_File_Contains($2,$3) }
    ;
  */


  string_list :            { [] }
| STRING string_list  { $1 :: $2 }
    ;

  copy_random_string_list : { [] }
| LPAREN string_list RPAREN copy_random_string_list { $2 :: $4 }
    ;


  tp_when_list :                 { [] }
| tp_when tp_when_list         { $1 :: $2 }
      ;
  tp_when :
| IF patch_STRING_right       { Tp.TP_Contains($2) }
| UNLESS patch_STRING_right   { Tp.TP_NotContains($2) }
| IF_SIZE_IS STRING { Tp.TP_IfSizeIs(my_int_of_string $2) }
| IF_EVAL patch_exp { Tp.TP_Eval($2) }
| BUT_ONLY_IF_IT_CHANGES        { Tp.TP_ButOnlyIfItChanges }
    ;

  str_str_list :               { [] }
| STRING STRING str_str_list { ($1,$2) :: $3 }
    ;

  patch_STRING_left : STRING           { Tp.PE_LiteralString $1 }
| EVALUATE_BUFFER patch_STRING_right  { Tp.PE_Evaluate $2 }
| DOLLARS patch_STRING_right LPAREN patch_STRING_list RPAREN { Tp.PE_Dollars($2,$4,false,true) }
    ;

  patch_STRING_name : STRING           { Tp.PE_LiteralString $1 }
| EVALUATE_BUFFER patch_STRING_name  { Tp.PE_Evaluate $2 }
| DOLLARS patch_STRING_right LPAREN patch_STRING_list RPAREN { Tp.PE_Dollars($2,$4,false,false) }
    ;

  patch_STRING_right : STRING           { Tp.PE_LiteralString $1 }
| EVALUATE_BUFFER patch_STRING_right  { Tp.PE_Evaluate $2 }
| DOLLARS patch_STRING_right LPAREN patch_STRING_list RPAREN { Tp.PE_Dollars($2,$4,true,false) }
    ;

  patch_STRING_list : { [] }
| patch_STRING_right patch_STRING_list { $1 :: $2 }
    ;

  dlg_or_patch_STRING_right : lse       { Tp.PE_Tlk($1) }
| EVALUATE_BUFFER patch_STRING_right  { Tp.PE_Pe (Tp.PE_Evaluate $2)  }
| DOLLARS patch_STRING_right LPAREN patch_STRING_list RPAREN { Tp.PE_Pe (Tp.PE_Dollars($2,$4,true,false)) }
    ;

  patch_exp : patch_STRING_right              { Tp.PE_String($1) }
| LPAREN patch_exp RPAREN       { $2 }
| patch_STRING_right STRING_COMPARE patch_STRING_right    { Tp.PE_StringEqual($1,$3,false,false) }
| patch_STRING_right STRING_COMPARE_CASE patch_STRING_right    { Tp.PE_StringEqual($1,$3,true,false) }
| patch_STRING_right STRING_EQUAL patch_STRING_right    { Tp.PE_StringEqual($1,$3,false,true) }
| patch_STRING_right STRING_EQUAL_CASE patch_STRING_right    { Tp.PE_StringEqual($1,$3,true,true) }
| patch_STRING_right STRING_MATCHES_REGEXP patch_STRING_right { Tp.PE_StringRegexp($1,$3,true) }
| patch_STRING_right STRING_CONTAINS_REGEXP patch_STRING_right { Tp.PE_StringRegexp($1,$3,false) }
| RANDOM LPAREN patch_exp patch_exp RPAREN { Tp.PE_Random($3,$4) }
| FILE_CONTAINS_EVALUATED LPAREN patch_STRING_right patch_STRING_right RPAREN
    { Tp.PE_FileContainsEvaluated($3,$4) }
| FILE_EXISTS patch_STRING_right { Tp.Pred_File_Exists($2) }
| FILE_IS_IN_COMPRESSED_BIF patch_STRING_right { Tp.Pred_File_Is_In_Compressed_Bif($2) }
| FILE_MD5 patch_STRING_right patch_STRING_right { Tp.Pred_File_MD5($2,$3) }
| FILE_EXISTS_IN_GAME patch_STRING_right { Tp.Pred_File_Exists_In_Game($2) }
| FILE_SIZE patch_STRING_right STRING { Tp.Pred_File_Size($2,my_int_of_string $3) }
| FILE_CONTAINS patch_STRING_right patch_STRING_right { Tp.Pred_File_Contains($2,$3) }
| patch_exp PLUS patch_exp      { Tp.PE_Add($1,$3) }
| patch_exp MINUS patch_exp      { Tp.PE_Sub($1,$3) }
| patch_exp TIMES patch_exp      { Tp.PE_Mul($1,$3) }
| patch_exp DIVIDE patch_exp      { Tp.PE_Div($1,$3) }
| patch_exp TIMESTIMES patch_exp      { Tp.PE_Exp($1,$3,Tp.Pred_True) }
| patch_exp TIMESTIMES LPAREN patch_exp patch_exp RPAREN
    { Tp.PE_Exp($1,$4,$5) }
| patch_exp EQUALS patch_exp  { Tp.PE_Equal($1,$3) }
| patch_exp EQUALSEQUALS patch_exp  { Tp.PE_Equal($1,$3) }
| patch_exp NOTEQUALS patch_exp { Tp.PE_Not(Tp.PE_Equal($1,$3)) }
| NOT patch_exp               { Tp.PE_Not($2) }
| patch_exp AND patch_exp  { Tp.PE_And($1,$3) }
| patch_exp OR patch_exp  { Tp.PE_Or($1,$3) }
| patch_exp GT patch_exp  { Tp.PE_GT($1,$3) }
| patch_exp GTE patch_exp  { Tp.PE_GTE($1,$3) }
| patch_exp LT patch_exp  { Tp.PE_LT($1,$3) }
| patch_exp LTE patch_exp  { Tp.PE_LTE($1,$3) }

| patch_exp BAND patch_exp  { Tp.PE_BAND($1,$3) }
| patch_exp BOR patch_exp  { Tp.PE_BOR($1,$3) }
| BNOT patch_exp  { Tp.PE_BNOT($2) }
| patch_exp BXOR patch_exp  { Tp.PE_BXOR($1,$3) }
| patch_exp BLSL patch_exp  { Tp.PE_BLSL($1,$3) }
| patch_exp BLSR patch_exp  { Tp.PE_BLSR($1,$3) }
| patch_exp BASR patch_exp  { Tp.PE_BASR($1,$3) }

| patch_exp QUESTION patch_exp COLON patch_exp { Tp.PE_If($1,$3,$5); }

| MOD_IS_INSTALLED STRING STRING { Tp.PE_ModIsInstalled($2,Tp.get_pe_int($3)) }
| GAME_IS STRING { Tp.PE_GameIs($2,true) }
| VARIABLE_IS_SET patch_STRING_name { Tp.PE_VariableIsSet($2) }
| IDS_OF_SYMBOL LPAREN STRING STRING RPAREN { Tp.PE_IdsOfSymbol($3,$4) }
| STATE_WHICH_SAYS lse FROM STRING {
  let file = fst (split $4) ^ ".dlg" in
  Tp.PE_StateWhichSays(Some($2),None,file) }
| STATE_WHICH_SAYS patch_exp IN STRING FROM STRING {
  let file = fst (split $6) ^ ".dlg" in
  Tp.PE_StateWhichSays(None,Some($2,$4),file) }
| IS_AN_INT patch_STRING_left { Tp.PE_IsAnInt($2) }
    ;

  optional_store_position :
    { Tp.TP_Store_First }
| AFTER patch_STRING_right { Tp.TP_Store_After $2 }
| BEFORE patch_STRING_right { Tp.TP_Store_Before $2 }
| LAST { Tp.TP_Store_Last }
| FIRST { Tp.TP_Store_First }
    ;

  tp_patch_list :   { [] }
| tp_patch tp_patch_list { $1 :: $2 }
    ;

  optional_case_sensitive : { None }
| CASE_SENSITIVE          { Some(true)  }
| CASE_INSENSITIVE        { Some(false) }
    ;

  optional_match_exact : { None }
| EXACT_MATCH          { Some(true)  }
| EVALUATE_REGEXP      { Some(false) }
    ;

  optional_null_terminated : { false }
| NULL { true }

    tp_patch   :
| SAY patch_exp lse { Tp.TP_PatchStrRef($2,$3) }
| SAY_EVALUATED patch_exp STRING { Tp.TP_PatchStrRefEvaluated($2,$3) }
| LAUNCH_PATCH_MACRO STRING { Tp.TP_Launch_Patch_Macro ($2,true) }
| REPLACE optional_case_sensitive optional_match_exact STRING lse { Tp.TP_PatchString($2,$3,$4,$5) }
| REPLACE_TEXTUALLY optional_case_sensitive optional_match_exact STRING STRING
    { Tp.TP_PatchStringTextually($2,$3,$4,$5,None) }
| REPLACE_TEXTUALLY optional_case_sensitive optional_match_exact STRING STRING LPAREN patch_exp RPAREN
    { Tp.TP_PatchStringTextually($2,Some(false),$4,$5,Some($7)) }
| REPLACE_EVALUATE optional_case_sensitive STRING BEGIN tp_patch_list END STRING
    { Tp.TP_PatchStringEvaluate($2,$3,$5,$7) }
| REPLACE_BCS_BLOCK STRING STRING { Tp.TP_PatchReplaceBCSBlock($2,$3,None,false,None) }
| REPLACE_BCS_BLOCK_REGEXP STRING STRING { Tp.TP_PatchReplaceBCSBlockRE($2,$3,None) }
| REPLACE_CRE_ITEM STRING string_ref_or_pe  string_ref_or_pe  string_ref_or_pe  STRING  STRING  optional_equip  optional_2h
    { Tp.TP_Replace_Cre_Item(
      { Tp.item_name = $2 ;
	Tp.i_charge1 = $3 ;
	Tp.i_charge2 = $4 ;
	Tp.i_charge3 = $5 ;
	Tp.i_flags = $6 ;
	Tp.item_slot = $7 ;
	Tp.equip = $8 ;
	Tp.twohanded_weapon = $9 ;
	Tp.nomove = false ;})}
| APPLY_BCS_PATCH STRING { Tp.TP_PatchApplyBCSPatch($2,None) }
| APPLY_BCS_PATCH_OR_COPY STRING STRING { Tp.TP_PatchApplyBCSPatch($2,Some($3)) }
| GET_STRREF patch_exp STRING { Tp.TP_PatchGetStrRef($2,Tp.PE_LiteralString $3,false,false) }
| READ_STRREF patch_exp STRING { Tp.TP_PatchReadStrRef($2,Tp.PE_LiteralString $3,None,false,false) }
| READ_STRREF patch_exp STRING ELSE STRING { Tp.TP_PatchReadStrRef($2,Tp.PE_LiteralString $3,Some($5),false,false) }
| READ_ASCII patch_exp STRING LPAREN patch_exp RPAREN optional_null_terminated
    { Tp.TP_PatchReadAscii($2,Tp.PE_LiteralString $3,None,$5,$7) }
| READ_ASCII patch_exp STRING ELSE STRING LPAREN patch_exp RPAREN optional_null_terminated
    { Tp.TP_PatchReadAscii($2,Tp.PE_LiteralString $3,Some($5),$7,$9) }
| READ_ASCII patch_exp STRING { Tp.TP_PatchReadAscii($2,Tp.PE_LiteralString $3,None,Tp.get_pe_int "8",true) }
| READ_ASCII patch_exp STRING ELSE STRING { Tp.TP_PatchReadAscii($2,Tp.PE_LiteralString $3,Some($5),Tp.get_pe_int "8",true) }
| READ_BYTE patch_exp STRING { Tp.TP_PatchReadByte($2,Tp.PE_LiteralString $3,None) }
| READ_SBYTE patch_exp STRING { Tp.TP_PatchReadSByte($2,Tp.PE_LiteralString $3,None) }
| READ_SSHORT patch_exp STRING { Tp.TP_PatchReadSShort($2,Tp.PE_LiteralString $3,None) }
| READ_BYTE patch_exp STRING ELSE patch_exp { Tp.TP_PatchReadByte($2,Tp.PE_LiteralString $3,Some($5) ) }
| READ_SHORT patch_exp STRING { Tp.TP_PatchReadShort($2,Tp.PE_LiteralString $3,None) }
| READ_SHORT patch_exp STRING ELSE patch_exp { Tp.TP_PatchReadShort($2,Tp.PE_LiteralString $3,Some($5)) }
| READ_LONG patch_exp STRING { Tp.TP_PatchReadLong($2,Tp.PE_LiteralString $3,None) }
| READ_LONG patch_exp STRING ELSE patch_exp { Tp.TP_PatchReadLong($2,Tp.PE_LiteralString $3,Some($5)) }
| WRITE_FILE patch_exp STRING { Tp.TP_PatchWriteFile($2,$3,false) }
| INSERT_FILE patch_exp STRING { Tp.TP_PatchWriteFile($2,$3,true) }
| APPEND_FILE STRING { Tp.TP_PatchAppendFile($2,false,false) }
| APPEND_FILE_EVALUATE STRING { Tp.TP_PatchAppendFile($2,false,true) }
| WRITE_BYTE patch_exp patch_exp { Tp.TP_PatchByte($2,$3) }
| WRITE_SHORT patch_exp patch_exp { Tp.TP_PatchShort($2,$3) }
| WRITE_LONG patch_exp patch_exp { Tp.TP_PatchLong($2,$3) }
| WRITE_ASCII patch_exp STRING { Tp.TP_PatchASCII($2,Tp.PE_LiteralString $3,false,None) }
| WRITE_ASCII patch_exp STRING string_ref_or_pe {
  Tp.TP_PatchASCII($2,Tp.PE_LiteralString $3,false,Some($4)) }
| WRITE_EVALUATED_ASCII patch_exp patch_STRING_right { Tp.TP_PatchASCII($2,$3,true,None) }
| WRITE_EVALUATED_ASCII patch_exp patch_STRING_right string_ref_or_pe { Tp.TP_PatchASCII($2,$3,true,Some($4)) }
| WRITE_ASCII_TERMINATED patch_exp STRING { Tp.TP_PatchASCIITerminated($2,$3) }
| INSERT_BYTES patch_exp patch_exp { Tp.TP_PatchInsertBytes($2,$3) }
| DELETE_BYTES patch_exp patch_exp { Tp.TP_PatchDeleteBytes($2,$3) }
| SET patch_STRING_left EQUALS patch_exp { Tp.TP_PatchSet($2,$4) }

| STRING PLUS_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_Add(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING MINUS_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_Sub(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING TIMES_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_Mul(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING DIVIDE_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_Div(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING OR_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_BOR(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING AND_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_BAND(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING BLSL_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_BLSL(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING BLSR_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_BLSR(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }
| STRING BXOR_EQUALS patch_exp
    { Tp.TP_PatchSet(Tp.PE_LiteralString $1,(Tp.PE_BXOR(Tp.PE_String(Tp.PE_LiteralString $1),$3))) }



| SET patch_STRING_left PLUS_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_Add(Tp.PE_String($2),$4))) }
| SET patch_STRING_left MINUS_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_Sub(Tp.PE_String($2),$4))) }
| SET patch_STRING_left TIMES_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_Mul(Tp.PE_String($2),$4))) }
| SET patch_STRING_left DIVIDE_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_Div(Tp.PE_String($2),$4))) }
| SET patch_STRING_left OR_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_BOR(Tp.PE_String($2),$4))) }
| SET patch_STRING_left AND_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_BAND(Tp.PE_String($2),$4))) }
| SET patch_STRING_left BLSL_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_BLSL(Tp.PE_String($2),$4))) }
| SET patch_STRING_left BLSR_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_BLSR(Tp.PE_String($2),$4))) }
| SET patch_STRING_left BXOR_EQUALS patch_exp
    { Tp.TP_PatchSet($2,(Tp.PE_BXOR(Tp.PE_String($2),$4))) }

| SET_IDS_SYMBOL_OF_INT STRING STRING patch_exp
    { Tp.TP_PatchSetIdsSymOfInt($2,$3,$4) }
| STRING EQUALS patch_exp { Tp.TP_PatchSet(Tp.PE_LiteralString $1,$3) }
| ADD_KNOWN_SPELL STRING string_ref_or_pe STRING { Tp.TP_Add_Known_Spell($2,$3,$4) }
| ADD_MEMORIZED_SPELL STRING string_ref_or_pe STRING
    { Tp.TP_Add_Memorized_Spell($2,$3,$4,Tp.get_pe_int "1") }
| ADD_MEMORIZED_SPELL STRING string_ref_or_pe STRING string_ref_or_pe
    { Tp.TP_Add_Memorized_Spell($2,$3,$4,$5) }
| WHILE patch_exp BEGIN tp_patch_list END { Tp.TP_PatchWhile($2,$4) }
| PATCH_BASH_FOR str_reg_list BEGIN tp_patch_list END
    { Tp.TP_PatchBashFor($2, $4) }
| PATCH_CLEAR_ARRAY patch_STRING_left { Tp.TP_PatchClearArray($2) }
| PATCH_DEFINE_ARRAY patch_STRING_left BEGIN string_list END
    { Tp.TP_PatchDefineArray($2,$4) }
| PATCH_FOR_EACH patch_STRING_right IN string_list BEGIN tp_patch_list END
    { Tp.TP_PatchForEach($2,$4,$6) }
| PATCH_PHP_EACH patch_STRING_left AS patch_STRING_left EQUALSGREATER
    patch_STRING_left BEGIN tp_patch_list END
    { Tp.TP_PatchPHPEach($2,$4,$6,$8) }
| FOR LPAREN tp_patch_list SEMICOLON patch_exp SEMICOLON tp_patch_list
    RPAREN BEGIN tp_patch_list END { Tp.TP_PatchFor($3,$5,$7,$10) }
| PATCH_INCLUDE string_list { Tp.TP_PatchInclude($2) }
| PATCH_REINCLUDE string_list { Tp.TP_PatchReinclude($2) }
| PATCH_RANDOM_SEED patch_exp { Tp.TP_PatchRandomSeed($2) }
| PATCH_IF patch_exp optional_then BEGIN tp_patch_list END { Tp.TP_PatchIf($2,$5,[]) }
| PATCH_READLN patch_STRING_left { Tp.TP_PatchReadLN($2) }
| PATCH_SILENT { Tp.TP_PatchSilent }
| PATCH_VERBOSE { Tp.TP_PatchVerbose }
| INNER_ACTION BEGIN tp_action_list END { Tp.TP_PatchInnerAction($3) }
| INNER_PATCH STRING BEGIN tp_patch_list END { Tp.TP_PatchInnerBuff($2,$4) }
| INNER_PATCH_FILE STRING BEGIN tp_patch_list END { Tp.TP_PatchInnerBuffFile($2,$4) }
| INNER_PATCH_SAVE patch_STRING_left STRING BEGIN tp_patch_list END { Tp.TP_PatchInnerBuffSave($2,$3,$5) }
| PATCH_IF patch_exp optional_then BEGIN tp_patch_list END ELSE BEGIN tp_patch_list END { Tp.TP_PatchIf($2,$5,$9) }
| PATCH_IF patch_exp optional_then BEGIN tp_patch_list END ELSE tp_patch { Tp.TP_PatchIf($2,$5,[$8]) }
| PATCH_PRINT lse       { Tp.TP_PatchPrint($2) }
| TO_LOWER patch_STRING_right { Tp.TP_PatchToLower($2) }
| TO_UPPER patch_STRING_right { Tp.TP_PatchToUpper($2) }
| SPRINT patch_STRING_left dlg_or_patch_STRING_right     { Tp.TP_PatchSprint($2,$3) }
| SNPRINT patch_exp patch_STRING_left dlg_or_patch_STRING_right     { Tp.TP_PatchSnprint($2,$3,$4) }
| TEXT_SPRINT patch_STRING_left patch_STRING_right { Tp.TP_PatchTextSprint($2,$3) }
| SPACES patch_STRING_left patch_STRING_right{ Tp.TP_PatchSpaces($2,$3) }
| QUOTE patch_STRING_left patch_STRING_right{ Tp.TP_PatchQuote($2,$3) }
| INSERT_2DA_ROW patch_exp patch_exp patch_exp {Tp.TP_PatchInsert2DARow($2,$3,$4) }
| SET_2DA_ENTRY patch_exp patch_exp patch_exp patch_exp { Tp.TP_Patch2DA($2,$3,$4,$5) }
| SET_2DA_ENTRY_LATER STRING patch_exp patch_exp patch_exp { Tp.TP_Patch2DALater($2,$3,$4,$5) }
| SET_2DA_ENTRIES_NOW STRING patch_exp { Tp.TP_Patch2DANow($2,$3) }
| ADD_MAP_NOTE string_ref_or_pe string_ref_or_pe STRING lse
    { Tp.TP_Add_Map_Note(
      {Tp.xcoord = $2 ;
       Tp.ycoord = $3 ;
       Tp.mstring = $5 ;
       Tp.colour = $4 ; })}
| COUNT_2DA_ROWS patch_exp STRING { Tp.TP_Get2DARows($2,Tp.PE_LiteralString $3) }
| COUNT_2DA_COLS STRING { Tp.TP_Get2DACols(Tp.PE_LiteralString $2) }
| PRETTY_PRINT_2DA patch_exp { Tp.TP_PrettyPrint2DA($2) }
| PRETTY_PRINT_2DA { Tp.TP_PrettyPrint2DA(Tp.get_pe_int "2") }
| COUNT_REGEXP_INSTANCES optional_case_sensitive optional_match_exact STRING STRING
    { Tp.TP_CountRegexpInstances($2,$3,$4,Tp.PE_LiteralString $5) }
| READ_2DA_ENTRY patch_exp patch_exp patch_exp STRING { Tp.TP_Read2DA($2,$3,$4,Tp.PE_LiteralString $5) }
| READ_2DA_ENTRIES_NOW STRING patch_exp { Tp.TP_Read2DANow($2,$3) }
| READ_2DA_ENTRY_FORMER STRING patch_exp patch_exp STRING { Tp.TP_Read2DAFormer($2,$3,$4,$5) }
| ADD_CRE_ITEM
    STRING
    string_ref_or_pe
    string_ref_or_pe
    string_ref_or_pe
    STRING
    STRING
    optional_equip
    optional_2h
    { Tp.TP_Add_Cre_Item(
      { Tp.item_name = $2 ;
	Tp.i_charge1 = $3 ;
	Tp.i_charge2 = $4 ;
	Tp.i_charge3 = $5 ;
	Tp.i_flags = $6 ;
	Tp.item_slot = $7 ;
	Tp.equip = $8 ;
	Tp.twohanded_weapon = $9 ;
	Tp.nomove = false ;})}
| PATCH_GAM STRING STRING string_ref_or_pe string_ref_or_pe { Tp.TP_Patch_Gam($2,$3,$4,$5) }
| ADD_STORE_ITEM optional_plus STRING optional_store_position string_ref_or_pe string_ref_or_pe string_ref_or_pe STRING string_ref_or_pe
    { Tp.TP_Add_S_Item({ Tp.overwrite_store_item = $2; },$3,$4,$5,$6,$7,$8,$9,None) }
| ADD_STORE_ITEM optional_plus STRING optional_store_position string_ref_or_pe string_ref_or_pe string_ref_or_pe STRING string_ref_or_pe STRING
    { Tp.TP_Add_S_Item({ Tp.overwrite_store_item = $2; },$3,$4,$5,$6,$7,$8,$9,Some($10)) }
| REMOVE_CRE_ITEM string_list { Tp.TP_Remove_Cre_Item($2) }
| REMOVE_KNOWN_SPELL string_list { Tp.TP_Remove_Known_Spell($2) }
| REMOVE_MEMORIZED_SPELL string_list { Tp.TP_Remove_Memorized_Spell($2) }
| REMOVE_STORE_ITEM string_list { Tp.TP_Remove_Store_Item($2) }
| EXTEND_MOS STRING patch_exp { Tp.TP_Extend_Mos($2,$3) }
| COMPILE_BAF_TO_BCS { Tp.TP_CompileBAFtoBCS }
| DECOMPILE_BCS_TO_BAF { Tp.TP_CompileBCStoBAF }
| COMPILE_D_TO_DLG { Tp.TP_CompileDtoDLG }
| DECOMPILE_DLG_TO_D { Tp.TP_CompileDLGtoD }
| EVALUATE_BUFFER { Tp.TP_EvaluateBuffer }
    ;

  tp_local_declaration_list :   { [] }
| tp_local_declaration tp_local_declaration_list { $1 :: $2 }
    ;

  tp_local_declaration   :
| LOCAL_SET patch_STRING_left EQUALS patch_exp { Tp.TP_LocalSet($2,$4) }
| LOCAL_SPRINT patch_STRING_left dlg_or_patch_STRING_right     { Tp.TP_LocalSprint($2,$3) }
| LOCAL_TEXT_SPRINT patch_STRING_left patch_STRING_right { Tp.TP_LocalTextSprint($2,$3) }
    ;

  tph_file : tp_action_list { $1 }
    ;

  tpp_file : tp_patch_list { $1 }
    ;
