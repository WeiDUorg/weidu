(* Text functions
*)

open Util
open Tp

let rec pe_to_str pe = "(" ^ (match pe with
  | Pred_File_MD5(s,_) -> Printf.sprintf "FILE_MD5 %s" (pe_str_str s)
  | Pred_File_Exists(s) -> Printf.sprintf "FILE_EXISTS %s" (pe_str_str s)
  | Pred_File_Exists_In_Game(s) -> Printf.sprintf "FILE_EXISTS_IN_GAME %s"
    (pe_str_str s)
  | Pred_File_Size(s,i) -> Printf.sprintf "FILE_SIZE %s %d" (pe_str_str s) i
  | Pred_File_Contains(s,i) -> Printf.sprintf "FILE_CONTAINS %s %s"
    (pe_str_str s) (pe_str_str i)
  | Pred_File_Is_In_Compressed_Bif(a) -> Printf.sprintf "FILE_IS_IN_COMPRESSED_BIF %s" (pe_str_str a)
  | PE_String(s) -> (pe_str_str s)
  | PE_StringEqual(s1,s2,b,c) -> Printf.sprintf "%s %s%s %s"
      (pe_str_str s1) (if c then "STRING_EQUAL" else "STRING_COMPARE") (if b then "_CASE" else "")
      (pe_str_str s2)
  | PE_StringRegexp(s1,s2,b) -> Printf.sprintf "%s %s %s"
      (pe_str_str s1) (if b then "STRING_MATCHES_REGEXP"
        else "STRING_CONTAINS_REGEXP")
      (pe_str_str s2)
  | PE_Not(e) -> Printf.sprintf "NOT %s" (pe_to_str e)
  | TP_PE_Byte_At(e) -> Printf.sprintf "BYTE_AT %s" (pe_to_str e)
  | TP_PE_SByte_At(e) -> Printf.sprintf "SBYTE_AT %s" (pe_to_str e)
  | TP_PE_Short_At(e) -> Printf.sprintf "SHORT_AT %s" (pe_to_str e)
  | TP_PE_SShort_At(e) -> Printf.sprintf "SSHORT_AT %s" (pe_to_str e)
  | TP_PE_Long_At(e) -> Printf.sprintf "LONG_AT %s" (pe_to_str e)
  | TP_PE_SLong_At(e) -> Printf.sprintf "SLONG_AT %s" (pe_to_str e)
  | PE_Add(e1,e2) -> Printf.sprintf "%s + %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Sub(e1,e2) -> Printf.sprintf "%s - %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Mul(e1,e2) -> Printf.sprintf "%s * %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Div(e1,e2) -> Printf.sprintf "%s / %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Exp(e1,e2,Pred_True) -> Printf.sprintf "%s ** %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Exp(e1,e2,e3) -> Printf.sprintf "%s ** (%s / %s)" (pe_to_str e1) (pe_to_str e2) (pe_to_str e3)
  | PE_Equal(e1,e2) -> Printf.sprintf "%s = %s" (pe_to_str e1) (pe_to_str e2)
  | PE_And(e1,e2) -> Printf.sprintf "%s AND %s" (pe_to_str e1) (pe_to_str e2)
  | PE_Or(e1,e2) -> Printf.sprintf "%s OR %s" (pe_to_str e1) (pe_to_str e2)
  | PE_GT(e1,e2) -> Printf.sprintf "%s > %s" (pe_to_str e1) (pe_to_str e2)
  | PE_GTE(e1,e2) -> Printf.sprintf "%s >= %s" (pe_to_str e1) (pe_to_str e2)
  | PE_LT(e1,e2) -> Printf.sprintf "%s < %s" (pe_to_str e1) (pe_to_str e2)
  | PE_LTE(e1,e2) -> Printf.sprintf "%s <= %s" (pe_to_str e1) (pe_to_str e2)

  | PE_BAND(e1,e2) -> Printf.sprintf "%s BAND %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BOR(e1,e2) -> Printf.sprintf "%s BOR %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BXOR(e1,e2) -> Printf.sprintf "%s BXOR %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BLSL(e1,e2) -> Printf.sprintf "%s BLSL %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BLSR(e1,e2) -> Printf.sprintf "%s BLSR %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BASR(e1,e2) -> Printf.sprintf "%s BASR %s" (pe_to_str e1) (pe_to_str e2)
  | PE_BNOT(e1) -> Printf.sprintf "BNOT %s" (pe_to_str e1)
  | PE_ABS(e1) -> Printf.sprintf "ABS %s" (pe_to_str e1)
  | PE_GameIs(e1,b1) -> if b1 then Printf.sprintf "GAME_IS ~%s~" e1 else Printf.sprintf "ENGINE_IS ~%s~" e1
  | PE_ModIsInstalled(e1,e2) -> Printf.sprintf "MOD_IS_INSTALLED ~%s~ %s" e1 (pe_to_str e2)
  | Pred_True -> Printf.sprintf "1"

  | PE_Random(e1,e2) -> Printf.sprintf "RANDOM(%s %s)" (pe_to_str e1) (pe_to_str e2)
	| PE_Buffer_Length -> "BUFFER_LENGTH"
	| PE_String_Length(e1) -> Printf.sprintf "STRING_LENGTH %s" (pe_str_str e1)

  | PE_FileContainsEvaluated(s1,s2) -> Printf.sprintf "FILE_CONTAINS_EVALUATED(%s %s)" (pe_str_str s1) (pe_str_str s2)

  | PE_If(e1,e2,e3) -> Printf.sprintf "%s ? %s : %s"
    (pe_to_str e1) (pe_to_str e2) (pe_to_str e3)
  
  | PE_VariableIsSet(s) -> Printf.sprintf "VARIABLE_IS_SET %s" (pe_str_str s)
  | PE_IdsOfSymbol(a,b) -> Printf.sprintf "IDS_OF_SYMBOL (%s %s)" a b
  | PE_StateWhichSays(Some(x),None,y) -> Printf.sprintf "STATE_WHICH_SAYS __ FROM %s"  y
  | PE_StateWhichSays(None,Some(a,b),y) -> Printf.sprintf "STATE_WHICH_SAYS %s IN %s FROM %s" (pe_to_str a) b y
  | PE_StateWhichSays(None,None,_)
  | PE_StateWhichSays(Some(_),Some(_),_) -> Printf.sprintf "INTERNAL ERROR"
  | PE_IsAnInt(x) -> Printf.sprintf "IS_AN_INT %s" (pe_str_str x)    
  ) ^ ")"

and pe_str_str s = match s with
  | PE_LiteralString(s) -> s
  | PE_Evaluate(s) -> Printf.sprintf "EVALUATE_BUFFER %s" (pe_str_str s)
  | PE_Lowercase(s) -> Printf.sprintf "LOWERCASE %s" (pe_str_str s)
  | PE_Uppercase(s) -> Printf.sprintf "UPPERCASE %s" (pe_str_str s)
  | PE_Dollars(s,a,r1,r2) -> let result = List.fold_left
			(fun acc this -> acc ^ " " ^ (pe_str_str this)) ("$" ^ (pe_str_str s) ^ "(") a in
      Printf.sprintf "%s)" result


let action_to_str a = match a with
  | TP_Copy _ -> "COPY"
  | TP_ClearMemory -> "CLEAR_MEMORY"
  | TP_Clear_Ids_Map -> "CLEAR_IDS_MAP"
  | TP_ActionClearArray _ -> "ACTION_CLEAR_ARRAY"
  | TP_CopyLarge _ -> "COPY_LARGE"
  | TP_CopyAllGamFiles _ -> "COPY_ALL_GAM_FILES"
  | TP_CopyRandom _ -> "COPY_RANDOM"
  | TP_RandomSeed _ -> "RANDOM_SEED"
  | TP_Compile _ -> "COMPILE"
  | TP_Inlined_File _ -> "<<<<<<<<"
  | TP_Load_Tra _-> "LOAD_TRA"
  | TP_Mkdir _ -> "MKDIR"
  | TP_Forbid_File _ -> "FORBID_FILE"
  | TP_Require_File _ -> "REQUIRE_FILE"
  | TP_Append _ -> "APPEND"
  | TP_Append_Col _ -> "APPEND_COL"
  | TP_Set_Col _ -> "SET_COL"
  | TP_Extend_Top _ -> "EXTEND_TOP"
  | TP_Extend_Bottom _ -> "EXTEND_BOTTOM"
  | TP_At_Exit _ -> "AT_EXIT"
  | TP_At_Interactive_Exit _ -> "AT_INTERACTIVE_EXIT"
  | TP_At_Uninstall _ -> "AT_UNINSTALL"
  | TP_At_Interactive_Uninstall _ -> "AT_INTERACTIVE_UNINSTALL"
  | TP_At_Now _ -> "AT_NOW"
  | TP_At_Interactive_Now _ -> "AT_INTERACTIVE_NOW"
  | TP_Add_Kit _ -> "ADD_KIT"
  | TP_CopyKit _ -> "COPY_KIT"
  | TP_Add_Music _ -> "ADD_MUSIC"
  | TP_Add_Projectile _ -> "ADD_PROJECTILE"
  | TP_Add_Spell _ -> "ADD_SPELL"
  | TP_String_Set _ -> "STRING_SET"
  | TP_String_Set_Evaluate _ -> "STRING_SET_EVALUATE"
  | TP_String_Set_Range _ -> "STRING_SET_RANGE"
  | TP_Alter_TLK _
  | TP_Alter_TLK_Range _
  | TP_Alter_TLK_List _ -> "TP_ALTER_TLK_*"
  | TP_Fail _ -> "FAIL"
  | TP_Print _ -> "PATCH_PRINT"
  | TP_If _ -> "PATCH_IF"
  | TP_Uninstall_Now _ -> "UNINSTALL"
  | TP_ActionBashFor _ -> "ACTION_BASH_FOR"
  | TP_ActionDefineArray _ -> "ACTION_DEFINE_ARRAY"
  | TP_ActionPHPEach _ -> "ACTION_PHP_EACH"
  | TP_Action_For_Each _ -> "ACTION_FOR_EACH"
  | TP_Biff _ -> "BIFF"
  | TP_Outer_For _ -> "OUTER_FOR"
  | TP_Outer_Inner_Buff _ -> "OUTER_INNER_PATCH"
  | TP_Outer_Inner_Buff_Save _ -> "OUTER_INNER_PATCH_SAVE"
  | TP_Outer_Set _ -> "OUTER_SET"
  | TP_Outer_Sprint _ -> "OUTER_SPRINT"
  | TP_Outer_Text_Sprint _ -> "OUTER_TEXT_SPRINT"
  | TP_ActionDefineAssociativeArray _ -> "ACTION_DEFINE_ASSOCIATIVE_ARRAY"
  | TP_Outer_While _ -> "OUTER_WHILE"
  | TP_Launch_Action_Macro _ -> "LAUNCH_ACTION_MACRO"
  | TP_Launch_Action_Function _ -> "LAUNCH_ACTION_FUNCTION"
  | TP_Include _ -> "INCLUDE"
  | TP_Reinclude _ -> "REINCLUDE"
  | TP_Define_Action_Macro _ -> "DEFINE_ACTION_MACRO"
  | TP_Define_Patch_Macro _ -> "DEFINE_PATCH_MACRO"
  | TP_Define_Action_Function _ -> "DEFINE_ACTION_FUNCTION"
  | TP_Define_Patch_Function _ -> "DEFINE_PATCH_FUNCTION"
  | TP_Silent -> "SILENT"
  | TP_Verbose -> "VERBOSE"
  | TP_Action_ReadLN _ -> "ACTION_READLN"
  | TP_GetFileArray(_,_,_,false) -> "GET_FILE_ARRAY"
  | TP_GetFileArray(_,_,_,true) -> "GET_DIRECTORY_ARRAY"




(************************************************************************
 * Set some default strings that can be overwritten.
 ************************************************************************)
let init_default_strings () = 
  let add i s = 
    Dc.add_trans_strings [i,(Dlg.Local_String({
      lse_male = s; 
      lse_male_sound = s;
      lse_female = s;
      lse_female_sound = s;
    }))]
  in
  add (-1000) "The %TP2_FILE_NAME% mod has" ;
  add (-1001) "distinct optional components.\nTo save time, you can choose what to do with them at a high level rather\nthan being asked about each one.\n" ;
  add (-1002) "What should be done with all components that are NOT YET installed?\n[I]nstall them, [S]kip them, [A]sk about each one? " ;
  add (-1003) "What should be done with all components that are ALREADY installed?\n[R]e-install them, [U]ninstall them, [S]kip them, [A]sk about each one? " ;

  add (-1004) "PLEASE email the file" ;
  add (-1005) "to" ;

  add (-1006) "Install Component [" ;
  add (-1007) "]?\n[R]e-Install, [N]o Change or [U]ninstall or [Q]uit? " ;
  add (-1008) "]?\n[I]nstall, or [N]ot Install or [Q]uit? " ;

  add (-1009) "Enter part of a module name: " ;

  add (-1010) "Re-Installing [";
  add (-1011) "] component " ;
  add (-1012) "Re-Installing Using Language" ;

  add (-1013) "Removing old installation of [" ;
  add (-1014) "] first ..." ;
  add (-1015) "SUCCESSFULLY REMOVED OLD" ;

  add (-1016) "Installing" ;
  add (-1017) "ERROR Installing [" ;
  add (-1018) "], rolling back to previous state" ;
  add (-1019) "SUCCESSFULLY INSTALLED     " ;
  add (-1020) "Skipping" ;
  add (-1021) "Removing [";
  add (-1022) "] (component #";
  add (-1023) ")" ;
  add (-1024) "SUCCESSFULLY REMOVED [" ;

  add (-1025) "]?\n[R]e-install, [N]o Change, [U]ninstall, [Q]uit or choose one:" ;
  add (-1026) "]?\n[N]o, [Q]uit or choose one:" ;
  add (-1027) " (currently installed)";

  add (-1028) "Would you like to display the components from [";
  add (-1029) "]?\n[Y]es, [N]o? " ;

  add (-1030) "]?\nchoose one:" ;
  add (-1031) "]?\n[R]e-install, [N]o Change, [Q]uit or choose one:" ;
  add (-1032) "NOT INSTALLED DUE TO ERRORS";
  add (-1033) "INSTALLED WITH WARNINGS    ";
  ()

let body_of_script buff =
  if buff = "" then "" else 
  try 
    let first_nl = String.index buff '\n' in
    let last_nl = (String.rindex buff 'S') - 1 in
    if first_nl = last_nl then
      "" 
    else 
      let length = (last_nl - first_nl) - 1 in 
      String.sub buff (first_nl + 1) length 
  with e -> 
    log_and_print "ERROR: not a BCS script\n" ;
    failwith "not a BCS script"

