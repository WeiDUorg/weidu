(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Text functions
 *)

open BatteriesInit
open Hashtblinit
open Util
open Tp

let rec pe_to_str pe = "(" ^ (match pe with
| Pred_File_MD5(s,_) -> Printf.sprintf "FILE_MD5 %s" (pe_str_str s)
| Pred_File_Exists(s) -> Printf.sprintf "FILE_EXISTS %s" (pe_str_str s)
| Pred_Directory_Exists(d) -> Printf.sprintf "DIRECTORY_EXISTS %s" (pe_str_str d)
| Pred_File_Exists_In_Game(s) -> Printf.sprintf "FILE_EXISTS_IN_GAME %s"
      (pe_str_str s)
| Pred_File_Size(s,i) -> Printf.sprintf "FILE_SIZE %s %d" (pe_str_str s) i
| PE_SizeOfFile(file) -> Printf.sprintf "SIZE_OF_FILE %s" (pe_str_str file)
| Pred_File_Contains(s,i) -> Printf.sprintf "FILE_CONTAINS %s %s"
      (pe_str_str s) (pe_str_str i)
| Pred_File_Is_In_Compressed_Bif(a) ->
    Printf.sprintf "FILE_IS_IN_COMPRESSED_BIF %s" (pe_str_str a)
| Pred_Biff_Is_Compressed(a) ->
    Printf.sprintf "BIFF_IS_COMPRESSED %s" (pe_str_str a)
| PE_Int32(i) -> Int32.to_string i
| PE_Int(i) -> string_of_int i
| PE_String(s) -> (pe_str_str s)
| PE_StringEqual(s1,s2,b,c) -> Printf.sprintf "%s %s%s %s"
      (pe_str_str s1) (if c then "STRING_EQUAL" else
      "STRING_COMPARE") (if b then "_CASE" else "")
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
| PE_Mod(e1,e2) -> Printf.sprintf "%s MODULO %s" (pe_to_str e1) (pe_to_str e2)
| PE_Exp(e1,e2,Pred_True) -> Printf.sprintf "%s ** %s"
      (pe_to_str e1) (pe_to_str e2)
| PE_Exp(e1,e2,e3) -> Printf.sprintf "%s ** (%s / %s)"
      (pe_to_str e1) (pe_to_str e2) (pe_to_str e3)
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
| PE_GameIs(e1,b1) -> if b1 then
    Printf.sprintf "GAME_IS ~%s~" e1 else Printf.sprintf "ENGINE_IS ~%s~" e1
| PE_GameIncludes(e1) -> Printf.sprintf "GAME_INCLUDES ~%s~" e1
| PE_ModIsInstalled(e1,e2) ->
    Printf.sprintf "MOD_IS_INSTALLED ~%s~ %s" e1 (pe_to_str e2)
| PE_IsInstalledAfter(e1,e2,e3,e4) ->
    Printf.sprintf "%s %s IS_INSTALLED_AFTER %s %s"
      (pe_str_str e1) (pe_to_str e2) (pe_str_str e3) (pe_to_str e4)
| PE_IdOfLabel(e1,e2) -> Printf.sprintf "ID_OF_LABEL %s %s"
      (pe_str_str e1) (pe_str_str e2)
| Pred_True -> Printf.sprintf "1"

| PE_Random(e1,e2) -> Printf.sprintf "RANDOM(%s %s)"
      (pe_to_str e1) (pe_to_str e2)
| PE_Buffer_Length -> "BUFFER_LENGTH"
| PE_String_Length(e1) -> Printf.sprintf "STRING_LENGTH %s" (pe_str_str e1)
| PE_Index(e0,e1,e2,e3,e4,e5) -> Printf.sprintf "%sINDEX (%s %s %s%s%s)"
      (if e0 then "" else "R")
      (if e1 = Some true then "CASE_SENSITIVE" else "CASE_INSENSITIVE")
      (if e2 = Some true then "EXACT_MATCH" else "EVALUATE_REGEXP")
      (pe_str_str e3)
      (match e4 with
      | None -> ""
      | Some x -> " " ^ pe_to_str x)
      (match e5 with
      | None -> ""
      | Some x -> " " ^ pe_str_str x)

| PE_FileContainsEvaluated(s1,s2) ->
    Printf.sprintf "FILE_CONTAINS_EVALUATED(%s %s)"
      (pe_str_str s1) (pe_str_str s2)

| PE_ResourceContains(s1, s2) ->
    Printf.sprintf "RESOURCE_CONTAINS %s %s" (pe_str_str s1) (pe_str_str s2)

| PE_If(e1,e2,e3) -> Printf.sprintf "%s ? %s : %s"
      (pe_to_str e1) (pe_to_str e2) (pe_to_str e3)

| PE_VariableIsSet(s) -> Printf.sprintf "VARIABLE_IS_SET %s" (pe_str_str s)
| PE_TraEntryExists(a,b) ->
    Printf.sprintf "TRA_ENTRY_EXISTS (%s %s)" (pe_str_str a)
      (if b <> [] then (List.fold_left (fun acc elt ->
        acc ^ " " ^ pe_str_str elt)
                          (pe_str_str (List.hd b)) (List.tl b)) else "")
| PE_IdsOfSymbol(a,b) -> Printf.sprintf "IDS_OF_SYMBOL (%s %s)" a b
| PE_StateWhichSays(Some(x),None,y) ->
    Printf.sprintf "STATE_WHICH_SAYS __ FROM %s"  y
| PE_StateWhichSays(None,Some(a,b),y) ->
    Printf.sprintf "STATE_WHICH_SAYS %s IN %s FROM %s" (pe_to_str a) b y
| PE_StateWhichSays(None,None,_)
| PE_StateWhichSays(Some(_),Some(_),_) -> Printf.sprintf "INTERNAL ERROR"
| PE_Resolve_Str_Ref(a) -> Printf.sprintf "RESOLVE_STR_REF (__)"
| PE_IsSilent -> "IS_SILENT"
| PE_IsAnInt(x) -> Printf.sprintf "IS_AN_INT %s" (pe_str_str x)
| PE_NextStrref -> "NEXT_STRREF"
| PE_ValidScriptActions s -> Printf.sprintf "VALID_SCRIPT_ACTIONS %s"
      (pe_str_str s)
| PE_ValidScriptTriggers s -> Printf.sprintf "VALID_SCRIPT_TRIGGERS %s"
      (pe_str_str s)
                             ) ^ ")"

and pe_str_str s = match s with
| PE_LiteralString(s) -> s
| PE_GetVar(s) -> Printf.sprintf "GET_VAR %s" (pe_str_str s)
| PE_Evaluate(s) -> Printf.sprintf "EVALUATE_BUFFER %s" (pe_str_str s)
| PE_Lowercase(s) -> Printf.sprintf "LOWERCASE %s" (pe_str_str s)
| PE_Uppercase(s) -> Printf.sprintf "UPPERCASE %s" (pe_str_str s)
| PE_Dollars(s,a,r1,r2) -> let result = List.fold_left
      (fun acc this ->
        acc ^ " " ^ (pe_str_str this)) ("$" ^ (pe_str_str s) ^ "(") a in
  Printf.sprintf "%s)" result


let action_to_str a = match a with
| TP_Copy _ -> "COPY"
| TP_Move _ -> "MOVE"
| TP_DisableFromKey _ -> "DISABLE_FROM_KEY"
| TP_ClearMemory -> "CLEAR_MEMORY"
| TP_ClearArrays -> "CLEAR_ARRAYS"
| TP_ClearCodes -> "CLEAR_CODES"
| TP_ClearInlined -> "CLEAR_INLINED"
| TP_Clear_Ids_Map -> "CLEAR_IDS_MAP"
| TP_ClearEverything -> "CLEAR_EVERYTHING"
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
| TP_At_Uninstall_Exit _ -> "AT_UNINSTALL_EXIT"
| TP_At_Interactive_Uninstall_Exit _ -> "AT_INTERACTIVE_UNINSTALL_EXIT"
| TP_At_Now _ -> "AT_NOW"
| TP_At_Interactive_Now _ -> "AT_INTERACTIVE_NOW"
| TP_Add_Kit _ -> "ADD_KIT"
| TP_CopyKit _ -> "COPY_KIT"
| TP_Add_Music _ -> "ADD_MUSIC"
| TP_Add_AreaType _ -> "ADD_AREA_TYPE"
| TP_Add_Projectile _ -> "ADD_PROJECTILE"
| TP_Add_2DA ("MSCHOOL.2DA",_,_) -> "ADD_SCHOOL"
| TP_Add_2DA ("MSECTYPE.2DA",_,_) -> "ADD_SECTYPE"
| TP_Add_2DA (_,_,_) -> "ADD_2DA"
| TP_Add_Spell _ -> "ADD_SPELL"
| TP_String_Set _ -> "STRING_SET"
| TP_String_Set_Evaluate _ -> "STRING_SET_EVALUATE"
| TP_String_Set_Range _ -> "STRING_SET_RANGE"
| TP_Alter_TLK _
| TP_Alter_TLK_Range _
| TP_Alter_TLK_List _ -> "TP_ALTER_TLK_*"
| TP_Reraise -> "ACTION_RERAISE"
| TP_Fail _ -> "FAIL"
| TP_Abort _ -> "ABORT"
| TP_Warn _ -> "WARN"
| TP_Print _ -> "PRINT"
| TP_Log _ -> "LOG"
| TP_If _ -> "ACTION_IF"
| TP_ActionTry _ -> "ACTION_TRY"
| TP_ActionMatch _ -> "ACTION_MATCH"
| TP_Uninstall_Now _ -> "UNINSTALL"
| TP_ActionBashFor _ -> "ACTION_BASH_FOR"
| TP_ActionDefineArray _ -> "ACTION_DEFINE_ARRAY"
| TP_ActionSortArrayIndices _ -> "ACTION_SORT_ARRAY_INDICES"
| TP_ActionPHPEach _ -> "ACTION_PHP_EACH"
| TP_Action_For_Each _ -> "ACTION_FOR_EACH"
| TP_Biff _ -> "BIFF"
| TP_Outer_For _ -> "OUTER_FOR"
| TP_Outer_Inner_Buff _ -> "OUTER_INNER_PATCH"
| TP_Outer_Inner_Buff_Save _ -> "OUTER_INNER_PATCH_SAVE"
| TP_Outer_Set _ -> "OUTER_SET"
| TP_Outer_Sprint _ -> "OUTER_SPRINT"
| TP_Outer_Text_Sprint _ -> "OUTER_TEXT_SPRINT"
| TP_Outer_Snprint _ -> "OUTER_SNPRINT"
| TP_ActionDefineAssociativeArray _ -> "ACTION_DEFINE_ASSOCIATIVE_ARRAY"
| TP_Outer_While _ -> "OUTER_WHILE"
| TP_Launch_Action_Macro _ -> "LAUNCH_ACTION_MACRO"
| TP_Launch_Action_Function _ -> "LAUNCH_ACTION_FUNCTION"
| TP_Include _ -> "INCLUDE"
| TP_Reinclude _ -> "REINCLUDE"
| TP_Define_Action_Macro _ -> "DEFINE_ACTION_MACRO"
| TP_Define_Patch_Macro _ -> "DEFINE_PATCH_MACRO"
| TP_Define_Action_Function _ -> "DEFINE_ACTION_FUNCTION"
| TP_Define_Dimorphic_Function _ -> "DEFINE_DIMORPHIC_FUNCTION"
| TP_Define_Patch_Function _ -> "DEFINE_PATCH_FUNCTION"
| TP_Silent -> "SILENT"
| TP_Verbose -> "VERBOSE"
| TP_Action_ReadLN _ -> "ACTION_READLN"
| TP_GetFileArray(_,_,_,false) -> "GET_FILE_ARRAY"
| TP_GetFileArray(_,_,_,true) -> "GET_DIRECTORY_ARRAY"
| TP_DecompressBiff _ -> "DECOMPRESS_BIFF"
| TP_ActionToLower _ -> "ACTION_TO_LOWER"
| TP_ActionToUpper _ -> "ACTION_TO_UPPER"
| TP_ActionGetStrRef _ -> "ACTION_GET_STRREF"
| TP_Delete _ -> "DELETE"
| TP_AddJournal _ -> "ADD_JOURNAL"
| TP_Create _ -> "CREATE"
| TP_WithTra (_, _) -> "WITH_TRA"
| TP_WithVarScope (_) -> "WITH_SCOPE"
| TP_ActionTime(_, _) -> "ACTION_TIME"


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
                                            }))] in
  add (-1000) "The %TP2_FILE_NAME% mod has" ;
  add (-1001) "distinct optional components.\nTo save time, you can choose \
    what to do with them at a high level rather\nthan being asked about \
  each one.\n" ;
  add (-1002) "What should be done with all components that are NOT YET \
  installed?\n[I]nstall them, [S]kip them, [A]sk about each one? " ;
  add (-1003) "What should be done with all components that are ALREADY \
  installed?\n[R]e-install them, [U]ninstall them, [S]kip them, \
  [A]sk about each one? " ;

  add (-1004) "Please make a backup of the file:" ;
  add (-1005) "and look for support at:" ;

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

  add (-1025) "]?\n[R]e-install, [N]o Change, [U]ninstall, \
  [Q]uit or choose one:" ;
  add (-1026) "]?\n[N]o, [Q]uit or choose one:" ;
  add (-1027) " (currently installed)";

  add (-1028) "Would you like to display the components from [";
  add (-1029) "]?\n[Y]es, [N]o? " ;

  add (-1030) "]?\nchoose one:" ;
  add (-1031) "]?\n[R]e-install, [N]o Change, [Q]uit or choose one:" ;
  add (-1032) "NOT INSTALLED DUE TO ERRORS";
  add (-1033) "INSTALLED WITH WARNINGS    ";
  add (-1034) "Would you like to display the readme? [Y]es [N]o";
  add (-1035) "Using Language";

  add (-1036) "Skipping GROUP [";
  add (-1037) "] because it fails its requirements.";

  add (-1038) "[A]sk about each component, [S]kip all, or choose a \
  pre-defined selection:";
  add (-1039) "[A]sk about each component, [R]einstall the current \
  configuration, [U]ninstall all, [S]kip all, or choose a \
  pre-defined selection:";

  add (-1040) "\nPlease choose the language in which you wish to play \
  the game.\nIf you later wish to play the game in a different language \
  you will need to edit the file weidu.conf and reinstall your mods.\n" ;

  add (-1041) "Czech" ;
  add (-1042) "German" ;
  add (-1043) "English" ;
  add (-1044) "Spanish" ;
  add (-1045) "French" ;
  add (-1046) "Italian" ;
  add (-1047) "Polish" ;
  add (-1048) "Portuguese" ;
  add (-1049) "Turkish" ;
  add (-1050) "Japanese" ;
  add (-1051) "Korean" ;
  add (-1052) "Simplified Chinese" ;
  add (-1053) "Russian" ;
  add (-1054) "Ukrainian" ;
  (* might be a good idea to leave a few numbers here,
   * in case there are additional translations made *)

  add (-1060) "\nThis game is available in multiple languages, but \
  WeiDU does not know which game language was used when this mod was \
  installed.\n\nPlease indicate which game language was used when you \
  installed this mod. Your choice will be used until WeiDU exits but \
  will not be remembered.\n" ;
  add (-1061) "\nUsing" ;
  add (-1062) " and " ;
  add (-1063) "INSTALLATION ABORTED" ;
  add (-1064) "Aborting installation of [" ;
  add (-1065) "], rolling back to previous state" ;
  ()

let get_trans i =
  Dc.single_string_of_tlk_string (Load.the_game ())
    (Dlg.Trans_String (Dlg.Int i))

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


let find_table_row buff colId reg =
  let lines = Str.split many_newline_or_cr_regexp buff in
  let rec walk lines i = match lines with
  | line :: tl ->
      let parts = Str.split many_whitespace_regexp line in
      if List.length parts > colId then begin
        if Str.string_match reg (List.nth parts colId) 0 then
          Int32.of_int i
        else walk tl (i+1)
      end else walk tl (i+1)
  | [] -> failwith ("find_table_row: couldn't find a match")
  in walk lines 0

let get_line_count file game =
  let buff = if file_exists file then
    load_file file
  else
    let a,b = split_resref file in
    let buff,path =
      Load.load_resource "FILE_CONTAINS_EVALUATED" game true a b
    in
    buff
  in
  let lines = List.tl (Str.split many_newline_or_cr_regexp buff) in
  let rec walk lines max' = match lines with
  | hd :: tl -> let cur = List.length (Str.split many_whitespace_regexp hd) in
    walk tl (max cur max')
  | [] -> max'
  in
  let max' = walk lines 0 in
  let rec count lines acc = match lines with
  | hd :: tl ->
      count tl (acc + if List.length
          (Str.split many_whitespace_regexp hd) = max' then 1 else 0)
  | [] -> acc
  in
  count lines 0

let check_missing_eval for_what str =
  if Modder.enabled "MISSING_EVAL" then begin
    let check s =
      if Var.get_string s <> s then begin
        Modder.handle_deb "MISSING_EVAL"
          (Printf.sprintf
             "\nWARNING: possible missing EVALUATE_BUFFER in\n[%s]\n"
             for_what);
        true
      end else begin
        false
      end
    in
    ignore ((check str) || (check ("%" ^ str ^ "%")))
  end

let version_greater i c =
  let installed =
    Str.split (Str.regexp_string ".")
      (Str.global_replace many_whitespace_or_nl_regexp "" i) in
  let cmp = Str.split (Str.regexp_string ".") c in
  let rec compare installed cmp = match installed, cmp with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 ->
      log_and_print "compare %s %s\n" hd1 hd2;
      if (int_of_string hd1) < (int_of_string hd2) then false
      else if (int_of_string hd1) > (int_of_string hd2) then true
      else compare tl1 tl2
  | _ -> failwith (Printf.sprintf
                     "version numbers have different lengths: [%s] vs [%s]"
                     i c) in
  compare installed cmp

let checks_passed = Hashtbl.create 5

let check_enhanced_engine game allow_tobhacks allow_tobex allow_gemrb allow_bgee =
  if Hashtbl.mem checks_passed (allow_tobhacks, allow_tobex, allow_gemrb, allow_bgee) then
    Hashtbl.find checks_passed (allow_tobhacks, allow_tobex, allow_gemrb, allow_bgee)
  else begin
    let ans = if allow_bgee && (Load.enhanced_edition_p game) then
      true
    else if allow_gemrb && Util.file_exists "gemrb_path.txt" then
      true
    else if
      (match allow_tobex with
      | None -> false
      | Some cmp_version ->
          if file_exists "tobex_ini/tobexver.txt" then begin
            let tobex_version =
              int_of_string (String.trim (load_file "tobex_ini/tobexver.txt")) in
            tobex_version >= cmp_version
          end else false) then
      true
    else
      (match allow_tobhacks with
      | None -> false
      | Some signature ->
          if file_exists "bgmain.exe" then begin
            let all_match = ref true in
            let count =
              Int32.to_int (Var.get_int32_extended (signature ^ "_count")) in
            if count <= 0 then false else begin
              let bgmain_buff = load_file "bgmain.exe" in
              for i = 1 to count do
                let offset = Int32.to_int
                    (Var.get_int32_extended
                       (signature ^ "_address_" ^ string_of_int i)) in
                let signature = Var.get_string_exact
                    ("%" ^ signature ^ "_patch_bytes_" ^
                     string_of_int i ^ "%") in
                let cmp = String.sub bgmain_buff offset
                    (String.length signature) in
                if (signature <> cmp) then begin
                  all_match := false;
                end else begin
                end
              done;
              !all_match
            end
          end else false) in
    if ans then
      Hashtbl.add checks_passed (allow_tobhacks, allow_tobex, allow_gemrb, allow_bgee) true;
    ans
  end

let ask_about_lang_dir ask_text: string =
  let languages = Load.bgee_language_options (Load.the_game ()) in
  let pretty_ht = Hashtbl.create (Array.length languages) in
  ignore (List.iter (fun (dirname, pretty) ->
    Hashtbl.add pretty_ht dirname pretty)
            [("cs_cz", (get_trans (-1041))); ("de_de", (get_trans (-1042)));
             ("en_us", (get_trans (-1043))); ("es_es", (get_trans (-1044)));
             ("fr_fr", (get_trans (-1045))); ("it_it", (get_trans (-1046)));
             ("pl_pl", (get_trans (-1047))); ("pt_br", (get_trans (-1048)));
             ("tr_tr", (get_trans (-1049))); ("ja_jp", (get_trans (-1050)));
             ("ko_kr", (get_trans (-1051))); ("zh_cn", (get_trans (-1052)));
             ("ru_ru", (get_trans (-1053))); ("uk_ua", (get_trans (-1054)));]) ;
  let prettify dir =
    if Hashtbl.mem pretty_ht dir then
      Hashtbl.find pretty_ht dir
    else dir in
  let answer = ref None in
  while !answer = None do
    log_and_print "%s" ask_text ;
    Array.iteri (fun i dir ->
      log_and_print "%2d [%s]\n" i dir) (Array.map prettify languages) ;
    try
      let i = read_int () in
      if i >= 0 && i < Array.length languages then begin
        answer := Some (Array.get languages i) ;
      end
    with _ -> ()
  done ;
  value_of_option !answer

let set_copy_vars src dest buff =
  let src_dir = Case_ins.filename_dirname src in
  Var.set_string "SOURCE_DIRECTORY" src_dir ;
  Var.set_string "SOURCE_FILESPEC" src ;
  Var.set_string "SOURCE_FILE" (Case_ins.filename_basename src) ;
  Var.set_string "SOURCE_RES"
    (let a,b = split_resref (Case_ins.filename_basename src) in a) ;
  Var.set_string "SOURCE_EXT"
    (let a,b = split_resref (Case_ins.filename_basename src) in b) ;
  let dest_dir = Case_ins.filename_dirname dest in
  Var.set_string "DEST_DIRECTORY" dest_dir ;
  Var.set_string "DEST_FILESPEC" dest ;
  Var.set_string "DEST_FILE" (Case_ins.filename_basename dest) ;
  Var.set_string "DEST_RES"
    (let a,b = split_resref (Case_ins.filename_basename dest) in a) ;
  Var.set_string "DEST_EXT"
    (let a,b = split_resref (Case_ins.filename_basename dest) in b) ;
  (match buff with
  | None -> Var.set_int32 "SOURCE_SIZE" (Int32.of_int ((Case_ins.unix_stat src).Unix.st_size))
  | Some(b) ->
      Var.set_int32 "SOURCE_SIZE" (Int32.of_int (String.length b)))
