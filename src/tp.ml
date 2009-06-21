(* TODO:
 *)

(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.tp.ml.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

(* Talk-Patcher / Installer *)
open Util

let always_yes = ref false
let always_uninstall = ref false
let sometimes_reinstall = ref false
let ask_all = ref false
let forced_language = ref ( 0-1 )
let force_install_these : int list ref = ref []
let force_uninstall_these : int list ref = ref []
let specified_specific_components = ref false
let skip_at_view = ref false
let quick_log = ref false
let debug_boiic = ref false
let debug_change = ref false

let has_if_eval_bug = ref true
let continue_on_error = ref false
let debug_pe = ref false

type tp_flag =
| Version of Dlg.tlk_string
| Auto_Tra of string
| TP_No_If_Eval of unit
| Menu_Style of string
| Ask_Every_Component
| Always of tp_action list
| Define_Action_Macro of string * tp_local_declaration list * tp_action list
| Define_Patch_Macro of string * tp_local_declaration list * tp_patch list
| Allow_Missing of string list
| Script_Style of Load.script_style
| Load_Macro of string list
| Readme of string list

and wrapper =
| Start_From_Tp  of tp_file
| Start_From_Tpa of tp_action list
| Start_From_Tpp of tp_patch list

and tp_file = {
  mutable tp_filename  : string ;
  backup    : string ;
  author    : string ;
  flags     : tp_flag list ;
  languages : tp_lang list ;
  module_list : tp_mod list ;
}

and tp_lang = {
  lang_name : string ;
  lang_dir_name : string ;
  lang_tra_files : string list ;
}

and tp_mod = {
  mod_name : Dlg.tlk_string ;
  mod_parts : tp_action list;
  mod_flags : tp_mod_flag list ;
}

and tp_mod_flag =
| TPM_Deprecated of Dlg.tlk_string (* should be uninstalled when encountered *)
| TPM_RequireComponent of string * int * Dlg.tlk_string
| TPM_ForbidComponent of string * int * Dlg.tlk_string
| TPM_RequirePredicate of tp_patchexp * Dlg.tlk_string
| TPM_SubComponents of Dlg.tlk_string * tp_patchexp * bool (* is forced? *)
| TPM_Designated of int
| TPM_NotInLog
| TPM_InstallByDefault
| TPM_Group of Dlg.tlk_string

and tp_copy_args = {
  copy_get_existing    : bool ;  (* get from biffs? *)
  copy_use_regexp      : bool ;
  copy_use_glob        : bool ;
  copy_file_list       : ( string * string ) list ; (* (source,dest) list *)
  copy_patch_list      : tp_patch list ;
  copy_constraint_list : tp_constraint list ;
  copy_backup          : bool ; (* normally TRUE *)
  copy_at_end          : bool ;
    (* write all of the output *after* doing all of the processing for
     * all souce files *)
  copy_save_inlined    : bool ; (* the result is not saved to the HD, but stored in the inlined Hashtable *)
}

and tp_copy_large_args = {
  copy_large_use_glob       : bool ;
  copy_large_file_list      : ( string * string ) list ; (* (source,dest) list *)
  copy_large_backup         : bool ; (* normally TRUE *)
}

and store_args = {
  overwrite_store_item  : bool ; (* normally TRUE *)
}

and tp_action =
  | TP_ActionBashFor of ((string * (bool option) * string) list) * (tp_action list)
  | TP_ActionDefineArray of tp_pe_string * string list
  | TP_ActionPHPEach of tp_pe_string * tp_pe_string * tp_pe_string * tp_action list
  | TP_Action_For_Each of tp_pe_string * string list * tp_action list
  | TP_Action_ReadLN of tp_pe_string
  | TP_ClearMemory
  | TP_Clear_Ids_Map
  | TP_ActionClearArray of tp_pe_string
  | TP_CopyAllGamFiles of tp_patch list * tp_constraint list
  | TP_Copy   of tp_copy_args
  | TP_CopyLarge of tp_copy_large_args
  | TP_CopyRandom of (string list list) * (tp_patch list) * (tp_constraint list)
  | TP_RandomSeed of tp_patchexp
  | TP_Compile of bool * (string list) * (tp_patch list) * (string list) (* eval, DLG, TRA *)
  | TP_Launch_Action_Macro of string
  | TP_Launch_Action_Function of string * (tp_pe_string * tp_patchexp) list *
	   (tp_pe_string * tp_pe_string) list * (tp_pe_string * tp_pe_string) list
  | TP_Reinclude of string list
  | TP_Include of string list
  | TP_Load_Tra of string list
  | TP_Inlined_File of (string * string)
  | TP_GetFileArray of tp_pe_string * tp_pe_string * tp_pe_string * bool
  | TP_Define_Action_Macro of string * tp_local_declaration list * tp_action list
  | TP_Define_Patch_Macro of string * tp_local_declaration list * tp_patch list
  | TP_Define_Patch_Function of string * (tp_pe_string * tp_patchexp) list *
	   (tp_pe_string * tp_pe_string) list * tp_pe_string list * tp_patch list
  | TP_Define_Action_Function of string * (tp_pe_string * tp_patchexp) list *
	   (tp_pe_string * tp_pe_string) list * tp_pe_string list * tp_action list
  | TP_Biff of string * ((string * (bool option) * string) list)
  | TP_Mkdir of string list
  | TP_Outer_For of (tp_patch list) * tp_patchexp * (tp_patch list) * (tp_action list)
  | TP_Outer_Inner_Buff of string * (tp_patch list)
  | TP_Outer_Inner_Buff_Save of tp_pe_string * string * (tp_patch list)
  | TP_Outer_Set of tp_pe_string * tp_patchexp
  | TP_Outer_Sprint of tp_pe_string * tp_pe_tlk_string
  | TP_Outer_Text_Sprint of tp_pe_string * tp_pe_string
  | TP_ActionDefineAssociativeArray of tp_pe_string * (tp_pe_string * tp_pe_string) list
  | TP_Outer_While of tp_patchexp * (tp_action list)
  | TP_Require_File of string * (Dlg.tlk_string)
  | TP_Forbid_File of string * (Dlg.tlk_string)
  | TP_Append of string * string * (tp_constraint list) * bool * bool
        (* When bool is true, load the file as bif or override; otherwise, it's loaded
         * as a normal file.
         * When bool is true, keep CRLF as they are. Otherwise, apply Weimer's wacky
      	 * CRLF handling.
         *)
  | TP_Append_Col of string * string * tp_patchexp * (tp_constraint list)
        (* prepend to the string int blank entries (EG for ADD_KIT) *)
  | TP_Set_Col of string * (string list) * int
  | TP_Extend_Top of bool * string * string * (tp_patch list) * (string list)
  | TP_Extend_Bottom of bool * string * string * (tp_patch list) * (string list)
  | TP_At_Exit of string * bool
  | TP_At_Interactive_Exit of string * bool
  | TP_At_Uninstall of string * bool
  | TP_At_Interactive_Uninstall of string * bool
  | TP_At_Now of string * bool
  | TP_At_Interactive_Now of string * bool
  | TP_Add_Kit of tp_add_kit
  | TP_CopyKit of string * string * (string* string) list
      (* old kit, new kit, changes *)
  | TP_Add_Music of tp_add_music
  | TP_Add_Projectile of tp_add_projectile
  | TP_Add_Spell of string * tp_patchexp * tp_patchexp * string * (tp_patch list) * (tp_constraint list)
  | TP_Silent
  | TP_Verbose
  | TP_String_Set of ((string * Dlg.tlk_string) list) * (string option)
  | TP_String_Set_Evaluate of ((tp_patchexp * Dlg.tlk_string) list) * (string option)
  | TP_String_Set_Range of tp_patchexp * tp_patchexp * string
  | TP_Fail of Dlg.tlk_string
  | TP_Print of Dlg.tlk_string
  | TP_If of tp_patchexp * (tp_action list) * (tp_action list)
  | TP_Uninstall_Now of string * tp_patchexp
  | TP_Alter_TLK of (tp_patch list)
  | TP_Alter_TLK_Range of tp_patchexp * tp_patchexp * (tp_patch list)
  | TP_Alter_TLK_List  of (tp_patchexp list) * (tp_patch list)

(*
and predicate =
  | Pred_And of predicate * predicate
  | Pred_Or of predicate * predicate
  | Pred_Not of predicate
  | Pred_True
  | Pred_Expr of tp_patchexp
  *)

and tp_add_kit = {
  kit_name : string ;
  clasweap : string ;
  weapprof : string ;
  abclasrq : string ;
  abclsmod : string ;
  abdcdsrq : string ;
  abdcscrq : string ;
  dualclas : string ;
  alignmnt : string ;
  ability_file : string ; 
  include_in : string ;
  lower : Dlg.tlk_string ;
  mixed : Dlg.tlk_string ;
  help : Dlg.tlk_string ;
  unused_class : string ;
  tob_start : string list ;
  tob_abbr : string ; 
} 

and tp_add_map_note = {
  xcoord : tp_patchexp ;
  ycoord : tp_patchexp ;
  mstring : Dlg.tlk_string ;
  colour : string ;
}

and tp_add_cre_item = {
  item_name : string ;
  i_charge1 : tp_patchexp ;
  i_charge2 : tp_patchexp ;
  i_charge3 : tp_patchexp ;
  i_flags : string ;
  item_slot : string ;
  equip : bool ; (* Normally FALSE *)
  twohanded_weapon : bool ; (* Normally TRUE *)
}

and tp_add_music = {
  music_name : string ;
  music_file : string ;
}

and tp_add_projectile = {
  pro_file : string ;
  missile_ids_name : string;
}

and tp_local_declaration =
  | TP_LocalSet of tp_pe_string * tp_patchexp
  | TP_LocalSprint of tp_pe_string * tp_pe_tlk_string
  | TP_LocalTextSprint of tp_pe_string * tp_pe_string

and tp_patch =
  | TP_PatchBashFor of ((string * (bool option) * string) list) * (tp_patch list)
  | TP_PatchClearArray of tp_pe_string
  | TP_PatchDefineArray of tp_pe_string * string list
  | TP_DefineAssociativeArray of tp_pe_string * (tp_pe_string * tp_pe_string) list
  | TP_PatchPHPEach of tp_pe_string * tp_pe_string * tp_pe_string * tp_patch list
  | TP_PatchForEach of tp_pe_string * string list * tp_patch list
  | TP_PatchStrRef of tp_patchexp * Dlg.tlk_string (* offset + text *)
  | TP_PatchStrRefEvaluated of tp_patchexp * string
  | TP_PatchString of (bool option) * (bool option) * string * Dlg.tlk_string (* regexp + text *)
  | TP_PatchStringTextually of (bool option) * (bool option) * string * string * (tp_patchexp option) (* regexp + text *)
  | TP_PatchStringEvaluate of (bool option) * string * (tp_patch list) * string (* see below *)
  | TP_PatchReplaceBCSBlock of string * string (* old + new *)
  | TP_PatchReplaceBCSBlockRE of string * string (* old + new *)
  | TP_PatchApplyBCSPatch of string (* patch *) * (string option) (* copyover *)
  | TP_PatchByte of tp_patchexp * tp_patchexp
  | TP_PatchShort of tp_patchexp * tp_patchexp
  | TP_PatchLong of tp_patchexp * tp_patchexp
  | TP_PatchGetStrRef of tp_patchexp * tp_pe_string * bool * bool
  | TP_PatchReadAscii of tp_patchexp * tp_pe_string * (string option) * tp_patchexp * bool
  | TP_PatchReadStrRef of tp_patchexp * tp_pe_string * (string option) * bool * bool
  | TP_PatchReadByte of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadSByte of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadSShort of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadShort of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadLong of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadSLong of tp_patchexp * tp_pe_string * (tp_patchexp option)
  | TP_PatchReadLN of tp_pe_string
  | TP_PatchGetOffsetArray of tp_pe_string * (tp_patchexp * tp_patchexp * tp_patchexp *
                          tp_patchexp * tp_patchexp * tp_patchexp * tp_patchexp)
  | TP_PatchGetOffsetArray2 of tp_pe_string * (tp_patchexp * tp_patchexp * tp_patchexp
          * tp_patchexp * tp_patchexp * tp_patchexp * tp_patchexp * tp_patchexp)
  | TP_Launch_Patch_Macro of string
  | TP_Launch_Patch_Function of string* (tp_pe_string * tp_patchexp) list *
	   (tp_pe_string * tp_pe_string) list * (tp_pe_string * tp_pe_string) list
  | TP_Add_Known_Spell of string * tp_patchexp * string
  | TP_Add_Memorized_Spell of string * tp_patchexp * string * tp_patchexp
  | TP_Read2DA of tp_patchexp * tp_patchexp * tp_patchexp * tp_pe_string
  | TP_Read2DANow of string * tp_patchexp
  | TP_Read2DAFormer of string * tp_patchexp * tp_patchexp * string
  | TP_Get2DARows of tp_patchexp * tp_pe_string
  | TP_Get2DACols of tp_pe_string
  | TP_CountRegexpInstances of (bool option) * (bool option) * string * tp_pe_string
  | TP_DescribeItem of string
  | TP_PatchPrint of Dlg.tlk_string
  | TP_PatchSprint of tp_pe_string * tp_pe_tlk_string
  | TP_PatchSprintf of tp_pe_string * tp_pe_tlk_string * tp_patchexp list
  | TP_PatchTextSprint of tp_pe_string * tp_pe_string
  | TP_PatchSpaces of tp_pe_string * tp_pe_string
  | TP_PatchQuote of tp_pe_string * tp_pe_string
  | TP_PatchToLower of tp_pe_string
  | TP_PatchToUpper of tp_pe_string
  | TP_PatchSnprint of tp_patchexp * tp_pe_string * tp_pe_tlk_string
  | TP_PatchASCII of
      tp_patchexp (* where? *)
    * tp_pe_string (* what? *)
    * bool (* evaluate? *)
    * (tp_patchexp option) (* minimum size? *)
  | TP_PatchASCIITerminated of
      tp_patchexp (* where? *)
    * string (* what? *)
  | TP_PatchInsertBytes of tp_patchexp * tp_patchexp
  | TP_PatchDeleteBytes of tp_patchexp * tp_patchexp
  | TP_PatchSet of tp_pe_string * tp_patchexp
  | TP_PatchSetIdsSymOfInt of string * string * tp_patchexp
  | TP_PatchWhile of tp_patchexp * (tp_patch list)
  | TP_PatchFor of
    (tp_patch list) * tp_patchexp * (tp_patch list) * (tp_patch list)
  | TP_PatchIf of tp_patchexp * (tp_patch list) * (tp_patch list)
  | TP_PatchReinclude of string list
  | TP_PatchInclude of string list
  | TP_PatchRandomSeed of tp_patchexp
  | TP_PatchInnerAction of tp_action list
  | TP_PatchInnerBuff of string * (tp_patch list)
  | TP_PatchInnerBuffFile of string * (tp_patch list)
  | TP_PatchInnerBuffSave of tp_pe_string * string * (tp_patch list)
  | TP_PatchInsert2DARow of tp_patchexp * tp_patchexp * tp_patchexp
  | TP_Patch2DA of tp_patchexp * tp_patchexp * tp_patchexp * tp_patchexp
  | TP_Patch2DALater of string * tp_patchexp * tp_patchexp * tp_patchexp
  | TP_Patch2DANow of string * tp_patchexp
  | TP_Remove_2DA_Row of tp_patchexp * tp_patchexp
  | TP_PrettyPrint2DA of tp_patchexp
  | TP_PatchSilent
  | TP_PatchVerbose


  | TP_PatchSavFile of tp_patchexp * (tp_patch list)
  | TP_Add_Map_Note of tp_add_map_note
  | TP_Patch_Gam of string * string * tp_patchexp * tp_patchexp
  | TP_Add_Cre_Item of tp_add_cre_item
  | TP_Replace_Cre_Item of tp_add_cre_item
  | TP_Add_S_Item of store_args * string * tp_store_position * tp_patchexp * tp_patchexp * tp_patchexp * string * tp_patchexp * (string option)
  | TP_Remove_Cre_Item of string list
  | TP_Set_BG2_Proficiency of tp_patchexp * tp_patchexp
  | TP_Remove_Known_Spell of string list
  | TP_Remove_Memorized_Spell of string list
  | TP_Remove_Store_Item of string list
  | TP_Remove_Known_Spells
  | TP_Remove_Memorized_Spells
  | TP_Remove_Cre_Effects
  | TP_Remove_Cre_Items
  | TP_Extend_Mos of string * tp_patchexp
  | TP_PatchWriteFile of tp_patchexp * string * bool (* where, what, insert? *)
  | TP_PatchAppendFile of string * bool (* evaluate the file to be appended? *)
  | TP_CompileBAFtoBCS
  | TP_CompileBCStoBAF
  | TP_CompileDtoDLG
  | TP_CompileDLGtoD
  | TP_EvaluateBuffer
  | TP_EvaluateBufferSpecial of string
  | TP_Decompress of tp_patchexp * tp_patchexp * tp_patchexp * tp_decompress_where
			(* start, clen, ulen, where to put the result *)
	| TP_Compress of tp_patchexp * tp_patchexp * tp_patchexp * tp_decompress_where
	| TP_RebuildCreFile

and tp_decompress_where =
	| TP_DW_ReplaceFile
	| TP_DW_Variable of tp_pe_string
	| TP_DW_IntoFile of tp_patchexp * tp_patchexp

and tp_store_position =
	| TP_Store_First
	| TP_Store_Last
	| TP_Store_At of tp_patchexp
	| TP_Store_After of string
	| TP_Store_Before of string

and tp_constraint =
  | TP_Contains of tp_pe_string
  | TP_NotContains of tp_pe_string
  | TP_IfSizeIs of int
  | TP_Eval of tp_patchexp
  | TP_ButOnlyIfItChanges

and tp_pe_string =
  | PE_LiteralString of string
  | PE_Evaluate of tp_pe_string
  | PE_Lowercase of tp_pe_string
  | PE_Uppercase of tp_pe_string
  | PE_Dollars of tp_pe_string * (tp_pe_string list) * bool * bool

and tp_pe_tlk_string =
  | PE_Tlk of Dlg.tlk_string
  | PE_Pe of tp_pe_string

and tp_patchexp =
  | Pred_True
  | TP_PE_Byte_At of tp_patchexp
  | TP_PE_SByte_At of tp_patchexp
  | TP_PE_Short_At of tp_patchexp
  | TP_PE_SShort_At of tp_patchexp
  | TP_PE_Long_At of tp_patchexp
  | TP_PE_SLong_At of tp_patchexp
	| PE_String of tp_pe_string
  | PE_StringEqual of tp_pe_string * tp_pe_string * bool * bool (* ignore-case? * returns bool vs. 1,-1,0 *)
  | PE_StringRegexp of tp_pe_string * tp_pe_string * bool (* match exactly?  *)
  | PE_Not of tp_patchexp
  | PE_Add of tp_patchexp * tp_patchexp
  | PE_Sub of tp_patchexp * tp_patchexp
  | PE_Mul of tp_patchexp * tp_patchexp
  | PE_Div of tp_patchexp * tp_patchexp
  | PE_Exp of tp_patchexp * tp_patchexp * tp_patchexp
  | PE_Equal of tp_patchexp * tp_patchexp
  | PE_And of tp_patchexp * tp_patchexp
  | PE_Or of tp_patchexp * tp_patchexp
  | PE_GT of tp_patchexp * tp_patchexp
  | PE_GTE of tp_patchexp * tp_patchexp
  | PE_LT of tp_patchexp * tp_patchexp
  | PE_LTE of tp_patchexp * tp_patchexp

  | PE_BAND of tp_patchexp * tp_patchexp
  | PE_BOR of tp_patchexp * tp_patchexp
  | PE_BNOT of tp_patchexp
  | PE_BXOR of tp_patchexp * tp_patchexp
  | PE_BLSL of tp_patchexp * tp_patchexp
  | PE_BLSR of tp_patchexp * tp_patchexp
  | PE_BASR of tp_patchexp * tp_patchexp
  
  | PE_ABS of tp_patchexp

  | PE_Random of tp_patchexp * tp_patchexp
  | PE_Buffer_Length
  | PE_String_Length of tp_pe_string
  | PE_FileContainsEvaluated of tp_pe_string * tp_pe_string

  | PE_If of tp_patchexp * tp_patchexp * tp_patchexp
  | PE_ModIsInstalled of string * tp_patchexp
  | PE_GameIs of string * bool
  | PE_VariableIsSet of tp_pe_string
  | PE_IdsOfSymbol of string * string
  | PE_StateWhichSays of (Dlg.tlk_string option) * ((tp_patchexp * string) option) * string
  | PE_IsAnInt of tp_pe_string

  | Pred_File_MD5 of tp_pe_string * tp_pe_string
  | Pred_File_Exists of tp_pe_string
  | Pred_File_Is_In_Compressed_Bif of tp_pe_string
  | Pred_File_Exists_In_Game of tp_pe_string
  | Pred_File_Size of tp_pe_string * int
  | Pred_File_Contains of tp_pe_string * tp_pe_string

(************************************************************************
 * Some shorthand for writing macroes.
 ************************************************************************)
let make_lse_internal male_string male_sound female_string female_sound =
  Dlg.Local_String ({ lse_male = male_string ; lse_male_sound = male_sound ;
                    lse_female = female_string ; lse_female_sound = female_sound; })

let get_lse the_string =
  make_lse_internal the_string "" the_string ""

let get_pe_int a_string =
  PE_String(PE_LiteralString(a_string))

let get_pe_string a_string =
  PE_LiteralString(a_string)

let get_pe_tlk_string a_string =
  PE_Pe(get_pe_string a_string)


type status = Installed | Temporarily_Uninstalled | Permanently_Uninstalled

type installed_mods = (string * int * int * (string option) * status) list
  (* module : language : component : status *)
let log_name = "WeiDU.log"

let the_log : installed_mods ref = ref []
