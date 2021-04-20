(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.var.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(*
 * TP2 Patch Variables
 *)

open BatteriesInit
open Hashtblinit
open Util

let debug_assign = ref false

type variable_value =
  | Int32 of Int32.t
  | String of string

let variables = ref(Hashtbl.create 255)

let arrays : (string, string list list) Hashtbl.t ref = ref(Hashtbl.create 255)

let variables_stack = ref []
let arrays_stack = ref []

let var_pop () =
  variables := List.hd !variables_stack ;
  variables_stack := List.tl !variables_stack ;
  arrays := List.hd !arrays_stack ;
  arrays_stack := List.tl !arrays_stack

let var_push () =
  variables_stack := !variables :: !variables_stack ;
  variables := Hashtbl.copy !variables ;
  arrays_stack := !arrays :: !arrays_stack ;
  arrays := Hashtbl.copy !arrays

let var_clear_push () =
  variables_stack := !variables :: !variables_stack ;
  arrays_stack := !arrays :: !arrays_stack ;
  variables := Hashtbl.create 255 ;
  arrays := Hashtbl.create 255

let cli_variables : string list option ref = ref None

let set_int32 name value =
  let name = "%" ^ name ^ "%" in
  (if !debug_assign then Util.log_and_print "SET %s = %ld\n" name value) ;
  Hashtbl.replace !variables name (Int32(value))

let set_int name value = set_int32 name (Int32.of_int value)

let get_int32 name =
  match Hashtbl.find !variables name with
  | Int32(v) -> v
  | String(s) -> Int32.of_string s

let get_int32_extended s =
  try
    Int32.of_string s
  with e ->
    begin
      try
        get_int32 ("%" ^ s ^ "%")
      with e ->
        begin
          try
            get_int32 s
          with e -> begin
            if !eval_pe_warn then
              log_and_print "ERROR: cannot convert %s or %%%s%% to an integer\n" s s ;
            raise e
          end
        end
    end

let set_string name value =
  let name = "%" ^ name ^ "%" in
  (if !debug_assign then Util.log_and_print "SET %s = ~%s~\n" name value) ;
  Hashtbl.replace !variables name (String(value))

let add_local_int32 name value =
  let name = "%" ^ name ^ "%" in
  (if !debug_assign then Util.log_and_print "SET %s = %ld\n" name value) ;
  Hashtbl.add !variables name (Int32(value))

let add_local_string name value =
  let name = "%" ^ name ^ "%" in
  (if !debug_assign then Util.log_and_print "LOCAL_SET %s = ~%s~\n" name value) ;
  Hashtbl.add !variables name (String(value))

let remove_local name =
  let name = "%" ^ name ^ "%" in
  Hashtbl.remove !variables name

(* var_subst:
 * Str.global_substitute replacement for replacing variables inside a string
 * It takes a (string -> option string) function and depending on whether the
 * return value is None or not resumes parsing at different positions.
 *   None:     resume directly after the start of the current match
 *   Some(s):  resume after the end of the current match (like global_substitute)
 *)
let var_subst reg subst s =
  let slen = String.length s in
  let buf = Buffer.create slen in
  let start_search = ref 0 in
  let res =
    begin
      try
        while true do
          let start_match = Str.search_forward reg s !start_search in
          let after_last_match = Str.match_end () in
          Buffer.add_substring buf s !start_search (start_match - !start_search) ;
          let ret = subst s in
          match ret with
          | None ->
              start_search := start_match + 1 ;
              Buffer.add_substring buf s start_match 1
          | Some(str) ->
              start_search := after_last_match ;
              Buffer.add_string buf str
        done
      with Not_found ->
        Buffer.add_substring buf s !start_search (slen - !start_search)
    end ;
    Buffer.contents buf
  in res

let variable_regexp = Str.regexp "%[^%]+%"

let get_string_special reg str =
  let res =
    var_subst reg
      (fun whole_thing -> (* "%foo%" *)
        let quoted_substr = Str.matched_string whole_thing in
        try
          (match Hashtbl.find !variables quoted_substr with
          | Int32(v) -> Some(Int32.to_string v)
          | String(s) -> Some(s))
        with Not_found -> if reg <> variable_regexp then begin
          try
            let try_substr = String.sub quoted_substr 1 (String.length quoted_substr - 2) in
            let try_substr = "%" ^ try_substr ^ "%" in
            (match Hashtbl.find !variables try_substr with
            | Int32(v) -> Some(Int32.to_string v)
            | String(s) -> Some(s))
          with Not_found -> None
        end else None) str
  in
  (if !debug_assign then Util.log_and_print "GET ~%s~ = ~%s~\n" str res) ;
  res

let get_string = get_string_special variable_regexp

let get_string_exact str =
  match Hashtbl.find !variables str with
  | Int32(v) -> Int32.to_string v
  | String(s) -> s

let clear_var () =
  let var_spec = Hashtbl.copy !variables in
  Hashtbl.iter (fun a b -> Hashtbl.remove !variables a) var_spec ;
  Hashtbl.clear !variables

let clear_arr () =
  let arr_spec = Hashtbl.copy !arrays in
  Hashtbl.iter (fun a b -> Hashtbl.remove !arrays a) arr_spec ;
  Hashtbl.clear !arrays

let assoc name value =
  let value = Int32.of_int value in
  Hashtbl.replace !variables name (Int32 value)

let all_the_assoc a =
  assoc "AREA_CITY" 0252 ;
  assoc "AREA_DAY" 0260 ;
  assoc "AREA_DUNGEON" 0256 ;
  assoc "AREA_FOREST" 0248 ;
  assoc "AREA_NIGHT" 0264 ;
  assoc "ATTACK1" 0220 ;
  assoc "ATTACK2" 0224 ;
  assoc "ATTACK3" 0228 ;
  assoc "ATTACK4" 0232 ;
  assoc "BATTLE_CRY1" 0200 ;
  assoc "BATTLE_CRY2" 0204 ;
  assoc "BATTLE_CRY3" 0208 ;
  assoc "BATTLE_CRY4" 0212 ;
  assoc "BATTLE_CRY5" 0216 ;
  assoc "BIO"  0x1cc ;
  assoc "BORED" 0196 ;
  assoc "COMPLIMENT1" 0352 ;
  assoc "COMPLIMENT2" 0356 ;
  assoc "COMPLIMENT3" 0360 ;
  assoc "CRITICAL_HIT" 0424 ;
  assoc "CRITICAL_MISS" 0428 ;
  assoc "DAMAGE" 0236 ;
  assoc "DESC"  0x54 ;
  assoc "DIALOGUE_DEFAULT" 0412 ;
  assoc "DIALOGUE_HOSTILE" 0408 ;
  assoc "DYING" 0240 ;
  assoc "EXISTANCE1" 0444 ;
  assoc "EXISTANCE2" 0448 ;
  assoc "EXISTANCE3" 0452 ;
  assoc "EXISTANCE4" 0456 ;
  assoc "EXISTANCE5" 0460 ;
  assoc "HAPPY" 0172 ;
  assoc "HURT" 0244 ;
  assoc "IDENTIFIED_DESC"  0x54 ;
  assoc "INITIAL_MEETING" 0164 ;
  assoc "INSULT" 0340 ;
  assoc "INTERACTION1" 0320 ;
  assoc "INTERACTION2" 0324 ;
  assoc "INTERACTION3" 0328 ;
  assoc "INTERACTION4" 0332 ;
  assoc "INTERACTION5" 0336 ;
  assoc "INVENTORY_FULL" 0436 ;
  assoc "LEADER" 0188 ;
  assoc "MISCELLANEOUS" 0384 ;
  assoc "MORALE" 0168 ;
  assoc "NAME1" 8 ;
  assoc "NAME2" 12 ;
  assoc "PICKED_POCKET" 0440 ;
  assoc "REACT_TO_DIE_GENERAL" 0376 ;
  assoc "REACT_TO_DIE_SPECIFIC" 0380 ;
  assoc "RESPONSE_TO_COMPLIMENT2" 0388 ;
  assoc "RESPONSE_TO_COMPLIMENT3" 0392 ;
  assoc "RESPONSE_TO_INSULT1" 0396 ;
  assoc "RESPONSE_TO_INSULT2" 0400 ;
  assoc "RESPONSE_TO_INSULT3" 0404 ;
  assoc "SELECT_ACTION1" 0292 ;
  assoc "SELECT_ACTION2" 0296 ;
  assoc "SELECT_ACTION3" 0300 ;
  assoc "SELECT_ACTION4" 0304 ;
  assoc "SELECT_ACTION5" 0308 ;
  assoc "SELECT_ACTION6" 0312 ;
  assoc "SELECT_ACTION7" 0316 ;
  assoc "SELECT_COMMON1" 0268 ;
  assoc "SELECT_COMMON2" 0272 ;
  assoc "SELECT_COMMON3" 0276 ;
  assoc "SELECT_COMMON4" 0280 ;
  assoc "SELECT_COMMON5" 0284 ;
  assoc "SELECT_COMMON6" 0288 ;
  assoc "SELECT_RARE1" 0416 ;
  assoc "SELECT_RARE2" 0420 ;
  assoc "SPECIAL1" 0364 ;
  assoc "SPECIAL2" 0368 ;
  assoc "SPECIAL3" 0372 ;
  assoc "TARGET_IMMUNE" 0432 ;
  assoc "TIRED" 0192 ;
  assoc "UNHAPPY_ANNOYED" 0176 ;
  assoc "UNHAPPY_BREAKING" 0184 ;
  assoc "UNHAPPY_SERIOUS" 0180 ;
  assoc "UNIDENTIFIED_DESC" 0080 ;
  assoc "HIDDEN_IN_SHADOWS" 0444 ;
  assoc "SPELL_DISRUPTED" 0448 ;
  assoc "SET_A_TRAP" 0452 ;
  assoc "STORE_NAME" 12 ;

  assoc "SCRIPT_OVERRIDE" 0x248 ;
  assoc "SCRIPT_CLASS" 0x250 ;
  assoc "SCRIPT_RACE" 0x258 ;
  assoc "SCRIPT_GENERAL" 0x260 ;
  assoc "SCRIPT_DEFAULT" 0x268 ;
  assoc "DEATHVAR" 0x280 ;
  assoc "DIALOG" 0x2cc ;
  assoc "AREA_SCRIPT" 0x94 ;
  assoc "SCRIPT_AREA" 0x94 ;

  assoc "PORTRAIT_SMALL" 0x34 ;
  assoc "PORTRAIT_LARGE" 0x3c ;

  assoc "DIALOG_DEFAULT" 0412 ;
  assoc "DIALOG_HOSTILE" 0408 ;
  assoc "UNHAPPY_BREAKING_POINT" 0184 ;

  for i = 0 to 31 do
    set_int32 (Printf.sprintf "BIT%d" i) (Int32.shift_left 1l i) ;
  done;

  set_string "WNL" "\r\n" ;
  set_string "MNL" "\r" ;
  set_string "LNL" "\n" ;
  set_string "TAB" "\t" ;

  List.iter (fun game ->
    let var = "REGISTRY_" ^ (String.uppercase game) ^ "_PATH" in
    try
      let res = Arch.game_path_by_type game in
      set_string var (if res = "." then "" else res)
    with _ -> set_string var "") ["bg1" ; "bg2" ; "pst" ; "iwd1" ; "iwd2"] ;
  ()

let _ =
  all_the_assoc () ;
  ()

(* Bear CLEAR_MEMORY in mind if any more of these are added. *)
let set_savedir_var path =
  set_string "SAVE_DIRECTORY" (path ^ "/save") ;
  set_string "MPSAVE_DIRECTORY" (path ^ "/mpsave")

let set_userdir_var path =
  set_string "USER_DIRECTORY" path

let set_game_vars game_path game_type =
  let user_dir = Util.get_user_dir game_path game_type in
  ignore (set_savedir_var user_dir) ;
  ignore (set_userdir_var user_dir)

let set_ee_language_var lang_dir =
  set_string "EE_LANGUAGE" lang_dir

let get_mod_folder dir =
  (try
    Some (String.sub dir 0
            (String.index
               (Str.global_replace
                  (Str.regexp "\\\\") "/" dir) '/'))
   with Not_found ->
     None)
