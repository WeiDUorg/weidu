(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 232. *)

(* Functions to find out the state of a component
 * Functions to store the current state of WeiDU *)

open BatteriesInit
open Util
open Tp

let safe_exit = ref false

let clear_memory = ref false

let strings_to_print_at_exit : (string * string) list ref = ref []

let append_to_strings_to_print_at_exit comment name =
  strings_to_print_at_exit := (comment,name) ::
    List.filter (fun (c,n) -> n <> name) !strings_to_print_at_exit

let rec get_menu_style fl = match fl with
| [] -> 0
| Menu_Style(i) :: tl -> int_of_string i
| hd :: tl -> get_menu_style tl

(************************************************************************
 * Common hashtables.
 ************************************************************************)
let loaded_tph: (string,tp_action list)Hashtbl.t = Hashtbl.create 5
let loaded_tpp: (string,tp_patch list)Hashtbl.t = Hashtbl.create 5
let macros: ((string*bool),tp_local_declaration list * tp_patch list)Hashtbl.t = Hashtbl.create 10
let functions: ((string*bool),(tp_pe_string * tp_patchexp) list *
                  (tp_pe_string * tp_pe_string) list * tp_pe_string list * tp_patch list) Hashtbl.t = Hashtbl.create 10
let readln_strings: string list ref = ref []

let clear_codes () =
  let var_spec = Hashtbl.copy macros in
  Hashtbl.iter (fun a b -> Hashtbl.remove macros a) var_spec ;
  Hashtbl.clear macros;
  let var_spec = Hashtbl.copy functions in
  Hashtbl.iter (fun a b -> Hashtbl.remove functions a) var_spec ;
  Hashtbl.clear functions

(************************************************************************
 * For handling lists of modules.
 ************************************************************************)

let get_nth_module tp_file n print_why =
  let lst = tp_file.module_list in
  let last = ref (-1) in
  let rec process lst = match lst with
  | [] ->
      if print_why then
        log_or_print "%s's %dth component not found." tp_file.tp_filename n;
      raise Not_found
  | hd :: tl ->
      begin
        List.iter (fun x -> match x with
        | TPM_Designated(i) -> last := pred i
        | _ -> ()) hd.mod_flags ;
        incr last;
        if (!last = n) then hd else process tl
      end
  in
  process lst

let get_last_module_index tp_file =
  let last = ref 0 in
  List.fold_left (fun acc elt ->
    let this_one =
      let rec process lst = match lst with
      | TPM_Designated(i) :: tl -> i
      | hd :: tl -> process tl
      | [] -> !last + 1
      in process elt.mod_flags
    in
    last := this_one ;
    max !last acc) 0 tp_file.module_list

let get_id_of_label tp_file label =
  let ans = ref None in
  let has_label c = List.mem (TPM_Label label) c.mod_flags in
  for i = 0 to get_last_module_index tp_file do
    try
      let c = get_nth_module tp_file i false in
      if has_label c then begin match !ans with
      | None -> ans := Some i
      | Some j ->
          ans := Some min_int;
          errors_this_component := true;
          log_and_print "WARNING: Duplicate LABEL [%s] in tp2 file [%s] (components %d and %d)\n" label tp_file.tp_filename i j
      end;
    with Not_found -> ()
  done;
  if !ans = None then begin
    errors_this_component := true;
    log_and_print "WARNING: LABEL [%s] not found in tp2 file [%s]\n" label tp_file.tp_filename end;
  !ans


(************************************************************************
 * Evaluate a TP2 Patch Expression
 ************************************************************************)
let log_match a b =
  let a = String.uppercase a in
  let b = String.uppercase b in
  Str.global_replace (Str.regexp "^SETUP-") "" (Case_ins.filename_basename (String.uppercase a)) =
  Str.global_replace (Str.regexp "^SETUP-") "" (Case_ins.filename_basename (String.uppercase b))

let any_installed tp2 =
  let rec is_installed lst = match lst with
  | [] -> false
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && d <> Permanently_Uninstalled -> true
  | hd :: tl -> is_installed tl
  in is_installed !the_log

let already_installed tp2 i =
  let rec is_installed lst = match lst with
  | [] -> false
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && c = i && d <> Permanently_Uninstalled -> true
  | hd :: tl -> is_installed tl
  in is_installed !the_log

let installed_lang_index tp2 =
  let rec is_installed lst = match lst with
  | [] -> None
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && d <> Permanently_Uninstalled -> Some(b)
  | hd :: tl -> is_installed tl
  in is_installed !the_log

let temporarily_uninstalled tp2 i =
  let rec is_installed lst = match lst with
  | [] -> false
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && c = i && d = Temporarily_Uninstalled -> true
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && c = i && d = Installed -> false
  | hd :: tl -> is_installed tl
  in is_installed !the_log

let installed_after tp21 i1 tp22 i2 =
  let rec walk lst tp2 i = match lst with
  | [] -> None
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && c = i && d = Installed -> Some tl
  | hd :: tl -> walk tl tp2 i
  in
  match walk !the_log tp22 i2 with
  | None -> false
  | Some tl -> None <> walk tl tp21 i1

(************************************************************************
 * Determine what has been installed.
 ************************************************************************)
let str_of_str_opt sopt = match sopt with
| Some(str) -> "~" ^ str ^ "~"
| None -> ""

let print_log () =
  List.iter (fun (n,i1,i2,sopt,st) ->
    log_or_print "%s %2d %2d %s %s\n" n i1 i2 (match st with
    | Installed -> "Installed"
    | Temporarily_Uninstalled -> "Temporarily_Uninstalled"
    | Permanently_Uninstalled -> "Permanently_Uninstalled")
      (str_of_str_opt sopt)) !the_log

let decompile_var s =
  match s with
  | Must_Get_Var x -> Var.get_string x

let sprintf_log game handle_tp2_filename handle_tra_filename get_tra_list_filename log tp2_ht tra_ht vocal intro =
  let out = Buffer.create 10000 in
  if vocal then (log_or_print "Saving This Log:\n" ; print_log ());
  if intro then begin
    Printf.bprintf out "// Log of Currently Installed WeiDU Mods\n" ;
    Printf.bprintf out "// The top of the file is the 'oldest' mod\n" ;
    Printf.bprintf out "// ~TP2_File~ #language_number #component_number%s"
      (if !quick_log then "\n" else " // [Subcomponent Name -> ] Component Name [ : Version]\n") ;
  end;
  let newline_regexp = one_newline_or_cr_regexp in
  List.iter (fun (a,b,c,sopt,d) ->
    let str =
      if not !quick_log then begin
        let component_name, subcomponent_name, version =
          try
            (* log_or_print "*** Looking Up %s.\n" a; *)
            let tp2 =
              try Hashtbl.find tp2_ht a
              with _ ->
                (* log_or_print "*** Parsing %s.\n" a; *)
                let res = handle_tp2_filename a in
                Hashtbl.add tp2_ht a res ; res
            in
            Dc.clear_state () ;
            Dc.push_trans ();
            let a_dir = Case_ins.filename_dirname a in
            (try
              let l = List.nth tp2.languages b in
              List.iter (fun s ->
                let s = decompile_var s in
                let x =
                  (* log_or_print "*** Loading %s for %s.\n" s a; *)
                  try Hashtbl.find tra_ht s
                  with _ ->
                    (let x = get_tra_list_filename (Arch.backslash_to_slash s) in
                    Hashtbl.add tra_ht s x; x)
                in
                Stats.time "adding translation strings" Dc.add_trans_strings x) l.lang_tra_files ;
            with _ -> ()) ;
            let m = get_nth_module tp2 c true in
            let comp_str = Dc.single_string_of_tlk_string_safe game m.mod_name in
            let subcomp_group the_comp =
              let rec walk lst = match lst with
              | TPM_SubComponents(ts,a,b) :: tl -> Some(ts)
              | hd :: tl -> walk tl
              | [] -> None
              in walk the_comp.mod_flags
            in
            let subcomp_str =
              (match subcomp_group m with
              | None    -> ""
              | Some(x) -> "" ^ (Dc.single_string_of_tlk_string_safe game x) ^ " -> ") in
            let rec get_version lst = match lst with
            | Version(lse) :: _ -> ": " ^ Dc.single_string_of_tlk_string_safe game lse
            |   _ :: tl -> get_version tl
            | [] -> ""
            in
            let version = get_version tp2.flags in
            Dc.clear_state () ;
            Dc.pop_trans ();
            (comp_str, subcomp_str,version)
          with _ ->
            ("??? -> ", "???", ": ???")
        in
        let component_name = Str.global_replace newline_regexp " " component_name in
        let subcomponent_name = Str.global_replace newline_regexp " " subcomponent_name in
        Printf.sprintf "~%s~ #%d #%d // %s%s%s\n"
          (String.uppercase a) b c subcomponent_name component_name version
      end
      else begin
        Printf.sprintf "~%s~ #%d #%d\n"
          (String.uppercase a) b c
      end
    in
    match d with
    | Installed -> Buffer.add_string out str
    | Temporarily_Uninstalled ->
        log_and_print "Internal Error: saving a log with temporarily uninstalled module %s" str
    | Permanently_Uninstalled ->
        Printf.bprintf out "// Recently Uninstalled: %s" str) log ;
  Buffer.contents out

let save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename =
  let tp2_ht = Hashtbl.create 511 in
  let tra_ht = Hashtbl.create 511 in
  let s = sprintf_log game handle_tp2_filename handle_tra_filename get_tra_list_filename !the_log tp2_ht tra_ht true true in
  let out = Case_ins.perv_open_out log_name in
  output_string out s;
  close_out out

type default_action = TP_Install | TP_Uninstall | TP_Skip | TP_Ask

let interactive = ref true

let safe_to_handle tp2 i =
  if not !safe_exit then true else
  if not (already_installed tp2 i) then true else
  begin
    let rec check lst = match lst with
    | [] -> true (* end of the line *)

          (* this is the entry in the list *)
    | (a,b,c,sopt,d) :: tl when log_match a tp2 && c = i -> true
    | (a,b,c,sopt,d) :: tl ->
        begin match d with
        | Permanently_Uninstalled
        | Temporarily_Uninstalled -> (* keep going *)
            check tl
        | Installed ->
            false
        end
    in
    check (List.rev !the_log)
  end
