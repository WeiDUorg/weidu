(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util
open Diff
open Tp
open Parsewrappers
open Tpstate
open Tphelp
open Tppe
open Tpuninstall
open Tppatch
open Tpaction

(**************************************************************************
* Globals
**************************************************************************)

let asked_about_group = ref (Hashtbl.create 5)

(**************************************************************************
* Helper functions
**************************************************************************)

let init_tp_state () =
  asked_about_group := Hashtbl.create 5

let group_enabled g =
  if Hashtbl.mem !asked_about_group g then
    Hashtbl.find !asked_about_group g
  else true

let rec hasgroup x =
  match x with
  | TPM_Group(c, _) :: b -> true
  | a :: b -> hasgroup b
  | [] -> false

let module_groups_ok m =
  if hasgroup m.mod_flags then List.exists (fun f ->
    match f with
    | TPM_Group (n, c) -> not (group_enabled n) ||
      is_true (eval_pe "" (Load.the_game ()) c)
    | _ -> false) m.mod_flags
  else true

let comp_flag_p tp m =
  List.exists (fun f -> match f with
  | TPM_RequireComponent(s, i, warn) -> not (already_installed s i)
  | TPM_ForbidComponent(s, i, warn) -> (already_installed s i)
  | TPM_Deprecated(warn) -> true
  | TPM_RequirePredicate(p,warn) ->
      not (is_true (eval_pe "" (Load.the_game ()) p))
  | TPM_Label(s) -> ignore (get_id_of_label tp s); false
  | _ -> false) m.mod_flags

let comp_part_p m =
  let game = Load.the_game () in
  List.exists (fun f -> match f with
  | TP_Require_File(file, warn) ->
      not (bigg_file_exists file game.Load.key)
  | TP_Forbid_File(file, warn) ->
      bigg_file_exists file game.Load.key
  | _ -> false) m.mod_parts

let fails_requirements tp m =
  comp_flag_p tp m || not (module_groups_ok m) || comp_part_p m

let choose_lang tp this_tp2_filename =
  match tp.languages with
  | [] -> (ref None, ref 0)
  | [l] -> (ref (Some l), ref 0)
  | _ ->
      let arr = Array.of_list tp.languages in
      let answer, answer_index =
        match installed_lang_index this_tp2_filename with
        | Some(i) when i >= 0 && (i < Array.length arr) ->
            Some(arr.(i)), i
        | _ -> None, 0
      in
      let answer, answer_index = ref answer, ref answer_index in
      if !forced_language >= 0 &&
        (!forced_language < Array.length arr) then begin
          answer := Some(arr.(!forced_language)) ;
          answer_index := !forced_language ;
        end ;
      while !answer = None do
        log_and_print "\nChoose your language:\n" ;
        Array.iteri (fun i l ->
          log_and_print "%2d [%s]\n" i l.lang_name) arr ;
        try
          let i = read_int () in
          if i >= 0 && i < Array.length arr then begin
            answer := Some(arr.(i)) ;
            answer_index := i
          end
        with _ -> ()
      done ;
      (answer, answer_index)

let lang_init our_lang =
  init_default_strings () ;
  begin
    match our_lang with
    | None -> ()
    | Some(l) ->
        log_and_print "%s [%s]\n" (get_trans (-1035)) l.lang_name ;
        log_or_print "[%s] has %d top-level TRA files\n"
          l.lang_name (List.length l.lang_tra_files) ;
        List.iter handle_tra_filename
          (List.map Arch.backslash_to_slash
             (List.map Var.get_string l.lang_tra_files)) ;
        Var.set_string "LANGUAGE" l.lang_dir_name

  end

let do_readme tp this_tp2_filename =
  List.iter (fun flag ->
    match flag with
    | Readme(str_l) -> if not (any_installed this_tp2_filename) then begin
        let rec walk str_l =
          (match str_l with
          | str::tail ->
              let str = Var.get_string str in
              if file_exists str then begin
                let str = "VIEW " ^ str in
                let str = Arch.handle_view_command str !skip_at_view in
                let answer = ref "" in
                if !always_uninstall || !always_yes || !sometimes_reinstall ||
                !force_uninstall_these <> [] || !force_install_these <> [] ||
                !chosen_quick_menu <> None then
                  answer := "N";
                if !skip_at_view || not !interactive then answer := "N";
                while !answer <> "Y" && !answer <> "N" do
                  log_and_print "\n%s\n" (get_trans (-1034));
                  answer := String.uppercase (read_line())
                done;
                if !answer = "Y" then
                  ignore (Unix.system str);
              end
              else walk tail
          | [] -> log_and_print
                "\nCouldn't open the readme: file not found.\n")
        in walk str_l
    end
    | _ -> ())  tp.flags

let subcomp_group the_comp =
  let rec walk lst = match lst with
  | TPM_SubComponents(ts,_,_) :: tl -> Some(ts)
  | hd :: tl -> walk tl
  | [] -> None
  in walk the_comp.mod_flags

let subcomp_predicate the_comp =
  let rec walk lst = match lst with
  | TPM_SubComponents(_,p,_) :: tl ->
      is_true (eval_pe "" (Load.the_game ()) p)
  | hd :: tl -> walk tl
  | [] -> false
  in walk the_comp.mod_flags

let subcomp_forced the_comp=
  let rec walk lst = match lst with
  | TPM_SubComponents(_,_,b) :: tl -> b
  | hd :: tl -> walk tl
  | [] -> false
  in walk the_comp.mod_flags

let count_comp_num tp =
  let comp_num = ref 0 in
  let comp_ht = Hashtbl.create 255 in
  List.iter (fun the_mod ->
    incr comp_num ;
    (match subcomp_group the_mod with
    | Some(ts) ->
        if Hashtbl.mem comp_ht ts then
          decr comp_num
        else
          Hashtbl.add comp_ht ts true
    | None -> ())) tp.module_list ;
  !comp_num

let any_member_of_subcomp_group_installed tp this_tp2_filename the_comp =
  let i = ref 0 in
  List.exists (fun the_mod ->
    List.iter (fun f -> match f with
    | TPM_Designated(i') -> i := i' ;
    | _ -> ()) the_mod.mod_flags ;
    (match subcomp_group the_mod with
    | Some(ts) ->
        if ts = the_comp &&
          already_installed this_tp2_filename !i then
          true
        else (incr i ; false)
    | None -> (incr i ; false))) tp.module_list

let check_installed_state tp this_tp2_filename last =
  let any_already = ref false in
  let any_not_yet = ref false in
  let subcomp_installed = Hashtbl.create 255 in

  for i = 0 to last do
    (try let m = get_nth_module tp i false in
    if already_installed this_tp2_filename i then begin
      any_already := true ;
      match subcomp_group m with
      | Some(ts) -> Hashtbl.add subcomp_installed ts true
      | None -> ()
    end else begin
      if not (fails_requirements tp m) then begin
        let group_already =
          match subcomp_group m with
          | Some(ts) -> Hashtbl.mem subcomp_installed ts
          | None -> false in
        if not group_already then
          any_not_yet := true
      end
    end
    with Not_found -> ()) ;
  done ;
  (!any_already, !any_not_yet)

let get_quickmenu_ask_list tp =
  let quickmenu = List.find (fun x ->
    match x with
    | Quick_Menu _ -> true
    | _ -> false) tp.flags in
  match quickmenu with
  | Quick_Menu (x, y) -> begin
      if List.mem 0 y then
        failwith "ERROR: QUICK_MENU has component 0 in ALWAYS_ASK" ;
      if List.exists (fun (name, lst) -> List.mem 0 lst) x then
        failwith "ERROR: QUICK_MENU has component 0 in a pre-defined selection" ;
      if !chosen_quick_menu <> None then begin
        if (value_of_option !chosen_quick_menu < 1 ||
        List.length x < value_of_option !chosen_quick_menu) then
          failwith (Printf.sprintf "--quick-menu %d out of range 1-%d"
                      (value_of_option !chosen_quick_menu) (List.length x));
        List.iter (fun lst -> List.iter (fun i ->
          if not (List.mem i y) then
            failwith
              (Printf.sprintf "Component %d is not in ALWAYS_ASK and --quick-menu was given" i)) lst)
          [!force_uninstall_these; !force_install_these];
        List.iter (fun i -> force_install_these := i :: !force_install_these)
          (snd (List.nth x (value_of_option !chosen_quick_menu - 1)));
      end;
      y
  end
  | _ -> (try assert false with Assert_failure (s, l, c) ->
      failwith (Printf.sprintf "Internal WeiDU failure: %s %d %d" s l c))

let module_defaults_helper tp this_tp2_filename i has_quickmenu =
  try
    let cli_uninstall =
      List.exists (fun h -> h = i) !force_uninstall_these in
    let cli_install =
      List.exists (fun h -> h = i) !force_install_these in
    let m = get_nth_module tp i false in
    if !always_yes || cli_install then
      TP_Install
    else if (!always_uninstall || cli_uninstall) &&
      (already_installed this_tp2_filename i) then
      TP_Uninstall
    else if (!always_uninstall || cli_uninstall) then
      TP_Skip
    else if !sometimes_reinstall &&
      (already_installed this_tp2_filename i) then
      TP_Install
    else if !sometimes_reinstall then
      TP_Skip
    else if (!force_install_these <> []) ||
    (!force_uninstall_these <> []) then
      TP_Skip
    else if List.mem Tp.TPM_InstallByDefault m.mod_flags then
      TP_Install
    else if has_quickmenu && i = 0 then
      TP_Install
    else if has_quickmenu && not (List.mem i (get_quickmenu_ask_list tp)) then
      TP_Skip
    else
      TP_Ask
  with Not_found -> TP_Ask

let validate_quickmenu tp =
  if !chosen_quick_menu <> None then
    failwith "--quick-menu given without a QUICK_MENU defined" ;
  try
    let m = get_nth_module tp 0 false in
    if m.mod_parts <> [] || m.mod_flags <> [] then
      failwith "ERROR: QUICK_MENU and component 0 is non-empty.";
  with Not_found ->
    failwith "ERROR: QUICK_MENU and component 0 isn't defined."

let handle_cli_vars args_backup_filename =
  if !interactive && (!Var.cli_variables = None) then begin
    let counter = ref 0 in
    let continue = ref true in
    Var.cli_variables := Some([]) ;
    while !continue do
      let varname = Printf.sprintf "%%argv[%d]%%" !counter in
      let varcont = Var.get_string varname in
      if varcont = varname then begin
        continue := false
      end else begin
        if !debug_ocaml then
          log_and_print "%%argv[%d]%% = %s\n" !counter varcont;
        Var.cli_variables :=
          Some(varcont :: (value_of_option !Var.cli_variables)) ;
        incr counter;
      end ;
    done ;
    Var.cli_variables := Some(value_of_option !Var.cli_variables)
  end ;
  if !interactive then begin
    Mymarshal.write_cli_vars args_backup_filename
      (List.rev (value_of_option !Var.cli_variables));
  end ;
  if (* not !interactive && *) file_exists args_backup_filename then begin
    (* re-loading when interactive and clearing of old argv[] variables is intended. *)
    let counter = ref 0 in
    let continue = ref true in
    while !continue do
      let varname = Printf.sprintf "%%argv[%d]%%" !counter in
      let varcont = Var.get_string varname in
      if varcont = varname then begin
        continue := false
      end else begin
        if !debug_ocaml then
          log_and_print "removing the pair %%argv[%d]%% = %s\n"
            !counter varcont;
        Hashtbl.remove !Var.variables varname ;
        incr counter;
      end ;
    done ;
    try
      let record = Mymarshal.read_cli_vars args_backup_filename in
      let counter = ref 0 in
      List.iter (fun s ->
        Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s;
        incr counter) record
    with e ->
      (try assert false with Assert_failure(file,line,col) ->
        set_errors file line);
      log_and_print "WARNING: Unable to read --args references from [%s]: %s\n"
        args_backup_filename (printexc_to_string e)
  end

let version_msg tp =
  let rec getmsg lst = match lst with
  | [] -> ""
  | Version(a) :: _ ->
      Printf.sprintf " [%s]"
        (Dc.single_string_of_tlk_string_safe (Load.the_game ()) a)
  | hd :: tl -> getmsg tl
  in getmsg tp.flags

let rec findgroup flags =
  match flags with
  | TPM_Group(x, co) :: b -> Some(x,co)
  | a :: b -> findgroup b
  | [] -> None

let is_my_group the_comp group =
  let rec walk lst = match lst with
  | TPM_Group(ts, co) :: tl when ts = group -> true
  | hd :: tl -> walk tl
  | [] -> false
  in walk the_comp.mod_flags

let ask_about_quickmenu tp this_tp2_filename using_quickmenu module_defaults
    quickmenu always last_module_index any_already_installed =
  let finished = ref false in
  while not !finished do
    if any_already_installed then
      log_and_print "\n%s\n" (get_trans (-1039))
    else
      log_and_print "\n%s\n" (get_trans (-1038));
    let cnt = ref 1 in
    List.iter (fun (title,components) ->
      let is_selection = ref true in
      for i = 0 to last_module_index do
        if List.mem i components && List.mem i always then
          failwith
            (Printf.sprintf
               "Component %d is both in ALWAYS_ASK and QUICK_MENU" i);
        let is_inst = already_installed this_tp2_filename i in
        let is_grp  = List.mem i components in
        let curr_is_ok = i = 0 || (is_inst && is_grp) ||
        (not is_inst && not is_grp) || (List.mem i always) in
        is_selection := !is_selection && curr_is_ok;
      done;
      log_and_print "%2d] %s%s\n" !cnt
        (Dc.single_string_of_tlk_string_safe (Load.the_game ()) title)
        (if !is_selection then (get_trans (-1027)) else "") ;
      incr cnt) quickmenu ;
    let ans = String.uppercase (read_line ()) in
    let set_state inst uninst always_inst always_uninst =
      for i = 0 to last_module_index do
        try
          let the_comp = get_nth_module tp i false in
          if List.mem TPM_InstallByDefault the_comp.mod_flags &&
            always_inst <> TP_Uninstall then begin
            module_defaults.(i) <- TP_Install
          end else begin
            if List.mem i always then begin
              if already_installed this_tp2_filename i then
                module_defaults.(i) <- always_inst
              else if List.mem i always then
                module_defaults.(i) <- always_uninst
            end else begin
              if (already_installed this_tp2_filename i) then
                module_defaults.(i) <- inst
              else
                module_defaults.(i) <- uninst
            end
          end
        with Not_found -> ()
      done
    in
    match ans with
    | "S" ->
        set_state TP_Skip TP_Skip TP_Skip TP_Skip;
        finished := true;
    | "A" ->
        set_state TP_Ask TP_Ask TP_Ask TP_Ask;
        module_defaults.(0) <- TP_Install;
        finished := true;
    | "R" ->
        if any_already_installed then begin
          set_state TP_Install TP_Skip TP_Install TP_Skip;
          module_defaults.(0) <- TP_Install;
          finished := true;
        end
    | "U" ->
        if any_already_installed then begin
          set_state TP_Uninstall TP_Skip TP_Uninstall TP_Skip;
          module_defaults.(0) <- TP_Uninstall;
          finished := true;
        end
    | _ -> begin try
        let which = int_of_string ans in
        if which < 1 || which >= !cnt then failwith "out of bounds";
        using_quickmenu := true;
        set_state TP_Uninstall TP_Skip TP_Ask TP_Ask;
        module_defaults.(0) <- TP_Install;
        let (title,components) = (List.nth quickmenu (which - 1)) in
        log_and_print "Installing selection %s\n"
          (Dc.single_string_of_tlk_string_safe (Load.the_game ()) title);
        List.iter (fun x ->
          module_defaults.(x) <- TP_Install) components;
        finished := true;
    with _ -> ()
    end
  done

let ask_about_ungrouped tp this_tp2_filename module_defaults
    any_not_yet_installed any_already_installed last_module_index specify =
  let finished = ref false in
  if any_not_yet_installed then
    while not !finished do
      finished := true ;
      (* log_and_print "\nWhat should be done with all components that are NOT YET installed?\n[I]nstall them, [S]kip them, [A]sk about each one? " ; *)
      log_and_print "\n%s" ((get_trans (-1002)));
      match String.uppercase(read_line ()) with
      | "R"
      | "I" ->
          for i = 0 to last_module_index do
            try
              let the_comp = get_nth_module tp i false in
              match subcomp_group the_comp with
              | Some(x) ->
                  (* if not (any_member_of_subcomp_group_installed tp this_tp2_filename x) then *)
                  module_defaults.(i) <- TP_Ask
              | None ->
                  if not (already_installed this_tp2_filename i) then
                    module_defaults.(i) <- TP_Install
            with Not_found -> ()
          done
      | "S"
      | "Q" ->
          for i = 0 to last_module_index do
            try
              if module_defaults.(i) <> TP_Ask then raise Not_found;
              let the_comp = get_nth_module tp i false in
              let c = get_nth_module tp i false in
              if subcomp_forced c && not (fails_requirements tp c) &&
                not (already_installed this_tp2_filename i) then
                module_defaults.(i) <- TP_Install
              else begin
                match subcomp_group the_comp with
                | Some(x) ->
                    if not (any_member_of_subcomp_group_installed tp this_tp2_filename x) then
                      module_defaults.(i) <- TP_Skip
                | None ->
                    if not (already_installed this_tp2_filename i) then
                      module_defaults.(i) <- TP_Skip
              end
            with Not_found -> ()
          done
      | "A" -> ()
      | "X" -> specify := true
      | _ -> finished := false
    done ;

  finished := false ;

  if any_already_installed then
    while not !finished do
      finished := true ;
      (* log_and_print "\nWhat should be done with all components that are ALREADY installed?\n[R]e-install them, [U]ninstall them, [S]kip them, [A]sk about each one? " ; *)
      log_and_print "\n%s" ((get_trans (-1003)));
      match String.uppercase(read_line ()) with
      | "I"
      | "R" ->
          for i = 0 to last_module_index do
            try
              let the_comp = get_nth_module tp i false in
              match subcomp_group the_comp with
              | Some(x) ->
                  (* if not (any_member_of_subcomp_group_installed tp this_tp2_filename x) then *)
                  module_defaults.(i) <- TP_Ask
              | None ->
                  if (already_installed this_tp2_filename i) then
                    module_defaults.(i) <- TP_Install
            with Not_found -> ()
          done
      | "S"
      | "Q" ->
          for i = 0 to last_module_index do
            try let the_comp = get_nth_module tp i false in
            if module_defaults.(i) <> TP_Ask then raise Not_found;
            let c = get_nth_module tp i false in
            if subcomp_forced c && not (fails_requirements tp c) &&
              not (already_installed this_tp2_filename i) then
              module_defaults.(i) <- TP_Install
            else begin
              match subcomp_group the_comp with
              | Some(x) ->
                  if (any_member_of_subcomp_group_installed tp this_tp2_filename x) then
                    module_defaults.(i) <- TP_Skip
              | None ->
                  if (already_installed this_tp2_filename i) then
                    module_defaults.(i) <- TP_Skip
            end
            with Not_found -> ()
          done
      | "U" ->
          for i = 0 to last_module_index do
            try let the_comp = get_nth_module tp i false in
            ( match subcomp_group the_comp with
            | Some(x) ->
                if (any_member_of_subcomp_group_installed tp this_tp2_filename x) then
                  module_defaults.(i) <- TP_Skip
            | None -> ()) ;
            if (already_installed this_tp2_filename i) then
              module_defaults.(i) <- TP_Uninstall
            with Not_found -> ()
          done
      | "A" -> ()
      | "X" -> specify := true
      | _ -> finished := false
    done

let ask_about_groups tp groups module_defaults last_module_index using_quickmenu always has_ask_only =
  let any_to_be_asked grp =
    let ans = ref false in
    for i = 0 to last_module_index do
      try
        let m = get_nth_module tp i false in
        if module_defaults.(i) = TP_Skip && is_my_group m grp &&
          (not !using_quickmenu || List.mem i always) then ans := true
      with _ -> ()
    done;
    !ans
  in

  for i = 0 to last_module_index do
    try
      let the_comp = get_nth_module tp i false in
      if module_defaults.(i) = TP_Ask && hasgroup the_comp.mod_flags &&
        (not !using_quickmenu || List.mem i always) then
        module_defaults.(i) <- TP_Skip;
    with Not_found -> ()
  done;
  List.iter (fun (this_grp, co) ->
    let pass = eval_pe_warn := false; try
      is_true (eval_pe "" (Load.the_game ()) co)
    with _ -> true in
    eval_pe_warn := true;
    if pass && any_to_be_asked this_grp then begin
      let finished = ref false in
      while not !finished do
        finished := true ;
        log_and_print "\n%s%s%s" (Var.get_string (get_trans (-1028)))
          (Dc.single_string_of_tlk_string_safe
             (Load.the_game ()) this_grp) (get_trans (-1029)) ;
        match String.uppercase(read_line ()) with
        | "Y" ->
            for i = 0 to last_module_index do
              try
                let the_comp = get_nth_module tp i false in
                if is_my_group the_comp this_grp &&
                  module_defaults.(i) = TP_Skip && (not !using_quickmenu ||
                  List.mem i always) &&
                  (not has_ask_only || List.mem i !Tp.ask_only) then
                  module_defaults.(i) <- TP_Ask ;
              with Not_found -> ()
            done;
            Hashtbl.add !asked_about_group this_grp true
        | "N" -> Hashtbl.add !asked_about_group this_grp false
        | _ -> finished := false
      done;
    end else
      log_and_print "\n%s%s%s\n" (get_trans (-1036))
        (Dc.single_string_of_tlk_string_safe
           (Load.the_game ()) this_grp) (get_trans (-1037))) !groups

let rollback_component game tp this_tp2_filename strset_backup_filename
    tlkpath_backup_filename our_lang_index i m =
  Dc.clear_state () ;
  record_strset_uninstall_info game strset_backup_filename ;
  record_tlk_path_info game tlkpath_backup_filename ;
  (match !backup_list_chn with
  | Some(chn) -> close_out chn ; backup_list_chn := None
  | None -> ()) ;
  (match !move_list_chn with
  | Some(chn) -> close_out chn ; move_list_chn := None
  | None -> ()) ;
  (match !mappings_list_chn with
  | Some(chn) -> close_out chn ; mappings_list_chn := None
  | None -> ()) ;
  let lang_name =
    (try
      let l = List.nth tp.languages !our_lang_index in
      l.lang_dir_name ;
    with _ -> "" ) in
  uninstall_tp2_component game tp this_tp2_filename i !interactive lang_name;
  print_log () ;
  if List.find_all (fun x -> x = TPM_NotInLog) m.mod_flags = [] && !safe_exit then begin
    let old_tp_quick_log = !Tp.quick_log in
    Tp.quick_log := true;
    Tpstate.save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename;
    Tp.quick_log := old_tp_quick_log;
  end

(*************************************************************************
 *************************************************************************
 * handle_tp
 *************************************************************************
 *************************************************************************)

let rec handle_tp game this_tp2_filename tp =

  ignore (init_tp_state ()) ;
  
  if !Tpstate.safe_exit && !Tp.force_uninstall_these <> [] then failwith "--safe-exit and --force-uninstall-* are not compatible";
  if !Tpstate.safe_exit && !Tp.always_uninstall then failwith "--safe-exit and --uninstall are not compatible";

  let old_allow_missing = !Load.allow_missing in
  Load.allow_missing :=
    List.fold_left (fun acc elt -> match elt with
    | Allow_Missing(lst) -> lst @ acc
    | _ -> acc) [] tp.flags ;

  let old_script_style = game.Load.script_style in
  List.iter (fun f -> match f with
  | Script_Style(s) -> (game.Load.script_style <- s;
                        match s with
                        | Load.BG2 -> Tlk.is_bg2 := true;
                        | _ -> Tlk.is_bg2 := false)
  | _ -> ()) tp.flags ;

  let last_module_index = get_highest_module_number tp.Tp.module_list in
  (* comp_num = number of user visible "component chunks", each of which
   * can have multiple sub-components *)
  let comp_num = count_comp_num tp in
  let has_quickmenu = List.exists (fun x ->
    match x with
    | Quick_Menu _ -> true
    | _ -> false) tp.flags in

  ignore (validate_uninstall_order tp);

(*************************************************************************
 * pick your language
 *************************************************************************)

  let our_lang, our_lang_index = choose_lang tp this_tp2_filename in

  ignore (set_prelang_tp2_vars tp) ;

  ignore (lang_init !our_lang) ;

  ignore (set_postlang_tp2_vars tp) ;

  if Load.enhanced_edition_p game then begin
    if not !Load.have_bgee_lang_dir_p then begin
      let dir = ask_about_lang_dir (get_trans (-1040)) in
      ignore (Load.set_bgee_lang_dir game (Some dir)) ;
      ignore (write_bgee_lang_dir game.Load.game_path dir) ;
    end ;
    (* todo: fix this mess *)
    Dc.cur_index := Array.length (Load.get_active_dialog game) ;
    log_and_print "%s %s%s\n" (get_trans (-1061))
      (Load.get_active_dialog_path game)
      (match Load.get_active_dialogf_path_opt game with
      | None -> ""
      | Some p -> (get_trans (-1062)) ^ p) ;
  end ;

  ignore (do_readme tp this_tp2_filename) ;

(**************************************************************************
 * mess begins here
 *************************************************************************)

  let any_already_installed, any_not_yet_installed =
    check_installed_state tp this_tp2_filename last_module_index in

  if has_quickmenu then
    validate_quickmenu tp ;

  let module_defaults = Array.init (last_module_index+1) (fun i ->
    module_defaults_helper tp this_tp2_filename i has_quickmenu) in

  let handle_letter_inner tp answer can_uninstall temp_uninst package_name
      m finished i =
    let subgroup_already =
      match subcomp_group m with
      | Some(ts) ->
          let res = any_member_of_subcomp_group_installed
              tp this_tp2_filename ts in
          res
      | None ->
          false in
    let answer = if temp_uninst && answer = "N" then "R" else answer in
    match answer with
    | "Q" -> begin
        for i = 0 to last_module_index do
          try
            if module_defaults.(i) <> TP_Ask then raise Not_found;
            let c = get_nth_module tp i false in
            if subcomp_forced c && not (fails_requirements tp c) then
              module_defaults.(i) <- TP_Install
            else module_defaults.(i) <- TP_Skip ;
            if !debug_ocaml then
              log_and_print "component %d is %s\n" i
                (match module_defaults.(i) with
                | TP_Install -> "Install"
                | TP_Skip -> "Skip"
                | _ -> "Internal Error");
          with Not_found -> ();
            finished := true
        done
    end
    | "N" ->
        log_and_print "\n%s [%s]\n"
          (* "\nSkipping [%s]\n" *)
          ((get_trans (-1020)))
          ((Tpstate.subcomp_str game m) ^ package_name) ;
        finished := true

    | _ when not (safe_to_handle tp.tp_filename i) -> ()

    | "I" | "Y" when subgroup_already &&
        not (already_installed tp.tp_filename i) ->
          log_or_print
            "Skipping [%s] because another subcomponent of [%s] is already installed.\n"
            package_name
            (match subcomp_group m with
            | Some(ts) -> Dc.single_string_of_tlk_string_safe game  ts
            | None -> "???") ;
          finished := true
    | "I" | "Y" | "R" -> begin
        if can_uninstall then begin
          log_and_print "\n%s%s%s\n"
            ((get_trans (-1013))) ((Tpstate.subcomp_str game m) ^ package_name)
            ((get_trans (-1014))) ;
          (if not (uninstall game handle_tp2_filename this_tp2_filename i !interactive) then
            failwith "uninstallation error");
          log_and_print
            "\n%s [%s]\n\n"
            ((get_trans (-1015))) ((Tpstate.subcomp_str game m) ^ package_name) ;
        end ;
        let subcomp_fails = ref false in
        let found = ref false in
        begin match subcomp_group m with
        | None -> ()
        | Some(x) ->
            for i = 0 to last_module_index do if not !found then
              try
                let m' = get_nth_module tp i false in
                if (subcomp_group m' = Some(x)) then begin
                  found := true;
                  subcomp_fails := fails_requirements tp m'
                end
              with _ -> ()
            done;
        end;
        if fails_requirements tp m || !subcomp_fails then begin
          finished := true;
          log_and_print "%s [%s%s\n"
            (get_trans (-1020)) ((Tpstate.subcomp_str game m) ^ package_name) (get_trans (-1037));
        end else begin
          set_backup_dir tp.backup i ;

          let strset_backup_filename =
            Printf.sprintf "%s/%d/UNSETSTR.%d" tp.backup i i in

          let tlkpath_backup_filename =
            Printf.sprintf "%s/%d/TLKPATH.%d" tp.backup i i in

          let args_backup_filename =
            Printf.sprintf "%s/%d/ARGS.%d" tp.backup i i in

          let readln_backup_filename =
            Printf.sprintf "%s/%d/READLN.%d" tp.backup i i in

          Var.set_string "WEIDU_EXECUTABLE" Sys.argv.(0);

          ignore (handle_cli_vars args_backup_filename) ;

          log_and_print "\n%s [%s]%s\n"
            (* "\nInstalling [%s]\n"  *)
            ((get_trans (-1016)))
            ((Tpstate.subcomp_str game m) ^ package_name) (version_msg tp);
          (try
            Var.set_int32 "COMPONENT_NUMBER" (Int32.of_int i) ;
            Var.set_int32 "INTERACTIVE"
              (Int32.of_int (if !interactive then 1 else 0)) ;
            let old_silent = !be_silent in
            be_silent := true;
            process_action_real our_lang game this_tp2_filename tp
              (TP_Include Tph.builtin_definitions);
            be_silent := old_silent;

            if List.find_all (fun x -> x = TPM_NotInLog) m.mod_flags = [] &&
              !safe_exit then begin
              let old_log = !the_log in
              the_log := !the_log @
                [((String.uppercase this_tp2_filename), !our_lang_index, i,
                  Some(package_name), Installed)];
              let old_tp_quick_log = !Tp.quick_log in
              Tp.quick_log := true;
              Tpstate.save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename;
              Tp.quick_log := old_tp_quick_log;
              the_log := old_log;
            end ;

            List.iter (fun flag -> match flag with
            | Always(al) -> List.iter (process_action_real our_lang game this_tp2_filename tp) al
            | TP_No_If_Eval () -> has_if_eval_bug := false ;
            | Define_Action_Macro(str,decl,al) ->
                Hashtbl.replace macros (str,false) (decl, [TP_PatchInnerAction al])
            | Define_Patch_Macro(str,decl,al) ->
                Hashtbl.replace macros (str,true) (decl, al)
            | _ -> ()) tp.flags ;
            if file_exists readln_backup_filename && not !interactive then begin
              try
                let record = Mymarshal.read_readln readln_backup_filename in
                readln_strings := record
              with e ->
                (try assert false with Assert_failure(file,line,col) ->
                  set_errors file line);
                log_and_print "WARNING: Unable to read readln references from [%s]: %s\n"
                  args_backup_filename (printexc_to_string e)
            end ;
            List.iter (process_action_real our_lang game this_tp2_filename tp) m.mod_parts ;
            if !interactive then begin
              Mymarshal.write_readln readln_backup_filename (List.rev !readln_strings);
            end ;
            readln_strings := [] ;
            be_silent := false ;
          with
          | Abort msg ->
              be_silent := false ;
              append_to_strings_to_print_at_exit
                (* INSTALLATION ABORTED *)
                (get_trans (-1063))
                ((Tpstate.subcomp_str game m) ^ package_name) ;
              log_and_print "\n%s%s%s\n"
                (get_trans (-1064)) ((Tpstate.subcomp_str game m) ^ package_name)
                (get_trans (-1065)) ;
              rollback_component game tp this_tp2_filename
                strset_backup_filename tlkpath_backup_filename
                our_lang_index i m ;
              finished := true ;
              raise (Abort msg)
          | e -> begin
              be_silent := false ;
              append_to_strings_to_print_at_exit
                (*  "\nNOT INSTALLED: ERRORS [%s]\n"  *)
                (get_trans (-1032))
                ((Tpstate.subcomp_str game m) ^ package_name) ;
              (* add this successful install to the log! *)
              exit_status := StatusInstallFailure ;
              log_and_print "\n%s%s%s\n"
                (*  "\nERROR Installing [%s], rolling back to previous state\n" *)
                ((get_trans (-1017)))
                ((Tpstate.subcomp_str game m) ^ package_name)
                ((get_trans (-1018))) ;
              rollback_component game tp this_tp2_filename
                strset_backup_filename tlkpath_backup_filename
                our_lang_index i m ;
              raise e
          end );
          log_and_print "\n\n" ;
          record_strset_uninstall_info game strset_backup_filename ;
          record_tlk_path_info game tlkpath_backup_filename ;
          let return_code = match !errors_this_component with
          | false -> -1019
          | true -> errors_this_component := false ;
              exit_status := StatusInstallWarning ; -1033
          in
          append_to_strings_to_print_at_exit
            (*  "\nSUCCESSFULLY INSTALLED [%s]\n"  *)
            (get_trans return_code)
            ((Tpstate.subcomp_str game m) ^ package_name) ;
          (* add this successful install to the log! *)
          begin
            if List.find_all (fun x -> x = TPM_NotInLog) m.mod_flags = [] then
              the_log := !the_log @
                [((String.uppercase this_tp2_filename),!our_lang_index,i,Some(package_name),Installed)]
            else (* log_and_print "NOT adding a WeiDU.log record. You cannot uninstall this.\n" *) ()
          end ;
          finished := true
        end
    end
    | "U" when not can_uninstall ->
        log_and_print "\nYou can't uninstall the non-installed component [%s] (component #%d)\n"
          ((Tpstate.subcomp_str game m) ^ package_name) i ;
        finished := false ;
    | "U" ->
        log_and_print "\n%s%s%s%d%s\n"
          (* "\nRemoving [%s] (component #%d)\n" *)
          ((get_trans (-1021)))
          ((Tpstate.subcomp_str game m) ^ package_name)
          ((get_trans (-1022)))
          i
          ((get_trans (-1023))) ;
        (if not (uninstall game handle_tp2_filename this_tp2_filename i !interactive ) then
          failwith "uninstallation error" );
        log_and_print "\n\n%s%s%s%d%s\n"
          (* "\n\nSUCCESSFULLY REMOVED [%s] (component #%d)\n\n" *)
          ((get_trans (-1024)))
          ((Tpstate.subcomp_str game m) ^ package_name)
          ((get_trans (-1022)))
          i
          ((get_trans (-1023))) ;
        finished := true
    | _ -> ()
  in

  let handle_letter tp answer can_uninstall temp_uninst package_name m finished i =
    saved_tp := Some tp;
    try
      let ans = handle_letter_inner tp answer can_uninstall temp_uninst package_name m finished i in
      saved_tp := None;
      ans
    with e ->
      saved_tp := None;
      raise e
  in

  let specify = ref false in

  let groups_ht = Hashtbl.create 5 in
  List.iter (fun m ->
    List.iter (fun f ->
      match f with
      | TPM_Group(x,c) ->
          if Hashtbl.mem groups_ht x then
            Hashtbl.replace groups_ht x (Tp.PE_Or(c, Hashtbl.find groups_ht x))
          else
            Hashtbl.add groups_ht x c
      | _ -> ()) m.mod_flags) (List.rev tp.module_list);

  let groups = ref [] in

  let rec found a lst = match lst with
  | (b,_) :: tl when b = a -> true
  | b :: tl -> found a tl
  | [] -> false
  in

  List.iter (fun x ->
    match findgroup x.mod_flags with
    | Some(a,_) -> if not (found a !groups) then begin
        groups := (a,Hashtbl.find groups_ht a) :: !groups;
    end ;
    | None -> ()) (List.rev tp.module_list) ;
  let hasgroups = !groups <> [] in

  let has_ask_every =
    !ask_all || List.exists (fun a -> a = Ask_Every_Component) tp.flags in

  let has_ask_only =
    !Tp.ask_only <> [] in

  let using_quickmenu = ref false in
  let quickmenu, always = if has_quickmenu then begin
    let quickmenu = List.find (fun x ->
      match x with
      | Quick_Menu _ -> true
      | _ -> false) tp.flags in
    match quickmenu with
    | Quick_Menu (x, y) -> (x, y)
    | _ -> (try assert false with Assert_failure (s, l, c) ->
        failwith (Printf.sprintf "Internal WeiDU failure: %s %d %d" s l c))
  end else ([], []) in

   (* for big mods, ask about things in general first *)
   if not !always_yes && not !always_uninstall &&
   not !sometimes_reinstall && not (!specified_specific_components)
   then begin
     if has_quickmenu then begin
       log_and_print "\n%s %d %s"
         (Var.get_string (get_trans (-1000))) comp_num (get_trans (-1001)) ;
       ask_about_quickmenu tp this_tp2_filename using_quickmenu module_defaults
         quickmenu always last_module_index any_already_installed ;
     end else if comp_num > 4 && not hasgroups && not has_ask_every &&
       not has_ask_only then begin
       (* add (-1000) "\nThis mod has %d distinct optional components.\nTo save time, you can choose what to do with them at a high level rather\nthan being asked about each one.\n" ; *)
       log_and_print "\n%s %d %s"
         (Var.get_string(get_trans (-1000))) comp_num (get_trans (-1001)) ;
       ask_about_ungrouped tp this_tp2_filename module_defaults
         any_not_yet_installed any_already_installed last_module_index specify ;
     end
   end ;

  (* ask only about these *)
  if has_ask_only then
    for i = 0 to last_module_index do
      if (not !using_quickmenu || List.mem i always) then
        try
          let m = get_nth_module tp i false in
          if List.mem i !Tp.ask_only then
            module_defaults.(i) <- TP_Ask
          else
            module_defaults.(i) <- TP_Skip
        with _ -> () ;
    done ;

  (* now ask about groups *)
  let any_group_to_be_asked =
    let ans = ref false in
    for i = 0 to last_module_index do
      try
        let m = get_nth_module tp i false in
        if module_defaults.(i) = TP_Ask && hasgroup m.mod_flags &&
          (not !using_quickmenu || List.mem i always) then ans := true
      with _ -> ()
    done;
    !ans
  in

  if hasgroups && not !always_yes && not !always_uninstall &&
    not !sometimes_reinstall && not (!specified_specific_components) &&
    any_group_to_be_asked
  then begin
    ask_about_groups tp groups module_defaults last_module_index
      using_quickmenu always (has_ask_only) ;
  end;

  let handle_error_generic always_yes specified_specific_components
      finished package_name =
    (fun e ->
      (match e with
      | Abort msg ->
          log_and_print "ABORT: %s\n" msg ;
          Dc.clear_state () ;
          if !always_yes || !specified_specific_components then begin
            log_and_print
              "Automatically skipping [%s] because it was aborted.\n"
              package_name ;
            finished := true
          end ;
          lang_init !our_lang
      | e ->
          exit_status := StatusInstallFailure ;
          log_and_print "ERROR: %s\n" (printexc_to_string e) ;
          Dc.clear_state () ;
          (if (!log_file <> "") then
            log_and_print "%s %s %s %s\n" ((get_trans (-1004))) !log_file
              (get_trans (-1005)) tp.author) ;
          (* log_and_print "Please make backup of the file: %s and look for support at: %s\n" !log_file tp.author;*)
          if !always_yes || !specified_specific_components then begin
            log_and_print "Automatically Skipping [%s] because of error.\n"
              package_name ;
            finished := true
          end ;
          lang_init !our_lang))
  in

  let asked_about_comp = Hashtbl.create 255 in

  let ask_about_module_with_subcomp current m subcomp handle_error =
    let subcomp_group_str =
      Dc.single_string_of_tlk_string_safe (Load.the_game ()) subcomp in
    if Hashtbl.mem asked_about_comp subcomp then
      log_or_print "Already Asked About [%s]\n" subcomp_group_str
    else begin
      Hashtbl.add asked_about_comp subcomp true ;
      let any_already = any_member_of_subcomp_group_installed
          tp this_tp2_filename subcomp in
      let finished = ref false in
      while not !finished do
        try
          let choice_num = ref 1 in
          let choice_ht = Hashtbl.create 255 in
          let already_ht = Hashtbl.create 255 in
          let uninstalled_because_predicate = ref false in
          let is_forced = ref false in
          let at_least_one_OK = ref false in
          for i = 0 to last_module_index do
            try let m = get_nth_module tp i false in
            match subcomp_group m with
            | Some(ts) when ts = subcomp && fails_requirements tp m &&
                not (subcomp_predicate m) &&
                already_installed this_tp2_filename i ->
                let can_uninstall = already_installed this_tp2_filename i in
                let temp_uninst   = temporarily_uninstalled this_tp2_filename i in
                uninstalled_because_predicate := true ;
                let package_name = Dc.single_string_of_tlk_string_safe
                    (Load.the_game ()) m.mod_name in
                log_and_print "Uninstalling the component %s because its predicate is no longer true.\n"
                  ((Tpstate.subcomp_str game m) ^ package_name) ;
                handle_letter tp "U" can_uninstall temp_uninst package_name m finished i ;
            | Some(ts) when ts = subcomp ->
                is_forced := !is_forced || (subcomp_forced m);
                  at_least_one_OK := (!at_least_one_OK ||
                  not (fails_requirements tp m)) && (subcomp_predicate m)
                | _ -> ()
            with Not_found -> ()
          done ;
          let any_already = any_already && not !uninstalled_because_predicate in
          if !at_least_one_OK then begin
            (if (any_already) then begin
              if not !is_forced then
                log_and_print "\n%s%s%s\n" (get_trans (-1006)) subcomp_group_str
                  (get_trans (-1025))
              else log_and_print "\n%s%s%s\n"  (get_trans (-1006)) subcomp_group_str
                  (get_trans (-1031))
            end else begin
              (if !is_forced then
                log_and_print "\n%s%s%s\n" (get_trans (-1006)) subcomp_group_str
                  (get_trans (-1030))
              else
                log_and_print "\n%s%s%s\n" (get_trans (-1006)) subcomp_group_str
                  (get_trans (-1026)) )
            end) ;
            let any_unsafe = ref false in
            for i = 0 to last_module_index do
              try let m = get_nth_module tp i false in
              match subcomp_group m with
              | Some(ts) when ts = subcomp && (subcomp_predicate m) &&
                  not (fails_requirements tp m) ->
                  if not (safe_to_handle tp.tp_filename i) then any_unsafe := true;
                  let this_subcomp_name = Dc.single_string_of_tlk_string_safe
                      game m.mod_name in
                  log_and_print "%2d] %s" !choice_num this_subcomp_name ;
                  (if already_installed this_tp2_filename i then begin
                    log_and_print "%s\n" (get_trans (-1027)) ;
                    Hashtbl.add already_ht () (m,i) ;
                    Hashtbl.add choice_ht !choice_num (m,i,true)
                  end else begin
                    log_and_print "\n" ;
                    Hashtbl.add choice_ht !choice_num (m,i,false) ;
                  end);
                  incr choice_num
              | _ -> ()
              with Not_found -> ()
            done ;
            if !any_unsafe then
              log_and_print "Because of --safe-exit, only [N] and [Q] are acceptable.\n";
            let answer = String.uppercase (read_line ()) in
            let answer = if Hashtbl.mem already_ht () then begin
              let (m,i) = Hashtbl.find already_ht () in
              let temp_uninst = temporarily_uninstalled this_tp2_filename i in
              if temp_uninst && answer = "N" then "R" else answer
            end else answer in
            (match answer with
            | "U" when !is_forced -> finished := false ;
            | "N" when any_already || not !is_forced ->
                finished := true;
            | "Q" ->
                for i = 0 to last_module_index do
                  try
                    if module_defaults.(i) <> TP_Ask then raise Not_found;
                    let c = get_nth_module tp i false in
                    if subcomp_forced c && not (fails_requirements tp c) then
                      module_defaults.(i) <- TP_Install
                    else module_defaults.(i) <- TP_Skip ;
                  with Not_found -> ();
                done;
                finished := true
            | _ when !any_unsafe -> finished := false ;
            | "U" | "R" ->
                if Hashtbl.mem already_ht () then begin
                  let (m,i) = Hashtbl.find already_ht () in
                  let can_uninstall = already_installed this_tp2_filename i in
                  let temp_uninst = temporarily_uninstalled this_tp2_filename i in
                  let package_name =
                    Dc.single_string_of_tlk_string_safe (Load.the_game ()) m.mod_name in
                  handle_letter tp answer can_uninstall temp_uninst
                    package_name m finished i ;
                  finished := true
                end
            | _ ->
                begin
                  let choice, ok = try (int_of_string answer,true)
                  with _ -> (0,false)
                  in
                  if (ok) then
                    if Hashtbl.mem choice_ht choice then begin
                      let (m,i,already) = Hashtbl.find choice_ht choice in
                      if (already) then
                        let can_uninstall = already_installed
                            this_tp2_filename i in
                        let temp_uninst = temporarily_uninstalled
                            this_tp2_filename i in
                        let package_name =
                          Dc.single_string_of_tlk_string_safe (Load.the_game ()) m.mod_name in
                        handle_letter tp "I" can_uninstall temp_uninst
                          package_name m finished i
                      else begin
                        log_or_print "Uninstalling All Other Subcomponents of [%s]\n"
                          subcomp_group_str ;
                        Hashtbl.iter (fun () (m,i) ->
                          let can_uninstall = already_installed
                              this_tp2_filename i in
                          let temp_uninst = temporarily_uninstalled
                              this_tp2_filename i in
                          let package_name =
                            Dc.single_string_of_tlk_string_safe (Load.the_game ()) m.mod_name in
                          handle_letter tp "U" can_uninstall temp_uninst
                            package_name m finished i) already_ht ;
                        log_or_print "Done Uninstalling All Other Subcomponents of [%s]\n"
                          subcomp_group_str ;
                        let can_uninstall = already_installed
                            this_tp2_filename i in
                        let temp_uninst = temporarily_uninstalled this_tp2_filename i in
                        let package_name =
                          Dc.single_string_of_tlk_string_safe (Load.the_game ()) m.mod_name in
                        handle_letter tp "Y" can_uninstall temp_uninst
                          package_name m finished i ;
                        finished := true
                      end
                    end
                end)
          end else finished := true ;
        with e -> handle_error e
      done
    end in

  let ask_about_module current m package_name handle_error = begin
    let finished = ref false in
    while not !finished && module_defaults.(!current) <> TP_Skip do try
      let can_uninstall = already_installed this_tp2_filename !current in
      let temp_uninst = temporarily_uninstalled this_tp2_filename !current in
      if can_uninstall then
        (* log_and_print "\nInstall Component [%s]\n[R]e-Install, [N]o Change or [U]ninstall or [Q]uit? "  package_name *)
        log_and_print "\n%s%s%s" (get_trans (-1006)) ((Tpstate.subcomp_str game m) ^ package_name) (get_trans (-1007))
      else
        (* log_and_print "\nInstall Component [%s]\n[Y]es or [N]o or [Q]uit? "  package_name ; *)
        log_and_print "\n%s%s%s" (get_trans (-1006)) ((Tpstate.subcomp_str game m) ^ package_name) (get_trans (-1008)) ;
      if not (safe_to_handle tp.tp_filename !current) then
        log_and_print "\nBecause of --safe-exit, only [N] and [Q] are acceptable. ";
      begin
        let answer = String.uppercase(read_line ()) in
        handle_letter tp answer can_uninstall temp_uninst package_name m
          finished !current ;
        Dc.clear_state () ;
      end
    with e -> handle_error e
    done
  end in

  print_log () ;

  let original_menu_style () =
    let current = ref (-1) in
    List.iter (fun m ->
      incr current ;
      List.iter (fun f -> match f with
      | TPM_Designated(i) -> current := i ;
      | _ -> ()) m.mod_flags ;
      let def = ref (module_defaults.(!current)) in
      let can_uninstall = already_installed this_tp2_filename !current in
      let temp_uninst = temporarily_uninstalled this_tp2_filename !current in
      List.iter (fun f -> match f with
      | TPM_InstallByDefault ->
          if !def = TP_Ask then def := TP_Install
      | _ -> ()) m.mod_flags ;
      let package_name = Dc.single_string_of_tlk_string_safe game m.mod_name in

      let preproc_fail msg warn already is_forbid_file =
        if (!def <> TP_Skip) && (!def <> TP_Uninstall) then begin
          (* WW: the "FORBID_FILE" bug -- don't uninstall things here! *)
          if (already) then
            def := (if is_forbid_file then TP_Ask else TP_Uninstall)
          else begin
            let warn = Dc.single_string_of_tlk_string_safe game warn in
            log_and_print "\n%s: [%s]\n\t%s\n" msg ((Tpstate.subcomp_str game m) ^ package_name) warn ;
            def := TP_Skip
          end
        end
      in

      List.iter (fun f -> match f with
      | TPM_Deprecated(warn) ->
          begin
            if can_uninstall then begin
              let warn = Dc.single_string_of_tlk_string game warn in
              log_and_print "\nNOTE: [%s] is deprecated. Uninstalling!\n\t%s\n"
                ((Tpstate.subcomp_str game m) ^ package_name) warn ;
              def := TP_Uninstall ;
            end else begin
              def := TP_Skip ;
            end
          end
      | TPM_RequireComponent(s,i,warn) ->
          begin
            if already_installed s i &&
              not (temporarily_uninstalled s i) then
              () (* good! *)
            else preproc_fail "SKIPPING" warn can_uninstall false
          end
      | TPM_ForbidComponent(s,i,warn) ->
          begin
            if already_installed s i &&
              not (temporarily_uninstalled s i) then
              preproc_fail "SKIPPING" warn can_uninstall true
            else
              () (* good! *)
          end
      | TPM_RequirePredicate(p,warn) ->
          begin
            if is_true (eval_pe "" game p) then
              ()
            else preproc_fail "SKIPPING" warn can_uninstall true
          end
      | TPM_Label(s) ->
          let old_errors_this_component = !errors_this_component in
          ignore(get_id_of_label tp s);
          errors_this_component := old_errors_this_component;
      | TPM_SubComponents(_,_,_) (* handled above *)
      | TPM_Designated(_)
      | TPM_InstallByDefault
      | TPM_Group _
      | TPM_NotInLog
      | TPM_Metadata(_) -> ()) m.mod_flags ;
      if not (module_groups_ok m) then
        preproc_fail "SKIPPING"
          (Dlg.Local_String {
           lse_male = "";
           lse_male_sound = "";
           lse_female = "";
           lse_female_sound = "";
         }) can_uninstall true;
      List.iter (fun a -> match a with
      | TP_Require_File(file,warn) ->
          begin
            if (bigg_file_exists file game.Load.key) then
              ()
            else preproc_fail "SKIPPING" warn can_uninstall false
          end
      | TP_Forbid_File(file,warn) ->
          begin
            if (bigg_file_exists file game.Load.key) then
              preproc_fail "SKIPPING" warn can_uninstall true
            else
              ()
          end
      | _ -> ()) m.mod_parts ;
      let finished = ref false in
      let handle_error = handle_error_generic always_yes
          specified_specific_components finished
          ((Tpstate.subcomp_str game m) ^ package_name) in
      Dc.clear_state () ;
      match !def with
      | TP_Install ->
          begin try
            handle_letter tp "Y" can_uninstall temp_uninst package_name
              m finished !current ;
          with e -> handle_error e
          end
      | TP_Uninstall ->
          begin try
            handle_letter tp "U" can_uninstall temp_uninst package_name
              m finished !current ;
          with e -> handle_error e
          end
      | TP_Skip ->
          if temp_uninst then
            handle_letter tp "I" can_uninstall temp_uninst package_name
              m finished !current;
          begin
            match subcomp_group m with
            | Some(ts) -> Hashtbl.add asked_about_comp ts true
            | None -> ()
          end
      | TP_Ask ->
          begin
            match subcomp_group m with
            | Some(ts) -> ask_about_module_with_subcomp current m ts handle_error
            | None -> ask_about_module current m package_name handle_error
          end) tp.module_list
  in

  if !specify then begin
    let finished = ref false in
    while not !finished do
      log_and_print "\n%s" (get_trans (-1009));
      let line = read_line () in
      if line = "" then
        finished := true
      else begin
        let reg = Str.regexp_string_case_fold line in
        let current = ref (-1) in
        List.iter (fun m ->
          incr current ;
          let rec process lst = match lst with
          | TPM_Designated(i) :: tl -> current := i ;
          | hd :: tl -> process tl
          | [] -> ()
          in process m.mod_flags ;
          let package_name = Dc.single_string_of_tlk_string_safe game m.mod_name in
          let handle_error = handle_error_generic (ref false) (ref false) (ref true)
              ((Tpstate.subcomp_str game m) ^ package_name) in
          try
            let _ = Str.search_forward reg package_name 0 in
            ask_about_module current m package_name handle_error
          with Not_found -> ()) tp.module_list
      end
    done
  end else begin
    original_menu_style () ;
  end ;

  interactive := false ;

  (* now we must handle every temporarily-uninstalled mods *)
  let re_installed = ref [] in
  let rec process lst = match lst with
  | [] -> []
  | (_,_,_,_,Installed) as head :: tl -> head :: (process tl)
  | (_,_,_,_,Permanently_Uninstalled) as head :: tl -> head :: (process tl)
  | (a,b,c,sopt,Temporarily_Uninstalled) as head :: tl ->
      begin
        try
          (* we must re-install it! *)
          begin
            (* log_and_print "\nRe-Installing [%s] component %d %s\n"
               a c (str_of_str_opt sopt); *)
            log_and_print "\n%s%s%s %d %s\n"
              (get_trans (-1010)) a (get_trans (-1011)) c
              (str_of_str_opt sopt);
            let tp_file = a in
            let tp2 = handle_tp2_filename tp_file in
            Load.allow_missing := !Load.allow_missing @
              List.fold_left (fun acc elt -> match elt with
              | Allow_Missing(lst) -> lst @ acc
              | _ -> acc) [] tp2.flags ;
            (* load their chosen language *)
            Dc.clear_state () ;
            Dc.push_trans ();
            init_default_strings () ;
            ignore (set_prelang_tp2_vars tp2) ;
            (try
              let l = List.nth tp2.languages b in
              our_lang := Some(l) ;
              our_lang_index := b ;
              List.iter handle_tra_filename (List.map
                                               Arch.backslash_to_slash
                                               (List.map Var.get_string l.lang_tra_files));
              (*  log_and_print "Re-Installing Using Language [%s]\n" l.lang_name ;*)
              log_and_print "%s [%s]\n" ((get_trans (-1012))) l.lang_name ;
              Var.set_string "LANGUAGE" l.lang_dir_name ;
              ignore (set_postlang_tp2_vars tp2) ;
            with _ ->
              our_lang := None ;
              our_lang_index := 0 ;
              ()) ;
            let m = get_nth_module tp2 c true in
            let package_name = Dc.single_string_of_tlk_string_safe game m.mod_name in
            let fails_requirements = List.exists (fun f -> match f with
            | TPM_RequireComponent(s,i,warn) ->
                begin
                  if already_installed s i then
                    false
                  else begin
                    log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                      a c (str_of_str_opt sopt);
                    true
                  end
                end
            | TPM_ForbidComponent(s,i,warn) ->
                begin
                  if not (already_installed s i) then
                    false
                  else begin
                    log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                      a c (str_of_str_opt sopt);
                    true
                  end
                end
            | TPM_Deprecated(warn) -> begin
                log_and_print "\n[%s] component %d %s is deprecated, *not* Re-Installing.\n"
                  a c (str_of_str_opt sopt);
                true
            end
            | TPM_RequirePredicate(p,warn) ->
                begin
                  if is_true (eval_pe "" game p) then
                    false
                  else begin
                    log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                      a c (str_of_str_opt sopt);
                    true
                  end
                end
            | TPM_Label(s) -> ignore(get_id_of_label tp2 s); false
            | _ -> false) m.mod_flags || (if module_groups_ok m then false else begin
                log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                  a c (str_of_str_opt sopt);
                true end) ||
                List.exists (fun f -> match f with
                | TP_Require_File(file,warn) ->
                    begin
                      if (bigg_file_exists file game.Load.key) then
                        false
                      else begin
                        log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                          a c (str_of_str_opt sopt);
                        true
                      end
                    end
                | TP_Forbid_File(file,warn) ->
                    begin
                      if (bigg_file_exists file game.Load.key) then begin
                        log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n"
                          a c (str_of_str_opt sopt);
                        true
                      end
                      else
                        false
                    end
                | _ -> false) m.mod_parts
            in
            begin
              if fails_requirements then begin
                temp_to_perm_uninstalled tp2.tp_filename c handle_tp2_filename game ;
                re_installed := !re_installed @ [(a,b,c,sopt,Permanently_Uninstalled)] ;
                the_log := (a,b,c,sopt,Permanently_Uninstalled) :: !the_log;
              end else begin
                handle_letter tp2 "R" false false package_name m (ref false) c ;
                re_installed := !re_installed @ [(a,b,c,sopt,Installed)] ;
                the_log := (a,b,c,sopt,Installed) :: !the_log;
              end
            end ;
            Dc.clear_state () ;
            Dc.pop_trans ();
          end ;
          (process tl)
        with e ->
          (try assert false with Assert_failure(file,line,col) ->
            set_errors file line);
          log_and_print "ERROR Re-Installing [%s] component %d %s\nTry to re-install it manually.\n%s\n"
            a c (str_of_str_opt sopt) (printexc_to_string e) ;
          exit_status := StatusInstallFailure ;
          (a,b,c,sopt,Permanently_Uninstalled) :: (process tl)
      end
  in
  let result = process !the_log in

  the_log := result @ !re_installed ;

  save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename ;

  Load.allow_missing := old_allow_missing ;
  (match old_script_style with
  | Load.BG2 -> Tlk.is_bg2 := true;
  | _ -> Tlk.is_bg2 := false);
  game.Load.script_style <- old_script_style
