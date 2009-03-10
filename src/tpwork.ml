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


(*************************************************************************
 *************************************************************************
                                 handle_tp
 *************************************************************************
 *************************************************************************)

let rec handle_tp
      game
      this_tp2_filename
      tp
= begin

  let old_allow_missing = !Load.allow_missing in
  Load.allow_missing :=
    List.fold_left (fun acc elt -> match elt with
      Allow_Missing(lst) -> lst @ acc
      | _ -> acc) [] tp.flags ;

  let old_script_style = game.Load.script_style in
  List.iter (fun f -> match f with
  | Script_Style(s) -> (game.Load.script_style <- s;
  		match s with
  			| Load.BG2 -> Tlk.is_bg2 := true;
  			| _ -> Tlk.is_bg2 := false
  	)
  | _ -> ()
  ) tp.flags ;

(*************************************************************************
                            pick your language
 *************************************************************************)
  let our_lang = ref None in
  let our_lang_index = ref 0 in
  begin
    match tp.languages with
      [] -> ()
    | [l] -> our_lang := Some(l)
    | _ ->
      let arr = Array.of_list tp.languages in
      let answer, answer_index =
        match !always_uninstall || !always_yes || !sometimes_reinstall || (!force_uninstall_these <> [] && !force_install_these = [])
                  , (installed_lang_index this_tp2_filename) with
        | true, Some(i) when i >= 0 && (i < Array.length arr) ->
            Some(arr.(i)), i
        | _, _ -> None, 0
      in
      let answer, answer_index = ref answer, ref answer_index in
      if !forced_language >= 0 && (!forced_language < Array.length arr) then begin
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
      our_lang := !answer ;
      our_lang_index := !answer_index ;
  end ;
  let lang_init () =
    init_default_strings () ;
    begin
      match !our_lang with
        None -> ()
      | Some(l) ->
          Var.set_string "LANGUAGE" l.lang_dir_name ;
          log_and_print "Using Language [%s]\n" l.lang_name ;
					Var.set_string "TP2_FILE_NAME" tp.tp_filename;
          log_or_print "[%s] has %d top-level TRA files\n"
            l.lang_name (List.length l.lang_tra_files) ;
          List.iter handle_tra_filename (List.map Arch.backslash_to_slash l.lang_tra_files)
    end
  in
  Var.set_string "TP2_AUTHOR" tp.author ;
  Var.set_string "TP2_FILE_NAME" tp.tp_filename;

  lang_init () ;

	List.iter (fun flag ->
		match flag with
		| Readme(str_l) -> begin
			let rec walk str_l =
				match str_l with
				| str::tail ->
					let str = Var.get_string str in
					if file_exists str then begin
						let str = "VIEW " ^ str in
						let str = Arch.handle_view_command str !skip_at_view in
						let answer = ref "" in
						if !always_uninstall || !always_yes || !sometimes_reinstall ||
						    !force_uninstall_these <> [] || !force_install_these <> [] then
							answer := "Y";
						if !skip_at_view || not !interactive then answer := "N";
						while !answer <> "Y" && !answer <> "N" do
							log_and_print "\nWould you like to display the readme? [Y]es [N]o\n";
							answer := String.uppercase (read_line())
						done;
						if !answer = "Y" then
							ignore (Unix.system (Arch.slash_to_backslash str));
						end
					else walk tail
				| [] -> log_and_print "\nCouldn't open the readme: file not found.\n"
				in walk str_l
			end
		| _ -> ()
	)  tp.flags
	;

  let subcomp_group the_comp =
    let rec walk lst = match lst with
    | TPM_SubComponents(ts,_,_) :: tl -> Some(ts)
    | hd :: tl -> walk tl
    | [] -> None
    in walk the_comp.mod_flags
  in
  let subcomp_predicate the_comp =
    let rec walk lst = match lst with
    | TPM_SubComponents(_,p,_) :: tl -> is_true (eval_pe "" game p)
    | hd :: tl -> walk tl
    | [] -> false
    in walk the_comp.mod_flags
  in
  let subcomp_forced the_comp=
    let rec walk lst = match lst with
    | TPM_SubComponents(_,_,b) :: tl -> b
    | hd :: tl -> walk tl
    | [] -> false
    in walk the_comp.mod_flags
  in
  let fails_requirements m = List.exists (fun f -> match f with
      | TPM_RequireComponent(s,i,warn) ->
          not (already_installed s i)
      | TPM_ForbidComponent(s,i,warn) ->
          (already_installed s i)
      | TPM_Deprecated(warn) ->
          true
      | TPM_RequirePredicate(p,warn) ->
          not (is_true (eval_pe "" game p))
      | _ ->
          false
    ) m.mod_flags
    ||
    List.exists (fun f -> match f with
      | TP_Require_File(file,warn) ->
          not (bigg_file_exists file game.Load.key)
      | TP_Forbid_File(file,warn) ->
          bigg_file_exists file game.Load.key
      | _ -> false
    ) m.mod_parts

  in

  let last_module_index = get_last_module_index tp in
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
    | None -> ()) 
  ) tp.module_list ; 
  let comp_num = !comp_num in
  (* comp_num = number of user visible "component chunks", each of which
   * can have multiple sub-components *)
   
  let any_member_of_subcomp_group_installed the_comp = 
    let i = ref 0 in
    List.exists (fun the_mod ->
      List.iter (fun f -> match f with
      | TPM_Designated(i') -> i := i' ;
      | _ -> ()
      ) the_mod.mod_flags ; 
      match subcomp_group the_mod with
      | Some(ts) -> if ts = the_comp &&
                       already_installed this_tp2_filename !i then
                      true
                    else (incr i ; false)
      | None -> (incr i; false)
    ) tp.module_list
  in


  let any_already_installed = ref false in 
  let any_not_yet_installed = ref false in 

  let subcomp_installed = Hashtbl.create 255 in 

  for i = 0 to last_module_index do 
    try let m = get_nth_module tp i false in
    if already_installed this_tp2_filename i then begin 
      any_already_installed := true ;
      match subcomp_group m with
      | Some(ts) -> Hashtbl.add subcomp_installed ts true
      | None -> ()
    end else begin
      let deprecated = List.exists (fun f -> match f with 
        | TPM_Deprecated(warn) -> true
        | _ -> false) m.mod_flags 
      in 
      if not deprecated then begin
        let group_already =
          match subcomp_group m with
          | Some(ts) -> Hashtbl.mem subcomp_installed ts
          | None -> false
        in
        if not group_already then
          any_not_yet_installed := true
      end
    end
    with Not_found -> ()
  done ;

  let rec hasgroup x =
    match x with
    | TPM_Group(c)::b -> true
    | a::b -> hasgroup b
    | [] -> false
  in

  let module_defaults = Array.init (last_module_index+1) (fun i ->
      try
        let cli_uninstall = ref (List.exists (fun h -> h = i) !force_uninstall_these)  in
        let cli_install   = ref (List.exists (fun h -> h = i) !force_install_these  )  in
        if !always_yes or !cli_install then
          TP_Install
        else if (!always_uninstall or !cli_uninstall) && (already_installed this_tp2_filename i) then
          TP_Uninstall
        else if (!always_uninstall or !cli_uninstall) then
          TP_Skip
        else if !sometimes_reinstall && (already_installed this_tp2_filename i) then
          TP_Install
        else if !sometimes_reinstall then
          TP_Skip
        else if (!force_install_these <> []) || (!force_uninstall_these <> []) then
          TP_Skip
        else if (hasgroup (get_nth_module tp i false).mod_flags) then
          TP_Skip
        else
          TP_Ask
      with Not_found -> TP_Ask)
  in

  let get_trans i = Dc.single_string_of_tlk_string game (Dlg.Trans_String(i)) in

  let handle_letter tp answer can_uninstall temp_uninst package_name m finished i =
    let subgroup_already =
      match subcomp_group m with
      | Some(ts) ->
          let res = any_member_of_subcomp_group_installed ts in
          res
      | None ->
          false
    in
		let answer = if temp_uninst && answer = "N" then "R" else answer in
    match answer with
    | "Q" -> begin
        for i = 0 to last_module_index do
          try
            let c = get_nth_module tp i false in
            if subcomp_forced c && not (fails_requirements c) then module_defaults.(i) <- TP_Install
            else module_defaults.(i) <- TP_Skip ;
            if !debug_ocaml then log_and_print "component %d is %s\n" i (match module_defaults.(i) with
    					| TP_Install -> "Install"
    					| TP_Skip -> "Skip"
    					| _ -> "Internal Error");
          with Not_found -> ();
          finished := true
        done
        end

    | "I" | "Y" when subgroup_already && not (already_installed tp.tp_filename i) ->
        log_or_print "Skipping [%s] because another subcomponent of [%s] is already installed.\n" package_name
        (match subcomp_group m with
          | Some(ts) -> Dc.single_string_of_tlk_string_safe game  ts
          | None -> "???") ;
        finished := true

    | "I" | "Y" | "R" -> begin
        if can_uninstall then begin
          try
            (* log_and_print
            "\nRemoving old installation of [%s] first ...\n" *)
            log_and_print "\n%s%s%s\n"
              ((get_trans (-1013))) package_name
              ((get_trans (-1014))) ;
            (if not (uninstall game handle_tp2_filename this_tp2_filename i !interactive) then failwith "uninstallation error");
            log_and_print
              (* "\nSUCCESSFULLY REMOVED OLD [%s]\n\n"  *)
              "\n%s [%s]\n\n"
              ((get_trans (-1015))) package_name ;
          with e ->
		     	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
            log_and_print "WARNING: unable to uninstall: %s\n"
              (Printexc.to_string e)
        end ;
        if fails_requirements m then begin
        	finished := true;
        log_and_print "Skipping [%s] because another its requirements aren't met.\n" package_name;
        end else begin
	        set_backup_dir tp.backup i ;
	
	        let strset_backup_filename =
	          Printf.sprintf "%s/%d/UNSETSTR.%d" tp.backup i i
	        in
	
	        let args_backup_filename =
	          Printf.sprintf "%s/%d/ARGS.%d" tp.backup i i
	        in
	
	        let readln_backup_filename =
	          Printf.sprintf "%s/%d/READLN.%d" tp.backup i i
	        in
	
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
	              if !debug_ocaml then log_and_print "%%argv[%d]%% = %s\n" !counter varcont;
	              Var.cli_variables := Some(varcont :: (value_of_option !Var.cli_variables)) ;
	              incr counter;
	            end ;
	          done ;
	          Var.cli_variables := Some(value_of_option !Var.cli_variables)
	        end ;
	        if !interactive then begin
	          let outchan = Case_ins.perv_open_out_bin args_backup_filename in
	          Marshal.to_channel outchan (List.rev (value_of_option !Var.cli_variables)) [] ;
	          close_out outchan ;
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
	              if !debug_ocaml then log_and_print "removing the pair %%argv[%d]%% = %s\n" !counter varcont;
	              Hashtbl.remove !Var.variables varname ;
	              incr counter;
	            end ;
	          done ;
	          try
	            let infile = Case_ins.perv_open_in_bin args_backup_filename in
	            let record : string list =
	              Marshal.from_channel infile
	            in
	            close_in infile ;
	            let counter = ref 0 in
	            List.iter (fun s ->
	              Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s;
	              incr counter
	            ) record
	          with e ->
			      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
	            log_and_print "WARNING: Unable to read --args references from [%s]: %s\n"
	                                         args_backup_filename (Printexc.to_string e)
	        end ;
	
	        log_and_print "\n%s [%s]\n"
	          (* "\nInstalling [%s]\n"  *)
	              ((get_trans (-1016)))
	          package_name ;
	        (try
	          Var.set_int32 "COMPONENT_NUMBER" (Int32.of_int i) ;
	          Var.set_int32 "INTERACTIVE" (Int32.of_int (if !interactive then 1 else 0)) ;
	          let old_silent = !be_silent in
	          be_silent := true;
	          process_action_real our_lang game this_tp2_filename tp (TP_Include Tph.list_of_includes);
	          be_silent := old_silent;
	
	          List.iter (fun flag -> match flag with
	            Always(al) -> List.iter (process_action_real our_lang game this_tp2_filename tp) al
	          | TP_No_If_Eval () -> has_if_eval_bug := false ;
	          | Define_Action_Macro(str,decl,al) ->
	            Hashtbl.replace action_macros str (decl, al)
	          | Define_Patch_Macro(str,decl,al) ->
	            Hashtbl.replace patch_macros str (decl, al)
	          | _ -> ()
	          ) tp.flags ;
	          if file_exists readln_backup_filename && not !interactive then begin
	            try
	              let infile = Case_ins.perv_open_in_bin readln_backup_filename in
	              let record : (tp_pe_string * string) list =
	                Marshal.from_channel infile
	              in
	              close_in infile ;
	              readln_strings := record
	            with e ->
				      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
	              log_and_print "WARNING: Unable to read readln references from [%s]: %s\n"
	                                           args_backup_filename (Printexc.to_string e)
	          end ;
	          List.iter (process_action_real our_lang game this_tp2_filename tp) m.mod_parts ;
	          if !interactive then begin
	            let outchan = Case_ins.perv_open_out_bin readln_backup_filename in
	            Marshal.to_channel outchan (List.rev !readln_strings) [] ;
	            close_out outchan ;
	          end ;
	          readln_strings := [] ;
	          be_silent := false ;
	        with e -> begin
	          be_silent := false ;
	          append_to_strings_to_print_at_exit
	           (*  "\nNOT INSTALLED: ERRORS [%s]\n"  *)
	              (get_trans (-1032))
	            package_name;
	        (* add this successful install to the log! *)
	          return_value := return_value_error_tp2_component_install ;
	          log_and_print "\n%s%s%s\n"
	          (*  "\nERROR Installing [%s], rolling back to previous state\n" *)
	              ((get_trans (-1017))) package_name ((get_trans (-1018))) ;
	          Dc.clear_state () ;
	          record_strset_uninstall_info game strset_backup_filename ;
	          (match !backup_list_chn with
	            Some(chn) -> close_out chn ; backup_list_chn := None
	          | None -> ()) ;
	          uninstall_tp2_component game tp this_tp2_filename i false ;
	          print_log () ;
	          raise e
	        end );
	        log_and_print "\n\n" ;
	        record_strset_uninstall_info game strset_backup_filename ;
	        let return_code = match !errors_this_component with
		        | false -> -1019
	  	      | true -> errors_this_component := false; -1033
	        in
	        append_to_strings_to_print_at_exit
	           (*  "\nSUCCESSFULLY INSTALLED [%s]\n"  *)
	              (get_trans return_code)
	            package_name;
	        (* add this successful install to the log! *)
	        begin
	        if List.find_all (fun x -> x = TPM_NotInLog) m.mod_flags = [] then
	          the_log := !the_log @
	            [ ((String.uppercase this_tp2_filename),!our_lang_index,i,Some(package_name),Installed) ]
	        else (* log_and_print "NOT adding a WeiDU.log record. You cannot uninstall this.\n" *) ()
	        end ;
	        finished := true
	      end
	    end
    | "N" ->
        log_and_print "\n%s [%s]\n"
         (* "\nSkipping [%s]\n" *)
              ((get_trans (-1020)))
          package_name ;
        finished := true
    | "U" when not can_uninstall ->
      log_and_print "\nYou can't uninstall the non-installed component [%s] (component #%d)\n"
               package_name i ;
      finished := false ;
    | "U" ->
        log_and_print "\n%s%s%s%d%s\n"
         (* "\nRemoving [%s] (component #%d)\n" *)
              ((get_trans (-1021)))
              package_name
              ((get_trans (-1022)))
              i
              ((get_trans (-1023))) ;
        (if not (uninstall game handle_tp2_filename this_tp2_filename i !interactive ) then failwith "uninstallation error" );
        log_and_print "\n\n%s%s%s%d%s\n"
         (* "\n\nSUCCESSFULLY REMOVED [%s] (component #%d)\n\n" *)
              ((get_trans (-1024)))
              package_name
              ((get_trans (-1022)))
              i
              ((get_trans (-1023))) ;
        finished := true
    | _ -> ()
  in

  let specify = ref false in

  let rec findgroup flags =
    match flags with
      | TPM_Group(x)::b -> Some(x)
      | a::b -> findgroup b
      | [] -> None
  in

  let groups = ref [] in
  List.iter (fun x ->
     match findgroup x.mod_flags with
    | Some(a) -> if not (List.mem a !groups) then begin
      groups := a :: !groups;
      end ;
    | None -> ()) (List.rev tp.module_list) ;
  let hasgroups = !groups <> [] in

  let has_ask_every = !ask_all ||
    List.exists (fun a -> a = Ask_Every_Component) tp.flags
  in

  (* for big mods, ask about things in general first *)
  if comp_num > 4 && not !always_yes && not !always_uninstall &&
     not !sometimes_reinstall &&
     not (has_ask_every) && not (!specified_specific_components) && not hasgroups
  then begin
  (* add (-1000) "\nThis mod has %d distinct optional components.\nTo save time, you can choose what to do with them at a high level rather\nthan being asked about each one.\n" ; *)
      log_and_print "\n%s %d %s" (Var.get_string(get_trans (-1000))) comp_num (get_trans (-1001)) ;
      let finished = ref false in

      if !any_not_yet_installed then
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
             (* if not (any_member_of_subcomp_group_installed x) then *)
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
            let the_comp = get_nth_module tp i false in
            let c = get_nth_module tp i false in
            if subcomp_forced c && not (fails_requirements c) then module_defaults.(i) <- TP_Install
            else begin
              match subcomp_group the_comp with
              | Some(x) ->
                if not (any_member_of_subcomp_group_installed x) then
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
      if !any_already_installed then
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
             (* if not (any_member_of_subcomp_group_installed x) then *)
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
            let c = get_nth_module tp i false in
            if subcomp_forced c && not (fails_requirements c) then module_defaults.(i) <- TP_Install
            else begin
              match subcomp_group the_comp with
              | Some(x) ->
                if (any_member_of_subcomp_group_installed x) then
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
              if (any_member_of_subcomp_group_installed x) then
                module_defaults.(i) <- TP_Skip
            | None -> () ) ;
              if (already_installed this_tp2_filename i) then
                module_defaults.(i) <- TP_Uninstall
            with Not_found -> ()
          done
        | "A" -> ()
        | "X" -> specify := true
        | _ -> finished := false
      done ;
  end ;

  let is_my_group the_comp group =
    let rec walk lst = match lst with
    | TPM_Group(ts) :: tl when ts = group -> true
    | hd :: tl -> walk tl
    | [] -> false
    in walk the_comp.mod_flags
  in

  (* now ask about groups *)
  if hasgroups  && not !always_yes && not !always_uninstall &&
     not !sometimes_reinstall && not (!specified_specific_components)
    then List.iter (fun this_grp ->
      let finished = ref false in
      while not !finished do
        finished := true ;
        log_and_print "\n%s%s%s" (Var.get_string(get_trans (-1028)))
                (Dc.single_string_of_tlk_string_safe game this_grp) (get_trans (-1029)) ;
        match String.uppercase(read_line ()) with
        | "Y" ->
          for i = 0 to last_module_index do
            try
            let the_comp = get_nth_module tp i false in
            if is_my_group the_comp this_grp then module_defaults.(i) <- TP_Ask ;
            with Not_found -> ()
          done
        | "N" -> ()
        | _ -> finished := false
      done ;
  ) !groups ;

  let handle_error_generic always_yes specified_specific_components finished package_name = (fun e ->
    return_value := return_value_error_tp2_component_install ;
    log_and_print "ERROR: %s\n" (Printexc.to_string e) ;
    Dc.clear_state () ;
    (if (!log_file <> "") then
      log_and_print "%s %s %s %s\n" ((get_trans (-1004))) !log_file
                                    (get_trans (-1005)) tp.author) ;
    (* log_and_print "PLEASE email the file %s to %s\n" !log_file tp.author);*)
    if !always_yes or !specified_specific_components then begin
      log_and_print "Automatically Skipping [%s] because of error.\n"
        package_name ;
      finished := true
    end ;
    lang_init ())
  in

  let asked_about_comp = Hashtbl.create 255 in

  let ask_about_module_with_subcomp current m subcomp handle_error = begin
    let subcomp_group_str = Dc.single_string_of_tlk_string_safe game subcomp in
    if Hashtbl.mem asked_about_comp subcomp then
      log_or_print "Already Asked About [%s]\n" subcomp_group_str
    else begin
      Hashtbl.add asked_about_comp subcomp true ;
    let any_already = any_member_of_subcomp_group_installed subcomp in
    let finished = ref false in
    while not !finished do
      try
        let choice_num = ref 1 in
        let choice_ht = Hashtbl.create 255 in
        let already_ht = Hashtbl.create 255 in
        let uninstalled_because_preditate = ref false in
        let is_forced = ref false in
        let at_least_one_OK = ref false in
        for i = 0 to last_module_index do
          try let m = get_nth_module tp i false in
          match subcomp_group m with
          | Some(ts) when ts = subcomp && not(subcomp_predicate m) && already_installed this_tp2_filename i ->
            let can_uninstall = already_installed this_tp2_filename i in
            let temp_uninst   = temporarily_uninstalled this_tp2_filename i in
            uninstalled_because_preditate := true ;
            let package_name = Dc.single_string_of_tlk_string_safe
              game m.mod_name in
            log_and_print "Uninstalling the component %s because its predicate is no longer true.\n"
              package_name ;
            handle_letter tp "U" can_uninstall temp_uninst package_name m finished i ;
          | Some(ts) when ts = subcomp ->
              is_forced := !is_forced || (subcomp_forced m);
              at_least_one_OK := !at_least_one_OK || (subcomp_predicate m)
          | _ -> ()
          with Not_found -> ()
        done ;
        let any_already = any_already && not !uninstalled_because_preditate in
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
          end ) ;
          for i = 0 to last_module_index do
            try let m = get_nth_module tp i false in
            match subcomp_group m with
            | Some(ts) when ts = subcomp && (subcomp_predicate m) ->
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
              end
              );
              incr choice_num
            | _ -> ()
            with Not_found -> ()
          done ;
          let answer = String.uppercase (read_line ()) in
          let answer = if Hashtbl.mem already_ht () then begin
            let (m,i) = Hashtbl.find already_ht () in
            let temp_uninst = temporarily_uninstalled this_tp2_filename i in
						if temp_uninst && answer = "N" then "R" else answer
					end else answer in
          (match answer with
          | "U" when !is_forced -> finished := false ;
          | "U" | "R" ->
              if Hashtbl.mem already_ht () then begin
                let (m,i) = Hashtbl.find already_ht () in
                let can_uninstall = already_installed this_tp2_filename i in
                let temp_uninst = temporarily_uninstalled this_tp2_filename i in
                let package_name =
                  Dc.single_string_of_tlk_string_safe game m.mod_name in
                handle_letter tp answer can_uninstall temp_uninst
                  package_name m finished i ;
                finished := true
              end
          | "N" when any_already || not !is_forced ->
              finished := true;
          | "Q" ->
            for i = 0 to last_module_index do
              try
                let c = get_nth_module tp i false in
                if subcomp_forced c && not (fails_requirements c) then module_defaults.(i) <- TP_Install
                else module_defaults.(i) <- TP_Skip ;
              with Not_found -> ();
              finished := true
            done
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
                        Dc.single_string_of_tlk_string_safe game m.mod_name in
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
                        Dc.single_string_of_tlk_string_safe game m.mod_name in
                      handle_letter tp "U" can_uninstall temp_uninst
                        package_name m finished i
                    ) already_ht ;
                    log_or_print "Done Uninstalling All Other Subcomponents of [%s]\n" subcomp_group_str ;
                    let can_uninstall = already_installed
                      this_tp2_filename i in
                   let temp_uninst = temporarily_uninstalled this_tp2_filename i in
                   let package_name =
                      Dc.single_string_of_tlk_string_safe game m.mod_name in
                    handle_letter tp "Y" can_uninstall temp_uninst
                      package_name m finished i ;
                    finished := true
                  end
                end
            end
          )
        end else finished := true ;
      with e -> handle_error e

    done
    end
  end in

  let ask_about_module current m package_name handle_error = begin
    let finished = ref false in
        while not !finished && module_defaults.(!current) <> TP_Skip do try
          let can_uninstall = already_installed this_tp2_filename !current in
          let temp_uninst = temporarily_uninstalled this_tp2_filename !current in
            if can_uninstall then
              (* log_and_print "\nInstall Component [%s]\n[R]e-Install, [N]o Change or [U]ninstall or [Q]uit? "  package_name *)
              log_and_print "\n%s%s%s" (get_trans (-1006)) package_name (get_trans (-1007))
            else
             (* log_and_print "\nInstall Component [%s]\n[Y]es or [N]o or [Q]uit? "  package_name ; *)
              log_and_print "\n%s%s%s" (get_trans (-1006)) package_name (get_trans (-1008)) ;
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
      | _ -> ()
      ) m.mod_flags ;
      let def = ref (module_defaults.(!current)) in
      let can_uninstall = already_installed this_tp2_filename !current in
      let temp_uninst = temporarily_uninstalled this_tp2_filename !current in
      List.iter (fun f -> match f with
      | TPM_InstallByDefault ->
        if !def = TP_Ask && (not can_uninstall) then def := TP_Install
      | _ -> ()
      ) m.mod_flags ;
      let package_name = Dc.single_string_of_tlk_string_safe game m.mod_name in

      let preproc_fail msg warn already is_forbid_file =
        if (!def <> TP_Skip) && (!def <> TP_Uninstall) then begin
          (* WW: the "FORBID_FILE" bug -- don't uninstall things here! *)
        if (already) then
            def := (if is_forbid_file then TP_Ask else TP_Uninstall)
          else begin
            let warn = Dc.single_string_of_tlk_string game warn in
            log_and_print "\n%s: [%s]\n\t%s\n" msg package_name warn ;
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
              package_name warn ;
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
      | TPM_SubComponents(ts,_,_) -> ()
      | TPM_Designated(i) -> () (* handled above *)
      | TPM_InstallByDefault
      | TPM_NotInLog
      | TPM_Group (_)  -> ()
      ) m.mod_flags ;
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
        | _ -> ()
      ) m.mod_parts ;
      let finished = ref false in
      let handle_error = handle_error_generic always_yes specified_specific_components finished
        package_name in
      Dc.clear_state () ;
      match !def with
      | TP_Install ->
        begin try
          handle_letter tp "Y" can_uninstall temp_uninst package_name m finished !current ;
          with e -> handle_error e
        end
      | TP_Uninstall ->
        begin try
          handle_letter tp "U" can_uninstall temp_uninst package_name m finished !current ;
          with e -> handle_error e
        end
      | TP_Skip ->
      		if temp_uninst then
      			handle_letter tp "I" can_uninstall temp_uninst package_name m finished !current;
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
        end
    ) tp.module_list
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
            package_name in
          try 
            let _ = Str.search_forward reg package_name 0 in
            ask_about_module current m package_name handle_error
          with Not_found -> () 
        ) tp.module_list
      end
    done 
  end else begin
    original_menu_style () ; 
  end ;

  interactive := false ;

  (* now we must handle every temporarily-uninstalled mods *)
  let re_installed = ref [] in 
  let rec process lst = match lst with
    [] -> []
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
            Allow_Missing(lst) -> lst @ acc
              | _ -> acc) [] tp2.flags ; 
        (* load their chosen language *)
        Dc.clear_state () ;
        Dc.push_trans ();
        init_default_strings () ; 
        Var.set_string "TP2_AUTHOR" tp2.author ;
        Var.set_string "TP2_FILE_NAME" tp2.tp_filename;
        (try
          let l = List.nth tp2.languages b in
          our_lang := Some(l) ;
          our_lang_index := b ;
          List.iter handle_tra_filename (List.map Arch.backslash_to_slash l.lang_tra_files);
        (*  log_and_print "Re-Installing Using Language [%s]\n" l.lang_name ;*)
          log_and_print "%s [%s]\n" ((get_trans (-1012))) l.lang_name ;
          Var.set_string "LANGUAGE" l.lang_dir_name ;
          Var.set_string "TP2_FILE_NAME" tp2.tp_filename ;
        with _ ->
          our_lang := None ; 
          our_lang_index := 0 ;
          () ) ;
        let m = get_nth_module tp2 c true in 
        let package_name = Dc.single_string_of_tlk_string_safe game m.mod_name in 
        let fails_requirements = List.exists (fun f -> match f with
          | TPM_RequireComponent(s,i,warn) ->
            begin
              if already_installed s i then
                false
              else begin
                log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n" a c (str_of_str_opt sopt); 
                true
              end 
            end
          | TPM_ForbidComponent(s,i,warn) ->
            begin
              if not (already_installed s i) then
                false
              else begin
                log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n" a c (str_of_str_opt sopt);
                true
              end
            end
          | TPM_Deprecated(warn) -> begin
              log_and_print "\n[%s] component %d %s is deprecated, *not* Re-Installing.\n" a c (str_of_str_opt sopt);
              true
            end
          | TPM_RequirePredicate(p,warn) ->
            begin
              if is_true (eval_pe "" game p) then
                false
              else begin
                log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n" a c (str_of_str_opt sopt);
                true
              end
            end
          | _ -> false) m.mod_flags
          ||
          List.exists (fun f -> match f with
            | TP_Require_File(file,warn) ->
              begin
                if (bigg_file_exists file game.Load.key) then
                  false
                else begin
                  log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n" a c (str_of_str_opt sopt);
                  true
                end
              end
            | TP_Forbid_File(file,warn) ->
              begin
                if (bigg_file_exists file game.Load.key) then begin
                  log_and_print "\n[%s] component %d %s fails component requirements, *not* Re-Installing.\n" a c (str_of_str_opt sopt);
                  true
                end
                else
                  false
              end
            | _ -> false
          ) m.mod_parts

        in
        begin
        if fails_requirements then begin
          handle_letter tp2 "U" false false package_name m (ref false) c ;
          re_installed := !re_installed @ [(a,b,c,sopt,Permanently_Uninstalled)] ;
        end else begin
          handle_letter tp2 "R" false false package_name m (ref false) c ;
          re_installed := !re_installed @ [(a,b,c,sopt,Installed)] ;
        end
        end ; 
        Dc.clear_state () ; 
        Dc.pop_trans ();
      end ;
      (process tl)  
    with e ->
     	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
      log_and_print "ERROR Re-Installing [%s] component %d %s\nTry to re-install it manually.\n%s\n" a c (str_of_str_opt sopt) (Printexc.to_string e) ;
      return_value := return_value_error_tp2_component_install ; 
      (a,b,c,sopt,Permanently_Uninstalled) :: (process tl)
    end 
  in 
  let result = (process !the_log) @ !re_installed in

  the_log := result ; 

  save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename ;

  Load.allow_missing := old_allow_missing ;
  (match old_script_style with
  			| Load.BG2 -> Tlk.is_bg2 := true;
  			| _ -> Tlk.is_bg2 := false
  	);
  game.Load.script_style <- old_script_style ;
end
