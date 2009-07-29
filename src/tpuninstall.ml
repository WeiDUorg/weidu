open Util
open Tp
open Parsewrappers
open Tpstate
open Tppe

(************************************************************************
 * Uninstall STRSET 
 ************************************************************************)
let uninstall_strset game filename = 
  if (file_exists filename) then begin
    (try
      let infile = Case_ins.perv_open_in_bin filename in
      let record : Load.str_set_record list =
        Marshal.from_channel infile
      in
      close_in infile ;
      List.iter (fun (i,m,f) ->
        if (i < 0 || i > Array.length game.Load.dialog) then begin
          log_only "WARNING: Cannot uninstall STRING_SET #%d, out of range 0 -- %d\n" i (Array.length game.Load.dialog) 
        end else begin
        (*
          log_only "Un-SET_STRING #%d from %s back to %s\n"
            i
            (Tlk.short_print game.Load.dialog.(i) 18)
            (Tlk.short_print m 18) ;
            *)
          game.Load.dialog.(i) <- m ;
          game.Load.dialog_mod <- true; 
          match game.Load.dialogf with
          | Some(a) -> a.(i) <- f ; game.Load.dialogf_mod <- true; 
          | None -> ()
        end
      ) record
    with e ->
      log_and_print "WARNING: Unable to uninstall STRING_SET references from [%s]: %s\n" filename (Printexc.to_string e);
      (try assert false with Assert_failure(file,line,col) -> set_errors file line));
    my_unlink filename;
  end else log_only "[%s] SET_STRING uninstall info not found\n" filename

let record_strset_uninstall_info game filename =
  if (game.Load.str_sets = []) then
    ()
  else begin
    try 
      let outchan = Case_ins.perv_open_out_bin filename in
      Marshal.to_channel outchan game.Load.str_sets [];
      close_out outchan ;
      game.Load.str_sets <- []
    with e ->
      log_and_print "WARNING: Unable to write STRING_SET uninstall info to [%s]: %s\n" filename (Printexc.to_string e);(try assert false with Assert_failure(file,line,col) -> set_errors file line)
  end

(************************************************************************
 * Uninstall a TP2 component
 ************************************************************************)
let handle_at_uninstall tp2 m do_uninstall do_interactive_uninstall game =
    let rec handle_al al =
      List.iter (fun a ->
			match a with
        | TP_At_Interactive_Uninstall(str,exact) ->
          if do_interactive_uninstall then begin
            let str = Var.get_string str in
            match (split (String.uppercase str)) with
            | _,"TP2" -> (enqueue_tp2_filename) str
            | _,_ -> let str = if exact then str else Arch.handle_view_command str !skip_at_view in
                   ignore(exec_command str exact)
          end
        | TP_At_Uninstall(str,exact) ->
          if do_uninstall then begin
          let str = Var.get_string str in
          match (split (String.uppercase str)) with
          | _,"TP2" -> (enqueue_tp2_filename) str
          | _,_ -> let str = if exact then str else Arch.handle_view_command str !skip_at_view in
                 ignore (exec_command str exact)
          end
        | TP_If(p,al1,al2) ->
          begin try
              eval_pe_warn := false ;
              let res = is_true (eval_pe "" game p) in
              (* log_or_print "IF evaluates to %b\n" res ; *)
              if res then begin
                handle_al al1
              end else begin
                handle_al al2
              end
            with _ -> () ;
            eval_pe_warn := true ;
          end
        | TP_Biff _ ->
            (* re-load the chitin *)
            let keyname = Load.find_file_in_path "." "^chitin.key$" in
            let keybuff = load_file keyname in
            game.Load.key <- Key.load_key keyname keybuff ;
            game.Load.loaded_biffs <- Hashtbl.create 5 ;
        | TP_Include(string_list) ->
          List.iter (fun file ->
          	let file = Var.get_string file in
            if file_exists file then begin
							let incl_al = if Hashtbl.mem loaded_tph file
	              then begin
	                if !debug_ocaml then log_and_print "%s is already loaded, restoring the image...\n" file ;
	                Hashtbl.find loaded_tph file
	              end
	              else begin
	                if !debug_ocaml then log_and_print "Loading %s...\n" file ;
	                let x = handle_tph_filename file in
	                Hashtbl.replace loaded_tph file x ;
	                x
	              end
	            in
	            handle_al incl_al ;
	          end
          ) string_list ;
        | TP_Reinclude(string_list) ->
          List.iter (fun file ->
          	let file = Var.get_string file in
            if file_exists file then begin
							let incl_al =
	              if !debug_ocaml then log_and_print "Loading %s...\n" file ;
	              let x = handle_tph_filename file in
	              Hashtbl.replace loaded_tph file x ;
	              x
	            in
	            handle_al incl_al ;
	          end
          ) string_list ;
        | _ -> ()
      ) al
    in
    let handle_flag fl =
      List.iter (fun flag ->
        match flag with
        | Always(al) -> handle_al al ;
        | _ -> ()
      ) fl
    in
    handle_flag tp2.flags ;
    handle_al m.mod_parts

let uninstall_tp2_component game tp2 tp_file i interactive =
  Stats.time "tp2 uninstall" (fun () ->
  try
    let result = tp2 in
    let d = result.backup ^ "/" ^ (string_of_int i) in
    let u_filename = (Printf.sprintf "%s/UNINSTALL.%d" d i) in
    let m_filename = (Printf.sprintf "%s/MAPPINGS.%d" d i) in
    let u_strset_filename = (Printf.sprintf "%s/UNSETSTR.%d" d i) in
    uninstall_strset game u_strset_filename ;
    let file_list = ref [] in
    let mappings_list = Hashtbl.create 300 in
		let restore backup_filename override_filename =
			log_or_print "  Restoring backed-up [%s]\n" backup_filename ;
			copy_large_file backup_filename override_filename "restoring a backup" ;
			if String.uppercase override_filename = "CHITIN.KEY" then begin
        let keyname = Load.find_file_in_path "." "^chitin.key$" in
        let keybuff = load_file keyname in
        game.Load.key <- Key.load_key keyname keybuff ;
        game.Load.loaded_biffs <- Hashtbl.create 5 ;
			end;
			my_unlink backup_filename
		in
		let inchan = Case_ins.perv_open_in_bin u_filename in
		try
			while true do
				file_list := (input_line inchan) :: !file_list ;
			done
		with End_of_file -> () ;
		close_in inchan;
		let has_mappings = ref true in
		(
		try
		let inchan = Case_ins.perv_open_in_bin m_filename in
			while true do
				let line = input_line inchan in
				let pieces = Str.split (Str.regexp " ") line in
				match pieces with
				a :: b :: [] -> Hashtbl.add mappings_list a b
				| _ -> ()
			done
		with End_of_file -> (close_in inchan)
		| Sys_error _ -> has_mappings := false;
		);
		file_list := List.rev (!file_list);
		log_and_print "Will uninstall %3d files for [%s] component %d.\n"
			(List.length !file_list) tp_file i;
		List.iter (fun override_filename ->
      my_unlink override_filename;
      try
			if !has_mappings then
				restore (Hashtbl.find mappings_list override_filename) override_filename
			else begin
				let base = Case_ins.filename_basename override_filename in
				let backup_filename = d ^ "/" ^ base in
				let backup_filename1 = d ^ "/" ^ (Str.global_replace (Str.regexp "[\\/]") "." override_filename) in
				if file_exists backup_filename then
					restore backup_filename override_filename
				else if file_exists backup_filename1 then
					restore backup_filename1 override_filename
			end
			with _ -> ()
			;
		) (!file_list)  ;
		(try
			my_unlink u_filename;
			Case_ins.unix_unlink m_filename with _ -> ());
    let m = get_nth_module result i true in
    handle_at_uninstall tp2 m true interactive game ;
    log_and_print "Uninstalled    %3d files for [%s] component %d.\n"
      (List.length !file_list) tp_file i; 
  with e ->
    log_and_print "Error Uninstalling [%s] component %d:\n%s\n"
      tp_file i (Printexc.to_string e);
      (try assert false with Assert_failure(file,line,col) -> set_errors file line)
  ) ()


let temp_to_perm_uninstalled tp2 i handle_tp2_filename game =
  let rec is_installed lst = match lst with
  | [] -> []
  | (a,b,c,sopt,d) :: tl when log_match a tp2
        && c = i && d = Temporarily_Uninstalled ->
        (* if there were any "at_uninstall" actions here, do them! *)
        let tp_file = a in
        let tp2 = handle_tp2_filename tp_file in 
        let lang_name = 
        (try
          let l = List.nth tp2.languages b in
          l.lang_dir_name ;
        with _ -> "" ) in 
        Var.set_string "TP2_AUTHOR" tp2.author ;
        Var.set_string "LANGUAGE" lang_name ;
        Var.set_string "TP2_FILE_NAME" tp2.tp_filename ;
        let m = get_nth_module tp2 c true in
        log_only "Running AT_INTERACTIVE_EXITs in ~%s~ %d %d %s\n"
          (String.uppercase a) b c
          (str_of_str_opt sopt) ;
        handle_at_uninstall tp2 m
          false (* "AT_UNINSTALL" was already done! *)
          true (* but the user just asked for this to be explicit *) game ;
        (* Var.remove_var "LANGUAGE" ; *) 
        (a,b,c,sopt,Permanently_Uninstalled) :: tl
  | hd :: tl -> hd :: (is_installed tl)
  in the_log := is_installed !the_log 

(************************************************************************
 * Do everything necessary to uninstall the given tp2 component. This
 * may require temporarily uninstalling stuff that has been installed
 * since our target was installed. 
 ************************************************************************)
let rec find_best_file lst =
  match lst with
    hd :: tl -> if file_exists hd then hd
                else if file_exists (Case_ins.filename_basename hd) then
                  (Case_ins.filename_basename hd)
                else find_best_file tl
  | [] -> failwith "TP2 not found!"  

let uninstall game handle_tp2_filename tp2 i interactive =
  log_or_print "uninstall: %s %d\n" tp2 i ;
  Var.set_int32 "COMPONENT_NUMBER" (Int32.of_int i);
  let worked = ref true in 
  if not (already_installed tp2 i) then begin
    log_and_print "Internal Error: trying to uninstall non-installed mod component [%s] %d\n" tp2 i ;(try assert false with Assert_failure(file,line,col) -> set_errors file line);
    false
  end else if (temporarily_uninstalled tp2 i) then begin
    log_or_print "uninstall: %s %d already temporarily uninstalled\n" tp2 i ;
    temp_to_perm_uninstalled tp2 i handle_tp2_filename game ;
    true
  end else begin
    let rec prepare lst = match lst with
    | [] -> [] (* end of the line *)

    (* this is the entry in the list *)
    | (a,b,c,sopt,d) :: tl when log_match a tp2 && c = i ->
      begin match d with
      | Permanently_Uninstalled -> (* some sort of error *) 
                  log_and_print "Internal Error: mod component [%s] %d already uninstalled\n" tp2 i;(try assert false with Assert_failure(file,line,col) -> set_errors file line) ; (a,b,c,sopt,Permanently_Uninstalled) :: tl
      | Temporarily_Uninstalled -> (* we just won't restore it! *)
                      (a,b,c,sopt,Permanently_Uninstalled) :: tl
      | Installed -> 
          begin 
          try
            let best = find_best_file [a ; tp2] in
            uninstall_tp2_component game (handle_tp2_filename best) a c interactive ;
            (a,b,c,sopt,Permanently_Uninstalled) :: tl
          with _ ->
            log_and_print "ERROR: This Mod is too old (or too new) to uninstall that component for you.\nUpgrade to the newest versions of this mod and that one and try again.\n" ;(try assert false with Assert_failure(file,line,col) -> set_errors file line);
            worked := false ;
            lst
          end 
      end

    | (a,b,c,sopt,d) :: tl -> 
      begin match d with
      | Permanently_Uninstalled    
      | Temporarily_Uninstalled -> (* keep going *)
                      (a,b,c,sopt,d) :: (prepare tl)
      | Installed ->
          log_or_print "We must temporarily uninstall [%s] component %d\n"
            a c ; 
          begin
          try
            let best = find_best_file [ a] in
            uninstall_tp2_component game (handle_tp2_filename best) a c false ;
            (* take away for now *)
            (a,b,c,sopt,Temporarily_Uninstalled) :: (prepare tl)
          with e ->
            log_and_print "ERROR: This Mod is too old (or too new) to uninstall that component for you.\nUpgrade to the newest versions of this mod and that one and try again.\n[%s]\n"
            (Printexc.to_string e);(try assert false with Assert_failure(file,line,col) -> set_errors file line);
            worked := false ; 
            lst 
          end
      end
    in
    let new_log = List.rev (prepare (List.rev !the_log)) in
    the_log := new_log ;
    !worked
  end
