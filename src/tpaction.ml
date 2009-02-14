open Util
open Diff
open Tp
open Parsewrappers
open Tpstate
open Tphelp
open Tppe
open Tppatch
open Tpuninstall


(*************************************************************************
													 process_action
 *************************************************************************)
let rec process_action_real our_lang game this_tp2_filename tp a =

let get_next_col_number file =
	let (a,b) = split file in
	let buff,path = Load.load_resource "getting 2DA columnns" game true a b in
	try
		let lst = Str.split many_newline_or_cr_regexp buff in
		let elt = List.nth lst 5 in
		let cols = split_apart elt in
		let num_cols = List.length cols in
		let last_col = num_cols - 2 in
		last_col + 1
	with e ->
		log_and_print "ERROR: cannot find col numbers in %s\n" file ;
		raise e
in

let get_next_line_number file =
	let (a,b) = split file in
	let buff,path = Load.load_resource "getting 2DA lines" game true a b in
	try
		let idx = Str.search_backward (Str.regexp "[\r\n][0-9]") buff
			((String.length buff) - 1) in
		let minibuff = Str.string_after buff (idx+1) in
		let lst = split_apart minibuff in
		let elt = List.hd lst in
		let last_number = int_of_string elt in
		last_number + 1
	with e ->
		log_and_print "ERROR: cannot find line numbers in %s\n" file	;
		raise e
in

let process_action = (process_action_real our_lang game this_tp2_filename) in
let process_patch2 = process_patch2_real process_action tp in
let str = action_to_str a in
Stats.time str (fun () ->
try
(match a with
	| TP_Require_File(file,error_msg) ->
			log_and_print "Checking for required files ...\n" ;
			let file = Arch.backslash_to_slash file in
			let size = file_size file in
			let test = ref false in
			test := bigg_file_exists file game.Load.key ;
			if !test then begin
				log_or_print "[%s] found: %d bytes\n" file size;
			end
			else begin
				log_and_print "[%s] not found\n" file ;
				log_and_print "\n%s\n" (Dc.single_string_of_tlk_string game error_msg) ;
				failwith file ;
			end
	
	| TP_Inlined_File(name,contents) ->
			let name = Var.get_string (Arch.backslash_to_slash name) in
			log_only_modder "Defined Inlined File [%s] (length %d)\n"
				name (String.length contents) ;
			Hashtbl.add inlined_files (Arch.backslash_to_slash name) contents
	
	| TP_Biff(dest,what) ->
			let find_list = ref [] in
			let dest = Var.get_string dest in
			game.Load.key_mod <- true ;
			List.iter ( fun (directory,exact_match,regexp_string) ->
				try
					let directory = Var.get_string directory in
					let regexp_string = Var.get_string regexp_string in
					if !debug_ocaml then log_and_print "MAKE_BIFF %s %s\n" directory regexp_string;
					let dh = Case_ins.unix_opendir directory in
					let reg = begin match exact_match with
							| Some(true) ->
								Str.regexp_string_case_fold
							| _ ->
								Str.regexp_case_fold
						end regexp_string
					in
					(try
						while true do
							let next = Unix.readdir dh in
							if ((Case_ins.unix_stat (directory ^ "/" ^ next)).Unix.st_kind =
								 Unix.S_REG) && (Str.string_match reg next 0) then
								find_list := (String.uppercase (directory ^ "/" ^ next)) :: !find_list
						done
					with End_of_file -> () );
					Unix.closedir dh ;
				with _ -> ()
			) what ;
			if !debug_ocaml then log_and_print "MAKE_BIFF: got file list\n";
			if !find_list <> [] then begin
				let data = if game.Load.script_style = Load.PST then "" else "data/" in
				let filename = data ^ dest ^ ".bif" in
				let new_key = Biff.save_biff game.Load.key filename !find_list in
				if !debug_ocaml then log_and_print "MAKE_BIFF: Calculated new key\n";
				let oc = open_for_writing "CHITIN.KEY" true in
				Key.save_key new_key oc ;
				close_out oc ;
				if !debug_ocaml then log_and_print "MAKE_BIFF: Saved the key\n";
				(* re-load the chitin *)
				let keybuff = load_file "chitin.key" in
				if !debug_ocaml then log_and_print "MAKE_BIFF: Loaded the key file\n";
				game.Load.key <- Key.load_key "chitin.key" keybuff ;
				game.Load.loaded_biffs <- Hashtbl.create 5 ;
				if !debug_ocaml then log_and_print "Unmarshaled the key\n";
			end
	
	| TP_ActionBashFor(where,al) ->
			let find_list = ref [] in
			let where = List.map (fun (a,b,c)-> Var.get_string a,b , Var.get_string c) where in
			List.iter ( fun (directory,exact_match,regexp_string) ->
				try
					let dh = Case_ins.unix_opendir directory in
					let reg = begin match exact_match with
							| Some(true) ->
								Str.regexp_string_case_fold
							| _ ->
								Str.regexp_case_fold
						end regexp_string
					in
					(try
						while true do
							let next = Unix.readdir dh in
							if ((Case_ins.unix_stat (directory ^ "/" ^ next)).Unix.st_kind =
								 Unix.S_REG) && (Str.string_match reg next 0) then
								find_list := (String.uppercase (directory ^ "/" ^ next)) :: !find_list
						done
					with End_of_file -> () );
					Unix.closedir dh ;
				with _ -> ()
			) where ;
			List.iter (fun file ->
				let directory = Case_ins.filename_dirname file in
				let filespec = Case_ins.filename_basename file in
				Var.set_string "BASH_FOR_DIRECTORY" directory ;
				Var.set_string "BASH_FOR_FILESPEC" file ;
				Var.set_string "BASH_FOR_FILE" filespec ;
				Var.set_string "BASH_FOR_RES"
					(let a,b = split filespec in a) ;
				Var.set_int "BASH_FOR_SIZE" (file_size file);
				List.iter (process_action tp) al ;
			) !find_list ;
	
	| TP_ActionDefineArray(arr,vals) ->
			let i = ref 0 in
			List.iter (fun x ->
				Var.set_string
					(eval_pe_str (PE_Dollars(arr,[get_pe_string (string_of_int !i)],
						false,true))) (Var.get_string x);
				incr i
			) vals;

	| TP_ActionDefineAssociativeArray(arr,vals) ->
		List.iter (fun (x,y) ->
			Var.set_string
				(eval_pe_str (PE_Dollars(arr,[x],
					false,true))) (Var.get_string (eval_pe_str y));
		) vals;
	
	| TP_Action_For_Each(var,sl,al) ->
			let var = eval_pe_str var in
			let sl = List.map Var.get_string sl in
			List.iter (fun x ->
				Var.set_string var x ;
				List.iter (process_action tp) al ;
			) sl ;
	
	
	| TP_ActionPHPEach(var,invar,outvar,al) ->
		let var_s = Var.get_string (eval_pe_str var) in
		let var = PE_LiteralString(var_s) in
		let values = try Hashtbl.find !Var.arrays var_s with _ -> [] in
		let outvar = eval_pe_str outvar in
		let  invar = eval_pe_str	invar in
		List.iter ( fun x ->
			let i = ref 0 in
			List.iter ( fun y ->
				if !i = 0 then Var.set_string invar y;
				Var.add_local_string (invar ^ "_" ^ string_of_int !i) y;
				incr i
			) x;
			let x = List.map get_pe_string x in
			let this_value = eval_pe_str (PE_Dollars(var,x,true,false)) in
			Var.set_string outvar this_value;
			List.iter (process_action tp) al ;
			for j = 0 to (List.length x) - 1 do
				Var.remove_local (invar ^ "_" ^ string_of_int j)
			done
		) (List.rev values);
	
	| TP_Outer_For(init,guard,inc,body) ->
			let cmd_list = init @ [ TP_PatchWhile(guard,[(TP_PatchInnerAction body)] @ inc) ] in
			let b = ref "" in
			let patch_filename = "" in
			b := List.fold_left (fun acc elt ->
					process_patch2 patch_filename game acc elt) !b cmd_list  ;
		 ()
	
	| TP_Outer_While(guard,body) ->
			let cmd_list = [ TP_PatchWhile(guard,[TP_PatchInnerAction body])] in
			let b = ref "" in
			let patch_filename = "" in
			b := List.fold_left (fun acc elt ->
					process_patch2 patch_filename game acc elt) !b cmd_list  ;
		 ()
	
	| TP_Outer_Inner_Buff(buff_var,pl) ->
			let new_buff = Var.get_string buff_var in
			let filename = Printf.sprintf "INNER_PATCH %S" buff_var in
			let dummy = List.fold_left (fun acc elt ->
					process_patch2 filename game acc elt) new_buff pl in
			()
	
		| TP_Outer_Inner_Buff_Save(store_var,buff_var,pl) ->
			let new_buff = Var.get_string buff_var in
			let filename = Printf.sprintf "INNER_PATCH_SAVE %S" buff_var in
			let result = List.fold_left (fun acc elt ->
					process_patch2 filename game acc elt) new_buff pl
			in
			Var.set_string (eval_pe_str store_var) (Var.get_string result)

		| TP_Outer_Set(name,value) ->
			let value = (eval_pe "" game value) in
			let name = eval_pe_str name in
			Var.set_int32 name value ;
	
	| TP_Outer_Sprint(name,msg) ->
			let name = eval_pe_str name in
			let (str : string) = eval_pe_tlk_str game msg in
			let value = Var.get_string str in
			Var.set_string name value  ;
	
	| TP_Outer_Text_Sprint (var,str) ->
			let var = eval_pe_str var in
			let str = Var.get_string (eval_pe_str str) in
			Var.add_local_string var str
	
	| TP_Forbid_File(file,error_msg) ->
			log_and_print "Checking for forbidden files ...\n" ;
			let file = Arch.backslash_to_slash file in
			let size = file_size file in
			let test = ref false in
			test := bigg_file_exists file game.Load.key ;
		 if !test then begin
				log_and_print "[%s] found: %d bytes\n" file size ;
				log_and_print "\n%s\n" (Dc.single_string_of_tlk_string game
				error_msg) ;
				failwith file
			end else begin
				log_or_print "[%s] not found (as desired)\n" file ;
			end

	| TP_Print(msg) ->
			let str = Dc.single_string_of_tlk_string game msg in
			be_silent := false ;
			let str = Var.get_string str in
			log_and_print "\n%s\n" str
	
	| TP_Fail(msg) ->
			let str = Var.get_string (Dc.single_string_of_tlk_string game msg) in
			log_and_print "FAILURE:\n%s\n" str ;
			failwith str
	
	| TP_If(p,al1,al2) ->
			let res = is_true (eval_pe "" game p) in
			(* log_or_print "IF evaluates to %b\n" res ; *)
			if res then begin
				List.iter (process_action tp) al1
			end else begin
				List.iter (process_action tp) al2
			end
	
	| TP_Define_Action_Macro(str,decl,al) ->
			Hashtbl.replace action_macros str (decl, al)

	| TP_Define_Patch_Macro(str,decl,al) ->
			Hashtbl.replace patch_macros str (decl, al)

	| TP_Define_Action_Function (str,a,b,c,d) ->
			Hashtbl.replace action_functions str (a,b,c,d)

	| TP_Define_Patch_Function (str,a,b,c,d) ->
			Hashtbl.replace patch_functions str (a,b,c,d)

	| TP_Launch_Action_Function (str,int_var,str_var,rets) ->
			let (f_int_args,f_str_args,f_rets,f_code) = try
					Hashtbl.find action_functions str
				with _ -> failwith (Printf.sprintf "Unknown function: %s" str)
			in
			let i_did_pop = ref false in
			begin try
				Var.var_push();
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					try ignore (Var.get_int32 ("%" ^ a ^ "%")) with _ -> Var.set_int32 a (eval_pe "" game b)
				) f_int_args;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					try ignore (Var.get_string_exact ("%" ^ a ^ "%")) with _ -> Var.set_string a (eval_pe_str b)
				) f_str_args;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					Var.set_int32 a (eval_pe "" game b)
				) int_var;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					Var.set_string a (eval_pe_str b)
				) str_var;
				List.iter (process_action tp) f_code;
				let final_returns = Hashtbl.create 5 in
				List.iter (fun a ->
					let a = eval_pe_str a in
					let v = Var.get_string_exact ("%" ^ a ^ "%") in
					Hashtbl.add final_returns a v;
				) f_rets;
				Var.var_pop();
				i_did_pop := true;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					let b = eval_pe_str b in
					Var.set_string a (try Hashtbl.find final_returns b with Not_found -> failwith (Printf.sprintf "Unknown return value: %s" b));
				) rets;
			with e -> (if not !i_did_pop then Var.var_pop(); raise e); end

	| TP_Launch_Action_Macro(str) ->
		let (decl, actions) =
			try
				Hashtbl.find action_macros (Var.get_string str)
			with _ ->
				failwith (Printf.sprintf "Unknown Macro: %s" str)
	(*					 ( [] , [] ) *)
		in
		List.iter (fun x -> match x with
			TP_LocalSet(var,pe) ->
				let pe = (eval_pe "" game pe) in
				let var = eval_pe_str var in
				Var.add_local_int32 var pe
			| TP_LocalSprint (var,str) ->
				let var = eval_pe_str var in
				let (str : string) = eval_pe_tlk_str game str in
				let str = Var.get_string str in
				Var.add_local_string var str
			| TP_LocalTextSprint (var,str) ->
					let var = eval_pe_str var in
					let str = Var.get_string (eval_pe_str str) in
					Var.add_local_string var str
		) decl ;
		List.iter (process_action tp) actions ;
		List.iter (fun x -> match x with
			TP_LocalSet(var,_)
		| TP_LocalSprint (var,_)
		| TP_LocalTextSprint (var,_) ->
			let var = eval_pe_str var in
			Var.remove_local var
		) decl ;
	
	| TP_Reinclude(string_list) ->
		let string_list = List.map Var.get_string string_list in
		List.iter (fun file ->
			let tph_parsed =
				if !debug_ocaml then log_and_print "Loading %s...\n" file ;
				let x = handle_tph_filename file in
				Hashtbl.replace loaded_tph file x ;
				x
			in
			List.iter (process_action tp) tph_parsed ;
		) string_list ;
	
	| TP_Include(string_list) ->
		let string_list = List.map Var.get_string string_list in
		List.iter (fun file ->
			let tph_parsed = if Hashtbl.mem loaded_tph file
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
			List.iter (process_action tp) tph_parsed ;
		) string_list ;
	
	| TP_Action_ReadLN(x) ->
			if !interactive then begin
				let y = read_line ()in
				Var.set_string (eval_pe_str x) y;
				readln_strings:= (x,y) :: !readln_strings;
			end else begin
				match !readln_strings with
				| (a,b) :: tl ->
						Var.set_string (eval_pe_str x) b;
						readln_strings := tl
				| [] ->
				log_and_print "Not enough backed up entries in your replies"; failwith "Missing READLN strings"
			end;
	
	| TP_Uninstall_Now(name,comp) ->
			let comp = Int32.to_int (eval_pe "" game comp) in
			let name = Var.get_string name in
			if already_installed name comp then begin
				if uninstall game handle_tp2_filename name comp false then
					()
				else
					failwith
						(Printf.sprintf "unable to uninstall %s component #%d"
							name comp)
			end else
				log_or_print "%s component #%d not present, good.\n"
					name comp
	
	| TP_RandomSeed(i) ->
		eval_pe_warn := false ;
		let _ = try
				let x = Int32.to_int (eval_pe "" game i) in
				Random.init x ;
			with _ ->
				begin
					Random.self_init () ;
				end
		in
		eval_pe_warn := true ;
	
	| TP_ClearMemory ->
		log_and_print "Clearing the memory.\n" ;
		let temp_author = Var.get_string "%TP2_AUTHOR%" in
		let temp_lang = Var.get_string "%LANGUAGE%" in
		let temp_name = Var.get_string "%TP2_FILE_NAME%" in
		Var.clear_var () ;
		Var.all_the_assoc () ;
		Var.set_string "TP2_AUTHOR" temp_author ;
		Var.set_string "LANGUAGE" temp_lang ;
		Var.set_string "TP2_FILE_NAME" temp_name ;
		Arch2.associate_these () ;
	
	| TP_Clear_Ids_Map ->
		log_and_print "Clearing the IDS map.\n" ;
		Bcs.clear_ids_map game ;
	
	| TP_Silent -> be_silent := true ;
	| TP_Verbose -> be_silent := false ;
	
	| TP_CopyRandom(slistlist,plist,wlist) ->
			List.iter (fun slist ->
			log_and_print "Randomizing %d file(s) ...\n" (List.length slist) ;
			let final_dlist = ref [] in
			let finished = ref false in
			while not !finished do
				let dlist = List.map (fun s -> (Random.bits (), s) ) slist in
				let dlist = List.sort (fun (a,b) (a',b') -> compare a a') dlist in
				final_dlist := dlist ;
				finished := true ;
				if List.length dlist > 1 then
					List.iter2 (fun (a,b) c -> if b = c then finished := false)
						dlist slist
			done ;
			let dlist = List.map (fun (a,b) -> "override/" ^ b) !final_dlist in
			let sdlist = List.combine slist dlist in
			let copy_args = {
				copy_get_existing = true;
				copy_use_regexp = false;
				copy_use_glob = false;
				copy_file_list = sdlist ;
				copy_patch_list = plist ;
				copy_constraint_list = wlist ;
				copy_backup = true;
				copy_at_end = true;
				copy_save_inlined = false;
			} in
			process_action tp (TP_Copy(copy_args))
			) slistlist
	
	| TP_CopyAllGamFiles(pl,wl) ->
			(* Patch the baldur.gam in the biff *)
			let my_copy_args = {
				copy_get_existing = true;
				copy_use_regexp = false;
				copy_use_glob = false ;
				copy_file_list = [ ("baldur.gam", "override") ] ;
				copy_patch_list = pl ;
				copy_constraint_list = wl ;
				copy_backup = true ;
				copy_at_end = false ;
				copy_save_inlined = false ;
			} in
			process_action tp (TP_Copy(my_copy_args)) ;
			let file_list = ref [] in
			let get_gam_list base_dir =
				let dlist = list_of_files_in_directory base_dir in
				List.iter (fun filename ->
					let filename = base_dir ^ "/" ^ filename in
					if is_directory filename					&&
						 filename <> base_dir ^ "/."		&&
						 filename <> base_dir ^ "/.."
					then
						file_list := !file_list @ [(filename ^ "/baldur.gam", filename ^ "/baldur.gam")] ;
				) dlist ;
				()
			in
			get_gam_list "save" ;
			get_gam_list "mpsave" ;
			let my_copy_args = {
				copy_get_existing = false;
				copy_use_regexp = false;
				copy_use_glob = false ;
				copy_file_list = !file_list ;
				copy_patch_list = pl ;
				copy_constraint_list = wl ;
				copy_backup = false ;
				copy_at_end = false ;
				copy_save_inlined = false ;
			} in
			process_action tp (TP_Copy(my_copy_args)) ;
	
	| TP_Copy(copy_args) ->
			let get_existing = copy_args.copy_get_existing in
			let use_reg = copy_args.copy_use_regexp in
			let use_glob = copy_args.copy_use_glob in
			let slist = copy_args.copy_file_list in
			let plist = copy_args.copy_patch_list in
			let clist = copy_args.copy_constraint_list in
			let make_a_backup = copy_args.copy_backup in
			let copy_at_end = copy_args.copy_at_end in
			let save_inlined = copy_args.copy_save_inlined in
			if true then begin
				let worklist = ref [] in
	
				let bts1, bts2 = match get_existing, use_reg with
					| false, false -> true,  true
					| true,  false -> false, true
					| true,  true  -> false, false
					| _ -> failwith "COPY type internal"
				in
				let slist = List.map (fun (x,y) ->
					(if bts1 then Arch.backslash_to_slash x else x),
					(if bts2 then Arch.backslash_to_slash y else y)
				) slist
				in
	
				let slist = List.map (fun (x,y) -> (Var.get_string x, Var.get_string y)) slist in
	
				let slist =
					if get_existing = true && use_reg = true then begin
						let files_in_chitin = Key.list_of_key_resources game.Load.key use_glob in
						let new_list = List.map (fun (s,p) ->
							let regexp = Str.regexp_case_fold s in
							let matches = ref [] in
							List.iter (fun possible ->
								if Str.string_match regexp possible 0 then begin
									matches := (possible, p ^ "/" ^ possible) :: !matches
								end
							) files_in_chitin;
							let matches = List.sort compare !matches in
							let matches =
								let rec nodup lst = match lst with
								| [] -> []
								| [hd] -> lst
								| a :: b :: tl -> if (a = b) then nodup (b :: tl)
																						 else a :: (nodup (b :: tl))
								in nodup matches
							in
							if (matches = []) then
								[(s,p)]
							else
								matches
						) slist in
						List.flatten new_list
					end else if use_glob then begin
						let res = ref [] in
						List.iter (fun (s,p) ->
							let myfun (glob_s : string) = begin
								log_only "Callback from Arch.glob: %s\n" glob_s;
								res := (glob_s, p) :: !res
							end in
							log_only "Calling Arch.glob: %s\n" s;
							let res = (Arch.glob s myfun) in
							log_only "Arch.glob returned.\n";
							res
						) slist ;
						!res
					end else
						slist
				in
	
				let len = List.length slist in
				log_and_print "Copying%s %d file%s ...\n"
					(if List.length plist > 0 then " and patching" else "")
					len
					(if len = 1 then "" else "s") ;
	
				let copy_one_file src dest = begin
					let dest =
						if use_reg || use_glob then
							dest
						else
							Var.get_string dest
					in
					let src =
						if use_reg || use_glob then
							src
						else
							Var.get_string src
					in
					if String.uppercase src = "DIALOG.TLK" then
						log_and_print_modder "\n\nUse COPY_LARGE rather than COPY on dialog.tlk!\n\n\n" ;
	
					let src_dir = Case_ins.filename_dirname src in
					Var.set_string "SOURCE_DIRECTORY" src_dir ;
					Var.set_string "SOURCE_FILESPEC" src ;
					Var.set_string "SOURCE_FILE" (Case_ins.filename_basename src) ;
					Var.set_string "SOURCE_RES"
						(let a,b = split (Case_ins.filename_basename src) in a) ;
					Var.set_string "SOURCE_EXT"
						(let a,b = split (Case_ins.filename_basename src) in b) ;
					let dest_dir = Case_ins.filename_dirname dest in
					Var.set_string "DEST_DIRECTORY" dest_dir ;
					Var.set_string "DEST_FILESPEC" dest ;
					Var.set_string "DEST_FILE" (Case_ins.filename_basename dest) ;
					Var.set_string "DEST_RES"
						(let a,b = split (Case_ins.filename_basename dest) in a) ;
					Var.set_string "DEST_EXT"
						(let a,b = split (Case_ins.filename_basename dest) in b) ;
					let buff =
						if not get_existing then
							load_file src
						else
							let a,b = split src in
							let buff,path = Load.load_resource "COPY" game true a b in
							buff
					in
					Var.set_int32 "SOURCE_SIZE" (Int32.of_int (String.length buff));
					let orig_buff = String.copy buff
					in
	
					(* if (buff <> "") then *) begin
						if (!has_if_eval_bug || List.exists (fun x -> match x with TP_Eval(_) -> true | _ -> false) clist) then begin try
								List.iter (fun p -> process_patch1 src game buff p) plist
							with _ -> ()
						end;
						let ok_to_copy = List.fold_left (fun acc elt -> acc &&
							match elt with
							| TP_Contains(s) -> begin
									let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
									try let _ = Str.search_forward my_regexp buff 0 in
										true
									with _ ->
										log_only_modder "Not copying [%s] to [%s] because it does NOT contain [%s]\n" src dest	(eval_pe_str s) ;
										false
									end
							| TP_NotContains(s) -> begin
									let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
									try let _ = Str.search_forward my_regexp buff 0 in
										log_only_modder "Not copying [%s] to [%s] because it DOES contain [%s]\n" src dest (eval_pe_str s) ;
										false
									with _ ->
										true
									end
							| TP_IfSizeIs(size) ->
									if String.length buff = size then
										true
									else begin
										log_only_modder "Not copying [%s] to [%s] because size is %d, NOT %d\n" src dest (String.length buff) size ;
										false
									end
							| TP_Eval(pe) ->
									let v = eval_pe buff game pe in
									if v = Int32.zero then begin
										log_only_modder "Not copying [%s] to [%s] because condition evaluates to %ld\n" src dest v ; false
									end else true
							| TP_ButOnlyIfItChanges -> true
						) true clist in
						if ok_to_copy then begin
							let result_buff =
								List.fold_left (fun acc elt ->
									try process_patch2 src game acc elt
									with e -> log_and_print
										"ERROR: [%s] -> [%s] Patching Failed (COPY) (%s)\n"
										src dest (Printexc.to_string e); raise e)
								buff plist
							in
							let dest =
								if is_directory dest then
									dest ^ "/" ^ (Case_ins.filename_basename src)
								else
									dest
							in
							let it_changed =
								Stats.time "BUT_ONLY" (fun () ->
								if List.mem TP_ButOnlyIfItChanges clist then
									begin
										let changed =
											((String.length result_buff) <>
											 (String.length orig_buff)) ||
											((String.compare orig_buff result_buff) <> 0)
										in
										let newsrc, newdes =
											( Case_ins.filename_basename src, Case_ins.filename_basename dest )
										in
										if !debug_boiic then log_and_print "[%s]%s->[%s]%s did%s change\n"
												newsrc (String.make (12 - (String.length newsrc)) ' ')
												newdes (String.make (12 - (String.length newdes)) ' ')
												(if changed then "" else "n't") ;
										changed
									end else begin
										if (!debug_change) && (plist <> []) then begin
											let changed =
												((String.length result_buff) <>
												 (String.length orig_buff)) ||
												((String.compare orig_buff result_buff) <> 0)
											in
											let newsrc, newdes =
												( Case_ins.filename_basename src, Case_ins.filename_basename dest )
											in
											if !debug_change && not changed then log_and_print "Patches on [%s]%s->[%s]%s don't alter the file\n"
													newsrc (String.make (12 - (String.length newsrc)) ' ')
													newdes (String.make (12 - (String.length newdes)) ' ') ;
											true
										end else true ;
										(* always copy *)
									end ) ()
							in
							if (it_changed) then begin
								let doit () = Stats.time "saving files" (fun () ->
									if not (save_inlined) then begin
										let out =
											try open_for_writing_internal make_a_backup dest true
											with e -> log_and_print
												"ERROR: COPY ~%s~ ~%s~ FAILED: cannot open target\n"
													src dest ;
												raise e
										in
										output_string out result_buff ;
										close_out out ;
										begin (* handle read-only files! *)
											try
												Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
											with e -> ()
												(* log_or_print "WARNING: chmod %s : %s\n" filename
													(Printexc.to_string e) *)
										end ;
										if make_a_backup then
											log_only "Copied [%s] to [%s]\n" src dest
										else
											log_only "Copied [%s] to [%s] (NO BACKUP MADE!)\n" src dest
									end else begin
										log_only_modder "Defined Inlined File [%s] (length %d)\n"
										dest (String.length result_buff) ;
										Hashtbl.add inlined_files (Arch.backslash_to_slash dest) result_buff
									end
								) ()
								in
								if copy_at_end then worklist := doit :: !worklist
								else doit ()
							end else begin
								log_only_modder
									"Not copying [%s] to [%s] because it did not change\n"
									src dest
							end
						end (* end: if ok_to_copy *)
					end (* (* end: if buff <> "" *)
					else log_or_print "Not copying [%s] to [%s] because it is empty\n"
						src dest
						*)
				end (* end: let copy_one_file = begin *)
				in
				List.iter (fun (src,dest) ->
					let src = Var.get_string src in
					if is_directory src then begin
						let dh = Case_ins.unix_opendir src in
						(try
							while true do
								let base = Unix.readdir dh in
								let source = src ^ "/" ^ base in
								if not (is_directory source) then
									copy_one_file source (dest ^ "/" ^ base)
							done
						with _ -> () ) ;
						Unix.closedir dh
					end else copy_one_file src dest
				) slist ;
				List.iter (fun f -> f ()) !worklist
			end
	
	| TP_CopyLarge(copy_large_args) ->
			let use_glob = copy_large_args.copy_large_use_glob in
			let slist = copy_large_args.copy_large_file_list in
			let make_a_backup = copy_large_args.copy_large_backup in
			let worklist = ref [] in
	
			let slist =
				List.map (fun (x,y) -> (Arch.backslash_to_slash x, Arch.backslash_to_slash y)) slist
			in
	
			let slist =
				if use_glob then begin
					let res = ref [] in
					List.iter (fun (s,p) ->
						let myfun (glob_s : string) = begin
							log_only "Callback from Arch.glob: %s\n" glob_s;
							res := (glob_s, p) :: !res
						end in
						log_only "Calling Arch.glob: %s\n" s;
						let res = (Arch.glob s myfun) in
						log_only "Arch.glob returned.\n";
						res
					) slist ;
					!res
				end else
					slist
			in
	
			let len = List.length slist in
			log_and_print "Copying%s %d file%s ...\n" "" len
				(if len = 1 then "" else "s") ;
	
			let copy_one_file src dest = begin
				let dest =
					if use_glob then
						dest
					else
						Var.get_string dest
				in
				let src =
					if use_glob then
						src
					else
						Var.get_string src
				in
				let dest =
					if is_directory dest then
						dest ^ "/" ^ (Case_ins.filename_basename src)
					else
						dest
				in
				match String.uppercase (snd (split dest)) with
				| ".IDS" -> Bcs.clear_ids_map game
				| _ -> ()
				;
				Stats.time "saving files" (fun () ->
					if Hashtbl.mem inlined_files (Arch.backslash_to_slash src) then begin
						let copy_args = {
							copy_get_existing = false;
							copy_use_regexp = false;
							copy_use_glob = use_glob;
							copy_file_list = [(src,dest)] ;
							copy_patch_list = [] ;
							(* C_L is <> [] to avoid looping *)
							copy_constraint_list = [TP_Eval(get_pe_int "1")] ;
							copy_backup = make_a_backup ;
							copy_at_end = false ;
							copy_save_inlined = false ;
						} in
						(* we came here from tp_copy, now we go back	:) *)
						process_action tp (TP_Copy(copy_args))
					end else begin
						if make_a_backup then
							backup_if_extant dest ;
						copy_large_file src dest "doing a COPY_LARGE" ;
						if make_a_backup then
							log_only "Copied [%s] to [%s]\n" src dest
						else
							log_only "Copied [%s] to [%s] (NO BACKUP MADE!)\n" src dest
					end
					) ()
			end (* end: let copy_one_file = begin *)
			in
			List.iter (fun (src,dest) ->
				let src = Var.get_string src in
				if is_directory src then begin
					let dh = Case_ins.unix_opendir src in
					(try
						while true do
							let base = Unix.readdir dh in
							let source = src ^ "/" ^ base in
							if not (is_directory source) then
								copy_one_file source (dest ^ "/" ^ base)
						done
					with _ -> () ) ;
					Unix.closedir dh
				end else copy_one_file src dest
			) slist ;
			List.iter (fun f -> f ()) !worklist
	
	| TP_Add_Music(m) -> begin
		let music_base_name = Case_ins.filename_basename m.music_file in
		if is_true (eval_pe "" game
			(PE_FileContainsEvaluated(PE_LiteralString "SONGLIST.2DA",
				PE_LiteralString ("^" ^ m.music_name ^ "\\b")))) then begin
			Var.set_int32 (music_base_name) (Bcs.int_of_sym game "SONGLIST.2DA" music_base_name) ;
			log_and_print "\n\nERROR: MUS [%s] already present! Skipping!\n\n"
			music_base_name
		end else begin
			log_and_print "Adding %s Music ...\n" m.music_name;
			let music_base_name_lower = Case_ins.filename_basename m.music_file in
			let this_music_number = get_next_line_number "SONGLIST.2DA" in
			let str_to_append = Printf.sprintf "%d %s %s"
				this_music_number m.music_name music_base_name in
	
			let a1 = TP_Append("SONGLIST.2DA",str_to_append,[],true,false) in
	
			let dest_music_file = "music/" ^ music_base_name_lower in
			let a2 = TP_Copy(
				{ copy_get_existing = false;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = [(m.music_file,dest_music_file)] ;
					copy_patch_list = [] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
					} ) in
			let action_list = [ a1 ; a2 ] in
			List.iter (process_action tp) action_list ;
			Var.set_int32 (m.music_name) (Int32.of_int this_music_number) ;
			log_and_print "Added %s Music\n" m.music_name;
		end
	end

	| TP_Add_Projectile(p) -> begin
		let p = {p with pro_file = Var.get_string p.pro_file;} in
		if is_true (eval_pe "" game
			(PE_FileContainsEvaluated(PE_LiteralString "PROJECTL.IDS",
				PE_LiteralString ("^" ^ p.pro_file ^ "\\b")))) then begin
			let this_pro_name = Case_ins.filename_chop_extension (Case_ins.filename_basename p.pro_file) in
			Var.set_int32 (this_pro_name) (Bcs.int_of_sym game "PROJECTL.IDS" this_pro_name) ;
			log_and_print "\n\nERROR: PRO [%s] already present! Skipping!\n\n"
			this_pro_name
		end else begin
			log_and_print "Adding projectile file %s ...\n" p.pro_file;
			let this_pro_name = Case_ins.filename_chop_extension (Case_ins.filename_basename p.pro_file) in
			let this_missile_name = Var.get_string p.missile_ids_name in
			let a1 = TP_Include ["lc_fix_missile_ids.tpa"] in
			List.iter (process_action tp) [a1];
			let this_pro_number = get_next_line_number "PROJECTL.IDS" in
			let a1 = TP_Append("PROJECTL.IDS",
												 (Printf.sprintf "%d %s" this_pro_number this_pro_name),[],true,false) in
			let dest_pro_file = "override/" ^ (Case_ins.filename_basename p.pro_file) in
			let a2 = TP_Copy(
				{ copy_get_existing = false;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = [(p.pro_file, dest_pro_file)] ;
					copy_patch_list = [] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
					} ) in
			process_action tp a1;
			if Load.file_exists_in_game game "missile.ids" then begin
				let this_miss_number = get_next_line_number "MISSILE.IDS" in
				let a1a = TP_Append("MISSILE.IDS",
					 (Printf.sprintf "%d %s" this_miss_number this_missile_name),[],true,false) in
				process_action tp a1a;
			end;
			process_action tp a2;
			Var.set_int32 (this_pro_name) (Int32.of_int (this_pro_number + 1)) ;
			log_and_print "Added projectile file %s\n" p.pro_file;
		end
	end
	
	| TP_Add_Spell(file,kind,level,ids_name,pl,cl) ->
		log_and_print "Adding spell %s\n" ids_name;
		let file = Var.get_string file in
		let ids_name = Var.get_string ids_name in
		let kind = eval_pe "" game kind in
		let level = eval_pe "" game level in
		let kind = Int32.to_int kind in
		let level = Int32.to_int level in
		let memo = match kind with
			| 1 -> "PR"
			| 2 -> "WI"
			| 3 -> "IN"
			| 4 -> "CL"
			| _ -> failwith "ADD_SPELL with spell type not in 1-4 range."
		in
		if level < 0 || level > 9 then failwith "ADD_SPELL with level outside the 0-9 range.";
		let rec try_it n =
			if n = 100 then failwith "Couldn't find space for the spell."
			else if not (Load.file_exists_in_game game (Printf.sprintf "SP%s%d%s%d.SPL" memo level (if n < 10 then "0" else "") n)) then
				n
			else try_it (n + 1)
		in
		let n = try_it 1 in
		if !debug_ocaml then log_and_print "installed as SP%s%d%s%d.SPL\n" memo level 
																							(if n < 10 then "0" else "") n;
		let number = kind * 1000 + level * 100 + n in
			let a1 = TP_Append("SPELL.IDS",
												 (Printf.sprintf "%d %s" number ids_name),[],true,false) in
			let dest_file = "override/" ^ (Printf.sprintf "SP%s%d%s%d.SPL" memo level
																							(if n < 10 then "0" else "") n) in
			let a2 = TP_Copy(
				{ copy_get_existing = false;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = [(file, dest_file)] ;
					copy_patch_list = pl ;
					copy_constraint_list = cl ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
					} ) in
			let action_list = [ a1 ; a2 ] in
			List.iter (process_action tp) action_list ;
			Var.set_int32 (ids_name) (Int32.of_int (number)) ;
			Bcs.clear_ids_map game ;
			log_and_print "Added spell %s\n" ids_name;
	
	
	| TP_CopyKit(oldString,newString,patches_list) ->
		let oldString = Var.get_string oldString in
		let newString = Var.get_string newString in
		let patches_list = List.map (fun(a,b)-> (Var.get_string a),(Var.get_string
			 b)) patches_list in
		let get_line file =
			let (a,b) = split file in
			let buff,path = Load.load_resource "getting 2DA columnns" game true a b
			in
			let my_regexp = Str.regexp_case_fold (Printf.sprintf "%s%s[\t ].*$"
										 (if file = "kitlist.2da" then " " else "^") oldString) in
			ignore(Str.search_forward my_regexp buff 0);
			Str.matched_string buff
		in
		let get_column file =
			let (a,b) = split file in
			let buff,path = Load.load_resource "getting 2DA columnns" game true a b
			in
			let lines = Str.split many_newline_or_cr_regexp buff in
			let cells = List.map (Str.split many_whitespace_regexp) lines in
			let headers = List.nth cells 2 in
			let rec get_where lst cnt = match lst with
				| hd :: tl -> if hd = oldString then cnt else get_where tl (cnt + 1)
				| [] -> failwith (Printf.sprintf "Unknown kit: %s" oldString)
			in
			let column = get_where headers 1 in
			List.fold_left (fun acc elt -> Printf.sprintf "%s%s " acc (List.nth elt
																																 column))
						(if file = "weapprof.2da" then oldString ^ " " else "")
						(List.tl (List.tl (List.tl cells)))
		in
		let get_entry file which =
			let line = get_line file in
			let split = Str.split many_whitespace_regexp line in
			List.nth split which
		in
		let fix_clab which =
			let a = TP_Copy(
				{ copy_get_existing = true ;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = [(which ^ ".2da","override")] ;
					copy_patch_list = [] ;
					copy_constraint_list = [] ;
					copy_backup = true ;
					copy_at_end = false;
					copy_save_inlined = false;
				} ) in
				process_action tp a;
				"override/" ^ which ^ ".2da"
		 in
		let patches = Hashtbl.create 5 in
		List.iter (fun (a,b) -> Hashtbl.add patches (String.lowercase a) b)
			patches_list;
		let get_clasweap file =
			try get_line file
			with _ -> oldString ^ " 1 1 1 1 1 1 1 1"
		in
		let get_it file how =
			let o =
				if !debug_ocaml then log_and_print "%s ... " file;
				if Hashtbl.mem patches file then Hashtbl.find patches file else
				match how with
					| "line" -> get_line (file ^ ".2da")
					| "column" -> get_column (file ^ ".2da")
					| "clasweap" -> get_clasweap (file ^ ".2da")
					| "clab" -> fix_clab (get_entry "kitlist.2da" 4)
					| "lower" -> get_entry "kitlist.2da" 1
					| "mixed" -> get_entry "kitlist.2da" 2
					| "help" -> get_entry "kitlist.2da" 3
					| "luabbr" -> get_entry "luabbr.2da" 1
					| "unused" -> (get_entry "kitlist.2da" 6) ^ " " ^ (get_entry
							"kitlist.2da" 7)
					| _ -> failwith (Printf.sprintf "CopyKit: %s" how)
				in
				if !debug_ocaml then log_and_print "~%s~\n" o;
				Str.global_replace (Str.regexp ("^" ^ oldString^"[ \t]"))
						(newString ^ " ") o
		in
		let copy_kit = {
			kit_name = newString;
			clasweap = get_it "clasweap" "clasweap";
			weapprof = get_it "weapprof" "column";
			abclasrq = get_it "abclasrq" "line";
			abclsmod = get_it "abclsmod" "line";
			abdcdsrq = get_it "abdcdsrq" "line";
			abdcscrq = get_it "abdcscrq" "line";
			dualclas = get_it "dualclas" "line";
			alignmnt = get_it "alignmnt" "line";
			ability_file = get_it "clab" "clab";
			include_in = "";
			lower = Dlg.TLK_Index (int_of_string (get_it "lower" "lower"));
			mixed = Dlg.TLK_Index (int_of_string (get_it "mixed" "mixed"));
			help	= Dlg.TLK_Index (int_of_string (get_it "help"  "help"));
			tob_abbr = get_it "luabbr" "luabbr";
			tob_start = (
				let str = get_it "25stweap" "column" in
				let lst = Str.split (Str.regexp "[ \t]+") str in
				List.map (fun elt -> if elt = "$" then "" else elt) lst
			);
			unused_class = get_it "unusabilities" "unused";
	
	(*				unused_class : string ;
			tob_start : string list ;*)
		}in
		process_action tp (TP_Add_Kit(copy_kit))
	
	| TP_Add_Kit(k) -> begin
		log_and_print "Adding %s Kit ...\n" k.kit_name;
	
		if is_true (eval_pe "" game
			(PE_FileContainsEvaluated(PE_LiteralString "kitlist.2da",
				PE_LiteralString ("^" ^ k.kit_name ^ "\\b")))) then begin
			Var.set_int32 (k.kit_name) (Bcs.int_of_sym game "KITLIST.2DA" k.kit_name) ;
			log_and_print "\n\nERROR: Kit [%s] already present! Skipping!\n\n"
			k.kit_name
		end else begin
			let a1 = TP_Append("CLASWEAP.2DA",k.clasweap,[],true,false) in
			let a2 = TP_Append_Col("WEAPPROF.2DA",k.weapprof,Tp.get_pe_int "2",[]) in
			let a3 = TP_Append("ABCLASRQ.2DA",k.abclasrq,[],true,false) in
			let a4 = TP_Append("ABDCDSRQ.2DA",k.abdcdsrq,[],true,false) in
			let a5 = TP_Append("ABDCSCRQ.2DA",k.abdcscrq,[],true,false) in
			let a_e1 = TP_Append("ABCLSMOD.2DA",k.abclsmod,[],true,false) in
			let a_e2 = TP_Append("DUALCLAS.2DA",k.dualclas,[],true,false) in
			let a6 = TP_Append("ALIGNMNT.2DA",k.alignmnt,[],true,false) in
			let abil_file = String.uppercase (Case_ins.filename_basename k.ability_file) in
			if !debug_ocaml then log_and_print "%s\n" abil_file;
			let abil_file_no_ext = Case_ins.filename_chop_extension abil_file in
			let dest_abil_file = "override/" ^ abil_file in
			let a7 = TP_Copy(
				{ copy_get_existing = false ;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = [(k.ability_file,dest_abil_file)] ;
					copy_patch_list = [] ;
					copy_constraint_list = [] ;
					copy_backup = true ;
					copy_at_end = false;
					copy_save_inlined = false;
					} ) in
			let include_list = split_apart k.include_in in
			let lower_index = match Dc.resolve_tlk_string game k.lower with
				Dlg.TLK_Index(i) -> i
			| _ -> log_and_print "ERROR: cannot resolve KIT lower string\n" ;
							failwith "resolve"
			in
			let mixed_index = match Dc.resolve_tlk_string game k.mixed with
				Dlg.TLK_Index(i) -> i
			| _ -> log_and_print "ERROR: cannot resolve KIT mixed string\n" ;
							failwith "resolve"
			in
			let help_index = match Dc.resolve_tlk_string game k.help with
				Dlg.TLK_Index(i) -> i
			| _ -> log_and_print "ERROR: cannot resolve KIT help string\n" ;
							failwith "resolve"
			in
			let this_kit_number = get_next_line_number "KITLIST.2DA" in
			let this_kit_prof_number = get_next_col_number "WEAPPROF.2DA" in
			let append_to_kitlist = Printf.sprintf
				"%d  %s %d %d %d %s %d %s"
				this_kit_number k.kit_name
				lower_index mixed_index help_index
				abil_file_no_ext this_kit_prof_number
				k.unused_class in
			let a8 = TP_Append("KITLIST.2DA",append_to_kitlist,[],true,false) in
			let include_actions = List.map (fun file ->
				let num = get_next_line_number (file ^ ".2DA" ) in
				let str = Printf.sprintf "%d	%d" num this_kit_number in
				TP_Append(file ^ ".2DA",str,[],true,false)
			) include_list in
			let abbr = Printf.sprintf  "%s		 %s" k.kit_name k.tob_abbr in
			let a9 = TP_Append("LUABBR.2DA",abbr,[],true,false) in
			let a10 = TP_Set_Col("25STWEAP.2DA",
				("" :: "" :: k.kit_name :: k.tob_start),this_kit_prof_number+1) in
			let a11 = TP_Append("KIT.IDS",
				(Printf.sprintf "0x%x %s" (0x4000 + this_kit_number)
					k.kit_name),[],true,false) in
			let fix2da1 = TP_Copy ({
					copy_get_existing = true;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = ["weapprof.2da", "override";
														"25stweap.2da", "override";
					] ;
					copy_patch_list = [ TP_PatchIf(PE_GT(get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
						TP_Read2DA(get_pe_int "2", get_pe_int "0", get_pe_int "0",get_pe_string "tb#kit_temp2");
						TP_Patch2DA(get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_int "%tb#kit_temp2% %tb#kit_temp2%");
						TP_PatchSet(get_pe_string "tb#kit_this_is_a_temp_var",get_pe_int "0");
						TP_Patch2DANow("tb#kit_this_is_a_temp_var", get_pe_int "0");
					],[])] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
			}) in
			let fix2da1a = TP_Copy ({
					copy_get_existing = true;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = ["kitlist.2da", "override"] ;
					copy_patch_list = [ TP_PatchIf(PE_GT(get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
						TP_Read2DA(get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_string "tb#kit_temp2a");
						TP_Patch2DA(get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_int "%tb#kit_temp2a% %tb#kit_temp2a%");
						TP_PatchSet(get_pe_string "tb#kit_this_is_a_temp_var",get_pe_int "0");
						TP_Patch2DANow("tb#kit_this_is_a_temp_var", get_pe_int "0");
					],[])] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
			}) in
			let action_list = a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 ::
				a9 :: a10 :: a11 :: a_e1 :: a_e2 :: fix2da1 :: fix2da1a :: include_actions	in
			let old_allow_missing = !Load.allow_missing in
			Load.allow_missing :=
				"LUABBR.2DA" :: "25STWEAP.2DA" :: old_allow_missing ;
			(* actually do it! *)
			List.iter (process_action tp) action_list ;
			let fix2da2 = TP_Copy ({
					copy_get_existing = true;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = ["weapprof.2da", "override";
														"25stweap.2da", "override";
					] ;
					copy_patch_list = [ TP_PatchIf(PE_GT(get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
						TP_PatchStringTextually(None,None,"^%tb#kit_temp2%",
														String.make (String.length (Var.get_string_exact "%tb#kit_temp2%")) ' ',None);
					],[])] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
			}) in
			let fix2da2a = TP_Copy ({
					copy_get_existing = true;
					copy_use_regexp = false;
					copy_use_glob = false;
					copy_file_list = ["kitlist.2da", "override"] ;
					copy_patch_list = [ TP_PatchIf(PE_GT(get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
						TP_PatchStringTextually(None,None,"^%tb#kit_temp2a%",
														String.make (String.length (Var.get_string_exact "%tb#kit_temp2a%")) ' ',None);
					],[])] ;
					copy_constraint_list = [] ;
					copy_backup = true;
					copy_at_end = false;
					copy_save_inlined = false;
			}) in
			process_action tp fix2da2;
			process_action tp fix2da2a;
			Load.allow_missing := old_allow_missing ;
			Var.set_int32 (k.kit_name) (Int32.of_int this_kit_number) ;
			log_and_print "Added %s Kit\n" k.kit_name;
			Bcs.clear_ids_map game ;
		end
	end
	
	| TP_String_Set_Range(min,max,tra_file) ->
		begin
			Dc.push_copy_trans_modder ();
			try
				let tra_file = Var.get_string tra_file in
				handle_tra_filename (Arch.backslash_to_slash tra_file) ;
				let min = Int32.to_int (eval_pe "" game min) in
				let max = Int32.to_int (eval_pe "" game max) in
				for i = min to max do
						Dc.set_string game i (Dlg.Trans_String(i)) false
				done ;
				Dc.pop_trans ()
			with e -> 
				(Dc.pop_trans (); raise e)
		end
	
	| TP_String_Set(lst,tra_file_opt) -> begin
		(match tra_file_opt with
		| None -> ()
		| Some(tra_file) ->
			begin
			let tra_file = Var.get_string tra_file in
			Dc.push_copy_trans ();
			handle_tra_filename (Arch.backslash_to_slash tra_file)
			end
		) ;
		List.iter (fun (s1,str) ->
			let i = try [int_of_string s1]
					with _ -> begin
						try
							Hashtbl.find_all game.Load.dialog_search s1
						with _ ->
							log_and_print "ERROR: Cannot find string [%s]\n" s1 ;
							failwith "ERROR: STRING_SET"
					 end
			in
			List.iter (fun i -> Dc.set_string game i str false) i
		) lst ;
		(match tra_file_opt with
		| None -> ()
		| Some(_) -> Dc.pop_trans ())
		end
	
	| TP_String_Set_Evaluate(lst,tra_file_opt) -> begin
		(match tra_file_opt with
		| None -> ()
		| Some(tra_file) ->
			begin
			let tra_file = Var.get_string tra_file in
			Dc.push_copy_trans ();
			handle_tra_filename (Arch.backslash_to_slash tra_file)
			end
		) ;
		List.iter (fun (pe,str) ->
			let i = Int32.to_int (eval_pe "" game pe) in
			if !debug_ocaml then log_and_print "Setting %d to \"%s\"\n" i
				(Dc.single_string_of_tlk_string game str) ;
			Dc.set_string game i str false ;
		) lst ;
		(match tra_file_opt with
		| None -> ()
		| Some(_) -> Dc.pop_trans ())
		end
	
	| TP_Alter_TLK (pl) ->
			let b = get_pe_int "0" in
			let e = get_pe_int (string_of_int (Array.length game.Load.dialog - 1)) in
			process_action tp (TP_Alter_TLK_Range(b,e,pl))
	
	| TP_Alter_TLK_Range (b,e,pl) ->
			let b = Int32.to_int (eval_pe "" game b) in
			let e = Int32.to_int (eval_pe "" game e) in
			let rec get i= match i with
				| i when i = e -> [i]
				| _ -> i :: (get (i+1))
			in
			process_action tp (TP_Alter_TLK_List(List.map (fun i -> get_pe_int (string_of_int i)) (get b),pl))
	
	| TP_Alter_TLK_List(lst,pl) -> begin
			List.iter (fun x ->
				let i = Int32.to_int (eval_pe "" game x) in
				let male = game.Load.dialog.(i) in
				let newmale = List.fold_left (fun acc elt ->
					process_patch2 "dialog.tlk" game acc elt) male.Tlk.text pl in
				let soundmale = male.Tlk.sound_name in
				let newfemale, soundfemale = match game.Load.dialogf with
					Some dialogf ->
						let female = dialogf.(i) in
						(List.fold_left (fun acc elt ->
							process_patch2 "dialog.tlk" game acc elt) female.Tlk.text pl, female.Tlk.sound_name)
					| None -> newmale, soundmale
				in
				Dc.set_string game i (Dlg.Local_String {
						lse_male = newmale;
						lse_male_sound = soundmale;
						lse_female = newfemale;
						lse_female_sound = soundfemale;
					}) false;
			) lst
		end
	
	| TP_Load_Tra(str_l) -> begin
			let str_l = List.map (fun x -> Arch.backslash_to_slash (Var.get_string x)) str_l in
			let numtra = List.length str_l in
			log_and_print "loading %d tra file%s\n" numtra
			 (if numtra = 1 then "" else "s") ;
			List.iter (fun str ->
					handle_tra_filename (Var.get_string str)
			) str_l
		end
	
	| TP_GetFileArray(toArr,path,pattern,doDirectories) -> begin
			let path = Var.get_string (eval_pe_str path) in
			let dh = Case_ins.unix_opendir path in
			let pattern = Var.get_string (eval_pe_str pattern) in
			let pattern = Str.regexp_case_fold (match pattern with
				| "" -> "^\\(\\|[^.]\\|\\.\\...*\\|[^.]..*\\|\\.[^.].*\\)$"
						(* the above means "everything, save `.' and `..'" *)
				| _ -> pattern)
			in
			let my_type = match doDirectories with
				| true -> Unix.S_DIR
				| false -> Unix.S_REG
			in
			let i = ref 0 in
			try
				while true do
					let filep = (Unix.readdir dh) in
					if	Str.string_match pattern filep 0 then begin
						let file = path ^ "/" ^ filep in
						let stats = Case_ins.unix_stat file in
						if stats.Unix.st_kind = my_type then begin
							Var.set_string
								(eval_pe_str(PE_Dollars(toArr,[get_pe_string(string_of_int !i)],
								false,true))) file;
							incr i
						end
					end
				done;
			with End_of_file -> Unix.closedir dh
		end
	
	| TP_Mkdir(str_l) -> begin
	
		let str_l = List.map (fun x -> Arch.backslash_to_slash x) str_l in
		let numdir = List.length str_l in
		log_and_print "Creating %d director%s\n" numdir
		 (if numdir = 1 then "y" else "ies") ;
		List.iter (fun str ->
			try
				recursive_mkdir (Var.get_string str) 511 (* 511 = octal 0777 = a+rwx *)
			with e ->
				(
					log_and_print "Problem %s on %s : tp.ml\n" (Printexc.to_string e) str
				)
		) str_l
		end
	
	| TP_Compile(eval,dlg_l,pl,tra_l) -> begin
			let dlg_l =  List.map (fun x -> Var.get_string x) dlg_l in
			let tra_l =  List.map (fun x -> Var.get_string x) tra_l in
			let dlg_l =  List.map (fun x -> Arch.backslash_to_slash x) dlg_l in
			let tra_l =  List.map (fun x -> Arch.backslash_to_slash x) tra_l in
			let numd = ref 0 in
			let nums = ref 0 in 
			let handle_one_d_file filespec = match split 
				(String.uppercase filespec) with
			| _,"BAF" -> incr nums 
			| _,"D" -> incr numd
			| _,_ -> ()
			in 
			List.iter (fun filespec ->
				if is_directory filespec then begin
					let dh = Case_ins.unix_opendir filespec in 
					(try
						while true do
							let base = Unix.readdir dh in 
							let source = filespec ^ "/" ^ base in
							if not (is_directory source) then
								handle_one_d_file source
						done
					with _ -> () ) ;
					Unix.closedir dh 
				end else handle_one_d_file filespec 
			) dlg_l ; 
			log_and_print "Compiling " ;
			(if (!numd > 0) then begin
				log_and_print "%d dialogue file%s " !numd
					(if (!numd > 1) then "s" else "") ;
				if (!nums > 0) then
					log_and_print "and "
				end
			) ;
			(if (!nums > 0) then begin
				log_and_print "%d script%s " !nums
					(if (!nums > 1) then "s" else "") ;
				end
			) ; 
			log_and_print "...\n" ;
	
			Dc.push_copy_trans_modder ();
			Dc.notChanged := true;
			begin 
				match !our_lang with
					Some(l) -> List.iter (fun path ->
							let my_regexp = Str.regexp_string "%s" in
							let tra_file = Str.global_replace 
								my_regexp l.lang_dir_name (Var.get_string path) in
							handle_tra_filename tra_file ;
						) tra_l 
				| _ -> List.iter (fun tra_file -> 
							handle_tra_filename tra_file ;
						) tra_l
			end ;
			let handle_one_d_file d =
				(* handle AUTO_TRA "solarom/%s" *)
				begin try
					Dc.push_copy_trans () ;
					List.iter (fun f ->
							match f,!our_lang with
							Auto_Tra(path),Some(l) ->
										let my_regexp = Str.regexp_string "%s" in
										let tra_file_dir = Str.global_replace
											my_regexp l.lang_dir_name path in
										let d_base,_ = split (Case_ins.filename_basename d) in
										let tra_file = tra_file_dir ^ "/" ^ d_base ^ ".TRA" in
										handle_tra_filename tra_file ;
						| Auto_Tra(path),None ->
										let d_base,_ = split (Case_ins.filename_basename d) in
										let tra_file = path ^ "/" ^ d_base ^ ".TRA" in
										handle_tra_filename tra_file
						| _ -> ()
					) tp.flags ;
					if !Dc.notChanged then Modder.handle_msg "SETUP_TRA" (Printf.sprintf "WARNING: COMPILE %s with strings from setup.tra\n" d);
					Dc.ok_to_resolve_strings_while_loading := Some(game) ;
					let newd = if eval then "tb#_compile_eval_buffer/" ^ d else d in
					if eval then begin
						let contents = Var.get_string (load_file d) in
						log_only_modder "Defined Inlined File [%s] (length %d)\n"
										newd (String.length contents) ;
						Hashtbl.add inlined_files (Arch.backslash_to_slash newd) contents
					end;
					let newd1 = if pl = [] then newd else "tb#compile_patches/" ^ newd in
					if pl <> [] then begin
						let contents = load_file newd in
						let contents = List.fold_left (fun acc elt ->
					process_patch2 d game acc elt) contents pl in
						log_only_modder "Defined Inlined File [%s] (length %d)\n"
										newd1 (String.length contents) ;
						Hashtbl.add inlined_files (Arch.backslash_to_slash newd1) contents
					end;
					(match split (String.uppercase (Case_ins.filename_basename d)) with
						| _,"BAF" -> compile_baf_filename game newd1
						| _,"D" -> handle_d_filename newd1
						| _,_ -> ()
					)
				with e ->
					Dc.pop_trans();
					if !debug_ocaml then log_and_print "Dc.pop_trans 5253\n";
					Dc.ok_to_resolve_strings_while_loading := None ;
					log_and_print "ERROR: compiling [%s]!\n" d ;
					raise e
				end;
				Dc.pop_trans();
				if !debug_ocaml then log_and_print "Dc.pop_trans 5253\n";
				Dc.ok_to_resolve_strings_while_loading := None ;
			in
			try
				List.iter (fun filespec ->
					if is_directory filespec then begin
						let dh = Case_ins.unix_opendir filespec in
						(try
							while true do
								let base = Unix.readdir dh in
								let source = filespec ^ "/" ^ base in
								if not (is_directory source) then
									handle_one_d_file source
							done
						with End_of_file -> () ) ;
						Unix.closedir dh ;
					end else handle_one_d_file filespec
				) dlg_l ;
				log_or_print "Processing %d dialogues/scripts ...\n"
					(List.length dlg_l) ;
				emit_dlg_files game "override" ;
				Dc.pop_trans () ;
			with e -> Dc.pop_trans ();
				raise e
			end
	
	| TP_Set_Col(file,new_col_list,col_num) ->
			log_and_print_modder "Setting game text column-wise ...\n" ;
			let eight,three = split (String.uppercase file) in
			let buff,loaded_path = Load.load_resource "SET_COLUMN" game true eight three in
			if buff = "" then
				log_or_print "[%s]: empty or does not exist\n" file
			else begin
				let dest = "override/" ^ file in
				let buff_as_lines = Str.split many_newline_or_cr_regexp buff in
				if List.length buff_as_lines <> List.length new_col_list then begin
					log_and_print "Cannot set column-wise because there are %d lines in %s but I was only given %d things to append\n" (List.length buff_as_lines) file (List.length new_col_list)	;
					failwith ("cannot set column-wise to " ^ file)
				end ;
				Stats.time "saving files" (fun () ->
				let out = open_for_writing dest true in
				let linecount = ref 0 in
				List.iter2 (fun orig_line new_col ->
					let line_as_cols = Str.split many_whitespace_regexp orig_line in
					let i = ref 0 in
					if !linecount = 2 then incr i; (* 2nd coloumn (counting from 0) has one less entry *)
					incr linecount;
					List.iter (fun orig_col ->
						(if !i = col_num then
							Printf.fprintf out "%-20s%-20s" new_col orig_col
						else
							Printf.fprintf out "%-20s" orig_col) ;
						incr i;
					) line_as_cols ;
					(if (!i <= col_num) then
							Printf.fprintf out "%-20s" new_col) ;
					output_string out "\r\n"
				) buff_as_lines new_col_list ;
				close_out out ;
				begin (* handle read-only files! *)
					try
						Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
					with e -> ()
						(* log_or_print "WARNING: chmod %s : %s\n" filename
							(Printexc.to_string e) *)
				end ;) () ;
				log_or_print "Set text in [%s] column-wise\n" file
			end
	
	| TP_Append_Col(file,src,count_prepend,con_l) ->
			let file = Arch.backslash_to_slash (Var.get_string file) in
			let temp = Var.get_string src in
			let src_list = Str.split (Str.regexp "[ \t]+") temp in
			let src_list = List.map (fun elt -> if elt = "$" then "" else elt) src_list in
			let count_prepend = Int32.to_int (eval_pe "" game count_prepend) in
			let rec prepend i string_list =
					if i = 1 then
						("" :: string_list)
					else begin
						if i > 1 then
							"" :: (prepend (i-1) string_list)
						else	(* i <= 0 *)
								 string_list
						 end
			in
	
			let src_list = prepend count_prepend src_list in
			log_and_print "Appending to files column-wise ...\n" ;
			let eight,three = split (String.uppercase file) in
			let buff,loaded_path = Load.load_resource "APPEND_COLUMN" game true eight three in
			if buff = "" then
				log_or_print "[%s]: empty or does not exist\n" file
			else begin 
			let okay = List.fold_left (fun acc elt -> acc &&
				match elt with 
					TP_Contains(s) -> begin
						let my_regexp = Str.regexp_case_fold	(Var.get_string (eval_pe_str s)) in
						try let _ = Str.search_forward my_regexp buff 0 in
							log_only "Appending cols to [%s] because it DOES contain [%s]\n" file (eval_pe_str s)  ;
							true
						with _ ->
							log_only "Not appending cols to [%s] because it does NOT contain [%s]\n" file (eval_pe_str s) ;
							false
						end
				| TP_NotContains(s) -> begin
						let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
						try let _ = Str.search_forward my_regexp buff 0 in 
							log_only "Not appending cols to [%s] because it DOES contains [%s]\n" file (eval_pe_str s) ;
							false
						with _ -> 
							log_only "Appending cols to [%s] because it does NOT contain [%s]\n" file (eval_pe_str s) ; 
							true
						end
				| TP_IfSizeIs(size) -> String.length buff = size 
				| TP_ButOnlyIfItChanges -> true 
				| TP_Eval(pe) ->
							let v = eval_pe buff game pe in 
							if v = Int32.zero then begin
								log_only "Not appending cols to [%s] because condition evaluates to %ld\n" file v ; false
							end else begin 
								log_only "Appending cols to [%s] because condition evaluates to %ld\n" file v ; true
							end 
			) true con_l in 
			if okay then begin (* do the append *) 
				let dest = "override/" ^ file in 
				let buff_as_lines = Str.split many_newline_or_cr_regexp buff in
				if List.length buff_as_lines <> List.length src_list then begin
					log_and_print "Cannot append column-wise because there are %d lines in %s but I was only given %d things to append\n" (List.length buff_as_lines) file (List.length src_list)  ;
					failwith ("cannot append column-wise to " ^ file)
				end ;
				Stats.time "saving files" (fun () -> 
				let out = open_for_writing dest true in
				List.iter2 (fun orig app ->
					output_string out orig ;
					output_string out " " ;
					output_string out app ;
					output_string out "\r\n" 
				) buff_as_lines src_list ; 
				close_out out ;
				begin (* handle read-only files! *)
					try
						Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
					with e -> ()
						(* log_or_print "WARNING: chmod %s : %s\n" filename
							(Printexc.to_string e) *)
				end ;) () ;
				log_or_print "Appended text to [%s] column-wise\n" file
			end
			end
	
	| TP_Append(file,src,con_l,frombif,keep_crlf) ->
			let file = Arch.backslash_to_slash file in
			if Case_ins.filename_check_suffix(String.lowercase file) "ids" then Bcs.clear_ids_map game ;
			log_and_print "Appending to files ...\n" ;
			let file = Var.get_string file in
			let src = Var.get_string src in
			let buff = if frombif then begin
					let eight,three = split (String.uppercase file) in
					let the_buff,loaded_path = Load.load_resource "APPEND" game true eight three in
					the_buff
				end else begin
					load_file file
				end
			in
			let okay = List.fold_left (fun acc elt -> acc &&
				match elt with 
					TP_Contains(s) -> begin
						let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
						try let _ = Str.search_forward my_regexp buff 0 in
							log_only "Appending [%.10s...] to [%s] because it DOES contain [%s]\n" src file (eval_pe_str s)  ;
							true
						with _ ->
							log_only "Not appending [%.10s...] to [%s] because it does NOT contain [%s]\n" src file (eval_pe_str s) ;
							false
						end
				| TP_NotContains(s) -> begin
						let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
						try let _ = Str.search_forward my_regexp buff 0 in
							log_only "Not appending [%.10s...] to [%s] because it DOES contains [%s]\n" src file (eval_pe_str s) ;
							false
						with _ -> 
							log_only "Appending [%.10s...] to [%s] because it does NOT contain [%s]\n" src file (eval_pe_str s) ;
							true
						end
				| TP_IfSizeIs(size) -> String.length buff = size 
				| TP_ButOnlyIfItChanges -> true
				| TP_Eval(pe) -> 
							let v = eval_pe buff game pe in 
							if v = Int32.zero then begin
								log_only "Not appending [%.10s] to [%s] because condition evaluates to %ld\n" src file v ; false
							end else begin 
								log_only "Appending [%.10s] to [%s] because condition evaluates to %ld\n" src file v ; true
							end
			) true con_l in
			if okay then begin (* do the append *) 
				let dest =
					if frombif then "override/" ^ file
					else file
				in
				Stats.time "saving files" (fun () ->
				let out = open_for_writing dest true in
				if keep_crlf then begin
					output_string out buff;
					if !debug_ocaml then log_and_print "%s\n" buff;
					if Str.last_chars buff 2 <> "\r\n" then output_string out "\r\n";
					let src = if String.lowercase dest = "quests.ini" && Arch.view_command = "start"
					then (Str.global_replace (Str.regexp "\\([^\r]\\)\n") "\\1\r\n" src) else src in
					output_string out src;
					if !debug_ocaml then log_and_print "%s\n" src;
					if Str.last_chars src 2 <> "\r\n" then output_string out "\r\n";
				end else begin
					let nice_newlines = Str.global_replace
						many_newline_or_cr_regexp "\r\n" (buff ^ "\r\n") in
					output_string out nice_newlines ;
					output_string out src ;
					output_string out "\r\n" ;
				end;
				close_out out ;
				begin (* handle read-only files! *)
					try
						Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
					with e -> ()
						(* log_or_print "WARNING: chmod %s : %s\n" filename
							(Printexc.to_string e) *)
				end ;) () ;
				log_or_print "Appended text to [%s]\n" file
			end 
	
	| TP_Extend_Top(use_reg,dest,src,pl,tra_l)
	| TP_Extend_Bottom(use_reg,dest,src,pl,tra_l) -> begin
			let dest = if not use_reg then Arch.backslash_to_slash dest else dest in
			let tra_l = List.map (fun x -> Arch.backslash_to_slash x) tra_l in
			log_and_print "Extending game scripts ...\n" ;
			let dest = (Var.get_string dest) in
			let src = (Var.get_string src) in
			let src = Arch.backslash_to_slash src in
			let dlist =
				if use_reg = false then
					[dest]
				else begin
					let files_in_chitin = Key.list_of_key_resources game.Load.key true in
					let regexp = Str.regexp_case_fold dest in
					let matches = ref [] in
					List.iter (fun possible ->
						if Str.string_match regexp possible 0 then begin
							matches := (possible) :: !matches
						end
					) files_in_chitin ;
					if (!matches = []) then
						[dest]
					else
						!matches
				end
			in
			Dc.push_copy_trans_modder () ;
			Dc.notChanged := true ;
				(* handle AUTO_TRA "solarom/%s" *)
			List.iter (fun f ->
				match f,!our_lang with
				Auto_Tra(path),Some(l) ->
							let my_regexp = Str.regexp_string "%s" in
							let tra_file_dir = Str.global_replace
								my_regexp l.lang_dir_name path in
							let d_base,_ = split (Case_ins.filename_basename src) in
							let tra_file = tra_file_dir ^ "/" ^ d_base ^ ".TRA" in
							handle_tra_filename tra_file ;
			| Auto_Tra(path),None ->
							let d_base,_ = split (Case_ins.filename_basename src) in
							let tra_file = path ^ "/" ^ d_base ^ ".TRA" in
							handle_tra_filename tra_file
			| _ -> ()
			) tp.flags ;
			begin
				match !our_lang with
					Some(l) -> List.iter (fun path ->
							let my_regexp = Str.regexp_string "%s" in
							let tra_file = Str.global_replace
								my_regexp l.lang_dir_name  (Var.get_string path) in
							handle_tra_filename tra_file ;
						) tra_l 
				| _ -> List.iter (fun tra_file -> 
							handle_tra_filename tra_file ; 
						) tra_l 
			end ;
			if !Dc.notChanged then Modder.handle_msg "SETUP_TRA" (Printf.sprintf "WARNING: EXTEND* %s with strings from setup.tra\n" src);
			let src_script =
				try
					let src_buff = load_file src in
					if (!has_if_eval_bug) then begin try
						List.iter (fun p -> process_patch1 src game src_buff p) pl ;
					with _ -> () end;
					let src_buff = List.fold_left (fun acc elt ->
								try process_patch2 src game acc elt
								with e ->
									log_and_print "ERROR: [%s] -> [%s] Patching Failed (EXTEND_TOP/BOTTOM)\n"
										src dest ; raise e) 
							src_buff pl 
					in 
					Dc.ok_to_resolve_strings_while_loading := Some(game) ; 
					(try 
						let res = handle_script_buffer src src_buff in
						Dc.ok_to_resolve_strings_while_loading := None ; 
						res
					with e -> 
						begin 
						Dc.ok_to_resolve_strings_while_loading := None ; 
						raise e
						end
					) ;
				with e ->
					if !debug_modder then raise e else []
			in
			List.iter (fun dest -> 
				let eight,three = split (String.uppercase dest) in 
				let dest_script = 
					let old_a_m = !Load.allow_missing in 
					Load.allow_missing := dest :: old_a_m ;
					let dest_buff, dest_path = 
						try 
							Load.load_resource "EXTEND_TOP/EXTEND_BOTTOM" game true eight three
						with _ -> 
							begin 
							log_only "[%s] not found, treating as empty.\n" dest ;
							"",""
							end 
					in
					Load.allow_missing := old_a_m ; 
					handle_script_buffer dest dest_buff
				in 
	
				let destpath = "override/" ^ dest in 
				Stats.time "saving files" (fun () -> 
				let out = open_for_writing destpath true in
				Bcs.save_bcs game (Bcs.Save_BCS_OC(out)) (match a with
					TP_Extend_Top(_,_,_,_,_) -> src_script @ dest_script
															 | _ -> dest_script @ src_script) ;
				close_out out ;
				begin (* handle read-only files! *)
					try
						Case_ins.unix_chmod destpath 511 ; (* 511 = octal 0777 = a+rwx *)
					with e -> ()
						(* log_or_print "WARNING: chmod %s : %s\n" filename
							(Printexc.to_string e) *)
				end ;) () ;
				log_or_print "Extended script [%s] with [%s]\n" dest src
			) dlist ;
			Dc.pop_trans () ; 
			end
	
	| TP_At_Interactive_Exit(str,exact) ->
			if !interactive then process_action tp (TP_At_Exit(str,exact))
	| TP_At_Interactive_Uninstall(str,exact) ->
			if !interactive then process_action tp (TP_At_Uninstall(str,exact))
	| TP_At_Interactive_Now(str,exact) ->
			if !interactive then process_action tp (TP_At_Now(str,exact))
	
	| TP_At_Exit(str,exact) ->
			begin
			let str = Var.get_string str in
			let a,b = split (String.uppercase str) in 
			match b with
			| "TP2" -> (enqueue_tp2_filename) str
			| _ ->
				let str = if exact then str else Arch.handle_view_command str !skip_at_view in
				if List.mem (str,exact) !execute_at_exit then
					()
				else
					execute_at_exit := (str,exact) :: !execute_at_exit
			end
	
	| TP_At_Now(str,exact) ->
			begin
			let str = Var.get_string str in
			let a,b = split (String.uppercase str) in
			match b with
			| "TP2" -> (enqueue_tp2_filename) str
			| _ ->
				let str = if exact then str else Arch.handle_view_command str !skip_at_view in
				ignore (exec_command str exact)
			end
	
	| TP_At_Uninstall(str,exact) -> ()
	);
if !clear_memory then begin
	clear_memory := false;
	process_action tp TP_ClearMemory;
	clear_memory := true;
end;
with e -> (* from: let rec process_action = try *)
	(if !continue_on_error then begin
		log_and_print "WARNING: Continuing despite [%s]\n"
			(Printexc.to_string e);

		(try assert false with Assert_failure(file,line,col) -> set_errors file line);
	end else begin
		return_value := return_value_error_tp2_component_install ;
		log_and_print "Stopping installation because of error.\n" ;
		raise e
	end)
	) ()
