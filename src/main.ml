(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
6 November 2005. All changes for this file are listed in
diffs/src.main.ml.diff file, as the output of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

open Util
open Xdiff
open Version
open Parsewrappers

let pause_at_end = ref false

type output_info = {
	mutable dir : string ;
	mutable file : string ;
	mutable chan : out_channel lazy_t ;
	mutable append : bool ;
}

let main () =
	let theout = {
		dir = ".";
		file = "";
		chan = lazy(stdout) ;
		append = false ;
	} in

	let print_theout fmt = Printf.fprintf (Lazy.force theout.chan) fmt in
	let output_theout s = print_theout "%s" s in
	let output_buffer_theout b = print_theout "%s" (Buffer.contents b) in
	let flush_theout () = flush (Lazy.force theout.chan) in

	let handle_out_boringness file ext_chop =
		try
			if !debug_ocaml then log_and_print "\n\nhandle_out_boringness received %s as a file name.\n" file ;
			if true then begin match theout.dir, theout.file with
				| ".", "" -> if !debug_ocaml then log_and_print "no --out\n"
				| _, "" -> if !debug_ocaml then log_and_print "--out a_dir: %s\n"  theout.dir
				| ".", _ -> if !debug_ocaml then log_and_print "--out a_file: %s\n"  theout.file
				| _ -> if !debug_ocaml then log_and_print "You're a pervert, decide where to put your stuff!\n"
			end ;
			let ext_chop = List.map String.uppercase ext_chop in
			let directory,(name,ext) = (Case_ins.filename_dirname file,split(Case_ins.filename_basename file)) in
			let fullname = match theout.dir, theout.file with
				| ".", "" -> file
				| _, "" -> theout.dir ^
					(match (String.get theout.dir (String.length theout.dir - 1)) with
						| '/'
						| '\\' -> ""
						| _ -> "/"
					) ^ name ^ "." ^ ext
				| ".", _ -> theout.file
				| _ -> file
			in
			let base,ext = split fullname in
			let result =
				if List.mem (String.uppercase ext) ext_chop then
					Case_ins.filename_chop_extension fullname
				else
					fullname
			in
			if !debug_ocaml then log_and_print "handle_out_boringness returns %s\n" result ;
			result
		with _ -> if !debug_ocaml then log_and_print "something happened...\nDefaulting to %s\n"	file ; file
	in

	let set_theout app s =
		theout.append <- app ;
		if is_directory s then begin
			theout.dir <- s
		end else begin
			theout.file <- s ;
			if app then
				theout.chan <- lazy (Case_ins.perv_open_out_gen [Open_append ; Open_wronly ; Open_creat ; Open_text ] 511 s)
			else
				theout.chan <- lazy (Case_ins.perv_open_out_gen [ Open_wronly ; Open_creat ; Open_text] 511 s)
		end;
	in

	let user_min = ref None in
	let user_max = ref None in

	let cmp_src = ref None in
	let cmp_dest = ref None in
	
	let list_lang = ref None in
	let list_comp = ref None in
	let list_comp_lang = ref 0 in

	let make_an_itemlist = ref false in
	let make_an_xplist = ref false in
	let do_cre_analysis = ref false in
	let do_itmsort = ref false in

	let dcmp_src = ref None in
	let dcmp_dest = ref None in

	let tcmp_src = ref None in
	let tcmp_dest = ref None in

	let bcmp_src = ref None in
	let bcmp_dest = ref None in

	let textcmp_src = ref None in
	let textcmp_dest = ref None in

	let d_toplevel = ref false in

	let bcmp_orig = ref None in
	let bcmp_patch = ref None in

	let tlkcmp_src = ref None in
	let tlkcmp_dest = ref None in
	let tlkcmp_strings = ref false in 

	let make_biff = ref None in 
	let remove_biff = ref None in 
	let make_tlk = ref [] in 

	let no_game = ref false in

	let transitive = ref false in 
	let two_pass = ref false in

	let dlg_list = ref [] in
	let d_list = ref [] in
	let ds_list = ref [] in
	let strapp_list = ref [] in
	let bc_list = ref [] in 
	let bg_list = ref [] in 
	let bcs_list = ref [] in 
	let baf_list = ref [] in 

	let no_auto_update = ref false in 
	let auto_update_all = ref false in
	let exit_now = ref false in

	let force_install_these_main	 = ref [] in
	let force_uninstall_these_main = ref [] in
	
	let change_log = ref [] in
	
	let process_script = ref "" in

	let automate_list = ref [] in
	let automate_file = ref None in
	let automate_min = ref None in

	let tlk_merge = ref [] in

	let extract_tlk = ref false in 
	let extract_kits = ref (0) in

	let tp_list = ref [] in 

	let strfind_list = ref [] in 

	let trans_list = ref [] in

	let d_headers = ref true in

	let list_biff = ref false in 
	let list_files = ref false in 

	let list_eff_list = ref [] in


	let bs_type_list = ref [] in
	let bs_str_list = ref [] in 

	let biff_short = ref 0 in
	let biff_short_at = ref 0 in 

	let output_dialog = ref None in 
	let output_dialogf = ref None in 

	let traify = ref None in
	let traify_old_tra = ref None in
	let traify_comment = ref false in
	let trbify = ref None in
	let traify_num = ref 0 in
	
	let untraify_d = ref None in
	let untraify_tra = ref None in

	let forceify = ref None in 

	let use_trans = ref false in 
	let test_trans = ref false in 

	let argv0_base, argv0_ext = split (String.uppercase 
		(Case_ins.filename_basename Sys.argv.(0))) in

	let auto () = begin
		pause_at_end := true ;
		if not !no_game then begin
			output_dialog := Some("dialog.tlk") ;
			output_dialogf := Some("dialogf.tlk") ;
			Load.set_dialog_tlk_path "dialog.tlk" ;
			Load.set_dialogf_tlk_path "dialogf.tlk" ;
		end;
		init_log Version.version (argv0_base ^ ".DEBUG") ;
		(try
			if (Arch.do_auto_update) then begin
				if not !no_auto_update then
					Autoupdate.verify_latest true
			end else
				log_and_print "[On this architecture, WeiDU does not auto-update.\n  You must ensure that you have the most recent version.]\n"
		with e ->
			begin
				log_and_print "ERROR: Cannot perform auto-update, going ahead anyway!\n\t%s\n"
					(Printexc.to_string e) ;
(*				 exit return_value_error_autoupdate *)
			end ) ;
		if List.exists (fun arg -> let a,b = split arg in (String.uppercase b) = "TP2")
			(Array.to_list Sys.argv) then
			() (* setup-solaufein.exe foo.tp2
					* runs foo.tp2, not setup-solaufein.tp2 *)
		else begin
			let rec try_it file_list = match file_list with
			| file :: lst ->
					if file_exists file then
						tp_list := file :: !tp_list
					else try_it lst
			| [] ->
				log_and_print "\n\n** ERROR ** [%s.TP2] not found.\nMake sure that you have unpacked the archive correctly and\nthat you are not trying to run this file from inside an archive." argv0_base
			in
			let chunk_list = Str.split (Str.regexp "[-]") argv0_base in
			let chunk = match chunk_list with
			| a :: b :: _ -> b
			| _ -> ""
			in
			try_it
			[ (argv0_base ^ ".TP2") ;
				(chunk ^ ".TP2") ;
				(chunk ^ "/" ^ argv0_base ^ ".TP2") ;
				(chunk ^ "/" ^ chunk ^ ".TP2") ; ]
		end
	end in

	let forced_script_style = ref Load.NONE in
	let counter = ref 0 in

	let usageMsg = Printf.sprintf "\t\tWeiDU (version %s: \"%s\")\n\nusage: WeiDU [options] BAF,BCS,D,DLG,TRA,TP,TP2-files\n\nGeneral Input Options:\n" version comment in
	let argDescr = [
		"--game", Myarg.String Load.add_game_path, "X\tset main game directory to X" ;
		"--game-by-type", Myarg.String (fun x -> Load.add_game_path(Arch.game_path_by_type x)), "X\tset main game directory to the one where X is installed (BG,BG2,IWD,IWD2,PST)";
		"--nogame", Myarg.Set no_game,"\tdo not load any default game files" ;
		"--search", Myarg.String Load.add_override_path, "X\tlook in X for input files (cumulative)" ;
		"--search-ids", Myarg.String Load.add_ids_path, "X\tlook in X for input IDS files (cumulative)" ;
		"--tlkin", Myarg.String Load.set_dialog_tlk_path,"X\tuse X as DIALOG.TLK" ;
		"--ftlkin", Myarg.String Load.set_dialogf_tlk_path,"X\tuse X as DIALOGF.TLK";
		"--tlkmerge", Myarg.String (fun s -> tlk_merge := !tlk_merge @ [s]),
			"X\tmerge X into loaded DIALOG.TLK" ;
		"--yes", Myarg.Set Tp.always_yes,"\tanswer all TP2 questions with 'Yes'";
		"--uninstall", Myarg.Set Tp.always_uninstall,"\tanswer all TP2 questions with 'Uninstall'" ;
		"--reinstall", Myarg.Set Tp.sometimes_reinstall,"\treinstall all installed TP2 components" ;
		"--language", Myarg.Int (fun d -> Tp.forced_language := d), "X\tSet the language to X" ;
		"--force-install",	 Myarg.Int (fun d -> force_install_these_main 	:= d :: !force_install_these_main;
		Tp.specified_specific_components := true),		 "\tX installs component X number (cumulative)" ;
		"--force-uninstall", Myarg.Int (fun d -> force_uninstall_these_main := d :: !force_uninstall_these_main;
		Tp.specified_specific_components := true), "\tX uninstalls component X number (cumulative)" ;
		"--force-install-rest", 	Myarg.Rest (fun d -> force_install_these_main 	:= (int_of_string d) :: !force_install_these_main;
		Tp.specified_specific_components := true),		 "\tX Y... installs component number X, Y... (cumulative)" ;
		"--force-install-list", 	Myarg.List (fun d -> force_install_these_main 	:= (int_of_string d) :: !force_install_these_main;
		Tp.specified_specific_components := true),		 "\tX Y... installs component number X, Y... (cumulative)" ;
		"--force-uninstall-rest", Myarg.Rest (fun d -> force_uninstall_these_main := (int_of_string d) :: !force_uninstall_these_main;
		Tp.specified_specific_components := true), "\tX Y... uninstalls component number X, Y... (cumulative)" ;
		"--force-uninstall-list", Myarg.List (fun d -> force_uninstall_these_main := (int_of_string d) :: !force_uninstall_these_main;
		Tp.specified_specific_components := true), "\tX Y... uninstalls component number X, Y... (cumulative)" ;
		"--process-script", Myarg.String (fun s -> process_script := s), "\tX process installation script X";
		"--skip-at-view", Myarg.Set Tp.skip_at_view, "kills AT_* ~VIEW this~";
		"--quick-log", Myarg.Set Tp.quick_log, "Doesn't print the name of components in weidu.log (much faster)";
		"--ask-every", Myarg.Set Tp.ask_all, "\task about every TP2 component" ;
		"--list-languages", Myarg.String (fun s -> list_lang := Some s), "\tX lists the languages in X";
		"--list-components", Myarg.Tuple [
			Myarg.String (fun s -> list_comp := Some s);
			Myarg.Int (fun s -> list_comp_lang := s);
		], "\tX Y lists all components in X using language Y";
		"--change-log",Myarg.String (fun s -> change_log := s :: !change_log), "\tgenerates a changelog for the given resource (cumulative)";
		"--change-log-list",Myarg.List (fun s -> change_log := s :: !change_log), "\tgenerates a changelog for the given resource (cumulative)";
		"--change-log-rest",Myarg.Rest (fun s -> change_log := s :: !change_log), "\tgenerates a changelog for the given resource (cumulative)";
		"--noautoupdate", Myarg.Set no_auto_update,"\tdo not auto-update WeiDU setup files" ;
		"--noselfupdatemsg", Myarg.Clear Autoupdate.self_update_message,"\tdo not print any self-updating messages" ;
		"--update-all", Myarg.Set auto_update_all,"\tauto-update all WeiDU setup files";
		"--args", Myarg.String (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter),
			"\tX X will be stored in the %argv[x]% variable (cumulative)";
		"--args-rest", Myarg.Rest (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter),
			"\tX Y... X, Y... will be stored in the %argvx% variables (cumulative)";
		"--args-list", Myarg.List (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter),
			"\tX Y... X, Y... will be stored in the %argvx% variables (cumulative)";
		"--debug-ocaml", Myarg.Set Util.debug_ocaml,"\tenables random debugging information for the OcaML source (rarely of interest to end-users" ;
		"--debug-boiic", Myarg.Set Tp.debug_boiic,"\tprints out which files have been changed by BUT_ONLY_IF_IT_CHANGES" ;
		"--debug-change", Myarg.Set Tp.debug_change,"\tprints a warning if a file is being COPY_EXISTED without receiving a change." ;
		"--clear-memory", Myarg.Set Tpstate.clear_memory,"\tcalls CLEAR_MEMORY after every action evaluation.";
		"--script-style", Myarg.String (fun s ->
			let n = match String.uppercase s with
				| "BG"
				| "BG2" -> (Tlk.is_bg2 := true;Load.BG2)
				| "BG1" -> (Tlk.is_bg2 := false;Load.BG1)
				| "PST" -> (Tlk.is_bg2 := false;Load.PST)
				| "IWD"
				| "IWD1" -> (Tlk.is_bg2 := false;Load.IWD1)
				| "IWD2" -> (Tlk.is_bg2 := false;Load.IWD2)
				| _ -> parse_error "unknown SCRIPT_STYLE"
			in forced_script_style := n),"X\tuse BCS/BAF style X (BG, PST, IWD1, IWD2)"

		^ "\n\nGeneral Output Options:\n" ;

		"--backup", Myarg.String (fun s -> backup_dir := Some(s)), "X\tbackup files to directory X before overwriting" ;
		"--tlkout", Myarg.String (fun s -> output_dialog := Some(s)), "X\temit X as new DIALOG.TLK" ;
		"--ftlkout", Myarg.String (fun s -> output_dialogf := Some(s)), "X\temit X as new DIALOGF.TLK\n\nD Options:\n" ; 

		"--transin", Myarg.String (fun s -> trans_list := !trans_list @ [s]), "X\tuse translation file X (cumulative)" ;
		"--testtrans", Myarg.Set test_trans, "\ttest all translations files" ;
		"--noheader", Myarg.Clear d_headers, "\tdo not emit .D header comments" ;
		"--nofrom", Myarg.Clear Dlg.emit_from, "\tdo not emit .D \"// from:\" comments" ;
		"--full-from", Myarg.Set two_pass, "\tGenerate complete \"// from:\" comments"; 
		"--nocom", Myarg.Clear Dlg.comments, "\tdo not emit ANY .D / .BAF comments" ;
		"--transitive", Myarg.Set transitive, "\tFollow EXTERN links when making D files" ;
		"--toplevel", Myarg.Set d_toplevel, "\tEmit top-level DLG states only" ; 
		"--text", Myarg.Set Dlg.emit_text, "\temit string text with refs in comments" ; 

		"--out", Myarg.String (set_theout false), "X\temit to file or directory X" ;
		"--append", Myarg.String (set_theout true), "X\tappend to file or directory X" ;

		(*
		"--out", Myarg.String (fun s -> output_dir := s), "X\temit all output files in directory X" ;
		"--dout", Myarg.String (fun s -> dout_list := !dout_list @ [s]), "X\tname of output .D file to emit (cumulative)" ;
		"--textout", Myarg.String (fun s -> textout := Case_ins.perv_open_out s), "X\tput text output in file X"; 
		"--textapp", Myarg.String (fun s -> 
				textout := Case_ins.perv_open_out_gen [Open_append ; Open_wronly ; Open_creat ; Open_text ] 511 s ) ,
				"X\tappend text output to end of file X"; 
		*)
		"--traify", Myarg.String (fun s -> traify := Some(s)), "X\tconvert .D file X to use TRAs (use with --out)" ;
		"--traify-old-tra", Myarg.String (fun s -> traify_old_tra := Some(s)), "X\tthe given .TRA file contains the initial strings to traify" ;
		"--traify#", Myarg.Int (fun d -> traify_num := d), "X\tstart --traify .TRA file at @X" ;
		"--traify-comment", Myarg.Set traify_comment, "\toutput @1 /* ~Hello~ */ rather than @1 when traifying" ;
		"--trbify", Myarg.String (fun s -> trbify := Some(s)), "X\tconvert .TRA file X to a TRB (use with --out)" ;
		"--untraify-d", Myarg.String (fun s -> untraify_d := Some(s)), "X\tconvert .D file X to use hardcoded strings...";
		"--untraify-tra", Myarg.String (fun s -> untraify_tra := Some(s)), "X\t...from TRA file X";
		"--extract-kits", Myarg.Int (fun d -> extract_kits := d), "X\textract all kits starting with kit #X";
		"--forceify", Myarg.String (fun s -> forceify := Some(s)), "X\tconvert .D file X to use forced strrefs (use with --dout)" ;
		"--transref", Myarg.Set Dlg.use_trans_ref, "\temit string reference numbers in TRA files" ;
		"--trans", Myarg.Set use_trans, "\temit coupled .D and .TRA files\n\nTLK String Options:\n" ;

		"--traify-tlk", Myarg.Set extract_tlk, "\temit a .TRA file for the given .TLK file (see --out, --min, --traify#)" ;
		"--make-tlk", Myarg.String (fun s -> make_tlk := s :: !make_tlk), "X\tmake a .TLK file from .TRA file X (cumulative, see --tlkout)" ;
		"--string", Myarg.Int (fun i -> ds_list := i :: !ds_list), "X\tdisplay string reference #X (cumulative)" ;
		"--strfind", Myarg.String (fun s -> strfind_list := s :: !strfind_list), "X\tdisplay strings that contain X (cumulative, regexp allowed)" ;
		"--strapp", Myarg.String (fun s -> strapp_list := s :: !strapp_list), "X\tappend string X to DIALOG.TLK (cumulative)\n\nBIFF Options:\n" ;
		"--exit", Myarg.Set exit_now, "\tprint version number and exit";
		"--list-biffs", Myarg.Set list_biff, "\tenumerate all BIFF files in CHITIN.KEY" ;
		"--list-files", Myarg.Set list_files, "\tenumerate all resource files in CHITIN.KEY"; 
		"--biff", Myarg.String (fun s -> bc_list := (String.uppercase s) :: !bc_list), "X\tenumerate contents of BIFF file X (cumulative)" ;
		"--biff-type", Myarg.String (fun s -> bs_type_list := s :: !bs_type_list), "X\texamine all BIFF resources of extension X ... (cumulative)" ;
		"--biff-str", Myarg.String (fun s -> bs_str_list := s :: !bs_str_list), "X\t... and list those containing X (cumulative, regexp allowed)" ;
		"--biff-name", Myarg.Int (fun i -> Load.content_name_offset := Some(i)),
			"X\tassume matching items have a strref name at offset X" ;
		"--biff-value", Myarg.Int (fun i -> biff_short := i), "X\t... or list those containing value X ..." ;
		"--biff-value-at", Myarg.Int (fun i -> biff_short_at := i), "X\t... at offset X" ;
		"--biff-get", Myarg.String (fun s -> bg_list := s :: !bg_list), "X\textract resource X from game BIFFs (cumulative, regexp allowed)" ;
		"--biff-get-rest", Myarg.Rest (fun s -> bg_list := s :: !bg_list), "X, Y, ...\textract resources X, Y, ... from game BIFFs (regexp allowed)" ;
		"--biff-get-list", Myarg.List (fun s -> bg_list := s :: !bg_list), "X, Y, ...\textract resources X, Y, ... from game BIFFs (regexp allowed)" ;
		"--make-biff", Myarg.String (fun s -> make_biff := Some(s)), "X\tmake data\\X.bif from all files in folder X, update CHITIN.KEY" ;
		"--remove-biff", Myarg.String (fun s -> remove_biff := Some(s)), "X\tremove references to biff X and its resources, update CHITIN.KEY" ;

(* 
		"--itemlist", Myarg.Set make_an_itemlist, "\tmake an item listing (IWD2)" ; 
		"--xplist", Myarg.Set make_an_xplist, "\tmake an XP listing (IWD2)" ; 
		"--cre_analysis", Myarg.Set do_cre_analysis, "\tdo CRE analysis (IWD2)" ;
		"--itemsort", Myarg.Set do_itmsort, "\tdo ITM sort (IWD2)" ;
		 *)

		"", Myarg.Unit (fun a -> a),	"\nARE/ITM/SPL/CRE Options:\n" ;
		"--automate", Myarg.String (fun s -> automate_list := s ::
		!automate_list), "X\tautomatically make a TP2 file for ARE/ITM/SPL/CRE/EFF/STO files in X" ;
		"--automate-file", Myarg.String (fun s -> automate_file := Some s), "X\tautomatically make a TP2 snippet for ARE/ITM/SPL/CRE/EFF/STO file X";
		"--automate-min", Myarg.Int (fun i -> automate_min := Some i),
			"X\tminimum strref # for --automate (default is SoA)";

		"--list-eff", Myarg.String (fun s -> list_eff_list := s :: !list_eff_list), "X\tlist effects in resource X" ;

		"", Myarg.Unit (fun a -> a),	"\nComparison Options:\n" ;
		"--cmp-from", Myarg.String (fun s -> cmp_src := Some(s)), "X\temit WRITE_BYTEs to turn this file ..." ;
		"--cmp-to", Myarg.String (fun s -> cmp_dest := Some(s)), "X\t... into this one";
		"--dcmp-from", Myarg.String (fun s -> dcmp_src := Some(s)), "X\temit REPLACEs to turn this DLG file ..." ;
		"--dcmp-to", Myarg.String (fun s -> dcmp_dest := Some(s)), "X\t... into this one";

		"--tcmp-from", Myarg.String (fun s -> tcmp_src := Some(s)), "X\tcompare this TRA file (or directory of TRA files)..." ;
		"--tcmp-to", Myarg.String (fun s -> tcmp_dest := Some(s)), "X\t... with this one (or this directory)";
		"--bcmp-from", Myarg.String (fun s -> bcmp_src := Some(s)), "X\temit APPLY_BCS_PATCH to turn this BCS file..." ;
		"--bcmp-to", Myarg.String (fun s -> bcmp_dest := Some(s)), "X\t... into this one" ;
		"--textcmp-from", Myarg.String (fun s -> textcmp_src := Some(s)), "X\temit APPLY_BCS_PATCH to turn this textual file..." ;
		"--textcmp-to", Myarg.String (fun s -> textcmp_dest := Some(s)), "X\t... into this one" ;
		(* For debugging patch/diff: *)
		"--bcmp-orig", Myarg.String (fun s -> bcmp_orig := Some(s)), "X\toriginal file to apply ..." ;
		"--bcmp-patch", Myarg.String (fun s -> bcmp_patch := Some(s)), "X\t... this patch to" ;
		"--tlkcmp-from", Myarg.String (fun s -> tlkcmp_src := Some(s)), "X\temit STRING_SETs to convert this TLK file ..." ;
		"--tlkcmp-to", Myarg.String (fun s -> tlkcmp_dest := Some(s)), "X\t... into this one";
		"--tlkcmp-use-strings", Myarg.Set tlkcmp_strings, "\tmodifies --tlkcmp behavior"; 
		"--min", Myarg.Int (fun i -> user_min := Some(i)), "X\tlower range for some commands (like --tlkcmp)" ;
		"--max", Myarg.Int (fun i -> user_max := Some(i)), "X\tupper range for some commands (like --string)" ;

		"", Myarg.Unit (fun a -> a),	"\nLog Options:\n" ;

		"--log", Myarg.String (fun s -> init_log Version.version s),"X\tlog output and details to X" ;
		"--autolog", Myarg.Unit (fun () -> init_log Version.version "WSETUP.DEBUG"), "\tlog output and details to WSETUP.DEBUG" ;
		"--logapp", Myarg.Set append_to_log,"\tappend to log instead of overwriting" ; 
		"--log-extern", Myarg.Set log_extern,"\talso log output from commands invoked by WeiDU " ; 
		"--debug-assign", Myarg.Set Var.debug_assign,"\tPrint out all values assigned to TP2 variables" ;
		"--debug-value", Myarg.Set Tp.debug_pe,"\tPrint out all value expressions" ;
		"--continue", Myarg.Set Tp.continue_on_error,"\tcontinue despite TP2 action errors" ;

		"", Myarg.Unit (fun a -> a),	"\nHelp Options:\n";

	] in
	let give_help () =
		Myarg.usage argDescr usageMsg ;
		exit return_value_error_argument
	in
	let handleArg str = begin
		let base,ext = split (String.uppercase str) in
		match ext with
		| "D" -> d_list := str :: !d_list
		| "DLG" -> dlg_list := (base,ext) :: !dlg_list 
		| "TLK" -> Load.set_dialog_tlk_path str 
		| "TP" 
		| "TP2" -> tp_list := !tp_list @ [str] 
		| "TRA"
		| "TRB" -> trans_list := !trans_list @ [str] 
		| "ITM"
		| "EFF" 
		| "SPL" -> list_eff_list := !list_eff_list @ [str]
		| "BCS" | "BS" -> bcs_list := !bcs_list @ [str]
		| "BAF" -> baf_list := !baf_list @ [str]
		| "" ->
			begin
			let do_state state_num = 
				Hashtbl.add Dlg.emit_these_states state_num () ;
				Dlg.emit_some_states_only := true
			in try 
				Scanf.sscanf str "%d-%d" (fun a b ->
					for i = a to b do do_state i done) ;
				with _ -> 
					begin try 
						Scanf.sscanf str "%d" (fun d -> do_state d)
					with _ ->
						log_and_print "Unknown argument: [%s]\n\n" str ; 
						give_help ()
					end 
			end
		| _ -> log_and_print "Unknown argument: [%s]\n\n" str ; give_help ()
	end in

	log_and_print "[%s] WeiDU version %s\n" Sys.argv.(0) version ;
	let version_int = int_of_string Version.version in
	if version_int mod 100 <> 0 then
		log_and_print "This is a non-stable version. Unless you're sure about what you're doing, consider downgrading.\n" ;

	if Arch.check_UAC () then begin
(* 		log_and_print "\nYou are running Vista and have UAC enabled. This can cause problems
unless you specifically run this program as administrator and have full access
to your game directory (which I cannot currently check).
If you are unsure, disable UAC and re-run this mod.\n\n"; *)
	end;
	(* see if AUTOUPDATE is in our base name *)
	begin try
		Autoupdate.self();
	with _ -> () end ;

	Load.game_paths := Load.registry_game_paths () ;

	Myarg.parse argDescr handleArg usageMsg  ;
	if !exit_now then exit 0;

	weidu_version := Version.version;

	if (!auto_update_all) then begin
		(if (Arch.do_auto_update) then
			Autoupdate.verify_latest true);
		exit return_value_success ;
	end ;

	(* see if SETUP is in our base name *)
	let setup_regexp = Str.regexp_case_fold "setup" in
	begin
	try
		let _ = Str.search_forward setup_regexp argv0_base 0 in
		auto () ;
	with _ ->
		if Array.length Sys.argv <= 1 then begin
			Myarg.usage argDescr usageMsg ;
			flush_all () ;
			log_and_print "\nEnter arguments: " ;
			let mystr = read_line () in
			if mystr = "" then exit return_value_error_argument
			else exit ( Sys.command (Sys.executable_name ^ " " ^ mystr))
		end ;
	end ;
	let game =
		if !no_game then
			Load.load_null_game ()
		else
			Load.load_game ()
	in

	if (!forced_script_style <> Load.NONE) then
		game.Load.script_style <- !forced_script_style ;
	let s = match game.Load.script_style with
	| Load.PST -> "PST"
	| Load.BG1 -> "BG1"
	| Load.BG2 -> "BG2"
	| Load.IWD1 -> "IWD1"
	| Load.IWD2 -> "IWD2"
	| Load.NONE -> "NONE (ERROR!!!)"
	in log_and_print "[%s] Using scripting style \"%s\"\n" Sys.argv.(0) s;

	Dc.cur_index := Array.length game.Load.dialog ;
	Load.saved_game := Some(game) ;


(*
	(  if (!make_an_itemlist) then Itemlist.make_item_list game print_theout );
	(  if (!make_an_xplist) then Itemlist.make_xplist game print_theout );
	( if (!do_cre_analysis) then Itemlist.cre_analysis game print_theout ) ; 
	( if (!do_itmsort) then Itemlist.itm_randomizer game print_theout ) ; 
		*)

	let automate_min = lazy(match !automate_min with
		| Some x -> x
		| None -> begin
			let rec walk lst = match lst with
				| (s,i) :: _ when Tppe.is_true (Tppe.eval_pe "" game (Tp.PE_GameIs(s,false))) -> i
				| hd :: tl -> walk tl
				| [] -> 0
			in walk ["BG2",62169; "TOB",74107; "BG1",22186; "TOTSC",24124; "IWD1",34502; "HOW",34502; "TOTLM",34502; "PST",106497; "IWD2",41422]
		end)
	in
	if !automate_list <> [] then begin
		Automate.automate game !automate_list (Lazy.force automate_min) (output_string (Lazy.force theout.chan)) ;
	end;
	
	(match !automate_file with
	| Some(x) -> Automate.automate_file game x (Lazy.force automate_min) (output_string (Lazy.force theout.chan));
	| None -> ()
	);

	(match !forceify with
		Some(file) -> begin
			try 
				let name,ext = split (String.uppercase file) in
				Dlg.local_string_ht := Some([]) ;
				begin
					match ext with
						"D" -> ignore (
									let inchan = Case_ins.perv_open_in file in
									let lexbuf = lex_init file inchan in
									ignore (Stats.time "parsing .D files"
									(fun () -> Dparser.d_file Dlexer.initial lexbuf) ());
									close_in inchan ;)
					| "TP2" -> ignore
									 (Tparser.parse_tp2_file (File file))
					| "TPA"
					| "TPH" -> ignore
									 (Tparser.parse_tpa_file (File file))
					| "TPP" -> ignore
									 (Tparser.parse_tpp_file (File file))
					| _ -> log_and_print "ERROR: don't know how to --forceify files with extension [%s]\n" ext ; failwith ext
				end ;
				pop_context ();
				log_or_print "[%s] parsed\n" file ;

				let buf = ref (load_file file) in

				let dout = output_theout in 

				let replace lse str =
					let my_regexp = Str.regexp (Str.quote str) in
					try
					let num = Tlk.find_string_fast lse game.Load.dialog 
							game.Load.dialogf game.Load.dialog_search
					in 
					let replace_with = Printf.sprintf "!%d %s" num str in 
					buf := Str.global_replace my_regexp replace_with !buf ;
					with Not_found -> 
						log_and_print "WARNING: cannot find [%s] in dialog.tlk: not --forceifying that string\n" lse.lse_male 
				in 

				(match !Dlg.local_string_ht with 
					Some(lst) -> 
					List.iter (fun	ls	-> match ls with
					| Dlg.Local_String(lse) -> 
							if lse.lse_male <> "" then begin 
								replace lse ("~" ^ lse.lse_male ^ "~" ); 
								replace lse ("%" ^ lse.lse_male ^ "%" ); 
								replace lse ("\"" ^ lse.lse_male ^ "\"" ); 
							end 
					| _ -> failwith "forceify1" 
				) (List.rev lst); 
				| None -> failwith "forceify2" 
				); 

				Dlg.local_string_ht := None ;
				dout !buf ; 
				()

			with e -> 
				log_and_print "ERROR: problem force-ifying file [%s]: %s\n" file
					(Printexc.to_string e) ;
				raise e
			end
	| _ -> () 
	) ;

	if !make_tlk <> [] then begin
		let results : (int * local_string_entry) list 
			= List.fold_left (fun acc filename ->
			let result = parse_file true (File filename) "parsing .tra files" 
				(Dparser.tra_file Dlexer.initial) in 
			log_or_print "[%s] has %d translation strings\n" filename (List.length result); 
			let result = List.rev_map (fun (i,ts) -> match ts with
				Dlg.Local_String(lse) -> (i,lse)
			| _ -> failwith "make_tlk"
			) result in 
			List.rev_append acc result) [] !make_tlk 
		in
		let max = 1 + (List.fold_left (fun acc (i,elt) -> 
			if i > acc then i else acc) 0 results) in
		log_and_print "New TLK will have %d entries\n" max ;
		let new_tlk = Array.make max ( { Tlk.flags = 7 ;
																 Tlk.sound_name = "";
																 Tlk.volume = 0; 
																 Tlk.pitch = 0;
																 Tlk.text = ""; } ) in
		List.iter (fun (i,lse) ->
			let male, female = Tlk.lse_to_tlk_string lse in
			new_tlk.(i) <- male
		) results ;

		game.Load.dialog_mod <- true ;
		game.Load.dialog <- new_tlk ;
	end ; 


	(if !extract_tlk then begin
			let tlk = game.Load.dialog in 
			let ftlk = game.Load.dialogf in 
			let my_min = match !user_min with
				Some(i) -> i
			| None -> 0 
			in
			let my_max = match !user_max with
				Some(i) -> i
			| None -> (Array.length tlk) - 1
			in
			let reg_list = List.map Str.regexp_case_fold !strfind_list in 
			strfind_list := [] ; 
			for i = my_min to my_max do
				let matches = reg_list = [] || 
					List.fold_left (fun acc r -> acc ||
						try 
							let _ = Str.search_forward r tlk.(i).Tlk.text 0 in
							true
						with _ -> false
					) false reg_list 
				in 
				if matches then begin 
					print_theout "@%-5d =" (i + !traify_num);
					let display ts = begin
						print_theout " %s" (Tlk.weidu_quotify ts.Tlk.text) ;
						if ts.Tlk.sound_name <> "" then 
						 print_theout " [%s]" ts.Tlk.sound_name ;
					end  in
					display tlk.(i) ;
					(match ftlk with 
						None -> ()
					| Some(a) ->
						if a.(i).Tlk.text <> tlk.(i).Tlk.text ||
							 a.(i).Tlk.sound_name <> tlk.(i).Tlk.sound_name then 
								display a.(i) )  ;
					print_theout "\n" ; 
				end 
			done 
	end) ;

	(if !extract_kits > 0 then Kit.extract game (output_string (Lazy.force theout.chan)) theout.dir
	!extract_kits) ;

	(match !cmp_dest with
		Some(d) ->
			let b1,s = match !cmp_src with
				| Some (x) -> (load_file x,x)
				| None ->
					let (base,ext) = split (Filename.basename d) in
					(fst (Load.load_resource "cmp-from" game true base ext),Filename.basename d)
			in
			let b2 = load_file d in
			let l1 = String.length b1 in
			let l2 = String.length b2 in
			if !cmp_src = None then
				print_theout "COPY_EXISTING ~%s~ ~override~\n" s
			else
				print_theout "COPY ~%s~ ~%s~\n" s d;
			print_theout "// patches to turn [%s] into [%s]\n" s d ;
			if (l1 <> l2) then begin
				print_theout "\t//[%s] is %d bytes while [%s] is %d bytes\n"
					s l1 d l2
			end;
			let (b1,b2) =
				if (l1 < l2) then begin
					print_theout "\tINSERT_BYTES 0x%x 0x%x\n" l1 (l2 - l1);
					b1 ^ String.make (l2 - l1) '\000',b2
				end else if (l1 > l2) then begin
					print_theout "\tDELETE_BYTES 0x%x 0x%x\n" l2 (l1 - l2);
					(String.sub b1 0 l2), b2
				end else begin
					b1,b2
				end
			in
			for i = 0 to l2 - 1 do
				if b1.[i] <> b2.[i] then begin
					print_theout "\tWRITE_BYTE 0x%x %d // 0x%02x"
						i (Char.code b2.[i]) (Char.code b2.[i]);
					if Char.code b2.[i] > 0x20 && Char.code b2.[i] < 0x80 then
						print_theout " == %c" b2.[i];
					print_theout "\n";
				end
			done
	| _ -> ()) ;


	(match !dcmp_dest with
		Some(d) ->
				let buff,s = match !dcmp_src with
					| Some (x) -> (load_file x,x)
					| None ->
						let (base,ext) = split (Filename.basename d) in
						(fst (Load.load_resource "cmp-from" game true base ext),Filename.basename d)
				in
				let b,e = split s in
				let imp_base = Case_ins.filename_basename b in
				let s_dlg = Dlg.load_dlg imp_base buff in

				let b,e = split d in
				let buff, final_path = Load.load_resource "DLG compare command" game true b e in
				let imp_base = Case_ins.filename_basename b in
				let d_dlg = Dlg.load_dlg imp_base buff in

				let new_buffer = Buffer.create (1024 * 32) in
				Dlg.dlg_compare new_buffer s_dlg d_dlg game.Load.dialog game.Load.dialogf reprint_d_action ;
				print_theout "<<<<<<<< .../%s\n" s;
				output_buffer_theout new_buffer;
				print_theout ">>>>>>>>\nCOMPILE ~.../%s~\n" s;

	| _ -> ()) ;

	(match !textcmp_dest with
		Some(d) ->
			let src_buff,s = match !textcmp_src with
				| Some (x) -> (load_file x,x)
				| None ->
					let (base,ext) = split (Filename.basename d) in
					(fst (Load.load_resource "cmp-from" game true base ext),Filename.basename d)
			in
			let out_name = ".../" ^ d ^ ".patch" in
			let b,e = split d in
			let dest_buff, final_path = Load.load_resource "BCS patch command" game true b e in begin
				try begin
					let res = Diff.get_patch src_buff dest_buff 20 in
					print_theout "<<<<<<<< %s\n%s>>>>>>>>\n" out_name res;
					print_theout "// TP2 patch to turn %s into %s.	For example using:\n" s d;
				if !textcmp_src = None then
					print_theout "COPY_EXISTING ~%s~ ~override~\n" s
				else
					print_theout "COPY ~%s~ ~%s~\n" s d;
					print_theout "\tAPPLY_BCS_PATCH ~%s~\n" out_name
				end
				with e ->
					Printf.printf "Failed to create patch for [%s] to [%s] : %s\n" s d
						(Printexc.to_string e)
			end
	| _ -> ()) ;

	(match !bcmp_dest with
		Some(d) ->
			let decompile (buff,patch_filename) =
				let bcs = handle_script_buffer (patch_filename ^ ".BCS") buff in
        let out_buff = Buffer.create 40960 in
        Bcs.print_script_text game (Bcs.Save_BCS_Buffer(out_buff))
          (Bcs.BCS_Print_Script(bcs)) false None ;
        Buffer.contents out_buff
      in
			let src_buff,s = match !bcmp_src with
				| Some (x) -> (decompile (load_file x,x),x)
				| None ->
					let (base,ext) = split (Filename.basename d) in
					(decompile(Load.load_resource "cmp-from" game true base ext),Filename.basename d)
			in
			let out_name = ".../" ^ d ^ ".patch" in
			let b,e = split d in
			let dest_buff = decompile (Load.load_resource "BCS patch command" game true b e) in
			begin
				try begin
					let res = Diff.get_patch src_buff dest_buff 20 in
					print_theout "<<<<<<<< %s\n%s>>>>>>>>\n" out_name res;
					print_theout "// TP2 patch to turn %s into %s.	For example using:\n" s d;
				if !bcmp_src = None then
					if String.uppercase e = "BS" then
						print_theout "COPY ~scripts/%s~ ~scripts~\n" s
					else
						print_theout "COPY_EXISTING ~%s~ ~override~\n" s
				else
					print_theout "COPY ~%s~ ~%s~\n" s d;
					print_theout "\tDECOMPILE_BCS_TO_BAF\n";
					print_theout "\tAPPLY_BCS_PATCH ~%s~\n" out_name;
					print_theout "\tCOMPILE_BAF_TO_BCS\n";
				end
				with e ->
					Printf.printf "Failed to create patch for [%s] to [%s] : %s\n" s d
						(Printexc.to_string e)
			end
	| _ -> ()) ;

(* For debugging patch/diff: *)
	(match !bcmp_orig,!bcmp_patch with
		Some(s),Some(d) ->
			let b,e = split s in
			let orig_buff, final_path = Load.load_resource "BCS patch compare command" game true b e in
			let b,e = split d in
			let patch_buff, final_path = Load.load_resource "BCS patch compare command" game true b e in begin
				try begin
					let new_buff, bad_chunks, app_chunks = Diff.do_patch orig_buff patch_buff true in begin
						if ( bad_chunks > 0 ) then begin
							log_and_print "ERROR: Cannot apply patch %s (%d bad chunks).\n" d bad_chunks ;
							failwith "Cannot Apply Patch"
						end ;
						if ( app_chunks > 0 ) then begin
							log_and_print "WARNING: %d chunks in patch file %s already applied.\n" app_chunks d
						end ;
						if (bad_chunks == 0) then
							if (new_buff = orig_buff) then
								log_and_print "File %s unchanged by patch %s.\n" s d
							else
								let out_name = s ^ ".new" in
								let out = Case_ins.perv_open_out_bin out_name in begin
									log_and_print "Saving new file to %s\n" out_name ;
									output_string out new_buff ;
									close_out out
								end
					end
				end
				with e -> 
					Printf.printf "Failed to patch file [%s] with patch [%s] : %s\n" s d
						(Printexc.to_string e)
			end
	| _,_ -> ()) ; 

	(match !tlkcmp_src,!tlkcmp_dest with
		Some(s),Some(d) -> 
			let stlk = Tlk.load_tlk s in
			let dtlk = Tlk.load_tlk d in
			if Array.length stlk <> Array.length dtlk then begin
				log_and_print "WARNING: %s has %d entries, %s has %d entries\n"
					s (Array.length stlk) d (Array.length dtlk) 
			end ; 
			let my_min = match !user_min with
				Some(i) -> i
			| None -> 0 
			in
			let my_max = match !user_max with
				Some(i) -> i
			| None -> (min (Array.length stlk) (Array.length dtlk)) - 1
			in 
			print_theout "<<<<<<<< .../tlkcmp.tra\n" ;
			let stlk_cmp = Array.map (fun entry -> {entry with Tlk.sound_name = String.uppercase entry.Tlk.sound_name}) stlk in
			let dtlk_cmp = Array.map (fun entry -> {entry with Tlk.sound_name = String.uppercase entry.Tlk.sound_name}) dtlk in
			for i = my_min to my_max do
				if (stlk_cmp.(i).Tlk.text <> dtlk_cmp.(i).Tlk.text) ||
					 (stlk_cmp.(i).Tlk.sound_name <> dtlk_cmp.(i).Tlk.sound_name) then
					print_theout "@%d = ~%s~ [%s]\n"
						(1000000 + i) dtlk.(i).Tlk.text dtlk.(i).Tlk.sound_name
			done ;
			print_theout ">>>>>>>>\n\nLOAD_TRA ~.../tlkcmp.tra~\n";
			for i = my_min to my_max do
				if (stlk_cmp.(i).Tlk.text <> dtlk_cmp.(i).Tlk.text) ||
					 (stlk_cmp.(i).Tlk.sound_name <> dtlk_cmp.(i).Tlk.sound_name) then begin
					if !tlkcmp_strings then
						print_theout "\tSTRING_SET %s @%d\n"
							(Tlk.weidu_quotify stlk.(i).Tlk.text) (1000000 + i)
					else
						print_theout "\tSTRING_SET %d @%d\n" i (1000000 + i)
				end
			done ;
			flush_theout ();
	| _,_ -> ()
	) ;

	(match !tcmp_src,!tcmp_dest with
		Some(s),Some(d) ->
			let tracompare s d = 
				try
				let schan = Case_ins.perv_open_in s in
				let lexbuf = lex_init s schan in 
				let sresult = Stats.time "parsing .TRA files" (fun () -> Dparser.tra_file Dlexer.initial lexbuf) () in
				log_or_print "[%s] parsed (%d translation strings)\n" s (List.length sresult);
				close_in schan ; 
				pop_context (); 

				let dchan = Case_ins.perv_open_in d in
				let lexbuf = lex_init d dchan in
				let dresult = Stats.time "parsing .TRA files" (fun () -> Dparser.tra_file Dlexer.initial lexbuf) () in
				log_or_print "[%s] parsed (%d translation strings)\n" d (List.length dresult); 
				close_in dchan ; 
				pop_context ();

				(* int * dlg.string list *)
				let left_out = ref [] in 
				List.iter (fun (si,sv) ->
					let found = List.fold_left (fun acc (di,_) -> acc || si = di)
						false dresult in
					if not found then
						left_out := (si, sv) :: !left_out 
				) sresult ; 
				let left_out = List.sort compare !left_out in 
				if left_out = [] then begin
					print_theout "\n// All Strings in [%s] are also in [%s]\n" s d ;
				end else begin 
					print_theout
						"\n// Strings in [%s] that are not in [%s]:\n" s d ;
					List.iter (fun (i,v) ->
						print_theout "@%-7d = ~%s~\n" i (Dc.single_string_of_tlk_string_safe game v)
					) left_out ; 
					print_theout "\n\n"
				end 
				with e -> 
					begin 
					print_theout "\nThe ENTIRE FILE [%s] is missing:\n\t%s\n"
						d (Printexc.to_string e) ; 
					Printf.printf "Skipping [%s] and [%s] : %s\n" s d
						(Printexc.to_string e)
					end 
			in
			if (Case_ins.unix_stat s).Unix.st_kind <> Unix.S_REG then begin
				let s_d_h = Case_ins.unix_opendir s in
				try
					while true do
						let s' = Unix.readdir s_d_h in
						let one = (s ^ "/" ^ s') in 
						let two = (d ^ "/" ^ s') in 
						if (Case_ins.unix_stat one).Unix.st_kind = Unix.S_REG then
							tracompare one two 
					done 
				with e -> (Unix.closedir s_d_h ) 
			end else begin
				tracompare s d 
			end

	| _,_ -> ()) ; 

	(* Display Strings *)
	let display_string i = 
		let male = Tlk.pretty_print game.Load.dialog i in
		let female = Tlk.pretty_print_opt game.Load.dialogf i in
		if (female = "" || male = female) then
			Printf.printf "String #%d is %s\n" i male 
		else
			Printf.printf "String #%d is %s (MALE)\nString #%d is %s (FEMALE)\n" i male i female 
	in 
	if !ds_list <> [] && ( !user_min <> None || !user_max <> None) then begin
		let my_min = match !user_min with
			Some(i) -> i
		| None -> 0 
		in
		let my_max = match !user_max with
			Some(i) -> i
		| None -> (Array.length game.Load.dialog) - 1
		in 
		for i = my_min to my_max do
			display_string i 
		done 
	end else List.iter display_string !ds_list ;

	(* display strings that match *)
	if (!strfind_list <> []) then begin
		let reg_list = List.map Str.regexp_case_fold !strfind_list in 
		Array.iteri (fun i s ->
			let matches_one =
				List.fold_left (fun acc r -> acc ||
					try 
						let _ = Str.search_forward r s.Tlk.text 0 in
						true
					with _ -> false
				) false reg_list 
			in
			if matches_one then 
				Printf.printf "String #%d is %s\n" i (Tlk.pretty_print game.Load.dialog i)
		) game.Load.dialog 
	end ; 

	(* List all BIFFs *)
	if (!list_biff) then begin
		Key.list_biff game.Load.key output_theout 
	end ;

	(* List all files *)
	if (!list_files) then begin
		Key.list_key game.Load.key output_theout 
	end ;

	(* List BIFF contents *)
	if (!bc_list <> []) then begin
		Key.list_biff_contents game.Load.key output_theout !bc_list
	end ;

	(* List languages in a tp2 *)
	begin match !list_lang with
		| None -> ()
		| Some(x) ->
			let tp2 = handle_tp2_filename x in
			let i = ref 0 in
			List.iter (fun lang ->
				output_theout (Printf.sprintf "%d:%s\n%!" !i lang.Tp.lang_name);
				incr i
			) tp2.Tp.languages
	end;

	(* list components of a tp2 *)
	begin match !list_comp with
		| None -> ()
		| Some(x) ->
			let tp2 = handle_tp2_filename x in
			let lang = !list_comp_lang in
			let tp2_ht = Hashtbl.create 511 in
			let tra_ht = Hashtbl.create 511 in
			let fake_log = ref [] in
			for i = 0 to Tpstate.get_last_module_index tp2 + 1 do
				try
					ignore (Tpstate.get_nth_module tp2 i false);
					fake_log := (x,lang,i,None,Tp.Installed) :: !fake_log;
				with _ -> ()
			done;
			output_theout (Tpstate.sprintf_log game handle_tp2_filename handle_tra_filename get_tra_list_filename
			(List.rev !fake_log) tp2_ht tra_ht false false
			);
	end;

	(* Regex on BIFF contents *)
	if (!bs_type_list <> [] && !bs_str_list <> []) then begin
		Load.search_biff_contents game output_theout !bs_type_list !bs_str_list
	end else if (!bs_type_list <> [] && !biff_short_at <> 0) then begin
		let size = if !biff_short > 65535 then 4
							 else if !biff_short > 255 then 2
							 else 1
		in 
		Load.search_biff_contents_fun game output_theout !bs_type_list
			(fun buff ->
				(String.length buff) >= (!biff_short_at + size) &&
				let i = (match size with 
				| 2 -> short_of_str_off 
				| 1 -> byte_of_str_off
				| _ -> int_of_str_off
				) buff !biff_short_at in
				i = !biff_short)
	end else if (!bs_type_list <> [] || !bs_str_list <> []) then begin
		log_and_print "WARNING: Please specify both --biff-type EXT and (--biff-str STRING or --biff-short-at OFFSET)\n"
	end  ;

	(* Grab resources from BIFFs *) 
	if (!bg_list <> []) then begin
		let files_in_chitin = Key.list_of_key_resources game.Load.key false in

		let try_to_load str = begin
			try begin
				let base,ext = split (String.uppercase str) in
				let path = theout.dir ^ "/" ^ str in 
				let out = open_for_writing path true in
				if ext <> "IDS" && ext <> "2DA" then begin
					let fullpath : string = Load.copy_resource game base ext out in
					close_out out ;
					log_and_print "[%s] created from [%s]\n" path fullpath 
				end else begin
					let buff, fullpath = Load.load_resource "--biff-get" game false  base ext in
					output_string out buff ;
					close_out out ;
					log_and_print "[%s] created from [%s]\n" path fullpath 
				end
			end with e ->
				Printf.printf "[%s] --biff-get error: %s\n" str (Printexc.to_string e)
		end in 

		List.iter (fun str -> 
			try 
				let any_matches = ref false in 
				let regexp = Str.regexp_case_fold str in 
				List.iter (fun possible ->
					if Str.string_match regexp possible 0 then begin
						any_matches := true ;
						try_to_load possible 
					end 
				) files_in_chitin ;
				if not !any_matches then
					log_and_print "\nNo matches for: %s\n" str
			with e ->
				log_and_print "\nERROR: %s\n" (Printexc.to_string e)	
		) !bg_list
	end ;

	(match !make_biff with
	| None -> () 
	| Some(s) -> begin
		let file_list = ref [] in
		let s_d_h = Case_ins.unix_opendir s in
		(try
			while true do 
				let s' = Unix.readdir s_d_h in
				if ((Case_ins.unix_stat (s ^ "/" ^ s')).Unix.st_kind =
					 Unix.S_REG) then file_list := (s ^ "/" ^ s') :: !file_list 
			done
		with _ -> () ) ;
		if !file_list <> [] then begin
			game.Load.key_mod <- true ;
			let data = if game.Load.script_style = Load.PST then "" else "data/" in
			let filename = "data/" ^ s ^ ".bif" in
			let new_key = Biff.save_biff game.Load.key filename !file_list in
			let oc = open_for_writing "CHITIN.KEY" true in 
			Key.save_key new_key oc ;
			close_out oc ;
		end 
		end 
	) ; 

	(match !remove_biff with
	| None -> ()
	| Some(f) -> 
		game.Load.key_mod <- true ;
		let new_key = Key.remove_biff game.Load.key f in 
		let oc = open_for_writing "CHITIN.KEY" true in 
		Key.save_key new_key oc ;
		close_out oc 
	) ; 


	(* Append Strings *)
	let lse_strapp_list = List.map (fun s -> 
		Dlg.Local_String( { lse_male = s; lse_female = s;
		lse_male_sound = "" ; lse_female_sound = ""; })) !strapp_list in
	if (lse_strapp_list <> []) then begin
		let _ = List.map (Dc.resolve_tlk_string game) lse_strapp_list in
		()
	end ; 


	(* Handle DLG files *)
	let loaded_dlgs = List.map (fun (b,e) -> 
		try 
			let buff, final_path = Load.load_resource "DLG decompile command" game true b e in
			let imp_base = Case_ins.filename_basename b in
			let dlg = 
				try Dlg.load_dlg imp_base buff 
				with e -> log_and_print "ERROR: problem loading [%s]: %s\n" b
				(Printexc.to_string e) ; raise e
			in 
			let out_name = 
				if theout.file = "" then 
					theout.dir ^ "/" ^ imp_base ^ ".d" 
				else
					theout.dir ^ "/" ^ theout.file
			in 
			let transout_name = (Case_ins.filename_chop_extension out_name ^ ".tra" ) in 
			(dlg,out_name,transout_name,b,e,final_path)
		with e -> log_and_print "ERROR: problem handling [%s]: %s\n" b
				(Printexc.to_string e) ; raise e
		) !dlg_list 
	in
	for i = 1 to if !transitive || !two_pass then 2 else 1 do 
	List.iter (fun (dlg,out_name,transout_name,b,e,final_path) -> 
		try 
			let out_chan = 
				if theout.append then Lazy.force theout.chan
				else open_for_writing out_name false 
			in
			let out_trans_chan = match !use_trans with
				true -> 
					log_and_print "[%s] created as translation file\n" transout_name ;
					Some(open_for_writing transout_name false) 
			| false -> None
			in 
			if (!d_headers) && !Dlg.comments then begin
				Printf.fprintf out_chan "// creator  : %s (version %s)\n" Sys.argv.(0) version; 
				Printf.fprintf out_chan "// argument : %s.%s\n" b e ;
				Printf.fprintf out_chan "// game		 : %s\n" game.Load.game_path;
				Printf.fprintf out_chan "// source	 : %s\n" final_path ;
				Printf.fprintf out_chan "// dialog	 : %s\n" game.Load.dialog_path ;
				Printf.fprintf out_chan "// dialogF  : %s\n\n" game.Load.dialogf_path ;
			end ; 
			let new_buff = Buffer.create (1024 * 32) in 
			Dlg.emit_d dlg out_name game.Load.dialog game.Load.dialogf new_buff out_trans_chan None reprint_d_action !transitive !d_toplevel;
			Buffer.output_buffer out_chan new_buff ;
			if not theout.append then close_out out_chan ; 
		with e -> 
			log_and_print "ERROR: problem creating [%s] from [%s]: %s\n" out_name
				b (Printexc.to_string e) ;
			raise e
	) loaded_dlgs ;
	done ;

	(* Handle TRA files *)
	Dc.ok_to_resolve_strings_while_loading := Some(game) ;
	List.iter handle_tra_filename !trans_list ;

	if !test_trans then begin
		Dc.test_trans output_theout game
	end ;

	(match !trbify with
	| Some(filename) -> begin
			let result = parse_file true (File filename) "parsing .tra files"
				(Dparser.tra_file Dlexer.initial) in
			log_or_print "[%s] has %d translation strings\n" filename
				(List.length result);
			let base = handle_out_boringness filename ["tra"; "trb"] in
			if !debug_ocaml then log_and_print "I'm trying to save to %s.trb\n\n" base ;
			let out = Case_ins.perv_open_out_bin (base ^ ".trb") in
			Marshal.to_channel out result [] ;
			close_out out
		end
	| _ -> ()
	) ;

	( match (!untraify_d,!untraify_tra) with
		| (Some(d),Some(tra)) ->
			let result = parse_file true (File tra) "parsing .tra files" (Dparser.tra_file Dlexer.initial) in
				log_or_print "[%s] has %d translation strings\n" tra
					(List.length result);
			let base,ext = split d in
			let base = handle_out_boringness base [ext] in
			if !debug_ocaml then log_and_print "I'm trying to save to %s.%s\n\n" base ext ;
			let filebuff = load_file d in
			let filebuff = List.fold_left (fun acc (i,ls) ->
				match ls with
				| Dlg.Local_String(lse) ->
						let buff =
							(if lse.lse_male <> "" then begin
								"~" ^ lse.lse_male ^ "~" ^ (
								if lse.lse_male_sound <> "" then begin
									 " [ " ^ lse.lse_male_sound ^ " ] ";
								end else "" )
							end else "") ^
							(if (lse.lse_female <> "") &&
								 (lse.lse_female <> lse.lse_male) then begin
									" ~" ^ lse.lse_female ^ "~ " ^
								(if lse.lse_female_sound <> "" then begin
									" [ " ^ lse.lse_female_sound ^ " ] ";
								end else "")
							end else "" )
						in
						Str.global_replace (Str.regexp (Printf.sprintf "@%d\\([^0-9]\\)" i)) (Printf.sprintf "%s\\1" buff) acc;
				| _ -> failwith "traify1"
			) filebuff result
			in
		let out = open_for_writing (base ^ "." ^ ext) true in
		output_string out filebuff;
		close_out out;
	| _ -> ()
	) ;

	(match !traify with
		Some(file) -> begin
			try
				let name,ext = split (String.uppercase file) in

				let buf = ref (load_file file) in

				let base = handle_out_boringness file [ "d"; "tra"; "tp2"; "baf"; "tpa"; "tpp"; "tph"] in
				if !debug_ocaml then log_and_print "I'm trying to save to %s.%s and %s.tra\n\n" base (String.lowercase ext) base ;
				let transout_name = base ^ ".tra" in
				let dout_name = base ^ "." ^ ext in 

				let dout = open_for_writing dout_name true in

				let traout = open_for_writing transout_name true in 

				let counter = traify_num in

				(* replace the given string with the tra-value of the counter *) 
				let replace str comment =
					let my_regexp = Str.regexp (Str.quote str) in
					let replace_with = (Printf.sprintf "@%d" !counter) ^ comment in
					buf := Str.global_replace my_regexp replace_with !buf ;
				in
				let remove str =
					let my_regexp = Str.regexp (Str.quote str) in
					buf := Str.global_replace my_regexp "" !buf ;
				in
				let fix_suckage comment =
					let my_regexp = Str.regexp (Str.quote (
									Printf.sprintf " /* @%d%s */" !counter comment)) in
					let replace_with = Printf.sprintf "%s" comment in
					buf := Str.global_replace my_regexp replace_with !buf ;
				in
				let add_refer comment =
					let my_regexp = Str.regexp (Printf.sprintf "@%d\\([^0-9]\\)" !counter) in
					let replace_with = Printf.sprintf "@%d%s\\1" !counter comment in
					buf := Str.global_replace my_regexp replace_with !buf;
					let my_regexp = Str.regexp (Str.quote (comment ^ comment)) in
					let replace_with = comment in
					buf := Str.global_replace my_regexp replace_with !buf;
				in

				begin match !traify_old_tra with
					| None -> ()
					| Some(x) ->
						let max = ref !counter in
						let inchan = Case_ins.perv_open_in_bin x in
						let lexbuf = lex_init file inchan in
						let tra_file = Stats.time "parsing.TRA files" (fun () -> Dparser.tra_file
							Dlexer.initial lexbuf) () in
						List.iter (fun (reference,ls) ->
							counter := reference;
							if !max <= reference then max := reference + 1;
							match ls with
							| Dlg.Local_String(lse) -> 
									if lse.lse_male <> "" (* ||
											lse.lse_male_sound <> "" *) then begin
										let comment_string = (if !traify_comment then
											(Printf.sprintf " /* " ^ lse.lse_male ^ " */") else "")
										in
										Printf.fprintf traout "@%-4d = ~%s~" !counter lse.lse_male ;
										replace ("~" ^ lse.lse_male ^ "~" ) comment_string;
										replace ("%" ^ lse.lse_male ^ "%" ) comment_string;
										replace ("\"" ^ lse.lse_male ^ "\"" ) comment_string;
										fix_suckage comment_string;
										if !traify_comment then add_refer comment_string;
										if lse.lse_male_sound <> "" then begin
											Printf.fprintf traout " [%s]" lse.lse_male_sound ;
											remove ("[" ^ lse.lse_male_sound ^ "]")
										end ;
									end ;
									if (lse.lse_female <> "" (* || lse.lse_male_sound <> ""*)) && 
											(lse.lse_female <> lse.lse_male (* || 
											lse.lse_female_sound <> lse.lse_male_sound *)) then begin
										Printf.fprintf traout " ~%s~" lse.lse_female ; 
										remove ("~" ^ lse.lse_female ^ "~" ); 
										remove ("%" ^ lse.lse_female ^ "%" ); 
										remove ("\"" ^ lse.lse_female ^ "\"" ); 
										if lse.lse_female_sound <> "" then begin
											Printf.fprintf traout " [%s]" lse.lse_female_sound ; 
											remove ("[" ^ lse.lse_female_sound ^ "]")
										end 
									end ; 
									Printf.fprintf traout "\n" ; 
									() 
							| _ -> failwith "traify1" 
						) tra_file;
						counter := !max;
						close_in inchan
				end ;

				Dlg.local_string_ht := Some([]) ;
				let old_ok = !Dc.ok_to_resolve_strings_while_loading in
				Dc.ok_to_resolve_strings_while_loading := None ;
				Dc.doing_traify := true ;

				Dlg.local_string_ht := Some([]) ;
				begin
					match ext with
						"D" -> ignore (
									let lexbuf = lex_init_from_string file !buf in
									ignore (Stats.time "parsing .D files"
									(fun () -> Dparser.d_file Dlexer.initial lexbuf) ()));
									pop_context ();
					| "TP2" -> ignore
									 (Tparser.parse_tp2_file (String (file,!buf)))
					| "TPA"
					| "TPH" -> ignore
									 (Tparser.parse_tpa_file (String (file,!buf)))
					| "TPP" -> ignore
									 (Tparser.parse_tpp_file (String (file,!buf)))
					| "BAF" ->
						(if (!Dlg.local_string_ht = None) then
							Dlg.local_string_ht := Some([]) ) ;
						 ignore (
									let lexbuf = lex_init_from_string file !buf in
									ignore (Stats.time "parsing .D files"
									(fun () -> Bafparser.baf_file Baflexer.initial lexbuf) ()));
									pop_context ();
					| _ -> log_and_print "ERROR: don't know how to --traify files with extension [%s]\n" ext ; failwith ext
				end ;
				log_or_print "[%s] parsed for --traify\n" file ;

				Dc.ok_to_resolve_strings_while_loading := old_ok ;
				Dc.doing_traify := false;

				(match !Dlg.local_string_ht with
					Some(lst) ->
				List.iter (fun	ls	->
					match ls with
					| Dlg.Local_String(lse) ->
							if lse.lse_male <> "" (* ||
								 lse.lse_male_sound <> "" *) then begin
								let comment_string = (if !traify_comment then
									(Printf.sprintf " /* " ^ lse.lse_male ^ " */") else "")
								in
								Printf.fprintf traout "@%-4d = ~%s~" !counter lse.lse_male ;
								replace ("~" ^ lse.lse_male ^ "~" ) comment_string;
								replace ("%" ^ lse.lse_male ^ "%" ) comment_string;
								replace ("\"" ^ lse.lse_male ^ "\"" ) comment_string;
								if lse.lse_male_sound <> "" then begin
									Printf.fprintf traout " [%s]" lse.lse_male_sound ;
									remove ("[" ^ lse.lse_male_sound ^ "]")
								end ;
							end ;
							if (lse.lse_female <> "" (* || lse.lse_male_sound <> ""*)) && 
								 (lse.lse_female <> lse.lse_male (* || 
									lse.lse_female_sound <> lse.lse_male_sound *)) then begin
								Printf.fprintf traout " ~%s~" lse.lse_female ; 
								remove ("~" ^ lse.lse_female ^ "~" ); 
								remove ("%" ^ lse.lse_female ^ "%" ); 
								remove ("\"" ^ lse.lse_female ^ "\"" ); 
								if lse.lse_female_sound <> "" then begin
									Printf.fprintf traout " [%s]" lse.lse_female_sound ; 
									remove ("[" ^ lse.lse_female_sound ^ "]")
								end 
							end ; 
							Printf.fprintf traout "\n" ; 
							incr counter ;
							() 
					| _ -> failwith "traify1" 
				) (List.rev lst); 
				| None -> failwith "traify2" 
				); 

				Dlg.local_string_ht := None ;

				Printf.fprintf dout "%s" !buf ; 

				close_out traout ;
				close_out dout ; 
				() 

			with e -> 
				log_and_print "ERROR: problem tra-ifying file [%s]: %s\n" file
					(Printexc.to_string e) ; 
				raise e
			end
	| _ -> () 
	) ;



	List.iter (fun str ->
		try
			let script = handle_baf_filename str in
			let name,ext = split (Case_ins.filename_basename str) in
			let out = Case_ins.perv_open_out_bin (theout.dir ^ "/" ^ name ^ ".bcs") in
			Bcs.save_bcs game (Bcs.Save_BCS_OC(out)) script ;
			close_out out
		with e -> log_and_print "ERROR: problem loading [%s]: %s\n" str
			(Printexc.to_string e) ; raise e
	) !baf_list ;


	(* Handle D files *)
	List.iter (handle_d_filename ) !d_list ;
	Dc.ok_to_resolve_strings_while_loading := None;

	(* Emit DLG files *) 
	emit_dlg_files game theout.dir ;

	(* Check that we can write to the given TLK file *) 
	(match !output_dialog with
		Some(path) when file_exists path -> begin
				try Unix.access path [Unix.W_OK] ;
						log_or_print "[%s] claims to be writeable.\n" path ; 
						if (Case_ins.unix_stat path).Unix.st_kind <> Unix.S_REG then
							failwith (path ^ " is a not a regular file") ;
						log_or_print "[%s] claims to be a regular file.\n" path ; 
						()
				with e -> 
					log_and_print "\nERROR: The file [%s] cannot be written to.
Perhaps it is in use by another process (close ShadowKeeper, all Infinity
Engine games and editors, etc.). It may also be naturally read-only: use
Windows Explorer and right-click on the file to pull up its properties.
Make sure that the \"read-only\" box is NOT checked. Please fix this
problem and try again.\n" path ;
					pause_at_end := true ;
					raise e
			end
		| _ -> ()
	) ;

	if !tp_list <> [] then begin
		pause_at_end := true ; 
		if !Tp.always_uninstall then pause_at_end := false ;
		if !Tp.always_yes then pause_at_end := false ;
		if !Tp.sometimes_reinstall then pause_at_end := false ;
		Tp.force_install_these	 := !force_install_these_main 	;
		Tp.force_uninstall_these := !force_uninstall_these_main ;
		if !Tp.force_install_these <> [] then pause_at_end := false ;
		if !Tp.force_uninstall_these <> [] then pause_at_end := false ;
		load_log () ;
		List.iter (fun tp_file -> Queue.add tp_file tp2_queues) !tp_list ;
		while not (Queue.is_empty tp2_queues) do
			let tp_file = Queue.take tp2_queues in
			try
			if file_exists tp_file then begin
				let result = handle_tp2_filename tp_file in
				Tpwork.handle_tp game tp_file result;
			end
			with e ->
				log_and_print "ERROR: problem parsing TP file [%s]: %s\n" tp_file
					(Printexc.to_string e) ;
				raise e
			done
	end ;

	if !process_script <> "" then begin
		pause_at_end := false ;
		load_log () ;
		let buff = try load_file !process_script with _ -> "" in
		let lines = Str.split many_newline_or_cr_regexp buff in
		List.iter (fun x -> Queue.add x tp2_queues) lines;
		while not (Queue.is_empty tp2_queues) do
			let this = Queue.take tp2_queues in
			let parts = Array.of_list (Str.split many_whitespace_regexp this) in
			let tp_file = parts.(0) in
			let action = ref "" in
			Tp.always_yes := false;
			Tp.always_uninstall := false;
			Tp.sometimes_reinstall := false;
			Tp.forced_language := int_of_string parts.(1);
			Tp.force_install_these := [];
			Tp.force_uninstall_these := [];
			Tp.specified_specific_components := true;
			let toproc = ref [] in
			Array.iteri (fun i s -> if i > 1 then begin
				match String.uppercase s with
					| "U"
					| "I" -> action := String.uppercase s
					| _ -> begin
							let num = int_of_string s in
							match !action with
							| "U"
							| "I" -> toproc := (num, !action) :: !toproc ;
							| _ -> failwith "wrong installation script"
						end
			end) parts ;
			List.iter (fun (a,b) -> log_and_print "%s %d\n" b a) !toproc;
			try
			if file_exists tp_file then begin
				let result = handle_tp2_filename tp_file in
				List.iter (fun (a,b) ->
					begin
						match b with
						| "U" ->
							Tp.force_install_these := [];
							Tp.force_uninstall_these := [a];
						| "I" ->
							Tp.force_uninstall_these := [];
							Tp.force_install_these := [a];
						| _ -> ()
					end ;
					Tpwork.handle_tp game tp_file result
					) (List.rev !toproc);
					List.iter (fun (s,e) ->
						log_or_print "Executing: [%s]\n" s ;
						ignore (exec_command s e))
					!execute_at_exit;
					execute_at_exit := [];
			end
			with e ->
				log_and_print "ERROR: problem parsing TP file [%s]: %s\n" tp_file
					(Printexc.to_string e) ;
				raise e
			done
	end ;

	if !change_log <> [] then begin
		load_log();
		let backup_lists = Hashtbl.create 1000 in
		let tp2s = Hashtbl.create 100 in
		let tras = Hashtbl.create 100 in
		let parse_tp2 tpfile =
			try
				Hashtbl.find tp2s tpfile
			with _ ->
(* 				Hashtbl.clear tp2s; *)
				let x = handle_tp2_filename tpfile in
				Hashtbl.add tp2s tpfile x;
				x
		in
		let get_file name =
			try
				Hashtbl.find backup_lists name
			with _ -> []
		in
		let add_tras s =
			let x =
				try Hashtbl.find tras s
				with _ ->
(* 					( if Hashtbl.length tras > 10 then Hashtbl.clear tras; *)
						let x = get_tra_list_filename (Arch.backslash_to_slash s) in
						Hashtbl.add tras s x; x
			in
			Stats.time "adding translation strings" Dc.add_trans_strings x
		in
		let change_log = List.map String.uppercase !change_log in
		List.iter (fun cur_mod ->
			let (tpfile,lang,comp,comp_name,status) = cur_mod in
			let tp2 = parse_tp2 tpfile in
			let backup_dir = tp2.Tp.backup in
			let infile = Case_ins.perv_open_in (Printf.sprintf "%s/%d/MAPPINGS.%d" backup_dir comp comp) in
			begin
				try
					while true do
						let line = input_line infile in
						let parts = Str.split many_whitespace_regexp line in
						let (a,b) = match parts with
							| a :: b :: _ -> (a,b)
							| a :: [] -> (a,"")
							| [] -> failwith "Empty line in a MAPPINGS file."
						in
						let (a,b) = (String.uppercase a, String.uppercase b) in
						let a_stem = Str.global_replace (Str.regexp "^OVERRIDE[/\\]") "" a in
						if (List.mem a_stem change_log) then begin
							log_and_print "%s\n" line;
							let m = Tpstate.get_nth_module tp2 comp true in
							Dc.push_trans();
								(try
									let l = List.nth tp2.Tp.languages lang in
									List.iter add_tras l.Tp.lang_tra_files ;
									with _ -> ()
								) ;
							let comp_str = Dc.single_string_of_tlk_string_safe game m.Tp.mod_name in
							Dc.pop_trans();
							Hashtbl.replace backup_lists a ((tpfile, lang, comp, comp_str, b) :: get_file a);
						end
					done
				with End_of_file -> close_in infile
			end;
		) !Tp.the_log;
		List.iter (fun file1 ->
			let file1 = String.uppercase file1 in
			let file = if Filename.check_suffix file1 ".EXE" || Filename.check_suffix file1 ".KEY" then file1 else "OVERRIDE/" ^ file1 in
			let file_log = List.rev (get_file file) in
			let (base,ext) = split file1 in
			let i = ref 0 in
			print_theout "\n\n\nMods affecting %s:\n" file1;
			List.iter (fun (tpfile,lang,comp,comp_str,backup) ->
				let out = Printf.sprintf "%s/%s.%05d.%s" theout.dir base !i ext in
				if file_exists backup then copy_large_file backup out "--change-log";
				print_theout "%05d: %s~%s~ %d %d // %s\n" !i
				(if backup = "" then "/* from game biffs */ "  else "") tpfile lang comp comp_str;
				incr i;
			) file_log
		) change_log;
	end;

	List.iter (fun str ->
		let name,ext = split (String.uppercase str) in
		let buff,path = Load.load_resource "list effects command" game true name ext in
		print_theout "[%s] has effects:\n" str ;
		let eff_arr = match ext with
		| "EFF" -> Load.eff_of_eff buff
		| _ -> Load.eff_of_spl_itm buff 
		in
		Array.iter (fun eff ->
			let op = eff.Load.opcode in 
			let eff_name = Eff_table.name_of_opcode op in
			if op = 139 then begin (* display string *)
				print_theout "\t%s %s #%d\n" eff_name 
					(Tlk.pretty_print game.Load.dialog eff.Load.arg1)
					eff.Load.arg1
			end else if op = 101 then begin
				print_theout "\t%s (%s)\n" eff_name 
					(Eff_table.name_of_opcode eff.Load.arg2)
			end else begin
				print_theout "\t%s\n" eff_name 
			end
		) eff_arr
	) !list_eff_list ;

	(* Handle BCS files *)
	List.iter (fun str -> 
		let b,e = split str in 
		try 
			let buff, _ = 
				if file_exists str then (load_file str),"" else 
				Load.load_resource "decompile BCS command" game true b (String.uppercase e) 
			in
			let script =	handle_script_buffer str buff in
			let base = Case_ins.filename_basename b in 
			let out_name = theout.dir ^ "/" ^ base ^ ".baf" in 
			let out = Case_ins.perv_open_out out_name in
			(try 
				Bcs.print_script_text game (Bcs.Save_BCS_OC(out))
					(Bcs.BCS_Print_Script(script)) (!Dlg.comments) None ;
				close_out out
			with e -> 
				log_and_print "ERROR: problem printing script [%s]: %s\n" b 
					(Printexc.to_string e) ; close_out out 
			)
		with e -> log_and_print "ERROR: problem handling [%s]: %s\n" b
				(Printexc.to_string e) 
		) !bcs_list ;

	(match !backup_list_chn with
		Some(c) -> close_out c ; backup_list_chn := None 
	| None -> () ) ;
	backup_dir := None ; 

	(* make sure we add all those strings! *)
	if not (Queue.is_empty !Dc.strings_to_add) then begin
		if (!output_dialog = None && !output_dialogf = None) then begin
			log_or_print "You did not specify '--tlkout dialog.tlk', so %d strings were not saved.\n" (Queue.length !Dc.strings_to_add);
		end else begin 
			let dc_lse_strapp_list = !Dc.strings_to_add in 
			Load.append_strings game dc_lse_strapp_list
		end
	end ;

	if not (game.Load.str_sets = []) then begin
		log_or_print "WARNING: %d SET_STRINGs were executed but no uninstall information was created.\n" (List.length game.Load.str_sets)
	end ;

	List.iter (fun str ->
		let name,ext = split (String.uppercase str) in
		let tlk = Tlk.load_tlk str in
		let max =
			if Array.length tlk > Array.length game.Load.dialog then
				Array.length game.Load.dialog
			else
				Array.length tlk
		in
		for i = 0 to max - 1 do
			game.Load.dialog.(i) <- tlk.(i)
		done ;
		game.Load.dialog_mod <- true
	) !tlk_merge ;

	(* Emit DIALOG.TLK *)
	(match !output_dialog, game.Load.dialog_mod with
		Some(path), true ->
			let outchan = open_for_writing path true in
			Tlk.save_tlk path game.Load.dialog outchan
	| _, _ -> ()) ;

	(* Emit DIALOGF.TLK *)
	(match !output_dialogf, game.Load.dialogf, game.Load.dialogf_mod with
		Some(path),Some(t),true ->
			let outchan = open_for_writing path true in
			Tlk.save_tlk path t outchan
	| _, _, _ -> () ) ;

	Hashtbl.iter (fun a x -> Unix.close x.Biff.fd) game.Load.loaded_biffs;
	Queue.iter my_unlink Load.cbifs_to_rem;

	List.iter (fun (a,b) -> log_and_print "\n%s %s\n" a b) (List.rev !Tpstate.strings_to_print_at_exit) ;
	()
;;


(try
	Stats.time "stuff not covered elsewhere" main ();
with e ->
	log_and_print "\nFATAL ERROR: %s\n" (Printexc.to_string e) )

;;

(match !Util.log_channel with
	Some(o) -> Stats.print o "\n\t\tWeiDU Timings\n" ; flush o
| None -> () )

;;

List.iter (fun (s,e) ->
	log_or_print "Executing: [%s]\n" s ;
	ignore (exec_command s e))
		!execute_at_exit

;;

(match !Util.log_channel with
	Some(o) -> close_out o
| None -> () )

;;

Util.log_channel := None
;;

if !pause_at_end || (!return_value <> return_value_success && Var.get_string "%WEIDU_OS%" =
		"x86_WIN32") then begin
	Printf.printf "\nPress ENTER to exit.\n" ;
	try ignore (read_line () ) with _ -> ()
end

;;
exit !return_value
;;
