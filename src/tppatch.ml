open Util
open Tp
open Parsewrappers
open Tpstate
open Tphelp
open Tppe

(************************************************************************
 * change location "where" in "buff" to point to str-ref "what"
 ************************************************************************)
let rec process_patch1 patch_filename game buff p =
  Stats.time "READ_*" (fun () ->
  let bounds_check idx size retfun eo ef =
    let len = String.length buff in
    let out_of_bounds = (idx < 0 || (idx + size) > len) in
    match out_of_bounds, eo with
    | false, _ -> (retfun ())
    | true, Some(v) -> ef v
    | true, None ->
    begin
      log_and_print "ERROR: illegal %d-byte read from offset %d of %d-byte file %s\n" size idx len patch_filename ;
      failwith (patch_filename ^ ": read out of bounds")
    end
  in
  match p with
  | TP_PatchReadByte(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 1
        (fun () -> Int32.of_int (byte_of_str_off buff where)) eo
        (fun v -> (eval_pe buff game v)) in
      Var.set_int32 name value
  | TP_PatchReadSByte(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 1
        (fun () -> Int32.of_int (signed_byte_of (byte_of_str_off buff where))) eo
        (fun v -> (eval_pe buff game v)) in
      Var.set_int32 name value
  | TP_PatchReadShort(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 2
        (fun () -> Int32.of_int (short_of_str_off buff where)) eo
        (fun v -> (eval_pe buff game v)) in
      Var.set_int32 name value
  | TP_PatchReadSShort(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 2
        (fun () -> Int32.of_int (signed_short_of (short_of_str_off buff where))) eo
        (fun v -> eval_pe buff game v) in
      Var.set_int32 name value
  | TP_PatchReadLong(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 4
        (fun () -> int32_of_str_off buff where) eo
        (fun v -> (eval_pe buff game v)) in
      Var.set_int32 name value
  | TP_PatchReadSLong(where,name,eo) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where 4
        (fun () -> int32_of_str_off buff where) eo
        (fun v -> (eval_pe buff game v)) in
      Var.set_int32 name value
  | TP_PatchGetStrRef(which,name,female,sound) ->
      let which = Int32.to_int (eval_pe buff game which) in
      let name = Var.get_string (eval_pe_str name) in
      let value = Dc.pretty_print_no_quote
				(
					if female then begin
						match game.Load.dialogf with
							| None -> game.Load.dialog
							| Some x -> x
					end else game.Load.dialog
				)
				which female sound in
      Var.set_string name value
  | TP_PatchReadStrRef(where,name,eo,female,sound) ->
  		let w = Int32.to_int (eval_pe buff game where) in
      ignore (bounds_check w 4
        (fun () ->
        	process_patch1 patch_filename game buff
						(TP_PatchGetStrRef(TP_PE_Long_At where,name,female,sound))
				)
				eo
        (
					fun v -> let v = Var.get_string v in
					let name = Var.get_string (eval_pe_str name) in
      		Var.set_string name v
				)
			)

  | TP_PatchReadAscii(where,name,eo,size,null_terminated) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let size = Int32.to_int (eval_pe buff game size) in
      let name = Var.get_string (eval_pe_str name) in
      let value = bounds_check where size
        (fun () ->
          if null_terminated then get_string_of_size buff where size
          else String.sub buff where size) eo
        (fun v ->  Var.get_string v)
      in
      Var.set_string name value

  | TP_Get2DARows(req_col, var_name) ->
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let var_name = Var.get_string (eval_pe_str var_name) in
      let lines = Str.split many_newline_or_cr_regexp buff in
      let entries = List.map (fun line ->
        Str.split many_whitespace_regexp line
      ) lines in
      let num_rows = List.fold_left (fun acc elt ->
        acc + if List.length elt >= req_col then 1 else 0
      ) 0 entries in
      Var.set_int32 var_name (Int32.of_int num_rows)

  | TP_Get2DACols(var_name) ->
      let lines = Str.split many_newline_or_cr_regexp buff in
      let var_name = Var.get_string (eval_pe_str var_name) in
      let rec num_cols lines current =
        match lines with
        | [] -> current
        | line :: rest -> num_cols rest
        	(max (List.length (Str.split many_whitespace_regexp line)) current)
      in
      Var.set_int32 var_name (Int32.of_int (num_cols lines 0))

  | TP_CountRegexpInstances(case_sens,match_exact,find, var_name) ->
      let find = Var.get_string find in
      let var_name = Var.get_string (eval_pe_str var_name) in
      let case_sens = match case_sens with
          None -> false
        | Some(x) -> x
      in
      let match_exact = match match_exact with
          None -> false
        | Some(x) -> x
      in
      let my_regexp = match case_sens, match_exact with
          false, false -> Str.regexp_case_fold        find
        | true , false -> Str.regexp                  find
        | false, true  -> Str.regexp_string_case_fold find
        | true , true  -> Str.regexp_string           find
      in
      let split = Str.split_delim my_regexp buff in
      let length = List.length split in
      Var.set_int32 var_name (Int32.of_int (length - 1))

  | TP_Read2DA(row, col, req_col, var_name) ->
      let var_name = Var.get_string (eval_pe_str var_name) in
      let row = Int32.to_int (eval_pe buff game row) in
      let col = Int32.to_int (eval_pe buff game col) in
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let lines = Str.split many_newline_or_cr_regexp buff in
      let entries = List.map (fun line ->
        let entry_list = Str.split many_whitespace_regexp line in
        Array.of_list entry_list) lines in
      let rec process line_list lines_left =
        match line_list, lines_left with
          [], 0 -> ()
        | [], _ ->
          begin
            log_and_print "ERROR: Cannot find %d rows with at least %d columns." row req_col ;
            failwith "Cannot Read 2DA Entry"
          end
        | hd :: tl, i  when Array.length hd < req_col -> process tl i
        | hd :: tl, 0 -> Var.set_string var_name hd.(col)
        | hd :: tl, i -> process tl (i - 1)
      in
      process entries row ;

  | _ -> ()
  ) ()

(*************************************************************************
                           2da helper functions
 *************************************************************************)
let split_apart str = Str.split many_whitespace_regexp	str

(*************************************************************************
                           process_patch2
 *************************************************************************)
let rec process_patch2_real process_action tp patch_filename game buff p =
	let process_patch2 = process_patch2_real process_action tp in
  process_patch1 patch_filename game buff p ;

  Stats.time "process_patch2" (fun () ->
  let bounds_check_write idx size (str : string) =
    let len = String.length buff in
    let out_of_bounds = (idx < 0 || (idx + size) > len) in
    if out_of_bounds then begin
      log_and_print "ERROR: illegal %d-byte write (%s) offset %d of %d-byte file %s\n" size str idx len patch_filename ;
      failwith (patch_filename ^ ": write out of bounds")
    end ;
    ()
  in
  match p with
  | TP_PatchReadByte(_)
  | TP_PatchReadSByte(_) 
  | TP_PatchReadShort(_) 
  | TP_PatchReadSShort(_)
  | TP_PatchReadLong(_)
  | TP_PatchReadSLong(_)
  | TP_PatchGetStrRef(_)
  | TP_PatchReadStrRef(_)
  | TP_PatchReadAscii(_) 
  | TP_Get2DARows(_) 
  | TP_Get2DACols(_) 
  | TP_CountRegexpInstances(_) 
  | TP_Read2DA(_) -> buff
	| TP_PatchStrRef(where,what) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let new_index = match Dc.resolve_tlk_string game what with
        Dlg.TLK_Index(i) -> i
      | _ -> log_and_print "ERROR: cannot resolve SAY patch\n" ; failwith "resolve"
      in
      bounds_check_write where 4 (Dc.single_string_of_tlk_string game what) ;
      let new_string = str_of_int new_index in
      String.blit new_string 0 buff where 4 ;
      buff

  | TP_DescribeItem(dest) ->
      let str = Eff_table.describe_itm buff in
      Var.set_string dest str ;
      buff

  | TP_PatchStrRefEvaluated(where,what) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let str = Var.get_string what in
      let lse = {
        lse_male = str ;
        lse_male_sound = "";
        lse_female = str ;
        lse_female_sound = "";
      } in 
      let new_index =
        match Dc.resolve_tlk_string game (Dlg.Local_String(lse)) with
        Dlg.TLK_Index(i) -> i
      | _ -> log_and_print "ERROR: cannot resolve SAY_EVALUATED patch\n" ;
             failwith "SAY_EVALUATED resolve"
      in
      let new_string = str_of_int new_index in
      bounds_check_write where 4 what ;
      String.blit new_string 0 buff where 4 ;
      buff

  | TP_PatchReplaceBCSBlock(old_file, new_file) -> begin
      let old_file = Arch.backslash_to_slash old_file in
      let new_file = Arch.backslash_to_slash new_file in
      let bcs_buff_of_baf_or_bcs file =
        let a,b = split (String.uppercase file) in
        if b = "BAF" then begin
          try
            let script = parse_file true (File file) "parsing .baf files"
              (Bafparser.baf_file Baflexer.initial) in
            let buff = Buffer.create 1024 in
            Bcs.save_bcs game (Bcs.Save_BCS_Buffer(buff)) script ;
            Buffer.contents buff
          with e ->
            log_and_print "ERROR: error compiling [%s]: %s\n"
              file (Printexc.to_string e) ;
            raise e
        end else begin
          load_file file
        end
      in
      let old_file_buff = bcs_buff_of_baf_or_bcs old_file in
      let string_to_find = body_of_script old_file_buff in
      let new_file_buff = bcs_buff_of_baf_or_bcs new_file in
      let string_to_sub_in = body_of_script new_file_buff in
      let my_regexp = Str.regexp_string string_to_find in
      try
        let _ = Str.search_forward my_regexp buff 0 in
        Str.global_replace my_regexp string_to_sub_in buff
      with Not_found ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
        log_and_print "WARNING: cannot find block matching [%s]\n"
          old_file ;
        buff
      end 

  | TP_PatchReplaceBCSBlockRE(old_file, new_file) -> begin
      let old_file = Arch.backslash_to_slash old_file in
      let new_file = Arch.backslash_to_slash new_file in
      let string_to_find = load_file old_file in
      let string_to_sub_in = load_file new_file in
      let my_regexp = Str.regexp string_to_find in
      try 
        let _ = Str.search_forward my_regexp buff 0 in
        Str.global_replace my_regexp string_to_sub_in buff 
      with Not_found ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
        log_and_print "WARNING: cannot find block matching [%s]\n"
          old_file ;
        buff
      end 

  | TP_PatchApplyBCSPatch(patch_file,opt_overwrite) -> begin
      let patch_file = Arch.backslash_to_slash patch_file in
      let patch_buff = load_file patch_file in
      if patch_buff <> "" then begin
	      try
	        let new_buff, bad_chunks, app_chunks =
	            Diff.do_patch buff patch_buff false in begin
	          if ( bad_chunks > 0 ) then begin
	            log_or_print
	              "ERROR: Cannot apply patch %s (%d bad chunks).\n"
	              patch_file bad_chunks ;
	            match opt_overwrite with
	            | Some(newfile) -> load_file (Arch.backslash_to_slash newfile)
	            | None -> failwith "Cannot Apply Patch"
	          end else begin
	          if ( app_chunks > 0 ) then begin
	            log_or_print "WARNING: %d chunks in patch file %s already applied.\n" app_chunks patch_file
	          end ;
	          new_buff
	          end
	        end
	      with Not_found ->
	      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
	        log_and_print "Error: applying patch %s failed.\n" patch_file ;
	        buff
	      end else buff
  end

  | TP_PatchInsertBytes(where,how_many) ->
      let how_many = Int32.to_int (eval_pe buff game how_many) in
      let blanks = String.make how_many ('\000') in
      let where = Int32.to_int (eval_pe buff game where) in
      if where > String.length buff then begin
      	log_and_print "INSERT_BYTES out of bounds - file is long %d, point given is %d\n"
				(String.length buff) where;
				failwith "INSERT_BYTES out of bounds";
			end;
      let before = Str.string_before buff where in
      let after = (* try  *)
        Str.string_after buff where
(*         with _ -> "" *)
      in
      before ^ blanks ^ after

  | TP_PatchDeleteBytes(where,how_many) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let how_many = Int32.to_int (eval_pe buff game how_many) in
      if where + how_many > String.length buff then begin
      	log_and_print "DELETE_BYTES out of bounds - file is long %d, point given is %d, bytes were %d\n"
				(String.length buff) where how_many;
				failwith "DELETE_BYTES out of bounds";
			end;
      let before = Str.string_before buff where in
      let after = Str.string_after buff (where + how_many) in
      before ^ after

  | TP_PatchSetIdsSymOfInt(var,ids,value) -> 
      let value = (eval_pe buff game value) in
      let ids = Var.get_string ids in
      let str = Bcs.sym_of_int game ids value in
      Var.set_string var str ;
      buff

  | TP_PatchSet(name,value) ->
      let value = (eval_pe buff game value) in
      let name = eval_pe_str name in 
      Var.set_int32 name value ;
      buff

  | TP_PatchInnerAction(al) ->
      List.iter (process_action tp) al ;
      buff

  | TP_PatchInnerBuff(buff_var,pl) ->
      let new_buff = Var.get_string buff_var in
      let filename = Printf.sprintf "INNER_PATCH %S" buff_var in
      let result = List.fold_left (fun acc elt ->
          process_patch2 filename game acc elt) new_buff pl
      in 
      (* intentionally throw away the result buff *)
      buff

  | TP_PatchInnerBuffFile(buff_var,pl) ->
      let buff_var = Arch.backslash_to_slash buff_var in
      let filename = Var.get_string buff_var in
      if is_true (eval_pe buff game (Pred_File_Exists_In_Game(PE_LiteralString filename))) then begin
        let a,b = split filename in
        let new_buff, path =
          Load.load_resource "INNER_PATCH_FILE" game true a b in
        let filename = Printf.sprintf "INNER_PATCH_FILE %S" buff_var in
        let result = List.fold_left (fun acc elt ->
            process_patch2 filename game acc elt) new_buff pl
        in
        ()
      end ;
      buff

  | TP_PatchInnerBuffSave(store_var,buff_var,pl) ->
      let new_buff = Var.get_string buff_var in
      let filename = Printf.sprintf "INNER_PATCH_SAVE %S" buff_var in
      let result = List.fold_left (fun acc elt ->
          process_patch2 filename game acc elt) new_buff pl
      in
      Var.set_string (eval_pe_str store_var) (Var.get_string result);
      buff

  | TP_PatchRandomSeed(i) ->
      eval_pe_warn := false ;
      let _ = try
          let x = (eval_pe buff game i) in
          Random.init (Int32.to_int x) ;
        with _ ->
          begin
            Random.self_init () ;
          end
      in
      eval_pe_warn := true ;
      buff

  | TP_PatchIf(pred,tb,eb) ->
      let b = ref buff in
      b := List.fold_left (fun acc elt ->
          process_patch2 patch_filename game acc elt) !b
          (if is_true (eval_pe buff game pred) then tb else eb) ;
      !b

	| TP_Launch_Patch_Function (str,int_var,str_var,rets) ->
			let (f_int_args,f_str_args,f_rets,f_code) = try
					Hashtbl.find patch_functions str
				with _ -> failwith (Printf.sprintf "Unknown function: %s" str)
			in
			let i_did_pop = ref false in
			begin try
				Var.var_push();
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					try ignore (Var.get_int32 ("%" ^ a ^ "%")) with _ -> Var.set_int32 a (eval_pe buff game b)
				) f_int_args;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					try ignore (Var.get_string_exact ("%" ^ a ^ "%")) with _ -> Var.set_string a (eval_pe_str b)
				) f_str_args;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					Var.set_int32 a (eval_pe buff game b)
				) int_var;
				List.iter (fun (a,b) ->
					let a = eval_pe_str a in
					Var.set_string a (eval_pe_str b)
				) str_var;
				let buff = List.fold_left (fun acc elt ->
        process_patch2 patch_filename game acc elt) buff f_code in
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
				buff
			with e -> (if not !i_did_pop then Var.var_pop(); raise e); end

  | TP_Launch_Patch_Macro(str) ->
      let (decl, actions) =
        try
          Hashtbl.find patch_macros (Var.get_string str)
        with _ ->
          failwith (Printf.sprintf "Unknown macro: %s" str)
(*           ( [] , [] ) *)
      in
      List.iter (fun x ->
        match x with
          TP_LocalSet(var,pe) ->
            let pe = (eval_pe buff game pe) in
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
      let b = ref buff in
      b := List.fold_left (fun acc elt ->
        process_patch2 patch_filename game acc elt) !b actions ;
      List.iter (fun x ->
        match x with
          TP_LocalSet(var,_)
        | TP_LocalSprint(var,_)
        | TP_LocalTextSprint(var,_)->
          let var = eval_pe_str var in
          Var.remove_local var
      ) decl ;
    !b

  | TP_PatchWhile(pred,pl) ->
      let b = ref buff in
      while is_true (eval_pe buff game pred) do
        b := List.fold_left (fun acc elt ->
            process_patch2 patch_filename game acc elt) !b pl
      done ;
      !b

  | TP_PatchBashFor(where,pl) ->
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
      let the_buff = ref buff in
      List.iter (fun file ->
        let directory = Case_ins.filename_dirname file in
        let filespec = Case_ins.filename_basename file in
        Var.set_string "BASH_FOR_DIRECTORY" directory ;
        Var.set_string "BASH_FOR_FILESPEC" file ;
        Var.set_string "BASH_FOR_FILE" filespec ;
        Var.set_string "BASH_FOR_RES"
          (let a,b = split filespec in a) ;
        Var.set_int "BASH_FOR_SIZE" (file_size file);
        the_buff := List.fold_left (fun acc elt ->
              process_patch2 patch_filename game acc elt) !the_buff pl
      ) !find_list ;
      !the_buff

	| TP_PatchClearArray(arr) ->
		let arr = eval_pe_str arr in
		while Hashtbl.mem !Var.arrays arr do
			Hashtbl.remove !Var.arrays arr
		done;
		buff

	| TP_PatchDefineArray(arr,vals) ->
			let i = ref 0 in
			List.iter (fun x ->
				Var.set_string
					(eval_pe_str (PE_Dollars(arr,[get_pe_string (string_of_int !i)],
						false,true))) (Var.get_string x);
				incr i
			) vals;
			buff

	| TP_DefineAssociativeArray(arr,vals) ->
			List.iter (fun (x,y) ->
				Var.set_string
					(eval_pe_str (PE_Dollars(arr,[x],
						false,true))) (Var.get_string (eval_pe_str y));
			) vals;
			buff
	
	| TP_PatchForEach(var,sl,pl) ->
      let var = eval_pe_str var in
      let sl = List.map Var.get_string sl in
      let the_buff = ref buff in
      List.iter (fun x ->
        Var.set_string var x ;
        the_buff := List.fold_left (fun acc elt ->
              process_patch2 patch_filename game acc elt) !the_buff pl
      ) sl ;
      !the_buff

	| TP_PatchPHPEach(var,invar,outvar,pl) ->
		let var_s = Var.get_string (eval_pe_str var) in
		let var = PE_LiteralString(var_s) in
		let values = try Hashtbl.find !Var.arrays var_s with _ -> [] in
		let outvar = eval_pe_str outvar in
		let  invar = eval_pe_str  invar in
		let the_buff = ref buff in
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
        the_buff := List.fold_left (fun acc elt ->
              process_patch2 patch_filename game acc elt) !the_buff pl;
				for j = 0 to (List.length x) - 1 do
					Var.remove_local (invar ^ "_" ^ string_of_int j)
				done
		) (List.rev values);
		!the_buff

  | TP_PatchFor(init,guard,inc,body) ->
      let cmd_list = init @ [ TP_PatchWhile(guard,body @ inc) ] in
      let b = ref buff in
      b := List.fold_left (fun acc elt ->
          process_patch2 patch_filename game acc elt) !b cmd_list  ;
      !b

	| TP_PatchReinclude(string_list) ->
		let string_list = List.map Var.get_string string_list in
		let b = ref buff in
		List.iter (fun file ->
			let tpp_parsed =
				if !debug_ocaml then log_and_print "Loading %s...\n" file ;
				let x = handle_tpp_filename file in
				Hashtbl.replace loaded_tpp file x ;
				x
			in
			b := List.fold_left (fun acc elt ->
			    process_patch2 patch_filename game acc elt) !b tpp_parsed ;
		) string_list ;
		!b

	| TP_PatchInclude(string_list) ->
		let string_list = List.map Var.get_string string_list in
		let b = ref buff in
		List.iter (fun file ->
			let tpp_parsed = if Hashtbl.mem loaded_tpp file
				then begin
					if !debug_ocaml then log_and_print "%s is already loaded, restoring the image...\n" file ;
					Hashtbl.find loaded_tpp file
				end
				else begin
					if !debug_ocaml then log_and_print "Loading %s...\n" file ;
					let x = handle_tpp_filename file in
					Hashtbl.replace loaded_tpp file x ;
					x
				end
			in
			b := List.fold_left (fun acc elt ->
			    process_patch2 patch_filename game acc elt) !b tpp_parsed ;
		) string_list ;
		!b

  | TP_PatchSilent -> be_silent := true ; buff
  | TP_PatchVerbose -> be_silent := false ; buff

  | TP_Patch2DALater(str,r,c,value) ->
      let row = (eval_pe buff game r) in
      let col = (eval_pe buff game c) in
      let str = Var.get_string str in
      eval_pe_warn := false ;
      let value =
        try
          let x = (eval_pe buff game value) in
          Int32.to_string x
        with _ ->
          begin
            match value with
            | PE_String(x) -> Var.get_string (eval_pe_str x)
            | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game value) ; "")
          end
      in
      let iidx = try Var.get_int32 ("%" ^ str ^ "%") with _ -> 0l in
      let idx = Int32.to_string iidx in
      let sidx = str ^ idx in
      Var.set_int32 (sidx ^ "r" ) (row) ;
      Var.set_int32 (sidx ^ "c" ) (col) ;
      Var.set_string (sidx ^ "v" ) value ;
      Var.set_int32 str (Int32.add iidx 1l) ;
      buff

  | TP_Patch2DANow(str, req_col) ->
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let str = Var.get_string str in
      let lines = Str.split many_newline_or_cr_regexp buff in
      let max = ref 4 in
      let entries = List.map (fun line ->
        let entry_list = Str.split many_whitespace_regexp line in
        List.iter (fun e ->
          let len = String.length e in
          if len > !max then max := len
        ) entry_list ;
        Array.of_list entry_list) lines
      in
      let updates = Hashtbl.create 255 in
      begin 
      try 
        let count = Int32.to_int (Var.get_int32 ("%" ^ str ^ "%")) in
        Var.set_int32 str 0l ; 
        for i = 0 to pred count do
          let sidx = Printf.sprintf "%%%s%d" str i in
          let row = Int32.to_int (Var.get_int32 (sidx ^ "r%")) in
          let col = Int32.to_int (Var.get_int32 (sidx ^ "c%")) in
          let value = Var.get_string (sidx ^ "v%") in
          Hashtbl.add updates (row,col) value
        done ;
        let buf = Buffer.create (String.length buff) in
        let this_line = ref 0 in 
        List.iter (fun entry_array ->
          let this_col_size = Array.length entry_array in 
          Array.iteri (fun this_col orig_elt -> 
            let elt = 
              if this_col_size >= req_col &&
                 Hashtbl.mem updates (!this_line,this_col) then
                 Hashtbl.find updates (!this_line,this_col)
              else orig_elt 
            in
             if this_col < (this_col_size - 1) then
               Printf.bprintf buf "%-.*s " !max elt
             else
               Printf.bprintf buf "%s" elt
          ) entry_array ;
          Buffer.add_char buf '\n' ;
          if this_col_size >= req_col then incr this_line ;
        ) entries ;
        Buffer.contents buf
      with _ -> buff
      end

  | TP_PrettyPrint2DA(indent) ->
    List.fold_left (fun acc elt ->
            process_patch2 patch_filename game acc elt) buff
      [TP_PatchSet(get_pe_string "tb#pretty_print_indent", indent);
       TP_PatchInclude ["tb#pretty_print.tpp"]]

  | TP_Read2DANow(str, req_col) ->
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let lines = Str.split many_newline_or_cr_regexp buff in
      let str = Var.get_string str in
      let i = ref 0 in
      let j = ref 0 in (* line and column counters *)
      List.iter (fun this_line ->
        let this_line = Str.split many_whitespace_regexp this_line in
        if (List.length this_line) >= req_col then begin
          j := 0;
          List.iter ( fun item ->
            let name = Printf.sprintf "%s_%d_%d" str !i !j in
            Var.set_string name item;
            j := !j + 1;
          ) this_line;
          i := !i + 1;
        end
      ) lines;
      let name = Printf.sprintf "%s" str in
      Var.set_int32 name (Int32.of_int !i);
      buff

  | TP_Read2DAFormer(str, row, col, var) ->
    let row = Int32.to_int (eval_pe buff game row) in
    let col = Int32.to_int (eval_pe buff game col) in
    let str = Var.get_string str in
    let str = Printf.sprintf "%%%s_%d_%d%%" str row col in
    Var.set_string var (Var.get_string_exact str);
    buff

  | TP_PatchInsert2DARow(row,req_col,value) ->
      let row = Int32.to_int (eval_pe buff game row) in
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let lines = Str.split many_newline_or_cr_regexp buff in
      eval_pe_warn := false ;
      let value =
        try
          let x = (eval_pe buff game value) in
          Int32.to_string x
        with _ ->
          begin
            match value with
            | PE_String(x) -> Var.get_string (eval_pe_str x)
            | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game value) ; "")
          end
      in
      let before = ref "" in
      let num_rows = ref 0 in
      let found = ref false in
      let avoid_first_nl = ref true in
      List.iter ( fun the_row ->
        before := !before ^ (if !avoid_first_nl then "" else "\r\n") ;
        if !avoid_first_nl then avoid_first_nl := false ;
        if !num_rows < row then begin
          let long_enough = ((List.length (Str.split many_whitespace_regexp the_row)) >= req_col) in
          if long_enough then num_rows := !num_rows + 1 ;
        end else
        if !num_rows = row then begin
          let long_enough = ((List.length (Str.split many_whitespace_regexp the_row)) >= req_col) in
          if long_enough then begin
            num_rows := !num_rows + 1 ;
            before := !before ^ value ^ "\r\n" ;
            found := true ;
          end ;
        end ;
        before := !before ^ the_row
      ) lines ;
      if !num_rows = row then begin
        before := !before ^ "\r\n" ^ value ;
        found := true ;
      end ;
      if not (!found) then failwith "Failed to find enough lines to perform an INSERT_2DA_ROW" ;
      !before ^ "\r\n" ;

  | TP_Remove_2DA_Row(row,req_col) ->
      let row = Int32.to_int (eval_pe buff game row) in
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let lines = Str.split many_newline_or_cr_regexp buff in
      let before = ref "" in
      let num_rows = ref 0 in
      let found = ref false in
      List.iter ( fun the_row ->
        if !num_rows = row then begin
          let long_enough = ((List.length (Str.split many_whitespace_regexp the_row)) >= req_col) in
          if long_enough then begin
            num_rows := !num_rows + 1 ;
            found := true ;
          end else begin
		        before := !before ^ the_row ^ "\r\n"
					end
				end else begin
          let long_enough = ((List.length (Str.split many_whitespace_regexp the_row)) >= req_col) in
          if long_enough then begin
            num_rows := !num_rows + 1 ;
            found := true ;
          end;
		        before := !before ^ the_row ^ "\r\n"
        end ;
      ) lines ;
      if not (!found) then failwith "Failed to find enough lines to perform a REMOVE_2DA_ROW" ;
      !before

  | TP_Patch2DA(row,col,req_col,value) ->
      let row = Int32.to_int (eval_pe buff game row) in
      let col = Int32.to_int (eval_pe buff game col) in
      let req_col = Int32.to_int (eval_pe buff game req_col) in
      let lines = Str.split many_newline_or_cr_regexp buff in

      eval_pe_warn := false ;
      let value =
        try
          let x = (eval_pe buff game value) in
          Int32.to_string x
        with _ ->
          begin
            match value with
            | PE_String(x) -> Var.get_string (eval_pe_str x)
            | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game value) ; "")
          end
      in 
      let slv = String.length value in
      let max = ref slv in 
      let entries = List.map (fun line -> 
        let entry_list = Str.split many_whitespace_regexp line in
        List.iter (fun e ->
          let len = String.length e in
          if len > !max then max := len
        ) entry_list ;
        Array.of_list entry_list) lines 
      in
      let newlines_sofar = ref 0 in
      let rec process line_list lines_left = 
        match line_list, lines_left with
          [], 0 -> ()
        | [], _ -> 
          begin
            log_and_print "ERROR: Cannot find %d rows with at least %d columns." row req_col ;
            failwith "Cannot Set 2DA Entry"
          end
        | hd :: tl, i  when Array.length hd < req_col -> 
            incr newlines_sofar ;
            process tl i
        | hd :: tl, 0 ->
            hd.(col) <- value 
        | hd :: tl, i -> begin
            incr newlines_sofar ;
            process tl (i - 1)
            end 
      in
      process entries row ;
      (try begin 
        let pos = ref 0 in 
        for n = 1 to !newlines_sofar do
          pos := (Str.search_forward many_newline_or_cr_regexp buff !pos) ;
          pos := Str.match_end () ;
        done ;
        for n = 1 to col do
          pos := (Str.search_forward many_whitespace_regexp buff !pos) ;
          pos := Str.match_end () ; 
        done ;
        let before = !pos in 
        let after = Str.search_forward many_not_whitespace_regexp buff !pos in
        let after = Str.match_end () in
        let before_str = Str.string_before buff before in
        let after_str = Str.string_after buff after in
        Printf.sprintf "%s%s%s" before_str value after_str
      end with _ -> begin 
        log_or_print "WARNING: Fast SET_2DA_ENTRY failed, falling back on old method...\n" ;
        let buf = Buffer.create (slv * 2) in 
        List.iter (fun entry_array ->
          Array.iter (fun elt -> 
             Printf.bprintf buf "%-.*s " !max elt
          ) entry_array ;
          Buffer.add_char buf '\n' 
        ) entries ;
        Buffer.contents buf 
        end )
      (*
      List.fold_left (fun line_acc entry_array -> 
        line_acc ^ (Array.fold_left (fun acc elt ->
          Printf.sprintf "%s%-.*s\t" acc !max elt) "" entry_array) ^ "\n")
        "" entries
        *)

  | TP_PatchStringTextually(case_sens,match_exact,find,what,length) ->
      let find = Var.get_string find in
      let what = Var.get_string what in
      let find_ref = ref find in
      let what_ref = ref what in
      let case_sens = match case_sens with
          None -> false
        | Some(x) -> x
      in
      let match_exact = match match_exact with
          None -> false
        | Some(x) -> x
      in
      begin match length with
          None -> ()
        | Some(x) -> begin
          let y = Int32.to_int (eval_pe buff game x) in
          while String.length !what_ref < y do
            what_ref := !what_ref ^ (String.make 1 (Char.chr 0)) ;
          done ;
          while String.length !find_ref < y do
            find_ref := !find_ref ^ (String.make 1 (Char.chr 0)) ;
          done ;
          if !debug_ocaml then begin
            for i = 0 to ( String.length !what_ref) - 1 do
              let from_c = String.get !find_ref i in
              let what_c = String.get !what_ref i in
              log_and_print "%c[%d] -> %c[%d] \n" from_c (Char.code from_c) what_c (Char.code what_c) ;
            done
          end ;
        end ;
      end ;
      let find = !find_ref in
      let my_regexp = match case_sens, match_exact with
          false, false -> Str.regexp_case_fold        find
        | true , false -> Str.regexp                  find
        | false, true  -> Str.regexp_string_case_fold find
        | true , true  -> Str.regexp_string           find
      in
      let what = !what_ref in
      Str.global_replace my_regexp what buff

  | TP_PatchStringEvaluate(case_sens,find, pl, replace) ->
      (* REPLACE_EVALUATE ~Give(\([0-9]+\))~ 
                          SET "RESULT" = "%MATCH1%"
                          ~Give(%RESULT%)~
         ( "%MATCH%" / 2 ) *)
      let find = Var.get_string find in
      let my_regexp = match case_sens with
          None -> Str.regexp find
        | Some(true) -> Str.regexp find
        | Some(false) -> Str.regexp_case_fold find
      in
      let i = ref 0 in
      let work_buff = ref buff in
      (try
        while true do
          let start_idx = Str.search_forward my_regexp !work_buff !i in
          for j = 0 to 20 do
            let v = Printf.sprintf "MATCH%d" j in 
            (* Var.remove_var v ;  *) 
            (try let group = Str.matched_group j !work_buff in
                 Var.set_string v group ; 
             with _ -> ()) 
          done ; 
          (try 
            ignore (
              List.fold_left (fun acc elt -> 
                process_patch2 patch_filename game acc elt) !work_buff pl
            ) ;
           with _ -> ());
          let this_replacement = Var.get_string replace in
          let old_before = Str.string_before !work_buff start_idx in
          let old_after = Str.string_after !work_buff start_idx in 
          let new_after = 
            Str.replace_first my_regexp this_replacement old_after
          in
          work_buff := (old_before ^ new_after) ;
          i := start_idx + 1
        done
      with _ -> () ) ;
      !work_buff

  | TP_PatchString(case_sens,match_exact,find,what) ->
      let find = Var.get_string find in
      let case_sens = match case_sens with
          None -> false
        | Some(x) -> x
      in
      let match_exact = match match_exact with
          None -> false
        | Some(x) -> x
      in
      let my_regexp = match case_sens, match_exact with
          false, false -> Str.regexp_case_fold        find
        | true , false -> Str.regexp                  find
        | false, true  -> Str.regexp_string_case_fold find
        | true , true  -> Str.regexp_string           find
      in
      let what = begin match what with
      | Dlg.Trans_String(a) -> what
      | Dlg.Local_String(lse) ->
          Dlg.Local_String ({ lse_male = Var.get_string lse.lse_male ;
                               lse_male_sound = Var.get_string lse.lse_male_sound ;
                                   lse_female = Var.get_string lse.lse_female ;
                             lse_female_sound = Var.get_string lse.lse_female_sound ; })
      | _ -> what
      end
      in
      let new_index = match Dc.resolve_tlk_string game what with
        Dlg.TLK_Index(i) -> i
      | _ -> log_and_print "ERROR: cannot resolve REPLACE patch\n" ; failwith "resolve"
      in 
      let new_string = Printf.sprintf "%d" new_index in
      Str.global_replace my_regexp new_string buff 

  | TP_PatchByte(where,what) -> 
      let where = Int32.to_int (eval_pe buff game where) in
      let what = Int32.to_int (eval_pe buff game what) in 
      let what = if what < 0 then what + 256 else what in
      let str = String.make 1 (Char.chr what) in
      bounds_check_write where 1 str ; 
      String.blit str 0 buff where 1 ;
      buff
  | TP_PatchShort(where,what) -> 
      let where = Int32.to_int (eval_pe buff game where) in 
      let what = Int32.to_int (eval_pe buff game what) in 
      let str = str_of_short what in
      bounds_check_write where 2 str ; 
      String.blit str 0 buff where 2 ;
      buff
  | TP_PatchLong(where,what) -> 
      let where = Int32.to_int (eval_pe buff game where) in
      let what = eval_pe buff game what in
      let str = str_of_int32 what in
      bounds_check_write where 4 str ;
      String.blit str 0 buff where 4 ;
      buff
  | TP_PatchAppendFile(filename, eval) ->
      let filename = Arch.backslash_to_slash filename in
      let filename = Var.get_string filename in
      let file_buff = load_file filename in
      buff ^ (if eval then Var.get_string file_buff else file_buff)

  | TP_PatchWriteFile(where,filename,insert) ->
      let filename = Arch.backslash_to_slash filename in
      let where = Int32.to_int (eval_pe buff game where) in 
      let filename = Var.get_string filename in 
      let file_buff = load_file filename in
      if insert then begin
        let before = Str.string_before buff where in
        let after = Str.string_after buff where in
        before ^ file_buff ^ after
      end else begin
        let file_len = String.length file_buff in
        let len = String.length buff in
        if (len - where) < file_len then begin 
          log_and_print "Not enough room for [%s] (%d bytes) if you start at position %d in a %d byte buffer!" (filename) (file_len) (where) (len) ;
          failwith "ERROR: cannot process WRITE_FILE"
        end ;
        String.blit file_buff 0 buff where file_len ;
        buff
      end 

  | TP_PatchASCII(where,what,evaluate,reqdSize) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let what = if evaluate then Var.get_string (eval_pe_str what)
                             else eval_pe_str what in 
      let what = match reqdSize with
      | None -> what
      | Some(i) -> 
        let i = (Int32.to_int (eval_pe buff game i)) in
        let res = String.make i '\000' in
        String.blit what 0 res 0 (min i (String.length what)) ;
        res
      in
      bounds_check_write where (String.length what) what ;
      String.blit what 0 buff where (String.length what) ;
      buff

  | TP_PatchASCIITerminated(where,what) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let what = Var.get_string what in
      let what = what ^ "\000" in
      bounds_check_write where (String.length what) what ;
      String.blit what 0 buff where (String.length what) ;
      buff

  | TP_Patch_Gam(cre_name,area,x,y) ->

        (* Patch baldur.gam in save directories *)

        let dlist = list_of_files_in_directory "save" in
        List.iter
        (fun filename ->
        let filename = "save/" ^ filename in
        if is_directory filename && filename <> "save/." && filename <> "save/.." then begin
        log_and_print "Patching baldur.gam in directory %s\n" filename;
        let baldur_buff = load_file (filename ^ "/baldur.gam") in
        if String.sub baldur_buff 0 8 <> "GAMEV1.1" then begin
        failwith "not a valid GAME V1.1 file (wrong sig)"
        end ;
        let non_party_npc_off = int_of_str_off baldur_buff 0x30 in
        let num_non_party_npc = int_of_str_off baldur_buff 0x34 in
        let variable_off = int_of_str_off baldur_buff 0x38 in
        let journal_off = int_of_str_off baldur_buff 0x50 in
        let last_npc_off = (non_party_npc_off + ((num_non_party_npc) * 352)) in

        (* Create the non-party NPC entry *)

        let npc_entry_buff = String.make 352 '\000' in
        write_short npc_entry_buff 0x2 0xFFFF;
        write_int npc_entry_buff 0x4 (variable_off + 352);
        write_int npc_entry_buff 0x8 (String.length buff);
        String.blit area 0 npc_entry_buff 0x18 (String.length area);
        let x = (Int32.to_int (eval_pe buff game x)) in
        let y = (Int32.to_int (eval_pe buff game y)) in
        write_short npc_entry_buff 0x20 x;
        write_short npc_entry_buff 0x22 y;

        (* Update the offsets *)

        write_int baldur_buff 0x38 (variable_off + (String.length buff) + 352);
        write_int baldur_buff 0x50 (journal_off + (String.length buff) + 352);
        write_int baldur_buff 0x34 (num_non_party_npc + 1);

        (* Splice in the non-party NPC entry and the cre file *)

        for i = 0 to num_non_party_npc - 1 do
        let cre_off = int_of_str_off baldur_buff (non_party_npc_off + (i * 352) + 0x4) in
        write_int baldur_buff (non_party_npc_off + (i * 352) + 0x4) (cre_off + 352)
        done;

        let before = Str.string_before baldur_buff last_npc_off in
        let after = Str.string_after baldur_buff variable_off in
        let cre_chunk = String.sub baldur_buff last_npc_off (variable_off - last_npc_off) in
        let new_buff = before ^ npc_entry_buff ^ cre_chunk ^ buff ^ after in

        let oc = Case_ins.perv_open_out_bin (filename ^ "/baldur.gam") in
        output_string oc new_buff;
        close_out oc;
    begin (* handle read-only files! *)
      try
        Case_ins.unix_chmod (filename ^ "/baldur.gam") 511 ; (* 511 = octal 0777 = a+rwx *)
      with e -> ()
        (* log_or_print "WARNING: chmod %s : %s\n" filename 
          (Printexc.to_string e) *)
    end ;
        end) dlist;

        (* Patch baldur.gam in mpsave directories *)

        let mpdlist = list_of_files_in_directory "mpsave" in
        List.iter
        (fun filename ->
        let filename = "mpsave/" ^ filename in
        if is_directory filename && filename <> "mpsave/." && filename <> "mpsave/.." then begin
        log_and_print "Patching baldur.gam in directory %s\n" filename;
        let baldur_buff = load_file (filename ^ "/baldur.gam") in
        if String.sub baldur_buff 0 8 <> "GAMEV1.1" then begin
        failwith "not a valid GAME V1.1 file (wrong sig)"
        end ;
        let non_party_npc_off = int_of_str_off baldur_buff 0x30 in
        let num_non_party_npc = int_of_str_off baldur_buff 0x34 in
        let variable_off = int_of_str_off baldur_buff 0x38 in
        let journal_off = int_of_str_off baldur_buff 0x50 in
        let last_npc_off = (non_party_npc_off + ((num_non_party_npc) * 352)) in

        (* Create the non-party NPC entry *)

        let npc_entry_buff = String.make 352 '\000' in
        write_short npc_entry_buff 0x2 0xFFFF;
        write_int npc_entry_buff 0x4 (variable_off + 352);
        write_int npc_entry_buff 0x8 (String.length buff);
        String.blit area 0 npc_entry_buff 0x18 (String.length area);
        let x = (Int32.to_int (eval_pe buff game x)) in
        let y = (Int32.to_int (eval_pe buff game y)) in
        write_short npc_entry_buff 0x20 x;
        write_short npc_entry_buff 0x22 y;

        (* Update the offsets *)

        write_int baldur_buff 0x38 (variable_off + (String.length buff) + 352);
        write_int baldur_buff 0x50 (journal_off + (String.length buff) + 352);
        write_int baldur_buff 0x34 (num_non_party_npc + 1);

        (* Splice in the non-party NPC entry and the cre file *)

        for i = 0 to num_non_party_npc - 1 do
        let cre_off = int_of_str_off baldur_buff (non_party_npc_off + (i * 352) + 0x4) in
        write_int baldur_buff (non_party_npc_off + (i * 352) + 0x4) (cre_off + 352)
        done;

        let before = Str.string_before baldur_buff last_npc_off in
        let after = Str.string_after baldur_buff variable_off in
        let cre_chunk = String.sub baldur_buff last_npc_off (variable_off - last_npc_off) in
        let new_buff = before ^ npc_entry_buff ^ cre_chunk ^ buff ^ after in

        let oc = Case_ins.perv_open_out_bin (filename ^ "/baldur.gam") in
        output_string oc new_buff;
        close_out oc;
        begin (* handle read-only files! *)
          try
            Case_ins.unix_chmod (filename ^ "/baldur.gam") 511 ; (* 511 = octal 0777 = a+rwx *)
          with e -> ()
            (* log_or_print "WARNING: chmod %s : %s\n" filename
              (Printexc.to_string e) *)
        end ;
        end) mpdlist;

        (* Patch the baldur.gam in the biff *)

        log_and_print "Patching starting baldur.gam (for new games)...\n";
        let nbaldur_buff,path = Load.load_resource "PATCH_GAM" game true "BALDUR" "GAM" in

        if String.sub nbaldur_buff 0 8 <> "GAMEV1.1" then begin
        failwith "not a valid GAME V1.1 file (wrong sig)"
        end ;
        let party_npc_off = int_of_str_off nbaldur_buff 0x20 in
        let journal_off = int_of_str_off nbaldur_buff 0x50 in
        let num_non_party_npc = int_of_str_off nbaldur_buff 0x34 in

        (* Create the non-party NPC entry *)

        let npc_entry_buff = String.make 352 '\000' in
        String.blit (String.uppercase cre_name) 0 npc_entry_buff 0xc (String.length cre_name);
        String.blit area 0 npc_entry_buff 0x18 (String.length area);
        let x = (Int32.to_int (eval_pe buff game x)) in
        let y = (Int32.to_int (eval_pe buff game y)) in
        write_short npc_entry_buff 0x20 x;
        write_short npc_entry_buff 0x22 y;
        for i = 140 to 147 do
        npc_entry_buff.[i] <- Char.chr 0xFF
        done;
        for i = 180 to 185 do
        npc_entry_buff.[i] <- Char.chr 0xFF
        done;

        (* Update all offsets *)

        write_int nbaldur_buff 0x20 (party_npc_off + 352);
        write_int nbaldur_buff 0x28 (party_npc_off + 352);
        write_int nbaldur_buff 0x50 (journal_off + 352);
        write_int nbaldur_buff 0x34 (num_non_party_npc + 1);

        (* Splice in the non-party NPC entry *)

        let before = Str.string_before nbaldur_buff party_npc_off in
        let after = Str.string_after nbaldur_buff party_npc_off in
        let new_buff = before ^ npc_entry_buff ^ after in

        let oc = Case_ins.perv_open_out_bin "override/baldur.gam" in
        output_string oc new_buff;
        close_out oc;
        begin (* handle read-only files! *)
          try
            Case_ins.unix_chmod "override/baldur.gam" 511 ; (* 511 = octal 0777 = a+rwx *)
          with e -> ()
            (* log_or_print "WARNING: chmod %s : %s\n" filename
              (Printexc.to_string e) *)
        end ;
        buff

  | TP_Remove_Known_Spell(sp_list) ->
    let sp_list = List.map (fun x -> String.uppercase (Var.get_string x)) sp_list in
    let cre = Cre.cre_of_string buff in
    let known_spells = List.filter (
      fun (sp_name, sp_lvl, sp_type) ->
        not (List.mem (String.uppercase sp_name) sp_list)
    ) cre.Cre.known_spells in
    Cre.string_of_cre { cre with Cre.known_spells = known_spells }

  | TP_Remove_Memorized_Spell(sp_list) ->
    let sp_list = List.map (fun x -> String.uppercase (Var.get_string x)) sp_list in
    let cre = Cre.cre_of_string buff in
    let memorized_info = List.map (
      fun (level, count, count2, spell_type, mlist) ->
        (level, count, count2, spell_type, List.filter (
          fun (spell_name, memorized) ->
            not (List.mem (String.uppercase spell_name) sp_list)
        ) mlist)
    ) cre.Cre.memorized_info in
    Cre.string_of_cre { cre with Cre.memorized_info = memorized_info }

  | TP_Add_Known_Spell(spell,level,sp_type) ->
		if !debug_ocaml then log_and_print "Attempting to ADD_KNOWN_CRE\n";
		let cre = Cre.cre_of_string buff in
		if !debug_ocaml then log_and_print "Unmarshaled the cre\n";
		let level = (Int32.to_int (eval_pe buff game level)) in
		let known_spells = cre.Cre.known_spells in
		let new_type =
			(match (String.uppercase (Var.get_string sp_type)) with
			| "PRIEST" -> 0
			| "INNATE" -> 2
			| "WIZARD" -> 1
			| _ ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
				log_and_print "WARNING: ADD_NEW_SPELL: Unknown flag %s.  Defaulting to INNATE for spell type.\n" (String.uppercase sp_type);
			2)
		in
		let this = (Var.get_string spell, level, new_type) in
		let known_spells = if not (List.exists (fun cur ->
			let (ospell, olevel, otype) = cur in
			(String.uppercase ospell = String.uppercase spell) && olevel = level
			  && otype = new_type
		) known_spells) then
			this :: known_spells else known_spells
		in
		if !debug_ocaml then log_and_print "Added the spell.\n";
		let out = Cre.string_of_cre { cre with Cre.known_spells = known_spells } in
		if !debug_ocaml then log_and_print "Marshaled the cre\n";
		out

  | TP_Remove_Cre_Item(items) ->
		if !debug_ocaml then log_and_print "Attempting to REMOVE_CRE_ITEM\n";
		let cre = Cre.cre_of_string buff in
		if !debug_ocaml then log_and_print "Unmarshaled the cre\n";
		let items = List.map String.uppercase (List.map Var.get_string items) in
		let new_cre_items = ref [] in
		List.iter (fun item ->
			let name,_ = item in
			if not (List.mem (String.uppercase name) items) then
				new_cre_items := item :: !new_cre_items;
		) cre.Cre.items;
		if !debug_ocaml then log_and_print "Added the spell.\n";
		let out = Cre.string_of_cre { cre with Cre.items = !new_cre_items } in
		if !debug_ocaml then log_and_print "Marshaled the cre\n";
		out

	| TP_Remove_Cre_Items ->
		Cre.string_of_cre { (Cre.cre_of_string buff)
			with Cre.items = []
		}

	| TP_Remove_Cre_Effects ->
		Cre.string_of_cre { (Cre.cre_of_string buff)
			with Cre.effects = ([],false)
		}

	| TP_Remove_Known_Spells ->
		Cre.string_of_cre { (Cre.cre_of_string buff)
			with Cre.known_spells = []
		}

	| TP_Remove_Memorized_Spells ->
		let x = Cre.cre_of_string buff in
		Cre.string_of_cre { x
			with Cre.memorized_info = List.map (fun (a,b,c,d,_) -> (a,b,c,d,[])) x.Cre.memorized_info
		}

  | TP_Set_BG2_Proficiency(name,value) ->
		let cre = Cre.cre_of_string buff in
		let (effects, v1) = cre.Cre.effects in
		let value = eval_pe buff game value in
		let effects_q = Queue.create () in
		let added = ref false in
		let prof =
			try
				begin
					match name with
					| PE_String(x) -> Bcs.int_of_sym game "STATS" (String.uppercase (Var.get_string (eval_pe_str x)))
					| _ -> failwith "go ahead"
				end
			with _ ->
				eval_pe buff game name
		in
		eval_pe_warn := true;
		if !debug_ocaml then log_and_print "SET_BG2_PROFICIENCY %ld points in %ld\n" prof value;
		let (len,  write_opc,  read_opc,        opc,pa1, pa2, tim, prb, write_prob) = if not v1 then
			   0x108,write_int,  int_of_str_off,  8,  0x14,0x18,0x1c,0x24,write_short else
			   0x30, write_short,short_of_str_off,0,  0x4, 0x8, 0xc, 0x12,write_byte
		in
		let new_eff_buff = if value <> 0l then begin
			let new_eff_buf = String.make len '\000' in
			write_opc   new_eff_buf opc 233;
			write_int32 new_eff_buf pa1 value;
			write_int32 new_eff_buf pa2 prof;
			write_opc   new_eff_buf tim 9;
			write_prob  new_eff_buf prb 100;
			let np = Int32.to_int prof in
			if not v1 && np >= 89 && np <= 134 then write_int new_eff_buf 72 (np - 89);
			new_eff_buf
		end else "" in
		List.iter (fun effect_buff ->
			let eff = read_opc         effect_buff opc in
			let cnt = int32_of_str_off effect_buff pa1 in
			let pro = int32_of_str_off effect_buff pa2 in
			if eff = 233 then begin
				if pro = prof then begin
					if not !added then begin
						()
					end else begin
						added := true;
						if new_eff_buff <> "" then Queue.add new_eff_buff effects_q;
					end
				end else begin
					Queue.add effect_buff effects_q;
				end
			end else begin
				Queue.add effect_buff effects_q;
			end
		) effects;
		if not !added then begin
			if new_eff_buff <> "" then Queue.add new_eff_buff effects_q;
		end;
		let out = Cre.string_of_cre { cre with Cre.effects = List.rev(Queue.fold (fun acc x -> x :: acc) [] effects_q),v1 } in
		out

	| TP_Add_Memorized_Spell(spell,level,stype,spcount) ->
		let cre = Cre.cre_of_string buff in
		let nlevel = (Int32.to_int (eval_pe buff game level)) in
		let memorized_info = ref cre.Cre.memorized_info in
		let spcount = Int32.to_int (eval_pe buff game spcount) in
		let spell = Var.get_string spell in
		let stype = Var.get_string stype in
		let new_type =
			(match (String.uppercase stype) with
			| "PRIEST" -> 0
			| "INNATE" -> 2
			| "WIZARD" -> 1
			| _ ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
				log_and_print "WARNING: ADD_NEW_SPELL: Unknown flag %s.  Defaulting to INNATE for spell type.\n" (String.uppercase stype);
			2)
		in
		for j = 1 to spcount do
 			memorized_info := List.map (fun (level,count,count2, spell_type, mlist)->
				if (new_type = spell_type && level = nlevel) then begin
					let length = List.length mlist in
					let mlist = (spell,true) :: mlist in
					let i n = if n <= length then length + 1 else n in
					(level, i count, i count2, spell_type, mlist)
				end else
					(level,count,count2,spell_type,mlist)
			) !memorized_info
		done;
		process_patch2 patch_filename game (Cre.string_of_cre { cre with Cre.memorized_info = !memorized_info })
		  (TP_Add_Known_Spell(spell,level,stype))

	| TP_PatchSavFile(lvl,pl) ->
			let sav = Sav.sav_of_str buff in
			let nsav = Queue.create () in
			while not (Queue.is_empty sav) do
				let current = Queue.pop sav in
				Var.set_string "SAV_FILE" current.Sav.filename;
				let result = List.fold_left (fun acc elt ->
          process_patch2 patch_filename game acc elt) current.Sav.contents pl in
				Queue.push { Sav.filename = current.Sav.filename;
					Sav.contents = result }  nsav;
			done;
			let b = Sav.str_of_sav (eval_pe buff game lvl) nsav in
			if !debug_ocaml then log_and_print "done PatchSavFile\n";
			b

  | TP_Add_Map_Note(m) ->

      (* Turn the colours into something usable *)

      let colour_flags =
      (match (String.uppercase (Var.get_string m.colour)) with
      | "GRAY" -> 0
      | "VIOLET" -> 1
      | "GREEN" -> 2
      | "ORANGE" -> 3
      | "RED" -> 4
      | "BLUE" -> 5
      | "DARKBLUE" -> 6
      | "LIGHTGRAY" -> 7
      | _ ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
				log_and_print "WARNING: ADD_MAP_NOTE: Unknown flag %s.  Defaulting to GRAY for flags.\n" (String.uppercase m.colour);
             0) in

      (* Create the new automap *)

      let auto_buff = String.make 52 '\000' in
      let xcoord = (Int32.to_int (eval_pe buff game m.xcoord)) in
      let ycoord = (Int32.to_int (eval_pe buff game m.ycoord)) in
      write_short auto_buff 0 xcoord;
      write_short auto_buff 2 ycoord;
      write_byte auto_buff 8 1;
      write_byte auto_buff 10 colour_flags;

      let new_index = match Dc.resolve_tlk_string game m.mstring with
          Dlg.TLK_Index(i) -> i
      | _ -> log_and_print "ERROR: ADD_MAP_NOTE: cannot resolve SAY patch\n" ; failwith "resolve"
      in
      let new_string = str_of_int new_index in
      String.blit new_string 0 auto_buff 4 4 ;

      (* Check for version *)

        let area_v = String.sub buff 0 8 in
      (match area_v with
       | "AREAV1.0" ->
         let automap_off = int_of_str_off buff 0xc4 in
         let auto_num = int_of_str_off buff 0xc8 in
         if automap_off = 0 then begin
         let new_automap_off = String.length buff in
         write_int buff 0xc4 new_automap_off;
         write_int buff 0xc8 (auto_num + 1)
         end else
         write_int buff 0xc8 (auto_num + 1)
       | "AREAV9.1" ->
         let automap_off = int_of_str_off buff 0xd4 in
         let auto_num = int_of_str_off buff 0xd8 in
         if automap_off = 0 then begin
         let new_automap_off = String.length buff in
         write_int buff 0xd4 new_automap_off;
         write_int buff 0xd8 (auto_num + 1)
         end else
         write_int buff 0xd8 (auto_num + 1)
       | _ -> failwith (Printf.sprintf "ADD_MAP_NOTE: Unknown area version %s" area_v));

      (* Splice it all together *)

      buff ^ auto_buff

  | TP_PatchPrint(msg) ->
        let str = Dc.single_string_of_tlk_string game msg in
        be_silent := false ;
        let str = Var.get_string str in
        log_and_print "\n%s\n" str ;
        buff

  | TP_PatchSprint(name,msg) ->
        let name = eval_pe_str name in
        let (str : string) = eval_pe_tlk_str game msg in
        let value = Var.get_string str in
        Var.set_string name value  ;
        buff

  | TP_PatchSprintf(name,msg,vars) ->
        let name = eval_pe_str name in
        let (str : string) = eval_pe_tlk_str game msg in
				let reg = Str.regexp "%[dsx]" in
				let parts = Str.full_split reg str in
				let buff' = Buffer.create (String.length str) in
				if !debug_ocaml then begin
					log_and_print "SPRINTF %s ~%s~ " name str;
					List.iter (fun cur ->
			      eval_pe_warn := false ;
		        log_and_print "%s " (try
		          let x = (eval_pe buff game cur) in
		          Int32.to_string x
		        with _ ->
		          begin
		            match cur with
		            | PE_String(x) -> Var.get_string (eval_pe_str x)
		            | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game cur) ; "")
		          end)
					) vars;
				log_and_print "\n"
				end;
				let vars = ref vars in
				List.iter (fun x ->
					Buffer.add_string buff' (match x with
						| Str.Text(y) -> y
						| Str.Delim(y) -> (
							let cur = List.hd !vars in
							vars := List.tl !vars;
							match y with
							| "%s" -> (
					      eval_pe_warn := false ;
				        try
				          let x = (eval_pe buff game cur) in
				          Int32.to_string x
				        with _ ->
				          begin
				            match cur with
				            | PE_String(x) -> Var.get_string (eval_pe_str x)
				            | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game cur) ; "")
				          end
							)
							| "%d" -> Int32.to_string (eval_pe buff game cur)
							| "%x" -> Printf.sprintf "0x%lx" (eval_pe buff game cur)
							| _ -> failwith (Printf.sprintf "Unknown SPRINTF mode: %s" y)
						)
				)) parts;
        Var.set_string name (Buffer.contents buff') ;
        if !vars <> [] then failwith "SPRINTF: too many arguments";
        buff

  | TP_PatchTextSprint (var,str) ->
      let var = eval_pe_str var in
      let str = Var.get_string (eval_pe_str str) in
      Var.add_local_string var str ;
      buff

  | TP_PatchSpaces (var,str) ->
      let var = eval_pe_str var in
      let str = Var.get_string (eval_pe_str str) in
      Var.add_local_string var (String.make (String.length str) ' ') ;
      buff

  | TP_PatchQuote (var,str) ->
      let var = eval_pe_str var in
      let str = Var.get_string (eval_pe_str str) in
      Var.add_local_string var (Str.quote str) ;
      buff

  | TP_PatchToLower(name) ->
        let name = eval_pe_str name in
        let value = Var.get_string_exact ("%" ^ name ^ "%") in
        Var.set_string name (String.lowercase value) ;
        buff

  | TP_PatchToUpper(name) ->
        let name = eval_pe_str name in
        let value = Var.get_string_exact ("%" ^ name ^ "%") in
        Var.set_string name (String.uppercase value) ;
        buff

  | TP_PatchSnprint(size,name,msg) ->
        let name = eval_pe_str name in
        let str = eval_pe_tlk_str game msg in
        let value = Var.get_string str in
        let wanted_size = Int32.to_int (eval_pe buff game size) in 
        let actual_size = String.length value in 
        let substr =
          if actual_size > wanted_size then
            Str.first_chars value wanted_size
          else 
            value
        in 
        Var.set_string name substr ; 
        buff

    | TP_Add_Cre_Item(i) ->
      let i = { i with
        item_name = String.uppercase (Var.get_string i.item_name);
        item_slot = Var.get_string i.item_slot ;
        i_flags = Var.get_string i.i_flags ;
      } in

      let i_charge1 = (Int32.to_int (eval_pe buff game i.i_charge1)) in
      let i_charge2 = (Int32.to_int (eval_pe buff game i.i_charge2)) in
      let i_charge3 = (Int32.to_int (eval_pe buff game i.i_charge3)) in

      (* Make the item type into something usable *)
      let i = { i with item_slot = List.fold_left (
        fun acc (from, into) ->
          Str.global_replace (Str.regexp_case_fold from) into acc
      ) (" " ^ i.item_slot ^ " ")
        [("[ \t]QITEM[ \t]"," QITEM1 QITEM2 QITEM3 ");
         ("[ \t]QUIVER[ \t]"," QUIVER1 QUIVER2 QUIVER3 ");
         ("[ \t]RING[ \t]"," LRING RRING ");
         ("[ \t]INV[ \t]", " INV1 INV2 INV3 INV4 INV5 INV6 INV7 INV8 INV9 INV10 INV11 INV12 INV13 INV14 INV15 INV16 ")]
      } in
      let possible_slots = List.map (fun str -> match String.uppercase str with
        | "HELMET"    -> 0
        | "ARMOR"     -> 1
        | "SHIELD"    -> 2
        | "GLOVES"    -> 3
        | "LRING"     -> 4
        | "RRING"     -> 5
        | "AMULET"    -> 6
        | "BELT"      -> 7
        | "BOOTS"     -> 8
        | "WEAPON1"   -> 9
        | "WEAPON2"   -> 10
        | "WEAPON3"   -> 11
        | "WEAPON4"   -> 12
        | "QUIVER1"   -> 13
        | "QUIVER2"   -> 14
        | "QUIVER3"   -> 15
        | "QUIVER4"   -> 16
        | "CLOAK"     -> 17
        | "QITEM1"    -> 18
        | "QITEM2"    -> 19
        | "QITEM3"    -> 20
        | "INV1"      -> 21
        | "INV2"      -> 22
        | "INV3"      -> 23
        | "INV4"      -> 24
        | "INV5"      -> 25
        | "INV6"      -> 26
        | "INV7"      -> 27
        | "INV8"      -> 28
        | "INV9"      -> 29
        | "INV10"     -> 30
        | "INV11"     -> 31
        | "INV12"     -> 32
        | "INV13"     -> 33
        | "INV14"     -> 34
        | "INV15"     -> 35
        | "INV16"     -> 36
        | _ ->
      	  (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
          log_and_print "WARNING: ADD_CRE_ITEM: Unknown slot %s.  Default to INV15 for placement.\n" (String.uppercase str) ;
          35
      ) (Str.split many_whitespace_regexp i.item_slot)
      in

      (* Make the flags into something useable *)
      let new_flags = match (String.uppercase i.i_flags) with
        | "NONE" -> 0
        | "IDENTIFIED" -> 1
        | "UNSTEALABLE" -> 2
        | "STOLEN" -> 4
        | "UNDROPPABLE" -> 8
        | "IDENTIFIED&STOLEN" -> 5
        | "IDENTIFIED&UNSTEALABLE" -> 3
        | "IDENTIFIED&UNDROPPABLE" -> 9
        | "UNSTEALABLE&UNDROPPABLE" -> 10
        | "STOLEN&UNDROPPABLE" -> 12
        | "IDENTIFIED&STOLEN&UNDROPPABLE" -> 13
        | "IDENTIFIED&UNSTEALABLE&UNDROPPABLE" -> 11
        | _ ->
      	  (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
          log_and_print "WARNING: ADD_CRE_ITEM: Unknown flag %s.  Defaulting to NONE for flags.\n" (String.uppercase i.i_flags) ;
          0     
      in

      let cre = Cre.cre_of_string buff in
      let items = ref cre.Cre.items in

      let get_empty_inv_slot () =
        try
          List.find (fun inv_slot ->
            (* no item is using this slot *)
            not (List.exists (fun (name, (unknown, q1, q2, q3, flags, slots)) ->
              List.mem inv_slot slots
            ) !items)
          ) [21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36]
        with Not_found ->
          (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
          log_and_print "WARNING: ADD_CRE_ITEM: Could not find empty inventory slot. Defaulting to INV16.\n" ;
          36
      in

      let move_to_inv slot =
        (* find item which is currently in the slot and change slots *)
        items := List.map (
          fun ((name, (unknown, q1, q2, q3, flags, slots)) as item) ->
            if List.mem slot slots then
              (name, (unknown, q1, q2, q3, flags, (List.map (fun s ->
                if s = slot then (get_empty_inv_slot () ) else s
              ) slots)))
            else item
        ) !items ;
        ()
      in
      
      (* find empty slot in possible_slots *)
      let target_slot = 
        try
          List.find (fun slot ->
            not (List.exists (fun (name, (unknown, q1, q2, q3, flags, slots)) ->
              List.mem slot slots
            ) !items)
          ) possible_slots
        with Not_found ->
          (* defaulting to first element in possible_slots, move item to inv *)
          let def_slot = List.hd possible_slots in
          move_to_inv def_slot ;
          def_slot
      in
      let equipped = ref cre.Cre.equipped in
      let is_twohanded = not i.twohanded_weapon in

      if i.equip && target_slot >= 9 && target_slot <= 12 then begin
        (* move potential shield to inv if two-handed *)
        if is_twohanded then move_to_inv 2 ;
        equipped := target_slot - 9
      end ;
      (* finally add the item *)
      items := List.append !items [(i.item_name, (0, i_charge1, i_charge2, i_charge3, new_flags, [target_slot]))] ;
      Cre.string_of_cre { cre with Cre.items = !items; Cre.equipped = !equipped } 

  | TP_Replace_Cre_Item(i) ->
      let i = { i with
        item_name = String.uppercase (Var.get_string i.item_name);
        item_slot = Var.get_string i.item_slot ;
        i_flags = Var.get_string i.i_flags ;
      } in

      let i_charge1 = (Int32.to_int (eval_pe buff game i.i_charge1)) in
      let i_charge2 = (Int32.to_int (eval_pe buff game i.i_charge2)) in
      let i_charge3 = (Int32.to_int (eval_pe buff game i.i_charge3)) in

      (* Make the item type into something usable *)
      let i = { i with item_slot = List.fold_left (
        fun acc (from, into) ->
          Str.global_replace (Str.regexp_case_fold from) into acc
      ) (" " ^ i.item_slot ^ " ")
        [("[ \t]QITEM[ \t]"," QITEM1 QITEM2 QITEM3 ");
         ("[ \t]QUIVER[ \t]"," QUIVER1 QUIVER2 QUIVER3 ");
         ("[ \t]RING[ \t]"," LRING RRING ");
         ("[ \t]INV[ \t]", " INV1 INV2 INV3 INV4 INV5 INV6 INV7 INV8 INV9 INV10 INV11 INV12 INV13 INV14 INV15 INV16 ")]
      } in
      let possible_slots = List.map (fun str -> match String.uppercase str with
        | "HELMET"    -> 0
        | "ARMOR"     -> 1
        | "SHIELD"    -> 2
        | "GLOVES"    -> 3
        | "LRING"     -> 4
        | "RRING"     -> 5
        | "AMULET"    -> 6
        | "BELT"      -> 7
        | "BOOTS"     -> 8
        | "WEAPON1"   -> 9
        | "WEAPON2"   -> 10
        | "WEAPON3"   -> 11
        | "WEAPON4"   -> 12
        | "QUIVER1"   -> 13
        | "QUIVER2"   -> 14
        | "QUIVER3"   -> 15
        | "QUIVER4"   -> 16
        | "CLOAK"     -> 17
        | "QITEM1"    -> 18
        | "QITEM2"    -> 19
        | "QITEM3"    -> 20
        | "INV1"      -> 21
        | "INV2"      -> 22
        | "INV3"      -> 23
        | "INV4"      -> 24
        | "INV5"      -> 25
        | "INV6"      -> 26
        | "INV7"      -> 27
        | "INV8"      -> 28
        | "INV9"      -> 29
        | "INV10"     -> 30
        | "INV11"     -> 31
        | "INV12"     -> 32
        | "INV13"     -> 33
        | "INV14"     -> 34
        | "INV15"     -> 35
        | "INV16"     -> 36
        | _ ->
      	  (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
          log_and_print "WARNING: REPLACE_CRE_ITEM: Unknown slot %s.  Default to INV15 for placement.\n" (String.uppercase str) ;
          35
      ) (Str.split many_whitespace_regexp i.item_slot)
      in

      (* Make the flags into something useable *)
      let new_flags = match (String.uppercase i.i_flags) with
        | "NONE" -> 0
        | "IDENTIFIED" -> 1
        | "UNSTEALABLE" -> 2
        | "STOLEN" -> 4
        | "UNDROPPABLE" -> 8
        | "IDENTIFIED&STOLEN" -> 5
        | "IDENTIFIED&UNSTEALABLE" -> 3
        | "IDENTIFIED&UNDROPPABLE" -> 9
        | "UNSTEALABLE&UNDROPPABLE" -> 10
        | "STOLEN&UNDROPPABLE" -> 12
        | "IDENTIFIED&STOLEN&UNDROPPABLE" -> 13
        | "IDENTIFIED&UNSTEALABLE&UNDROPPABLE" -> 11
        | _ ->
      	  (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
				  log_and_print "WARNING: REPLACE_CRE_ITEM: Unknown flag %s.  Defaulting to NONE for flags.\n" (String.uppercase i.i_flags) ;
          0     
      in

      let cre = Cre.cre_of_string buff in
      let items = ref cre.Cre.items in

      let get_empty_inv_slot () =
        try
          List.find (fun inv_slot ->
            (* no item is using this slot *)
            not (List.exists (fun (name, (unknown, q1, q2, q3, flags, slots)) ->
              List.mem inv_slot slots
            ) !items)
          ) [21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36]
        with Not_found ->
          (try assert false with Assert_failure(file,line,col) -> set_errors file line) ;
          log_and_print "WARNING: REPLACE_CRE_ITEM: Could not find empty inventory slot. Defaulting to INV16\n." ;
          36
      in

      let move_to_inv slot =
        (* find item which is currently in the slot and change slots *)
        items := List.map (
          fun ((name, (unknown, q1, q2, q3, flags, slots)) as item) ->
            if List.mem slot slots then
              (name, (unknown, q1, q2, q3, flags, (List.map (fun s ->
                if s = slot then (get_empty_inv_slot () ) else s
              ) slots)))
            else item
        ) !items ;
        ()
      in
      
      (* find non-empty slot in possible_slots *)
      let target_slot = 
        try
          List.find (fun slot ->
            List.exists (fun (name, (unknown, q1, q2, q3, flags, slots)) ->
              List.mem slot slots
            ) !items
          ) possible_slots
        with Not_found -> -1
      in
      if target_slot <> -1 then begin
        let equipped = ref cre.Cre.equipped in
        let is_twohanded = not i.twohanded_weapon in
        if i.equip && target_slot >= 9 && target_slot <= 12 then begin
          (* move potential shield to inv if two-handed *)
          if is_twohanded then move_to_inv 2 ;
          equipped := target_slot - 9
        end ;
        (* replace item in target_slot *)
        items := List.map (
          fun ((name, (unknown, q1, q2, q3, flags, slots)) as item) ->
            if List.mem target_slot slots then
              (i.item_name, (0, i_charge1, i_charge2, i_charge3, new_flags, [target_slot]))
            else item
        ) !items ;
        Cre.string_of_cre { cre with Cre.items = !items; Cre.equipped = !equipped } 
      end else process_patch2 patch_filename game buff (TP_Add_Cre_Item(i))

  | TP_CompileBAFtoBCS ->
        (try
        let old_ok = !Dc.ok_to_resolve_strings_while_loading in
       	Dc.ok_to_resolve_strings_while_loading := Some(game);
        let bcs = handle_script_buffer (patch_filename ^ ".BAF") buff in
        let out_buff = Buffer.create 40960 in
        Bcs.save_bcs game (Bcs.Save_BCS_Buffer(out_buff)) bcs ;
       	Dc.ok_to_resolve_strings_while_loading := old_ok;
        Buffer.contents out_buff
         with Modder.Modder_error e -> failwith e
				 | _ -> buff)

  | TP_CompileDLGtoD ->
        handle_dlg_buffer game patch_filename buff

  | TP_CompileDtoDLG ->
        handle_d_buffer game patch_filename buff

  | TP_CompileBCStoBAF ->
        (try
        let bcs = handle_script_buffer (patch_filename ^ ".BCS") buff in
        let out_buff = Buffer.create 40960 in
        Bcs.print_script_text game (Bcs.Save_BCS_Buffer(out_buff))
          (Bcs.BCS_Print_Script(bcs)) false None ;
        Buffer.contents out_buff
         with Modder.Modder_error e -> failwith e
				 | _ -> buff)

  | TP_EvaluateBuffer -> Var.get_string buff

  | TP_EvaluateBufferSpecial s ->
  	if String.length s <> 1 then failwith "EVALUATE_BUFFER_SPECIAL with more or less than one character";
  	let s = Str.quote s in
		Var.get_string_special (Str.regexp (Printf.sprintf "%s[^%s]+%s" s s s)) buff


  | TP_PatchReadLN(x) ->
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
            log_and_print "Not enough backed up entries in your replies";
            failwith "Missing READLN strings"
      end ;
      buff

	| TP_PatchGetOffsetArray2 (arr, args) ->
  	let (indoff,offsetO,offsetBL,countO,countBL,indexO,indexBL,length) = args in
  	let countO = PE_Add(countO,indoff) in
  	let indexO = PE_Add(indexO,indoff) in
  	let args = (offsetO,offsetBL,countO,countBL,indexO,indexBL,length) in
  	process_patch2 patch_filename game buff (TP_PatchGetOffsetArray(arr,args))

  | TP_PatchGetOffsetArray (arr, args) ->
  	let (offsetO,offsetBL,countO,countBL,indexO,indexBL,length) = args in
		let bounds_check idx size retfun =
			let len = String.length buff in
			let out_of_bounds = (idx < 0 || (idx + size) > len) in
			match out_of_bounds with
			| false -> (retfun ())
			| true -> 
			begin 
				log_and_print "ERROR: illegal %d-byte read from offset %d of %d-byte file %s\n" size idx len patch_filename ;
				failwith (patch_filename ^ ": read out of bounds (GET_OFFSET_ARRAY)")
			end 
		in 
  	let read4 where =
			let value = bounds_check where 4 
				(fun () -> int32_of_str_off buff where) in
			Int32.to_int value
		in
		let read2 where =
      let value = bounds_check where 2
        (fun () -> Int32.of_int (short_of_str_off buff where)) in
      Int32.to_int value
    in
		let read where howMany =
			let where = Int32.to_int (eval_pe buff game where) in
			let howMany = Int32.to_int (eval_pe buff game howMany) in
			match howMany with
			| 2 -> read2 where
			| 4 -> read4 where
			| 0 -> 0
			| _ -> failwith "TP_PatchGetOffsetArray internal"
		in
		let offset = read offsetO offsetBL in
		let count  = read  countO  countBL in
		let index  = read  indexO  indexBL in
		let length = Int32.to_int (eval_pe buff game length) in
		if !debug_ocaml then log_and_print "offset %d count %d index %d length %d\n"
			offset count index length;
		let arrS = eval_pe_str arr in
		if not (Hashtbl.mem !Var.arrays arrS) then Hashtbl.add !Var.arrays arrS [];
		for i = index to index + count - 1 do
			Var.set_string
				(eval_pe_str (PE_Dollars(arr,[get_pe_string (string_of_int (i - index))],
				  false,true))) (string_of_int (i * length + offset));
		done;
		buff

  | TP_Add_S_Item(store_args,item,where,charge1,charge2,charge3,flags,stock,supply) ->

      (* Create the new item *)
      let item = Var.get_string item in
      let item = String.uppercase item in

      let item_buff = String.make 28 '\000' in
      String.blit item 0 item_buff 0 (String.length item);
      let charge1 = Int32.to_int (eval_pe buff game charge1) in
      let charge2 = Int32.to_int (eval_pe buff game charge2) in
      let charge3 = Int32.to_int (eval_pe buff game charge3) in
      write_short item_buff 0xa charge1;
      write_short item_buff 0xc charge2;
      write_short item_buff 0xe charge3;

      let new_flags =
      (match (String.uppercase flags) with
      | "NONE" -> 0
      | "IDENTIFIED" -> 1
      | "UNSTEALABLE" -> 2
      | "STOLEN" -> 4
      | "IDENTIFIED&STOLEN" -> 5
      | "IDENTIFIED&UNSTEALABLE" -> 3
      | _ ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
				log_and_print "WARNING: ADD_STORE_ITEM: Unknown flag %s.  Defaulting to 0 for flags.\n" (String.uppercase flags);
             0)
      in

      let supply_tog = match supply with
      | None -> 0
      | Some(str) -> match (String.uppercase (Var.get_string str)) with
        | "LIMITED" -> 0
        | "UNLIMITED" -> 1
        | _ ->
      	(try assert false with Assert_failure(file,line,col) -> set_errors file line);
				log_and_print "WARNING: ADD_STORE_ITEM: Unknown value %s. Defaulting to 0 (limited) for supply.\n"
                 (String.uppercase str) ; 0
      in

      let stock = if supply_tog = 1 then 1 else (Int32.to_int (eval_pe buff game stock)) in

      write_int item_buff 0x10 new_flags;
      write_int item_buff 0x14 stock;
      write_int item_buff 0x18 supply_tog;

      (* Grab the state of the + and uppercase the item string *)

      let possible_plus = store_args.overwrite_store_item in

      (* Read in the offsets that we need to update *)

      let drinksoffset = int_of_str_off buff 0x4c in
      let ipurchasedoffset = int_of_str_off buff 0x2c in
      let isaleoffset = int_of_str_off buff 0x34 in
      let cureoffset = int_of_str_off buff 0x70 in

      (* Read in the # of items for sale *)

      let numisale = int_of_str_off buff 0x38 in


      (* Grab all items *)

      let items_list = ref [] in
      if !debug_ocaml then log_and_print "Trying to String.sub buff %d %d\n" isaleoffset (ipurchasedoffset - isaleoffset);
      let items_string = String.sub buff isaleoffset (numisale * 28) in
      if !debug_ocaml then log_and_print "Done String.sub\n" ;
      for i = 0 to numisale - 1 do
      let item_check = String.sub items_string (i * 28) 8 in
      let item_check = get_string_of_size item_check 0 8 in
      items_list := item_check :: !items_list
      done;

      items_list := List.rev !items_list;

      (* If we have a + sign and the item already exists *)

      if not possible_plus && (List.mem item !items_list) then begin
      let i = ref 0 in
      let str_before = ref "" in
      let str_after = ref "" in
      let new_buff = ref "" in
      List.iter (fun patch_it ->

      if patch_it = item then begin

      (* Overwrite the item *)

      str_before := Str.string_before buff (isaleoffset + (!i * 28));
      str_after := Str.string_after buff (isaleoffset + (!i * 28) + 28);
      new_buff := !str_before ^ item_buff ^ !str_after;
      end
      else
      incr i
      ) !items_list;
      !new_buff
      end

      (* No + sign and the item exists *)

      else if possible_plus && (List.mem item !items_list) then begin
      log_and_print "%s.ITM is already in the store.  Skipping...\n" item;
      buff
      end

      (* No + sign and the item doesn't exist *)

      else begin
        log_and_print "Patching %s.ITM into store...\n" (String.uppercase item);
        (* Update the offsets by 28 bytes *)
        if ipurchasedoffset >= isaleoffset then write_int buff 0x2c (ipurchasedoffset + 28);
        if cureoffset >= isaleoffset then write_int buff 0x70 (cureoffset + 28);
        if drinksoffset >= isaleoffset then write_int buff 0x4c (drinksoffset + 28);
        (* Add 1 to the #items for sale *)
        write_int buff 0x38 (numisale + 1);
        let added = ref false in
        let buff_ref = ref (Str.string_before buff (isaleoffset)) in
        begin match where with
	        | TP_Store_Before store_pos_arg
	        | TP_Store_After store_pos_arg ->
	          let before_what = Var.get_string (eval_pe_str store_pos_arg) in
	          let before_what_list = Str.split (Str.regexp "[\t ]") before_what in
						let item_reg = List.map (fun x ->
	            let y = if String.length x > 8 then Str.string_before x 8 else x in
	            (y ^ String.make (8 - (String.length y)) '.')
	          ) before_what_list in
	          let item_reg = List.map Str.regexp_case_fold item_reg in
	          let item_string = Str.string_after (Str.string_before buff (isaleoffset + 28 * numisale)) isaleoffset in
	          let length = (String.length item_string) / 28 in
	          List.iter (fun this_test ->
	            if not !added then
	              for i = 0 to length - 1 do
	                let this_one_long = Str.string_before (Str.string_after item_string (i * 28)) 28 in
	                if not !added then begin
	                  let this_one = Str.string_before (this_one_long) 8 in
	                  if !debug_ocaml then log_and_print "Item %d is \"%s\"\n" i this_one ;
	                  if Str.string_match this_test this_one 0 then begin
	                    buff_ref := Printf.sprintf "%s%s%s" !buff_ref
	                    (* t_i_l i_b
	                       i_b t_i_l *)
	                      (if where = TP_Store_After store_pos_arg then this_one_long else item_buff)
	                      (if where = TP_Store_After store_pos_arg then item_buff else this_one_long) ;
	                    added := true
	                  end
	                  else buff_ref := !buff_ref ^ this_one_long
	                end else buff_ref := !buff_ref ^ this_one_long ;
	              done ;
	          ) item_reg ;
	          if not !added then log_and_print "Not found space for %s %s %s.\n" item (if where = TP_Store_After store_pos_arg
	              then "after" else "before") before_what ;
	        | _ -> ()
        end ;
        let fix x =
          let x = Int32.to_int (eval_pe buff game x) in
          if x > numisale then (log_and_print "ADD_STORE_ITEM AT %d out of range 0-%d - defaulting to %d" x numisale numisale;numisale)
          else if x < 0 then (log_and_print "ADD_STORE_ITEM AT %d out of range 0-%d - defaulting to 0" x numisale;0)
          else x
        in
        let after = Str.string_after buff (match !added, where with
          | (true,_)
          | (false,TP_Store_Last) -> isaleoffset + 28 * numisale
          | (false,TP_Store_At x) -> isaleoffset + 28 * (fix x)
					| _     -> isaleoffset
        ) in
        if not !added then begin
          let before = Str.string_before buff (match where with
            | TP_Store_Last -> isaleoffset + 28 * numisale
          	| TP_Store_At x -> isaleoffset + 28 * (fix x)
            | _ -> isaleoffset
          ) in
          buff_ref := before ^ item_buff
        end ;
        buff_ref := !buff_ref ^ after ;
        !buff_ref
      end

  | TP_Remove_Store_Item(items) ->

      (* Create the new item *)
      let items = List.map Var.get_string items in
      let items = List.map String.uppercase items in

      (* Read in the offsets that we need to update *)

      let drinksoffset = int_of_str_off buff 0x4c in
      let ipurchasedoffset = int_of_str_off buff 0x2c in
      let isaleoffset = int_of_str_off buff 0x34 in
      let cureoffset = int_of_str_off buff 0x70 in

      (* Read in the # of items for sale *)

      let numisale = int_of_str_off buff 0x38 in


      (* Grab all items *)

      if !debug_ocaml then log_and_print "Trying to String.sub buff %d %d\n" isaleoffset (ipurchasedoffset - isaleoffset);
      let items_string = String.sub buff isaleoffset (numisale * 28) in
      if !debug_ocaml then log_and_print "Done String.sub\n" ;
      let str_before = ref (Str.string_before buff isaleoffset) in
      let str_after = Str.string_after buff (isaleoffset + numisale * 28) in
      let delta = ref 0 in
      for i = 0 to numisale - 1 do
	      let item_buff = String.sub items_string (i * 28) 28 in
	      let item_name = get_string_of_size item_buff 0 8 in
	      if not (List.mem (String.uppercase item_name) items) then begin
	      	str_before := !str_before ^ item_buff
	      end else begin
	      	incr delta
	      end
      done;

      let buff = !str_before ^ str_after in

			(* Update the offsets by 28 bytes *)
			if ipurchasedoffset >= isaleoffset then write_int buff 0x2c (ipurchasedoffset - 28 * !delta);
			if cureoffset >= isaleoffset then write_int buff 0x70 (cureoffset - 28 * !delta);
			if drinksoffset >= isaleoffset then write_int buff 0x4c (drinksoffset - 28 * !delta);
			(* Add 1 to the #items for sale *)
			write_int buff 0x38 (numisale - !delta);
			buff

  | TP_Decompress(start,length,ulen,where) ->
  	let start = Int32.to_int (eval_pe buff game start) in
  	let length = Int32.to_int (eval_pe buff game length) in
  	let ulen = Int32.to_int (eval_pe buff game ulen) in
  	let outbuff = Cbif.uncompress buff start length ulen in
  	begin
  		match where with
  		| TP_DW_ReplaceFile -> outbuff
  		| TP_DW_Variable(name) ->
  				let name = eval_pe_str name in
  				Var.set_string name outbuff;
  				buff
  		| TP_DW_IntoFile(start,stop) ->
		  	let start = Int32.to_int (eval_pe buff game start) in
		  	let stop = Int32.to_int (eval_pe buff game stop) in
		  	(Str.string_before buff start) ^ outbuff ^ (Str.string_after buff stop)
  	end;

  | TP_Compress(start,length,level,where) ->
  	let start = Int32.to_int (eval_pe buff game start) in
  	let length = Int32.to_int (eval_pe buff game length) in
  	let level = Int32.to_int (eval_pe buff game level) in
  	let outbuff = Cbif.compress level buff start length in
  	begin
  		match where with
  		| TP_DW_ReplaceFile -> outbuff
  		| TP_DW_Variable(name) ->
  				let name = eval_pe_str name in
  				Var.set_string name outbuff;
  				buff
  		| TP_DW_IntoFile(start,stop) ->
		  	let start = Int32.to_int (eval_pe buff game start) in
		  	let stop = Int32.to_int (eval_pe buff game stop) in
		  	(Str.string_before buff start) ^ outbuff ^ (Str.string_after buff stop)
  	end;


	| TP_RebuildCreFile ->
		Cre.string_of_cre (Cre.cre_of_string buff)

	| TP_Extend_Mos(where,howMuch) ->
  	begin
   		let howMuch = Int32.to_int (eval_pe buff game howMuch) in
   		let where = String.uppercase (Var.get_string where) in
   		let (buff',xSizeP,ySizeP) = Mos.size_of_str buff in
			let first,duplicate = where.[0], Str.string_after where 1 in
			let isdup,duplicate = try true, int_of_string duplicate with _ -> false, 0 - 1 in
			let (nxSizeP, nySizeP) = match where with
				| "TOP" | "BOTTOM" | "VCENT" -> if ySizeP > howMuch then
					       failwith "EXTEND_MOS can't shrink" else xSizeP, howMuch
				| "LEFT" | "RIGHT" | "HCENT"  -> if xSizeP > howMuch then
					       failwith "EXTEND_MOS can't shrink" else howMuch, ySizeP
				| _ -> (match first,isdup with
						| 'V',true -> if ySizeP > howMuch then
					       failwith "EXTEND_MOS can't shrink" else xSizeP, howMuch
						| 'H',true -> if xSizeP > howMuch then
					       failwith "EXTEND_MOS can't shrink" else howMuch, ySizeP
						| _ -> failwith (Printf.sprintf "Unknown EXTEND_MOS mode: %s" where)
					)
			in
			if (nxSizeP,nySizeP) <> (xSizeP,ySizeP) then begin
				let old_mos = Mos.mos_of_str buff' in
				if !debug_ocaml then log_and_print "Extend_mos %s %d to a %dx%d (%c %b)\n" where howMuch xSizeP ySizeP first isdup;
				let startPos = match String.uppercase where with
					| "TOP"    -> (0,nySizeP - ySizeP)
					| "BOTTOM" -> (0,0)
					| "VCENT"  -> (0,(nySizeP - ySizeP)/2)
					| "LEFT"   -> (nxSizeP - xSizeP,0)
					| "RIGHT"  -> (0,0)
					| "HCENT"  -> ((nxSizeP - xSizeP)/2,0)
					| _ -> (0,0)
				in
				let (xStart,yStart) = startPos in
				let new_mos = Array.init nxSizeP (fun i -> (Array.make nySizeP (Mos.Pixel(0,0,0,0))) ) in
				let getY i =
					if isdup then begin
						if first = 'V' then begin
							if i < duplicate then i else i + nySizeP - ySizeP
						end else i
					end else i
				in
				let getX i =
					if isdup then begin
						if first = 'H' then begin
							if i < duplicate then i else i + nxSizeP - xSizeP
						end else i
					end else i
				in
				for i = 0 to xSizeP - 1 do
					for j = 0 to ySizeP - 1 do
						new_mos.(xStart + getX i).(yStart + getY j) <- old_mos.(i).(j);
					done
				done;
				if isdup then begin try
					if first = 'H' then begin
						for i = duplicate to duplicate + nxSizeP - xSizeP do
							for j = 0 to ySizeP - 1 do
								new_mos.(i).(j) <- old_mos.(duplicate + (i mod 100)).(j);
							done
						done
					end else begin
						for i = 0 to xSizeP - 1 do
							for j = duplicate to duplicate + nySizeP - ySizeP do
								new_mos.(i).(j) <- old_mos.(i).(duplicate + (j mod 100));
							done
						done
					end
				with _ -> () end;
				Mos.str_of_mos new_mos
			end else buff
  	end
  (* end: process_patch2 *)
  ) ()


