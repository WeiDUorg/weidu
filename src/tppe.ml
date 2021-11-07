(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Functions to evaluate a patch expression
 *)

open BatteriesInit
open Hashtblinit
open Util
open Tp
open Parsewrappers
open Tpstate
open Tphelp

(************************************************************************
 * For handling the problem w/ REQUIRE_FILE & akin on bif
 * w/ multiple installs.
 ************************************************************************)
let bigg_file_exists file key =
  if Str.string_match (Str.regexp_case_fold "data.25.*\\.bif") file 0 then
    Key.bif_exists_in_key key (Load.fix_biff_path file)
  else
    file_exists file

let is_true i = i <> 0l
(* (Int32.compare i 0l) <> 0 *)
let if_true p = if p then 1l else 0l

let rec eval_pe_str s = match s with
| PE_LiteralString(s) -> (
	match (the_tp()).is_auto_eval_string with
	| false -> s
	| true -> Var.get_string s)
| PE_GetVar(p) -> (try Var.get_string_exact ("%" ^ eval_pe_str p ^ "%") with _ -> eval_pe_str p)
| PE_Evaluate(p) -> Var.get_string (eval_pe_str p)
| PE_Uppercase(s) -> String.uppercase (eval_pe_str s)
| PE_Lowercase(s) -> String.lowercase (eval_pe_str s)
| PE_Dollars(s,a,do_eval,do_add) ->
    let a = List.map (fun x -> Var.get_string (eval_pe_str x)) a in
    let s = Var.get_string (eval_pe_str s) in
    
    let array_text = "$\"" ^ s ^ "\"(" ^ (List.fold_left (fun acc this -> acc ^ (if acc = "" then "\"" else "\"\"") ^ this) "" a) ^ "\")" in
    List.iter (check_missing_eval ("array parameter at " ^ array_text)) a;
    check_missing_eval ("array name at " ^ array_text) s;
    let result = List.fold_left (fun acc this -> acc ^ "_" ^ this) s a in
    let result = if do_eval then
      ( try
	ignore (int_of_string result); result
      with _ ->
	try
	  Var.get_string_exact ("%" ^ result ^ "%")
	with _ -> result) else result
    in
    if do_add then (
      let old = try Hashtbl.find !Var.arrays s with Not_found -> [] in
      if not (List.mem a old) then Hashtbl.add !Var.arrays s (a :: old);
     );
    result

let eval_pe_tlk_str game s = match s with
| PE_Tlk(d) -> Dc.single_string_of_tlk_string game d
| PE_Pe(p) -> eval_pe_str p

exception FoundInt of int

let rec eval_pe buff game p =
  let bounds_check idx size retfun ef =
    let len = String.length buff in
    let out_of_bounds = (idx < 0 || (idx + size) > len) in
    match out_of_bounds with
    | false -> (retfun ())
    | true ->
	begin
	  log_and_print "ERROR: illegal %d-byte read from offset %d of %d-byte file\n" size idx len ;
	  failwith ("Read out of bounds")
	end
  in
  match p with
  | Pred_True -> 1l
  | PE_Int32 i -> i
  | PE_Int i -> Int32.of_int i
  | TP_PE_Byte_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 1
          (fun () -> Int32.of_int (byte_of_str_off buff where))
          (fun v -> (eval_pe buff game v)) in
      value
  | TP_PE_SByte_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 1
          (fun () -> Int32.of_int (signed_byte_of (byte_of_str_off buff where)))
          (fun v -> (eval_pe buff game v)) in
      value
  | TP_PE_Short_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 2
          (fun () -> Int32.of_int (short_of_str_off buff where))
          (fun v -> (eval_pe buff game v)) in
      value
  | TP_PE_SShort_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 2
          (fun () -> Int32.of_int (signed_short_of (short_of_str_off buff where)))
          (fun v -> eval_pe buff game v) in
      value
  | TP_PE_Long_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 4
          (fun () -> int32_of_str_off buff where)
          (fun v -> (eval_pe buff game v)) in
      value
  | TP_PE_SLong_At(where) ->
      let where = Int32.to_int (eval_pe buff game where) in
      let value = bounds_check where 4
          (fun () -> int32_of_str_off buff where)
          (fun v -> (eval_pe buff game v)) in
      value
  | PE_String(s) ->
      let s = eval_pe_str s in
      begin
	try
          Int32.of_string s
	with e ->
	  begin 
	    try
              Var.get_int32 ("%" ^ s ^ "%")
	    with e -> 
              begin
		try
		  Var.get_int32 s
		with e ->
		  begin
		    (if !eval_pe_warn then log_and_print
			"ERROR: cannot convert %s or %%%s%% to an integer\n" s s) ;
		    raise e
		  end 
              end
	  end 
      end
  | PE_VariableIsSet(s) ->
      let s = eval_pe_str s in
      if Hashtbl.mem !Var.variables s then 1l
      else if Hashtbl.mem !Var.variables ("%" ^ s ^ "%") then 1l
      else 0l

  | PE_TraEntryExists(s,tra_l) ->
    let s = Var.get_string (eval_pe_str s) in
	let tra_l = List.map Var.get_string (List.map eval_pe_str tra_l) in
	if tra_l <> [] then Dc.push_trans();
	List.iter handle_tra_filename tra_l;
    let old_eval_pe_warn = !eval_pe_warn in
	eval_pe_warn := false;
	let ans = begin try
	  ignore (Dc.single_string_of_tlk_string game (Dlg.Trans_String(Dlg.String s)));
	  1l
	with _ -> 0l end in
	eval_pe_warn := old_eval_pe_warn;
	if tra_l <> [] then Dc.pop_trans();
	ans
  
  | PE_IdsOfSymbol(file,entry) ->
      begin try
        let file = Var.get_string file in
        let entry = Var.get_string entry in
        Bcs.int_of_sym game file entry
      with _ -> -1l
      end


  | PE_Random(lb,ub) ->
      let lb = eval_pe buff game lb in
      let ub = eval_pe buff game ub in
      if lb > ub then 0l
      else if lb = ub then lb
	  (* random(3,5) = 3 + rand_zero_exclusive((5-3)+1) *)
      else Int32.add lb (Random.int32 (Int32.succ (Int32.sub ub lb)))

  | PE_String_Length(s) ->
      Int32.of_int (String.length (Var.get_string (eval_pe_str s)))

  | PE_Buffer_Length ->
      Int32.of_int (String.length buff)

  | PE_Index(from_end,case_sens,match_exact,what,start,where) ->
    let case_sens = match case_sens with
            None -> false
          | Some(x) -> x
    in
    let match_exact = match match_exact with
            None -> false
          | Some(x) -> x
    in
    let find = Var.get_string (eval_pe_str what) in
    let my_regexp = match case_sens, match_exact with
      false, false -> Str.regexp_case_fold        find
    | true , false -> Str.regexp                  find
    | false, true  -> Str.regexp_string_case_fold find
    | true , true  -> Str.regexp_string           find
    in
    let where = match where with
    | None -> buff
    | Some x -> Var.get_string (eval_pe_str x)
    in
    let start = match from_end, start with
    | false, None -> 0
    | true, None -> String.length where
    | _, Some x -> Int32.to_int (eval_pe where game x)
    in
    Int32.of_int (try (if from_end then Str.search_backward else Str.search_forward) my_regexp where start with _ -> 0 - 1)
      
  | PE_FileContainsEvaluated(filename, reg) ->
      begin
	let filename = eval_pe_str filename in 
	let reg = eval_pe_str reg in
	let filename = Var.get_string filename in
	let reg = Var.get_string reg in
	let old_allow_missing = !Load.allow_missing in 
	Load.allow_missing := [String.uppercase filename] ; 
	let answer = 
	  try
	    let buf = 
              if file_exists filename then load_file filename 
              else 
		let a,b = split_resref filename in
		Load.skip_next_load_error := true; 
		let buff,path = 
		  Load.load_resource "FILE_CONTAINS_EVALUATED" game true a b
		in buff
	    in
	    if buf = "" then 0l
	    else
	      let regexp = Str.regexp_case_fold reg in
              let _ = Str.search_forward regexp buf 0 in 
              1l
	  with Not_found -> 0l
	in
	Load.allow_missing := old_allow_missing ;
	answer
      end

  | PE_ResourceContains(filename, regexp) ->
      let filename = Var.get_string (eval_pe_str filename) in
      let regexp = Var.get_string (eval_pe_str regexp) in
      let res, ext = split_resref filename in
      let old_allow_missing = !Load.allow_missing in
      Load.allow_missing := [ String.uppercase_ascii filename ] ;
      let result =
        (try
          Load.skip_next_load_error := true ;
          let buff, _ = Load.load_resource "RESOURCE_CONTAINS"
              game true res ext in
          if buff = "" then 0l
          else
            let regexp = Str.regexp_case_fold regexp in
            let _ = Str.search_forward regexp buff 0 in
            1l
        with Not_found -> 0l) in
      Load.allow_missing := old_allow_missing ;
      result

  | Pred_File_MD5(f,s) -> if_true (
      let f = eval_pe_str f in
      let s = eval_pe_str s in
      if file_exists f then begin
        let digest = Digest.file f in
        let hex = Digest.to_hex digest in
        log_only "File [%s] has MD5 checksum [%s]\n" f hex ;
        (String.uppercase hex) = (String.uppercase s)
      end else begin
        false
      end)

  | Pred_File_Exists(f) -> if_true (
      let f = eval_pe_str f in
      let filename = (Var.get_string f) in
      bigg_file_exists filename game.Load.key  )

  | Pred_Directory_Exists(dir) -> if_true (
      let dir = Arch.backslash_to_slash (Var.get_string (eval_pe_str dir)) in
      (* Windows says foo/bar/ does not exist, so we make that foo/bar *)
      is_directory (Str.global_replace (Str.regexp "/$") "" dir))

  | Pred_File_Is_In_Compressed_Bif(f) -> if_true (
      let f = eval_pe_str f in
      let filename = (Var.get_string f) in
      let (a,b) = split_resref filename in
      try
        let (a,b,c,this_biff) = Load.find_in_key game a b in
        this_biff.Biff.compressed
      with _ -> false )
	  
  | Pred_Biff_Is_Compressed(f) -> if_true (
      let f = Var.get_string (eval_pe_str f) in
	  try
		(Load.load_bif_in_game game f).Biff.compressed
	  with _ -> false
    )

  | Pred_File_Exists_In_Game(f) -> if_true  (
      let f = Var.get_string (eval_pe_str f) in
      let res,ext = split_resref f in
      (try
        Load.resource_exists game res ext
      with _ -> false))

(*
      let old_allow_missing = !Load.allow_missing in
      Load.allow_missing := [] ;
      let res =
	(try
          let a,b = split f in
          Load.skip_next_load_error := true;
          let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME" game true a b in
          (String.length buff > 0) 
	with Failure _ -> false
	| Invalid_argument "String.create" -> true (* File is > 2^24 bytes *)
	| _ -> false
	) in
      Load.allow_missing := old_allow_missing ;
      res )
*)
  | Pred_File_Size(f,s) ->
      let f = eval_pe_str f in 
      if_true (file_size (Var.get_string f) = s)
  | Pred_File_Contains(f,r) -> if_true (
      let f = eval_pe_str f in
      let r = eval_pe_str r in
      let buf = load_file f in
      let regexp = Str.regexp_case_fold r in 
      try
        let _ = Str.search_forward regexp buf 0 in 
        true
      with Not_found -> false)

  | PE_StringRegexp(s1,s2,exact) ->
      let s1 = eval_pe_str s1 in
      let s2 = eval_pe_str s2 in
      let s1 = Var.get_string s1 in
      let s2 = Var.get_string s2 in
      let r = Str.regexp_case_fold s2 in
      let b = 
        if exact then Str.string_match r s1 0
        else
          try
            let _ = Str.search_forward r s1 0 in
            true
          with _ -> false
      in
      if b then 0l else 1l

  | PE_StringEqual(s1,s2,ignore_case,isbool) ->
      let s1 = eval_pe_str s1 in
      let s2 = eval_pe_str s2 in
      let s1 = Var.get_string s1 in
      let s2 = Var.get_string s2 in 
      let comparison = if ignore_case then 
        (fun s1 s2 -> String.compare (String.uppercase s1)
            (String.uppercase s2)) else String.compare
      in
      let result = Int32.of_int (comparison s1 s2) in
      let bigg_make_bool () =
        (if isbool then
          if result = Int32.of_int 0 then Int32.of_int 1 else Int32.of_int 0
        else
          result )
      in
      bigg_make_bool ()
  | PE_Add(a,b) -> let a = eval_pe buff game a in let b = eval_pe buff game b in Int32.add a b
  | PE_Sub(a,b) -> let a = eval_pe buff game a in let b = eval_pe buff game b in Int32.sub a b
  | PE_Mul(a,b) -> let a = eval_pe buff game a in let b = eval_pe buff game b in Int32.mul a b
  | PE_Div(a,b) -> let a = eval_pe buff game a in let b = eval_pe buff game b in 
    (try Int32.div a b with Division_by_zero -> Int32.zero)
  | PE_Mod(a,b) -> let a = eval_pe buff game a in let b = eval_pe buff game b in
    (try Int32.rem a b with Division_by_zero -> Int32.zero)
  | PE_Exp(a,b,c) -> 
      (try 
	let a = Int32.to_float (eval_pe buff game a) in
	let b = Int32.to_float (eval_pe buff game b) in
	let c = Int32.to_float (eval_pe buff game c) in
	let bc = b /. c in 
	let answer = a ** bc in
	Int32.of_float answer
      with _ -> 0l)

  | PE_Not(a) ->   if is_true (eval_pe buff game a)
  then Int32.zero else Int32.one
  | PE_And(a,b) -> if is_true (eval_pe buff game a) && is_true (eval_pe buff game b)
  then Int32.one else Int32.zero
  | PE_Or(a,b) -> if is_true (eval_pe buff game a) || is_true (eval_pe buff game b)
  then Int32.one else Int32.zero
  | PE_Equal(a,b) -> if Int32.compare (eval_pe buff game a) (eval_pe buff game b) = 0
  then Int32.one else Int32.zero
  | PE_GT(a,b) -> if Int32.compare (eval_pe buff game a) (eval_pe buff game b) > 0
  then Int32.one else Int32.zero
  | PE_GTE(a,b) -> if Int32.compare (eval_pe buff game a) (eval_pe buff game b) >= 0
  then Int32.one else Int32.zero
  | PE_LT(a,b) -> if Int32.compare (eval_pe buff game a) (eval_pe buff game b) < 0
  then Int32.one else Int32.zero
  | PE_LTE(a,b) -> if Int32.compare (eval_pe buff game a) (eval_pe buff game b) <= 0
  then Int32.one else Int32.zero

  | PE_BAND(a,b) -> Int32.logand (eval_pe buff game a) (eval_pe buff game b)
  | PE_BOR(a,b) -> Int32.logor (eval_pe buff game a) (eval_pe buff game b)
  | PE_BXOR(a,b) -> Int32.logxor (eval_pe buff game a) (eval_pe buff game b)
  | PE_BNOT(a) -> Int32.lognot (eval_pe buff game a)
  | PE_ABS(a) -> Int32.abs (eval_pe buff game a)
  | PE_BLSL(a,b) -> Int32.shift_left (eval_pe buff game a) (Int32.to_int (eval_pe buff game b))
  | PE_BLSR(a,b) -> Int32.shift_right_logical (eval_pe buff game a) (Int32.to_int (eval_pe buff game b) )
  | PE_BASR(a,b) -> Int32.shift_right (eval_pe buff game a) (Int32.to_int (eval_pe buff game b) )

  | PE_If(p,t,e) -> if is_true (eval_pe buff game p) then eval_pe buff game t else eval_pe buff game e
  | PE_ModIsInstalled(filename,number) -> let filename = Var.get_string filename in
    let number = Int32.to_int (eval_pe buff game number) in
    if_true (already_installed filename number && not (temporarily_uninstalled filename number))
  | PE_IsInstalledAfter(filename1,number1,filename2,number2) ->
	let filename1 = Var.get_string (eval_pe_str filename1) in
	let filename2 = Var.get_string (eval_pe_str filename2) in
	let number1 = Int32.to_int (eval_pe buff game number1) in
	let number2 = Int32.to_int (eval_pe buff game number2) in
	if_true (installed_after filename1 number1 filename2 number2)

  | PE_IdOfLabel(filename,name) ->
      let filename = Var.get_string (eval_pe_str filename) in
      let tp2s = List.filter Util.file_exists
          (Util.all_possible_tp2s
             (Util.tp2_name (Case_ins.filename_basename (try
               Case_ins.filename_chop_extension filename with _ ->
                 filename)))) in
      let name = Var.get_string (eval_pe_str name) in
      Int32.of_int (match tp2s with
      | tp2 :: rest -> (match get_id_of_label
            (Parsewrappers.handle_tp2_filename_caching tp2 true) name with
        | None -> Int32.to_int Int32.min_int
        | Some x -> x)
      | [] -> Int32.to_int Int32.min_int)

  | PE_GameIs(game_list,isGameCheck) -> begin
      let game_list = Str.split many_whitespace_regexp (Var.get_string game_list) in
      let f x = (eval_pe buff game (Pred_File_Exists_In_Game (PE_LiteralString x))) = 1l in
      let tutu     = if isGameCheck then f "fw0125.are" else false in
      let  bgt     = if isGameCheck then f "ar7200.are" else false in
      let   ca     = if isGameCheck then f "tc1300.are" else false in
      let iwdinbg2 = if isGameCheck then f "ar9201.are" else false in
      let eet = if isGameCheck then f "eet.flag" else false in
      let  bg2 = f "ar0083.are"   in
      let  tob = f "ar6111.are"   in
      let iwd2 = f "ar6050.are"   in
      let  pst = f "ar0104a.are"  in
      let  bg1 = f "ar0125.are"   in
      let tosc = f "ar2003.are"   in
      let iwd1 = f "ar2116.are"   in
      let  how = f "ar9109.are"   in
      let tolm = f "ar9715.are"   in
      let ttsc = f "fw2003.are"   in
      let bgee = f "oh1000.are"   in
      let bg2ee = f "oh6000.are"  in
      let iwdee = f "howparty.2da" in
      let pstee = f "pstchar.2da" in
      let res = List.exists (fun this ->
        match String.uppercase this with
        | "BG2"
        | "SOA"        -> bg2 && not tutu && not tob && not ca && not iwdinbg2
        | "TOB"        -> bg2 && not tutu &&     tob && not ca && not iwdinbg2 && not bg2ee
        | "IWD2"       -> iwd2
        | "PST"        -> pst && not pstee
        | "BG1"        -> bg1 && not tosc && not bg2
        | "TOTSC"      -> bg1 &&     tosc && not bg2 && not iwd1 && not bgee
        | "IWD"
        | "IWD1"       -> iwd1 && not how && not tolm && not bg2
        | "HOW"        -> iwd1 &&     how && not tolm && not bg2
        | "TOTLM"      -> iwd1 &&     how &&     tolm && not bg2 && not iwdee
        | "TUTU"       -> tutu && not ttsc
        | "TUTU_TOTSC"
        | "TUTU+TOTSC" -> tutu &&     ttsc
        | "BGT"        -> bgt
        | "CA"         -> ca
        | "IWD-IN-BG2"
        | "IWD_IN_BG2"
        | "IWDINBG2"   -> bg2 && iwdinbg2
        | "BG2EE"      -> bg2ee && not eet
        | "BGEE"       -> bgee && not bg2ee && not eet
        | "IWDEE"      -> iwdee
        | "PSTEE"      -> pstee
        | "EET"        -> eet
        | _ -> log_and_print "WARNING: No rule to identify %s\n" (String.uppercase this) ; false
      ) game_list in
      if res then 1l else 0l;
  end

  | PE_GameIncludes(game_set) -> begin
      let bg1 = ["BG1"; "TOTSC"; "TUTU"; "TUTU_TOTSC"; "BGT"; "BGEE"; "EET"] in
      let totsc = ["TOTSC"; "TUTU_TOTSC"; "BGT"; "BGEE"; "EET"] in
      let soa = ["SOA"; "TOB"; "BGT"; "BG2EE"; "EET"] in
      let tob = ["TOB"; "BGT"; "BG2EE"; "EET"] in
      let pst = ["PST"; "PSTEE"] in
      let iwd = ["IWD"; "HOW"; "TOTLM"; "IWD_IN_BG2"; "IWDEE"] in
      let how = ["HOW"; "TOTLM"; "IWD_IN_BG2"; "IWDEE"] in
      let totlm = ["TOTLM"; "IWD_IN_BG2"; "IWDEE"] in
      let iwd2 = ["IWD2"] in
      let ca = ["CA"] in
      (match String.uppercase game_set with
      | "SOD" -> eval_pe "" game (PE_ResourceContains
                                    (PE_LiteralString "CAMPAIGN.2DA",
                                     PE_LiteralString "SOD"))
      | _ -> begin
          let list = (match String.uppercase game_set with
          | "BG1" -> bg1
          | "TOTSC" -> totsc
          | "BG2"
          | "SOA" -> soa
          | "TOB" -> tob
          | "PST" -> pst
          | "IWD1"
          | "IWD" -> iwd
          | "HOW" -> how
          | "TOTLM" -> totlm
          | "IWD2" -> iwd2
          | "CA" -> ca
          | _ -> log_and_print "WARNING: GAME_INCLUDES has no rule for %s\n" (String.uppercase game_set) ; [(String.uppercase game_set)]) in
          eval_pe buff game (PE_GameIs((String.concat " " list), true))
      end)
  end

  | PE_IsAnInt(x) -> let old_eval_pe_warn = !eval_pe_warn in (eval_pe_warn := false ;
		      try
			let _ = (eval_pe buff game (PE_String(x))) in
			eval_pe_warn := old_eval_pe_warn;
			1l
		      with _ ->
			eval_pe_warn := old_eval_pe_warn;
			0l
		     )
  | PE_IsSilent -> if (!be_silent) then 1l else 0l
  
  | PE_Resolve_Str_Ref(lse) ->
	let new_index = match Dc.resolve_tlk_string game lse with
          Dlg.TLK_Index(i) -> i
	  | _ -> log_and_print "ERROR: cannot RESOLVE_STR_REF\n" ; failwith "resolve"
	in
    Int32.of_int new_index	

  | PE_SizeOfFile(file) ->
      let file = Arch.backslash_to_slash (Var.get_string (eval_pe_str file)) in
      if Hashtbl.mem inlined_files file then
        Int32.of_int (String.length (Hashtbl.find inlined_files file))
      else
        Int32.of_int (file_size file) ;

  | PE_StateWhichSays(lse,traref,file) -> begin
      let lookforit game lse lst =
        let rec lookforit game lse lst =
          if !debug_ocaml then log_and_print "in lookforit \n";
          try (match lst with
          | [] -> []
          | hd :: tl -> looksingle game lse hd; lookforit game lse tl
              ) with
          | FoundInt(i) -> i :: lookforit game lse (List.tl lst)
          | e -> raise e
        and looksingle game lse this =
          if !debug_ocaml then log_and_print "looking in %s\n" this;
          handle_tra_filename this ;
          try
            ( match Dc.resolve_tlk_string_internal false false game lse
            with
            | Dlg.TLK_Index(i) -> if !debug_ocaml then begin
                let str = Dc.single_string_of_tlk_string game (Dlg.TLK_Index(i)) in
                let str = Var.get_string str in
                log_and_print "\n%d - %s\n" i str
            end ; raise(FoundInt i)
            | _ -> ();
             )
          with
            FoundInt(i) -> raise (FoundInt i)
          | a -> if !debug_ocaml then log_and_print "%s\n" (printexc_to_string a)
        in
        
        Dc.push_copy_trans_modder ();
        try
          let ans = lookforit game lse lst in
          Dc.pop_trans ();
          ans
        with e ->
          Dc.pop_trans ();
          raise e
      in
      let theref = match traref with
      | None (*going by tra + @ref*) ->
          begin try
            ( match Dc.resolve_tlk_string_internal false true game (value_of_option lse)
            with
            | Dlg.TLK_Index(i) -> [i]
            | _ -> log_and_print "ERROR: cannot resolve SAY patch\n" ; failwith "resolve"
             )
          with Not_found -> [0 - 3]
          | e -> raise e
          end
      | Some(num,file) (* going by hard string *) ->
          begin
            let lst = try
              let regexp = Str.regexp "%s" in
              let addr = Str.search_forward regexp (Var.get_string file) 0 in
              let parts = Str.split regexp file in
              if List.length parts > 2 then begin
                log_and_print "Can't use more than one %%s in STATE_WHICH_SAYS ... IN\n" ;
                failwith "STATE_WHICH_SAYS %s/%s"
              end ;
              let dh = Case_ins.unix_opendir (List.nth parts 0) in
              let rec readdir dh =
                try
                  let x = Unix.readdir dh in
                  if !debug_ocaml then log_and_print "%s-\n" x;
                  if (Case_ins.unix_stat ((List.nth parts 0) ^ x)).Unix.st_kind = Unix.S_DIR &&
                    (List.nth parts 0) <> "." && (List.nth parts 0) <> ".." then
                    ((List.nth parts 0) ^ x ^ (List.nth parts 1)) :: readdir dh
                  else readdir dh
                with End_of_file -> Unix.closedir dh ;[]
              in
              readdir dh
            with
            | Not_found -> [file]
            | e -> raise e
            in
            if !debug_ocaml then List.iter (fun x -> log_and_print "%s\n" x) lst;
            let x = begin try
              lookforit game (Dlg.Trans_String(Dlg.Int(Int32.to_int(eval_pe buff game num)))) lst
            with
            | Not_found -> [0 - 3]
            | e -> Dc.pop_trans ();  raise e
            end
            in
            x
          end
      in
      if !debug_ocaml then log_and_print "Looking for these: \n" ;
      if !debug_ocaml then List.iter (fun x -> log_and_print "%d\n" x) theref ;
      if theref = [] then log_and_print "Couldn't find any suitable string in STATE_WHICH_SAYS\n";
      if theref = [] then failwith "resolve" ;
      if theref = [0 - 3] then Int32.of_int (0 - 3) else begin
        let (a,b) = split_resref (Var.get_string file) in
        let buff,path = Load.load_resource "STATE_WHICH_SAYS" game true a b in
        if !debug_ocaml then log_and_print "%s \n" path ;
        let numstates = int_of_str_off buff 8  in
        let stateoff  = int_of_str_off buff 12 in
        let whichstate = ref (0 - 1) in
        let howmany = ref 0 in
        for i = 0 to numstates - 1 do
          let this_say = int_of_str_off buff ((16 * i) + stateoff) in
          if List.mem this_say theref then whichstate := i ;
          if List.mem this_say theref && !debug_ocaml then log_and_print "Found number %d at state %d\n" this_say i;
          if List.mem this_say theref then incr howmany ;
        done ;
        if !howmany = 0 then Int32.of_int (0 - 1) else if !howmany > 1 then Int32.of_int (0 - 2) else
        Int32.of_int !whichstate
      end
  end

  | PE_NextStrref -> Int32.of_int !Dc.cur_index
  | PE_ValidScriptActions(buff) -> begin
      let buff = Var.get_string (eval_pe_str buff) in
      (try
        let _ = Parsewrappers.handle_script_al buff in
        1l
      with _ -> 0l)
  end
  | PE_ValidScriptTriggers(buff) -> begin
      let buff = Var.get_string (eval_pe_str buff) in
      (try
        let _ = Parsewrappers.handle_script_tl buff in
        1l
      with _ -> 0l)
  end

let eval_pe buff game pe =
  let res = Stats.time "eval_pe" (fun () -> eval_pe buff game pe) () in
  (if !debug_pe then log_and_print "Value [%s] = %ld\n"
      (pe_to_str pe) res ) ;
  res


let string_of_pe buff game pe =
  eval_pe_warn := false ;
  let value = try
      let x = (eval_pe buff game pe) in
      Int32.to_string x
    with _ ->
    begin
      match pe with
        | PE_String(x) -> Var.get_string (eval_pe_str x)
        | _ -> (eval_pe_warn := true ; ignore (eval_pe buff game pe) ; "")
    end
  in
  eval_pe_warn := true ; 
  value
  
let engine_is str =
  1l = eval_pe "" (Load.the_game()) (PE_GameIs (str,false))
  
