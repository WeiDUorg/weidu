(* Functions to evaluate a patch expression
 *)

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
  let test = ref false in
  if Str.string_match (Str.regexp_case_fold "data.25.*\\.bif") file 0 then
  begin
(*    log_and_print "\nbigg_file_exist special case\n" ; *)
    test := Key.bif_exists_in_key key (Str.global_replace (Str.regexp "[\\/:]")
            Arch.biff_path_separator file)
  end
  else begin
(*    log_and_print "\nbigg_file_exist special case not triggered: %s\n" file ; *)
    test := file_size file >= 0
  end ;
  !test


let is_true i = i <> 0l
(* (Int32.compare i 0l) <> 0 *)
let if_true p = if p then 1l else 0l

let eval_pe_warn = ref true

let rec eval_pe_str s = match s with
  | PE_LiteralString(s) -> s
  | PE_Evaluate(p) -> Var.get_string (eval_pe_str p)
  | PE_Uppercase(s) -> String.uppercase (eval_pe_str s)
  | PE_Lowercase(s) -> String.lowercase (eval_pe_str s)
  | PE_Dollars(s,a,do_eval,do_add) ->
	  let a = List.map (fun x -> Var.get_string (eval_pe_str x)) a in
		let s = eval_pe_str s in
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
          let a,b = split filename in 
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

  | Pred_File_Is_In_Compressed_Bif(f) -> if_true (
      let f = eval_pe_str f in
      let filename = (Var.get_string f) in
      let (a,b) = split filename in
      try
        let (a,b,c,this_biff) = Load.find_in_key game a b in
        this_biff.Biff.compressed
      with _ -> failwith (Printf.sprintf "couldn't find file %s in a biff" f) )

  | Pred_File_Exists_In_Game(f) -> if_true  (
      let f = eval_pe_str f in
      let old_allow_missing = !Load.allow_missing in
      let f = Var.get_string f in
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
			if_true (already_installed filename number &&
             not (temporarily_uninstalled filename number))
  | PE_GameIs(game_list,game_or_engine) -> begin
      let game_list = Str.split many_whitespace_regexp (Var.get_string game_list) in
      let f x = (eval_pe buff game (Pred_File_Exists_In_Game (PE_LiteralString x))) = 1l in
      let tutu = if game_or_engine then f "fw0125.are" else false in
      let  bgt = if game_or_engine then f "ar7200.are" else false in
      let   ca = if game_or_engine then f "tc1300.are" else false in
      let  bg2 = f "ar0803.are"   in
      let  tob = f "ar6111.are"   in
      let iwd2 = f "ar6050.are"   in
      let  pst = f "ar0104a.are"  in
      let  bg1 = f "ar0125.are"   in
      let tosc = f "ar2003.are"   in
      let iwd1 = f "ar2116.are"   in
      let  how = f "ar9109.are"   in
      let tolm = f "ar9715.are"   in
      let ttsc = f "fw2003.are"   in
      let res = List.exists (fun this ->
          match String.uppercase this with
          | "BG2"
          | "SOA"        -> bg2 && not tutu && not tob && not ca
          | "TOB"        -> bg2 && not tutu &&     tob && not ca
          | "IWD2"       -> iwd2
          | "PST"        -> pst
          | "BG1"        -> bg1 && not tosc && not bg2
          | "TOTSC"      -> bg1 &&     tosc && not bg2 && not iwd1
          | "IWD"
          | "IWD1"       -> iwd1 && not how && not tolm
          | "HOW"        -> iwd1 &&     how && not tolm
          | "TOTLM"      -> iwd1 &&     how &&     tolm
          | "TUTU"       -> tutu && not ttsc
          | "TUTU_TOTSC"
          | "TUTU+TOTSC" -> tutu &&     ttsc
          | "BGT"        -> bgt
	  | "CA"         -> ca
					| _ -> failwith (Printf.sprintf "No rule to identify %s" (String.uppercase this))
      ) game_list in
      if res then 1l else 0l;
    end
  | PE_IsAnInt(x) -> (eval_pe_warn := false ;
    try
      let _ = (eval_pe buff game (PE_String(x))) in
      1l
    with _ ->
      0l
    )
  | PE_StateWhichSays(lse,traref,file) -> begin
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
        Dc.push_copy_trans_modder () ;
        handle_tra_filename this ;
        try
            ( match Dc.resolve_tlk_string_internal false false game lse
              with
                | Dlg.TLK_Index(i) -> Dc.pop_trans (); if !debug_ocaml then begin
                  let str = Dc.single_string_of_tlk_string game (Dlg.TLK_Index(i)) in
                  let str = Var.get_string str in
                  log_and_print "\n%d - %s\n" i str
                end ; raise(FoundInt i)
                | _ -> Dc.pop_trans ();
            )
          with
            FoundInt(i) -> raise (FoundInt i)
            | a -> if !debug_ocaml then log_and_print "%s\n" (Printexc.to_string a)
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
              lookforit game (Dlg.Trans_String(Int32.to_int(eval_pe buff game num))) lst
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
        let (a,b) = split (Var.get_string file) in
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

let eval_pe buff game pe =
  let res = Stats.time "eval_pe" (fun () -> eval_pe buff game pe) () in
  (if !debug_pe then log_and_print "Value [%s] = %ld\n"
    (pe_to_str pe) res ) ;
  res

