open Load
open Key
open Util
open Tlk
open Pretty

type il_entry = {
    file : string ;
    file_re : Str.regexp ;
    name : string ; 
    desc : string ;
    emit : bool ; 
    mutable stores : (string * int) list ; 
    mutable creatures : (string * int) list ; 
    mutable ndcreatures : (string * int) list ; 
    mutable dlgs : (string * int) list ; 
    mutable areas : string list ; 
    mutable rt_aliases : ( il_entry * string ) list ; 
    mutable random : string list ; 
  } 

let make_il_entry file name desc emit =
  { file = String.uppercase file ;
    file_re = Str.regexp_case_fold file ;
    name = name ;
    desc = desc ;
    emit = emit ;
    stores = [] ;
    creatures = [] ;
    ndcreatures = [] ;
    dlgs = [] ;
    areas = [] ;
    rt_aliases = [] ;
    random = [] ; 
  } 

let parse_rt_file name name2 ic il = 
  let _ = input_line ic in
  let _ = input_line ic in
  let _ = input_line ic in (* skip header *)

  let find_or_create_ile (nom : string) : il_entry = 
    let rec search lst = match lst with
      [] -> let new_rte = make_il_entry nom "" "" false in
      il := new_rte :: !il ;
      new_rte
    | hd :: tl -> if hd.file = nom then hd else search tl
    in 
    search !il
  in 

  let find_ile nom = 
    let rec search lst = match lst with
      [] -> None
    | hd :: tl -> if hd.file = nom then Some(hd) else search tl
    in 
    search !il
  in 

  (try 
    while true do
      let this_line = input_line ic in
      let words = Str.split (Str.regexp "[ \t\r\n]+") this_line in
      if words <> [] then begin 
        let the_alias = String.uppercase (List.hd words) in
        let the_drops = List.tl words in
        let alias_ile = find_or_create_ile the_alias in

        match the_drops with 
          [] -> ()
        | [drop] -> begin
            let drop = String.uppercase drop in 
            let ile = find_ile drop in
            match ile with
              Some(ile) -> 
		ile.rt_aliases <- (alias_ile, name) :: ile.rt_aliases ;
		log_or_print "Linking item %8s with random drop %8s%s\n" 
                  drop the_alias name
            | None -> () 
        end

        | many -> begin
            List.iter (fun drop ->
              let drop = String.uppercase drop in 
              let ile = find_ile drop in
              match ile with
                Some(ile) -> 
                  ile.random <- name2 :: ile.random ; 
                  log_or_print "Linking item %8s with random mode %s\n" 
                    drop name2
              | None -> ()
		      ) many ;
        end 
      end 
    done 
  with _ -> ()) ;
  ()

let handle_iwd game il =
  let list_of_random_tres_files = 
    [ ("RNDTRES", "-Rnd", "All Games") ;
      ("RT_NORM", "-Nrm", "Nrm") ;
      ("RT_FURY", "-HoF", "HoF") ;
    ] 
  in 
  List.iter (fun (res,name,name2) ->
    let buff,path = load_resource "itemlist" game true res "2DA" in 
    let oc = Case_ins.perv_open_out "weidu.temp" in
    Printf.fprintf oc "%s" buff ;
    close_out oc ;
    let ic = Case_ins.perv_open_in "weidu.temp" in
    parse_rt_file name name2 ic il ;
    close_in ic ;
    Case_ins.unix_unlink "weidu.temp"
	    ) list_of_random_tres_files 


let chap_of_string s = 0
(*
  try
  let prefix = String.sub s 0 2 in
  int_of_string prefix
  with _ -> 
  0 
 *)

let unique l = 
  let res = List.sort compare l in
  let rec filter l = match l with
  | [] -> []
  | a :: b :: tl when a = b -> filter (b :: tl)
  | hd :: tl -> hd :: (filter tl)
  in
  filter res 

let make_item_list game o = begin
  let itm_key = Key.key_of_ext true "ITM" in 
  let sto_key = Key.key_of_ext true "STO" in 
  let cre_key = Key.key_of_ext true "CRE" in 
  let are_key = Key.key_of_ext true "ARE" in 
  let dlg_key = Key.key_of_ext true "DLG" in 
  let il = ref [] in 

  let dlg_len = Array.length game.Load.dialog in
  let in_bounds idx = idx >= 0 && idx < dlg_len in

  Array.iter (fun r ->
    if (r.bif_index < 0 || r.bif_index >= (Array.length game.key.biff)) then
      ()
    else begin 
      let biff = game.key.biff.(r.bif_index) in
      if r.res_type = itm_key then begin
	try 
	  log_and_print "Loading %s.%s\n" r.res_name (ext_of_key r.res_type) ; 
	  let buff,path = 
            load_resource "itemlist" game true r.res_name (ext_of_key r.res_type) in
	  if (String.length buff >= 0x50 + 4) then begin 
	    let id_name_index = int_of_str_off buff 0xc in
	    let name_index = int_of_str_off buff 0x8 in 
	    let id_desc_index = int_of_str_off buff 0x54 in
	    let desc_index = int_of_str_off buff 0x50 in

	    let desc_idx = if id_desc_index = -1 then desc_index else id_desc_index in
	    let name_idx = if id_name_index = -1 then name_index else id_name_index in

	    let flags = int_of_str_off buff 0x18 in


	    if (in_bounds desc_idx && in_bounds name_idx &&
		(flags land 4 = 4 && flags land 8 = 8)) then begin
		  let il_entry = make_il_entry r.res_name 
		      game.Load.dialog.(name_idx).text
		      game.Load.dialog.(desc_idx).text
		      true 
		  in 
		  il := il_entry :: !il ; 
		end 
	  end 
	with _ -> () 
      end 
    end 
	     ) game.key.resource ;

  log_and_print "... got here!\n" ;

  let remove_regexp = Str.regexp "[\r\n]" in
  let get_string_from buff off =
    let id = int_of_str_off buff off in
    if id >= 0 && id < Array.length game.dialog then 
      Str.global_replace remove_regexp "" game.dialog.(id).text 
    else
      Printf.sprintf "???"  
  in 


(*
  handle_iwd game il ; 

 *)
  let max = Array.length game.key.resource in

  Array.iteri (fun i r ->
    let ext = ext_of_key r.res_type in 
    log_or_print "Iteration %5d/%5d %s.%s\n" i max r.res_name ext ; 
    if (r.res_type = cre_key ||
    r.res_type = are_key ||
    r.res_type = dlg_key ||
    r.res_type = sto_key ) then begin
      try 
	let buff,path = 
          load_resource "itemlist" game true r.res_name ext in

	if (String.length buff >= 16)  then 
	  List.iter (fun ile ->
            try
              let pos = Str.search_forward ile.file_re buff 0 in
              if r.res_type = cre_key then begin
		let itemflag = int_of_str_off buff (pos + 8) in 
		if itemflag land 1 <> 1 then 
		  ile.creatures <- 
                    ((get_string_from buff 0x8), chap_of_string r.res_name )
                    :: ile.creatures 
		else
		  ile.ndcreatures <- 
                    ((get_string_from buff 0x8), chap_of_string r.res_name )
                    :: ile.ndcreatures 
              end else if r.res_type = are_key then 
		ile.areas <- r.res_name :: ile.areas  
              else if r.res_type = dlg_key then 
		ile.dlgs <- (r.res_name, chap_of_string r.res_name) :: ile.dlgs 
              else if r.res_type = sto_key then 
		ile.stores <- 
                  ((get_string_from buff 0xc) , chap_of_string r.res_name) ::
                  ile.stores 
            with _ -> () 
		    ) !il ; 
      with _ -> ()
    end 
	      ) game.key.resource ;

  log_or_print "\n\nDone!\n\n" ; 

  let backr = (Str.regexp "[\r]") in
  let backn = (Str.regexp "[\n]") in
  let space = (Str.regexp " ") in

  let chapter_text i where =
    "" (*
	 if i = 0 then where
	 else Printf.sprintf " (Chp %d.%d%s)" (i / 10) (i mod 10) where
	*)
  in 

  il := List.sort (fun a b -> compare a.file b.file) !il ;

  let name_of_cre r =
    try let buff,path = load_resource "itemlist" game true r "CRE" in
    if String.length buff >= 16 then 
      ((get_string_from buff 0x8))
    else r
    with _ -> r
  in 

  let describe_locations ile = 
    let extra = ref false in 

    let stores = unique ((List.map (fun s -> s,"") ile.stores) @
			 (List.flatten (List.map (fun (ile,str) -> List.map (fun s -> s,str) ile.stores) ile.rt_aliases))) in
    let areas = unique ((List.map (fun s -> s,"") ile.areas) @
			(List.flatten (List.map (fun (ile,str) -> List.map (fun s -> s,str) ile.areas) ile.rt_aliases))) in
    let creatures = unique ((List.map (fun s -> s,"") ile.creatures) @
			    (List.flatten (List.map (fun (ile,str) -> List.map (fun s -> s,str) ile.creatures) ile.rt_aliases))) in
    let ndcreatures = unique ((List.map (fun s -> s,"") ile.ndcreatures) @
			      (List.flatten (List.map (fun (ile,str) -> List.map (fun s -> s,str) ile.ndcreatures) ile.rt_aliases))) in
    let dlgs = unique ((List.map (fun s -> s,"") ile.dlgs) @
		       (List.flatten (List.map (fun (ile,str) -> List.map (fun s -> s,str) ile.dlgs) ile.rt_aliases))) in
    let random = [] in (*
			 let random = unique ((List.map (fun s -> s) ile.random) @
			 (List.flatten (List.map (fun (ile,str) -> ile.random) ile.rt_aliases))) in
			*)

    if stores <> [] then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Purchased From: @[%a@]@!" 
		(docList (chr ',' ++ break) (fun ((elt,chp),where) -> 
		  dprintf "%s%s" elt (chapter_text chp where))) stores)
    end  ;
    if areas <> [] then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Found In: @[%a@]@!" (docList (chr ',' ++ break)
						       (fun (elt,where) -> dprintf "%s%s" elt where)) areas)
    end  ;
    if List.length creatures > 12 then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Carried By: More Than 12 Creatures@!")
    end else 
      if creatures <> [] then begin
	extra := true ; 
	ignore (Pretty.fprintf o "Carried By: @[%a@]@!" 
		  (docList (chr ',' ++ break) (fun ((elt,chp),where)-> 
		    dprintf "%s%s" elt (chapter_text chp where))) creatures)
      end  ;
    (*
      if ndcreatures <> [] then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Undroppable By: @[%a@]@!" 
      (docList (chr ',' ++ break) (fun ((elt,chp),where)-> 
      dprintf "%s%s" elt (chapter_text chp where))) ndcreatures)
      end  ;
     *)
    if dlgs <> [] then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Dialogues: @[%a@]@!" (docList (chr ',' ++
								 break) (fun ((elt,chp),where) -> dprintf "%s%s" (name_of_cre elt) 
								     (chapter_text chp where)))
		dlgs)
    end  ;
    if random <> [] then begin
      extra := true ; 
      ignore (Pretty.fprintf o "Random Drop: @[%a@]@!" (docList (chr ',' ++
								   break) (fun elt -> dprintf "%s" elt)) random)
    end  ;

    !extra
  in 

  List.iter (fun ile -> 
    if ile.emit then begin
      let desc = Str.global_replace backr "" ile.desc in 
      let desc = Str.global_replace backn " \n " desc in 
      let desc_list = Str.split space desc in 
      ignore (Pretty.fprintf o "[%s.ITM] %s@!@!  @[%a@]@!@!" ile.file ile.name 
		(docList (Nil) (fun elt -> if elt = "\n" then line else 
		(text elt ++ break))) desc_list) ;

      let extra = describe_locations ile in 

      if extra then 
        ignore (Pretty.fprintf o "@!---@!@!") 
      else 
        ignore (Pretty.fprintf o "---@!@!")  
    end
	    ) !il ; 

end

(* check exptable.2da *)
let make_xplist game o = begin
  let dlg_key = Key.key_of_ext true "DLG" in 
  let bcs_key = Key.key_of_ext true "BCS" in 
  let addxp = Str.regexp_case_fold "AddXPVar(\"\\([^\"]+\\)\",\\([0-9]+\\))" in 
  let addxpbcs = Str.regexp  
      "238OB[^A]*!\\([0-9]+\\) 0 0 0 0\"\\([^\"]+\\)\" \"\" AC"
  in 
  let replace_regexp = Str.regexp "[\r\n]" in
  let remove_regexp = Str.regexp "[\r\n]" in
  let get_string_from buff off =
    let id = int_of_str_off buff off in
    if id >= 0 && id < Array.length game.dialog then 
      Str.global_replace remove_regexp "" game.dialog.(id).text 
    else
      Printf.sprintf "???"  
  in 

  let name_of_cre r =
    try let buff,path = load_resource "itemlist" game true r "CRE" in
    if String.length buff >= 16 then 
      ((get_string_from buff 0x8))
    else r
    with _ -> r
  in 

  let exptable,path = load_resource "itemlist" game true "exptable" "2da" in

  let xp_of_label l =
    try
      let pos = Str.search_forward (Str.regexp_case_fold l) exptable 0 in
      let after = Str.string_after exptable pos in 
      Scanf.sscanf after "%s %s" (fun _ i -> i)
    with _ -> l 
  in 

  let sid = ref [] in 
  let skill_regexp = Str.regexp_case_fold
      "Check\\(Skill[^(]*\\)([^,]*,\\([^,]*\\),\\([^)]*\\)" in 

  Array.iteri (fun i r ->
    try begin 
      let ext = ext_of_key r.res_type in 

      if (r.res_type = dlg_key ||
      r.res_type = bcs_key) then begin
	let buff,path = 
          load_resource "itemlist" game true r.res_name ext 
	in
	let results = ref [] in 
	let i = ref 0 in 
	if r.res_type = dlg_key then 
	  (
	   try
             while true do
               i := (Str.search_forward addxp buff !i) +1 ;
               results := (Str.matched_group 1 buff, 
			   Str.matched_group 2 buff) :: !results
             done 
	   with _ -> ()
	  ) else if (r.res_type = bcs_key) then (
	    try
              let buff = Str.global_replace replace_regexp "!" buff in 
              while true do
		i := (Str.search_forward addxpbcs buff !i) +1;
		results := (Str.matched_group 2 buff, 
			    Str.matched_group 1 buff) :: !results
              done 
	    with _ -> () 
	   ) ;
	if !results <> [] then begin
          results := unique !results ; 
          let name = name_of_cre r.res_name in 
          ignore (Pretty.fprintf o "[%s.%s] %s@!%a@!" r.res_name 
		    (ext_of_key r.res_type)
		    name 
		    (docList (nil) (fun (a,b) -> 
		      dprintf "%s %s@!" 
			game.dialog.(int_of_string b).text (xp_of_label a))) !results) ;
          flush o ;
	end ;
	if (r.res_type = dlg_key) then begin
          let dlg = Dlg.load_dlg r.res_name buff in 
          Array.iter (fun s ->
            Array.iter (fun t ->
              match (t.Dlg.trans_trigger) with
		Some(s) -> begin
                  try
                    let pos = Str.search_forward skill_regexp s 0 in
                    let check_type = Str.matched_group 1 s in 
                    let check_amount = Str.matched_group 2 s in 
                    let check_skill = Str.matched_group 3 s in 
                    let t = match t.Dlg.trans_str with
                      Some(Dlg.TLK_Index(i)) -> game.Load.dialog.(i).text
                    | _ -> "<no text>"
                    in 
                    sid := (r.res_name, check_type, 
			    check_amount, check_skill, t) :: !sid ;
                  with _ -> () 
                end
              | None -> () 
		       ) s.Dlg.trans
		     ) dlg.Dlg.state 
	end 
      end
    end with e -> log_or_print "Exception %s\n" (Printexc.to_string e)
	      ) game.key.resource ;

  log_or_print "%d dlg-skill entires.\n" (List.length !sid) ;

  List.iter (fun (n,check_type,check_amount,check_skill,t) -> 
    ignore (Pretty.fprintf o "[%s.DLG] %s %s %s %s@!  @[%s@]@!@!"
	      n (name_of_cre n)
	      check_skill check_type check_amount 
	      t)
	    ) (unique !sid) ;

  ()

end 

let cre_analysis game o = begin
  let cre_key = Key.key_of_ext true "CRE" in 
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if r.res_type = cre_key then begin
      let buff,path = 
        load_resource "itemlist" game true r.res_name (ext_of_key r.res_type) in
      let items_offset = int_of_str_off buff 0x616 in 
      let num_items = int_of_str_off buff 0x61a in 
      let cre_name_index = int_of_str_off buff 0x8 in 
      for i = 0 to num_items - 1 do
        let myoffset = items_offset + (i * 20) in 
        let item_name = get_string_of_size buff myoffset 8 in 
        let flags = short_of_str_off buff (myoffset + 8) in 
        if flags land 1 = 1 then begin
          Printf.fprintf o "[%s] %s carries [%s]\n"
            r.res_name 
            game.Load.dialog.(cre_name_index).text
            item_name
        end 
      done 
    end 
	     ) game.key.resource ;

end 

let itm_randomizer game o = 
  let random_compare a b = 
    if Random.bool () then -1 else 1 
  in 

  let lst = ref [] in 
  let d_h = Case_ins.unix_opendir "." in
  let max_idx = Array.length game.Load.dialog in 
  (try while true do
    let f = Unix.readdir d_h in
    Printf.printf "Processing [%s]\n" f ; 
    try 
      let buff = load_file f in
      let price = int32_of_str_off buff 0x34 in 
      let max = short_of_str_off buff 0x38 in 
      let name_idx = int_of_str_off buff 0xC in
      let name = 
        if name_idx < 0 || name_idx >= max_idx
        then "?" else game.Load.dialog.(name_idx).text in 
      lst := (f,name,price,max) :: !lst 
    with _ -> () 
  done
  with _ -> () ) ;
  let one_list = List.filter (fun (f,n,p,m) -> m = 1) !lst in 
  let two_list = List.filter (fun (f,n,p,m) -> m > 1) !lst in 

  let process lst = 
    let l = List.length lst in 
    let twenty = 20 in 
    let lol = Array.create ((l / twenty)+1) [] in
    let bin = ref 0 in 
    let count = ref twenty in 
    let rec f lst = match lst with
      [] -> () 
    | hd :: tl -> begin 
        lol.(!bin) <- hd :: lol.(!bin) ;
        decr count ;
        if !count = 0 then begin
          count := twenty ;
          incr bin 
        end ;
        f tl 
    end 
    in 
    f lst ;
    Array.iter (fun lst ->
      let rnd_lst = List.sort random_compare lst in
      List.iter2 (fun (f1,n1,p1,m1) (f2,n2,p2,m2) -> 
        Printf.fprintf o "[%12s] %6ld -- [%12s] %6ld\n" f1 p1 f2 p2
		 ) lst rnd_lst ;
      Printf.fprintf o " ** \n" 
	       ) lol ;
  in
  let cost_compare (f,n,p,m) (f',n',p',m') = Int32.compare p p' in
  let one_sort = List.sort cost_compare one_list in
  let two_sort = List.sort cost_compare two_list in 
  let myfun (f,n,p,m) = 
    let f = "~" ^ f ^ "~" in 
    Printf.fprintf o "    %15s // %6ld ~%s~\n" f p n in
  List.iter myfun one_sort ; 
  List.iter myfun two_sort ; 
  ()
    


