(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 232. *)

(* Kit Extraction Code *)

open BatteriesInit
open Util

let lines buffer = Str.split (Str.regexp "[\r\n]+") buffer
let words buffer = Str.split (Str.regexp "[ \t]+") buffer

let extract game o output_dir min_num = 
  let load name = 
    let buff, path = Load.load_resource "extract kit" game true name "2DA" in
    lines buff
  in 
  let kitlist_l  = load "KITLIST"  in 
  let clasweap_l = load "CLASWEAP" in 
  let abclasrq_l = load "ABCLASRQ" in 
  let abclsmod_l = load "ABCLSMOD" in 
  let abdcdsrq_l = load "ABDCDSRQ" in 
  let abdcscrq_l = load "ABDCSCRQ" in 
  let alignmnt_l = load "ALIGNMNT" in 
  let dualclas_l = load "DUALCLAS" in 
  let kittable_l = load "KITTABLE" in 
  let weapprof_l = load "WEAPPROF" in 
  let tfstweap_l = load "25STWEAP" in 
  let luabbr_l   = load "LUABBR" in

  let rec get_line_starting_with line_list s_regexp = match line_list with
    [] -> raise Not_found
  | hd :: tl -> 
      begin
	try
          let i = Str.search_forward s_regexp hd 0 in
          if i = 0 then hd
          else raise Not_found
	with _ -> get_line_starting_with tl s_regexp 
      end 
  in 
  let get_line line_list str = 
    let s_regexp = Str.regexp str in
    try
      get_line_starting_with line_list s_regexp
    with _ -> begin
      log_and_print "Kit.extract: cannot find line starting with ~%s~\n" str ;
      "!XXX YOU MUST FIX ME XXX!"
    end 
  in 

  let kittable_list = ref [] in 

  List.iter (fun ktl ->
    let ktw = words ktl in 
    if (List.length ktw >= 8) then begin
      List.iter (fun ktw ->
        try 
          Load.skip_next_load_error := true ; 
          let buff, _ = Load.load_resource "extract kit" game true ktw "2DA" in
          kittable_list := (ktw,(lines buff)) :: !kittable_list 
        with _ -> () 
		) ktw 
    end 
	    ) kittable_l ;

  let kittable_mentions id =
    let partial = List.filter (fun (nom, lines) ->
      List.exists (fun line ->
        let words = words line in 
        List.length words > 1 && (
        try int_of_string (List.nth words 1) == id
        with _ -> false)
		  ) lines
			      ) !kittable_list in
    List.map (fun (nom,_) -> nom) partial
  in 

  let get_col ll name =
    let where = ref (-1) in
    let res = ref [] in 
    List.iter (fun line ->
      let w = Array.of_list (words line) in
      if (!where = -1) then begin
        for i = 0 to (Array.length w) - 1 do 
          if w.(i) = name then where := (i+1)
        done 
      end else begin
        res := w.(!where) :: !res
      end 
	      ) ll ;
    List.rev !res
  in 

  let abils = Hashtbl.create 511 in 
  let process_ability buff =
    List.iter (fun line ->
      List.iter (fun word ->
        if (String.length word > 4 && word.[2] = '_') then begin
          let spl = Str.string_after word 3 in 
          Hashtbl.add abils spl true 
        end 
		) (words line)
	      ) (lines buff)
  in 

  List.iter (fun kitlist_line ->
    let kitlist_w = words kitlist_line in 
    try 
      if (List.length kitlist_w >= 9) && 
        int_of_string (List.hd kitlist_w) > min_num then begin  
          let id = int_of_string (List.hd kitlist_w) in 
          let name = List.nth kitlist_w 1 in
          let lower = List.nth kitlist_w 2 in
          let mixed = List.nth kitlist_w 3 in
          let help = List.nth kitlist_w 4 in
          let abilities = List.nth kitlist_w 5 in
          let prof = List.nth kitlist_w 6 in
          let unusable = List.nth kitlist_w 7 in
          let klass = List.nth kitlist_w 8 in
          log_and_print "Kit.extract: Processing %s\n" name ; 


          o (Printf.sprintf "ADD_KIT ~%s~\n" name) ;
          o (Printf.sprintf "~%s~\n" (get_line clasweap_l name)) ;
          o (Printf.sprintf "~%s" name) ;
          List.iter (fun w -> o (Printf.sprintf " %s " w))
            (get_col weapprof_l name) ;
          o (Printf.sprintf "~\n" ) ;
          List.iter (fun ll -> 
            o (Printf.sprintf "~%s~\n" (get_line ll name)) ;
		    ) [ abclasrq_l ; abclsmod_l ; abdcdsrq_l ; abdcscrq_l ;
			alignmnt_l ; dualclas_l ; ] ;

          let buff, _ = 
            try Load.load_resource "extract kit" game true abilities "2DA" 
            with _ -> Load.load_resource "extract kit" game true "clabfi01" "2DA" in
          let dst = Printf.sprintf "%s/%s.2DA" output_dir abilities in 
          o (Printf.sprintf "~%s~\n" dst) ; 
          let oc = Case_ins.perv_open_out_bin dst in
          output_string oc buff ;
          close_out oc ;
          process_ability buff; 

          o (Printf.sprintf "~" ) ;
          List.iter (fun s -> o (Printf.sprintf " %s " s))
            (kittable_mentions id) ;
          o (Printf.sprintf "~\n" ) ;

          o (Printf.sprintf "~%s %s~\n" unusable klass) ;

          let ll = try 
            let the_line = get_line luabbr_l name in
            let the_words = words the_line in
            List.nth the_words 1
          with _ -> "Fi0" 
          in 
          o (Printf.sprintf "~%s~\n" ll) ;

          o (Printf.sprintf "~" ) ;
          List.iter (fun w -> o (Printf.sprintf " %s " w)) 
            (get_col tfstweap_l name) ;
          o (Printf.sprintf "~\n" ) ;

          List.iter (fun str ->
            let i = int_of_string str in 
            let male = Tlk.pretty_print (Load.get_active_dialog game) i in 
            let female = Tlk.pretty_print_opt (Load.get_active_dialogf_opt game) i in
            if (female = "" || male = female) then 
              o (Printf.sprintf "SAY %s\n" male)
            else 
              o (Printf.sprintf "SAY %s %s\n" male female)
		    ) [ lower ; mixed ; help ] ; 

          o "\n\n/**************************************************************************/\n\n" 
	end 
    with e -> begin
      log_and_print "\nKit.extract: ~%s~\n\t%s\n\n" kitlist_line 
        (printexc_to_string e) ; exit 1 
    end 
	    ) kitlist_l ;

  Hashtbl.iter (fun k _ -> 
    try 
      let buff, _ = Load.load_resource "extract kit" game true k "SPL" in 
      let dst = Printf.sprintf "%s/%s.SPL" output_dir k in 
      let oc = Case_ins.perv_open_out_bin dst in
      output_string oc buff ;
      close_out oc ;
    with _ -> () 
	       ) abils ;
  Automate.automate game [output_dir] 0 o ;

  () 
