(* Note added due to LGPL terms.

This file was edited by Valerio Bigiani, AKA The Bigg, starting from
1 January 2006, according to FredSRichardson's diffs. All changes for 
this file are listed in diffs/src.load.ml.diff file, as the output 
of a diff -Bw -c -N command.

It was originally taken from Westley Weimer's WeiDU 185. *)

open Util
open Cbif

let registry_game_paths () = 
  let str_list = "." :: "../" :: "../../" :: "../../../" :: "../../../../" :: !Arch.registry_paths in
  List.map (fun str ->
    if str = "." then str else Case_ins.filename_dirname str
  ) str_list 

let game_paths = ref [] 

let add_game_path path = game_paths := (Str.global_replace (Str.regexp "[\\\\/]*$") "" path) :: !game_paths

let override_paths = ref [
  (* compton wants this gone *)
  (* "." ; *)
]

let ids_paths = ref []

let allow_missing = ref []

let cbifs_to_rem = Queue.create ()

let ok_missing file =
  let file = String.uppercase file in
  let rec check lst = match lst with
    [] -> false
  | hd :: tl -> if (String.uppercase hd) = file then true else
                check tl
  in check !allow_missing

let add_override_path path = override_paths := !override_paths @ [path]
let add_ids_path path = ids_paths := !ids_paths @ [path]

let dialog_tlk_path = ref None
let dialogf_tlk_path = ref None

let set_dialog_tlk_path s = dialog_tlk_path := Some(s) 
let set_dialogf_tlk_path s = dialogf_tlk_path := Some(s) 

(* old index, old value, old female value *)
type str_set_record = (int * Tlk.tlk_string * Tlk.tlk_string)

type script_style = BG1 | BG2 | IWD1 | IWD2 | PST | NONE

type game = {
  mutable key          : Key.key ;
          game_path    : string ;
          cd_path_list : string list ;
  mutable override_path_list : string list ;
  mutable ids_path_list : string list ;
  mutable loaded_biffs : (string, Biff.biff) Hashtbl.t ;
  mutable dialog       : Tlk.tlk ;
  mutable dialog_search : (string, int) Hashtbl.t ;
  mutable dialogf      : Tlk.tlk option ;
  mutable str_sets     : str_set_record list ;
    (* most recent STRING_SET or forced strref is the head of the list *)
          dialog_path  : string ;
          dialogf_path : string ;
  mutable dialog_mod   : bool ; (* changed? *)
  mutable dialogf_mod  : bool ; (* changed? *)
  mutable key_mod      : bool ; (* changed? *)
  mutable script_style : script_style ; 
} 

let saved_game = ref (None : game option) 

let the_game () = match !saved_game with
  | None -> parse_error "no game loaded" 
  | Some(g) -> g

let create_dialog_search g =
  Array.iteri (fun i t -> Hashtbl.add g.dialog_search t.Tlk.text i)
    g.dialog 

let append_strings g lse_q =

  Stats.time "add strings to TLK" (fun () -> 

    let num_entries = Queue.length lse_q in 

    let ma = Array.make num_entries Tlk.blank_tlk_string in
    let fa = Array.make num_entries Tlk.blank_tlk_string in

    let i = ref 0 in 

    let char_count = ref 0 in 

    Queue.iter (fun lse -> 
      let m, f = Tlk.lse_to_tlk_string lse in 

      char_count := (String.length m.Tlk.text) + !char_count ; 

      ma.(!i) <- m ;
      fa.(!i) <- f ;

      incr i; 

    ) lse_q ; 

    log_or_print "%d characters, %d entries added to DIALOG.TLK\n" 
      !char_count num_entries ; 

    Queue.clear lse_q ; 

    g.dialog <- Array.append g.dialog ma ;
    Hashtbl.clear g.dialog_search ; 
    create_dialog_search g ; 

    (match g.dialogf with
    | Some(a) -> g.dialogf <- Some(Array.append a fa) ;
                g.dialogf_mod <- true
    | None -> ()) ;

    g.dialog_mod <- true

  ) ()

exception FoundKey of Key.key * string

let load_null_game () =
  let d,dp = 
    match !dialog_tlk_path with
      Some(p) -> 
        (Tlk.load_tlk p),p
    | None -> Tlk.null_tlk () , " -- NO DIALOG.TLK -- " 
  in 
  let result = 
  {
    key = Key.null_key () ;
    game_path = " -- NO GAME -- " ;
    cd_path_list = [] ; 
    override_path_list = !override_paths ;
    ids_path_list = !ids_paths ;
    loaded_biffs = Hashtbl.create 1 ;
    dialog = d ; 
    dialog_search = Hashtbl.create (1 + ((Array.length d) * 2)) ;
    dialogf = None ; 
    dialog_path = dp ; 
    str_sets = [] ; 
    dialogf_path = " -- NO DIALOGF.TLK -- " ;
    dialog_mod = false ;
    dialogf_mod = false ;
    key_mod = false ;
    script_style = BG2 ;
  } in
  create_dialog_search result ;
  result 
  
(* ww: this looks for all case-variants of 'file', so 'CHITIN.KEY' and
 * 'chitin.key' both match! *) 
let find_file_in_path path file =
  try
  begin
    let h = Case_ins.unix_opendir path in
    let regexp = Str.regexp_case_fold file in 
    let res = ref None in  
    ( try
      while true do
        let f = Unix.readdir h in 
        if (Str.string_match regexp f 0) then 
          res := Some(f) 
      done
    with e -> () 
    ) ; Unix.closedir h ;
    match !res with
      Some(e) -> (path ^ "/" ^ e)
    | None -> (path ^ "/" ^ file)
  end 
  with _ -> path ^ "/" ^ file 

let load_game () =
  let key, gp =
  try
    List.iter (fun path ->
      let keyname = find_file_in_path path "^chitin.key$" in
      if file_exists keyname then begin
        let keybuff = load_file keyname in
        raise (FoundKey((Key.load_key keyname keybuff),path))
      end
    ) !game_paths ;
    log_and_print "\nERROR: Unable to find CHITIN.KEY in:\n" ;
    List.iter (fun path -> log_and_print "\t%s\n" path) !game_paths ;
    failwith "Unable to find CHITIN.KEY: run me in an Infinity Engine game directory"
  with FoundKey(k,gp) -> k, gp
  in
  let dt_path = match !dialog_tlk_path with
    Some(p) -> p
  | None -> find_file_in_path gp "^dialog.tlk$"
  in
  let dialog_tlk, dialog_path =
    if file_exists dt_path then begin
      (Tlk.load_tlk dt_path ),dt_path
    end else begin
      log_and_print "\nERROR: Unable to find DIALOG.TLK in:\n\t%s\n" dt_path ;
      log_and_print "\nPlease run this program in your Infinity Engine game directory.\n" ;
      failwith "Unable to find DIALOG.TLK"
    end
  in
  let cd_paths =
  try
    let s_d_h = Case_ins.unix_opendir gp in
    let sofar = ref [] in
    begin
    try
      while true do
        let s = Unix.readdir s_d_h in
        let base,ext = split s in
        if (String.uppercase ext) = "INI" then begin
          let buff = load_file (gp ^ "/" ^ s) in
          try
            let cd_regexp = Arch.cd_regexp in
            let i = ref 0 in
            while true do
              i := (Str.search_forward cd_regexp buff !i) + 1 ;
              let cd_path = Str.matched_group 1 buff in
              let cd_path_list = Str.split (Str.regexp ";") cd_path in
              List.iter (fun cd_path ->
                log_only "Possible HD/CD Path: [%s]\n" cd_path ;
                sofar := cd_path :: !sofar) cd_path_list ;
            done
          with _ -> ()
        end
      done
    with _ -> ()
    end ;
    !sofar
  with _ -> [ gp ^ "/CD1" ; gp ^ "/CD2" ; gp ^ "/CD3" ;
              gp ^ "/CD4" ; gp ^ "/CD5" ; gp ^ "/CD6" ] in
  let cd_paths = if Sys.os_type = "Unix" then cd_paths @ [ gp ^ "/CD1" ; gp ^ "/CD2" ; gp ^ "/CD3" ; gp ^ "/CD4" ; gp ^ "/CD5" ; gp ^ "/CD6" ] else cd_paths in
  let df_path = match !dialogf_tlk_path with
    Some(p) -> p
  | None -> find_file_in_path gp "dialogf.tlk"
  in
  let dialogf_tlk, dialogf_path =
    if file_exists df_path then begin
      let df = Tlk.load_tlk df_path in
      Some(df), df_path
    end else
      None, "(none)"
  in
  let result =
  {
    key = key ;
    game_path = gp ;
    cd_path_list = cd_paths ;
    override_path_list = !override_paths @ [ (gp ^ "/override") ] ;
    ids_path_list = !ids_paths @ !override_paths @ [ (gp ^ "/override") ];
    loaded_biffs = Hashtbl.create 5 ;
    dialog = dialog_tlk ;
    dialog_search = Hashtbl.create ((Array.length dialog_tlk) * 2) ;
    dialogf = dialogf_tlk ;
    str_sets = [] ; (* and keep it that way! :-) *)
    dialog_path = dialog_path ;
    dialogf_path = dialogf_path ;
    dialog_mod = false ;
    dialogf_mod = false ;
    key_mod = false ;
    script_style =
      begin
        try
          let _ = Key.find_resource key "SUBRACE" "IDS" in (Tlk.is_bg2 := false;IWD2)
        with Not_found ->
          begin
            try
              let _ = Key.find_resource key "BONES" "IDS" in (Tlk.is_bg2 := false;PST)
            with Not_found ->
              begin
                try
                  let _ = Key.find_resource key "CLOWNRAN" "IDS" in (Tlk.is_bg2 := false;IWD1)
                with Not_found -> 
									try
										let _ = Key.find_resource key "FLYTHR01" "MVE" in(Tlk.is_bg2 := true; BG2)
									with Not_found -> (Tlk.is_bg2 := false;BG1)
              end
          end
      end
  } in
  (match result.dialogf with
    Some(df) -> begin
      let dfl = Array.length df in
      let dl = Array.length result.dialog in
      if (dfl > dl) then begin
        let uneven = Array.sub df dl (dfl - dl) in
        log_and_print "*** %s has %d too few entries, padding.\n"
          result.dialog_path (dfl - dl) ;
        result.dialog <- Array.append result.dialog uneven ;
        result.dialog_mod <- true;
      end else if (dfl < dl) then begin
        let uneven = Array.sub result.dialog dfl (dl - dfl) in
        log_and_print "*** %s has %d too few entries, padding.\n"
          result.dialogf_path (dl - dfl) ;
        result.dialogf <- Some(Array.append df uneven) ;
        result.dialogf_mod <- true;
      end
    end
  | None -> ()
  )  ;
  create_dialog_search result ;
  result

exception FoundRes of string * string
exception Missing

let skip_next_load_error = ref false

let load_bif_in_game game bif_file =
    if Hashtbl.mem game.loaded_biffs bif_file then
      Hashtbl.find game.loaded_biffs bif_file (* already here *)
    else begin
      (* we must load the BIF *)
      let biff_path = begin
        let rec trial f lst =
          match lst with
            [] -> find_file_in_path game.game_path f
          | hd :: tl ->
              let perhaps = find_file_in_path hd f in
            log_only "BIFF may be in hard-drive CD-path [%s]\n" perhaps ; 
            if file_exists perhaps then 
              perhaps
              else trial f tl
      in
        (* Check to see if the bif file exists, if it doesn't try for a .CBF file *)
        let bf = trial bif_file (game.cd_path_list @ [ game.game_path ^ "/cache" ] ) in
        if file_exists bf then
          bf
        else begin
          let cbf = Case_ins.filename_chop_extension bif_file ^ ".cbf" in
          let cbf_file = trial cbf (game.cd_path_list) in
          if file_exists cbf_file then
            let cache_file = game.game_path ^ "/cache/" ^ bif_file in
            if not (file_exists cache_file) then Queue.add cache_file cbifs_to_rem;
            let sz = Cbif.cbf2bif cbf_file cache_file in
            let _ = log_and_print "[%s] decompressed bif file %d bytes\n" cbf_file sz in
            cache_file
          else
            bf
        end
      end in
      let the_biff = Biff.load_biff biff_path in
      Hashtbl.add game.loaded_biffs bif_file the_biff ;
      the_biff
    end

let find_in_key game name ext =
  let bif_file,bif_index,tis_index = Key.bif_of_resource game.key name ext in
  (* do we have that BIF already? *)
  let this_biff = load_bif_in_game game bif_file  in 
  (bif_file, bif_index, tis_index, this_biff)

let copy_resource game name ext oc = 
  let bif_file, bif_index, tis_index, this_biff = 
    find_in_key game name ext in 
  (if ext = "TIS" then Biff.copy_file this_biff (tis_index-1) oc true 
  else Biff.copy_file this_biff bif_index oc false) ;
  (game.game_path ^ "/" ^ bif_file)

let load_resource for_what game override_allowed name ext =
  let skip_this_error = !skip_next_load_error in
  skip_next_load_error := false ;
  let ext_up = String.uppercase ext in
  let full = name ^ "." ^ ext in
  let a,b =
  if (Case_ins.filename_is_implicit name) then begin
    try
      (* first, try all of the overrides *)
      if override_allowed then
        List.iter (fun op ->
          let path = op ^ "/" ^ full in
          if file_exists path then
            raise (FoundRes(load_file path, path))
        ) (if ext_up = "IDS" then game.ids_path_list else game.override_path_list) ;

      (* Now get it from the BIFs -- look it up in the KEY *)
      let bif_file, bif_index, tis_index, this_biff =
        find_in_key game name ext in
      (* get it out of the BIF *)
      if ext_up = "TIS" then
        (Biff.extract_tis this_biff (tis_index-1)) skip_this_error,(game.game_path ^ "/" ^ bif_file)
      else
        (Biff.extract_file this_biff bif_index) skip_this_error,(game.game_path ^ "/" ^ bif_file)
    with
    | (FoundRes(b,p)) -> b,p
    | Invalid_argument "String.create" -> raise (Invalid_argument "String.create")
    | _ ->
      if ok_missing (name ^ "." ^ ext) then
        "","(resource not found)"
      else begin
        if file_exists full then begin
          (load_file full, full)
        end else begin
        if not skip_this_error then begin
          log_and_print "\nERROR locating resource for '%s'\n" for_what ;
          let keypath = game.game_path ^ "/chitin.key" in
          log_and_print "Resource [%s.%s] not found in KEY file:\n\t[%s]\n"
            name ext keypath ;
        end ;
        failwith (Printf.sprintf "resource [%s.%s] not found for '%s'"
          name ext for_what)
        end
      end
  end else
    (load_file full), full
  in
  if ((ext_up = "IDS" || ext_up = "2DA") && Xor.is_encrypted a) then
    (Xor.decrypt a,b)
  else
    (a,b)

open Key

type effect = {
  opcode : int ;
  target : int ;
  arg1 : int ;
  arg2 : int ;
  resist_dispel : int ;
  raw_offset : int ;
}

let eff_of_eff buff =
    if String.sub buff 0 8 <> "EFF V2.0" then begin
      failwith "not a valid EFF v2.0 file (wrong sig)" 
    end ; 
    [|{ opcode = int_of_str_off buff 0x10 ;
      target = int_of_str_off buff 0x14 ;
      arg1 = int_of_str_off buff 0x1c ;
      arg2 = int_of_str_off buff 0x20 ;
      resist_dispel = 0  ;
      raw_offset = 0
    }|]

let eff_of_spl_itm buff =
  try
    let offset_loc = match String.sub buff 0 8 with
    | "SPL V1  " 
    | "ITM V1  " 
    | "ITM V2.0" 
    -> 0x6a
    | x -> failwith 
      (Printf.sprintf "not a valid SPL/ITM file (unknown signature [%s])" x)
    in 
    let offset_eff = int_of_str_off buff offset_loc in 
    let count_eff = ((String.length buff) - offset_eff) / 48 in 
    Array.init count_eff (fun i ->
      try 
        let base = offset_eff + (i * 48) in
        { opcode = short_of_str_off buff base ; 
          target = byte_of_str_off buff (base + 0x2) ;
          arg1 = int_of_str_off buff (base + 4) ;
          arg2 = int_of_str_off buff (base + 8) ;
          resist_dispel = byte_of_str_off buff (base + 0xd) ; 
          raw_offset = base ; 
        } 
      with e -> { opcode = 0; target = 0; arg1 = 0; arg2 = 0; 
        resist_dispel = 0; raw_offset = 0;} 
    ) 
  with e -> 
    Printf.printf "ERROR: %s\nERROR: Problem Listing Effects (perhaps not a BG2 resource?)\n" (Printexc.to_string e); 
    [| |] 

(*

let search_biff_contents_eff game o tl =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in 
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin
      try 
      let buff,path = 
        load_resource game true r.res_name (ext_of_key r.res_type) in
      let effs = match (ext_of_key r.res_type) with
      | "SPL" | "ITM" -> eff_of_spl_itm buff
      | _ -> [| |]
      in 
      Array.iter (fun eff ->
        if eff.target <> 0 && eff.target <> 1 &&
           eff.resist_dispel <> 1 then 
        let name_string_index = match (ext_of_key r.res_type) with
          "SPL" -> int_of_str_off buff 8 
        | "ITM" -> int_of_str_off buff 12 
        | _ -> -1 in 
        Printf.fprintf o "[%8s.%3s] %s does %s.\n"
          r.res_name ( ext_of_key r.res_type ) 
          (Tlk.pretty_print game.dialog name_string_index)
          (Eff_table.name_of_opcode eff.opcode) 
      ) effs 
      with _ -> ()
    end  ;
  ) game.key.resource 

let search_biff_contents_all game o tl sl =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in 
  let reg_list = List.map (fun s -> Str.regexp_case_fold s, s) sl in 
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin 
      try 
      let has = ref [] in 
      let buff,path = 
        load_resource game true r.res_name (ext_of_key r.res_type) in
      List.iter (fun (regexp,orig) -> 
        try
            let _ = Str.search_forward regexp buff 0 in
            has := orig :: !has 
        with _ -> ()) reg_list ;
      if !has <> [] then begin
        let name_string_index = int_of_str_off buff 8 in
        Printf.fprintf o "[%8s.%3s] %s has"
          r.res_name ( ext_of_key r.res_type ) 
          (Tlk.pretty_print game.dialog name_string_index) ;
        List.iter (fun s -> Printf.fprintf o " %s" s) !has ;
        Printf.fprintf o "\n"
      end
      with e -> ()
    end  ;
  ) game.key.resource 
  *)

let content_name_offset = ref None 

let search_biff_contents game o tl sl =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in 
  let reg_list = List.map Str.regexp_case_fold sl in 
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin (* type match *)
      try 
      let buff,path = load_resource "searching BIFF contents" game true r.res_name (ext_of_key r.res_type) in
      let matches_one = 
        List.fold_left (fun acc r -> acc ||
          try 
            let _ = Str.search_forward r buff 0 in
            true
          with _ -> false
        ) false reg_list 
      in
      if matches_one then begin 
        match !content_name_offset with
        | Some(off) when String.length buff >= off + 4 -> 
            let name_id = int_of_str_off buff off in 
            o (Printf.sprintf "%8s.%3s in [%s] matches [%s]\n"
              r.res_name ( ext_of_key r.res_type ) biff.filename 
              (Tlk.pretty_print game.dialog name_id))
        | _ -> o (Printf.sprintf "%8s.%3s in [%s] matches\n"
            r.res_name ( ext_of_key r.res_type ) biff.filename)
      end 
      with e -> ()
    end  ;
  ) game.key.resource 

let search_biff_contents_fun game o tl matches =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in 
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin (* type match *)
      try 
      let buff,path = load_resource "searching BIFF contents" game true r.res_name (ext_of_key r.res_type) in
      if matches buff then begin 
        match !content_name_offset with
        | Some(off) when String.length buff >= off + 4 -> 
            let name_id = int_of_str_off buff off in 
            o (Printf.sprintf "%8s.%3s in [%s] matches [%s]\n"
              r.res_name ( ext_of_key r.res_type ) biff.filename 
              (Tlk.pretty_print game.dialog name_id))
        | _ -> o (Printf.sprintf "%8s.%3s in [%s] matches\n"
            r.res_name ( ext_of_key r.res_type ) biff.filename)
      end 
      with e -> ()
    end  ;
  ) game.key.resource 

let file_exists_in_game game f =
  let old_allow_missing = !allow_missing in
  allow_missing := [] ;
  let res =
  (try
    let a,b = split f in
    skip_next_load_error := true;
    let buff,path = load_resource "FILE_EXISTS_IN_GAME" game true a b in
      (String.length buff > 0)
  with _ -> false ) in
  allow_missing := old_allow_missing ;
  res

