(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   1 January 2006, according to FredSRichardson's diffs. All changes for
   this file are listed in diffs/src.load.ml.diff file, as the output
   of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

open BatteriesInit
open Hashtblinit
open Util
open Cbif

let registry_game_paths () =
  let str_list = "." :: "../" :: "../../" :: "../../../" ::
    "../../../../" :: !Arch.registry_paths in
  List.map (fun str ->
    if str = "." then str else Case_ins.filename_dirname str) str_list

let game_paths = ref []

let override_paths = ref [
  (* compton wants this gone *)
  (* "." ; *)
]

let ids_paths = ref []

let allow_missing = ref []

let cbifs_to_rem = Queue.create ()

let add_game_path path =
  game_paths := (Str.global_replace (Str.regexp "[\\\\/]*$")
                   "" path) :: !game_paths

let add_override_path path = override_paths := !override_paths @ [path]
let add_ids_path path = ids_paths := !ids_paths @ [path]

let add_gemrb_path file =
  let lines = Util.read_lines file in
  ignore (List.iter (fun line ->
    let parts = List.map String.trim (String.split_on_char '=' line) in
    (match parts with
    | left :: right :: [] when String.equal left "GemRB_Data_Path" ->
        let path = Case_ins.fix_name right in
        if (Util.is_directory path) then begin
          log_only "Adding GemRB data path: [%s]\n" path ;
          add_override_path path ;
          add_override_path (path ^ "/../shared/")
        end else
          log_and_print "WARNING: GemRB path is not a directory: [%s]\n" path ;
    | _ -> ())) lines)

let ok_missing file =
  let file = String.uppercase file in
  let rec check lst = match lst with
  | [] -> false
  | hd :: tl -> if (String.uppercase hd) = file then true else
    check tl
  in check !allow_missing

type tlk = {
    mutable contents : Tlk.tlk ;
    path : string ;
  }
and tlk_pair = {
    dialog : tlk ;
    dialogf : tlk option ;
    mutable dialog_mod : bool ; (* changed? *)
    mutable dialogf_mod : bool ;
    mutable loaded : bool ;
  }

let dialog_tlk_path : string option ref = ref None
let dialogf_tlk_path : string option ref = ref None

let set_dialog_tlk_path s = dialog_tlk_path := Some(s)
let set_dialogf_tlk_path s = dialogf_tlk_path := Some(s)

(* old index, old value, old female value *)
type str_set_record = (int * Tlk.tlk_string * Tlk.tlk_string)

type script_style = BG1 | BG2 | IWD1 | IWD2 | PST | NONE

type game = {
    mutable key : Key.key ;
    game_path : string ;
    mutable cd_path_list : string list ;
    mutable override_path_list : string list ;
    mutable ids_path_list : string list ;
    mutable loaded_biffs : (string, Biff.biff) Hashtbl.t ;
    mutable dialog_search : (string, int) Hashtbl.t ;
    mutable str_sets : str_set_record list ;
    (* most recent STRING_SET or forced strref is the head of the list *)
    mutable script_style : script_style ;
    game_type : game_type ;
    dialogs : tlk_pair array ;
    mutable dialog_index : int ;
  }

let saved_game = ref (None : game option)

let the_game () = match !saved_game with
| None -> failwith "ERROR: no game loaded"
| Some(g) -> g

let get_active_dialog g =
  (Array.get g.dialogs g.dialog_index).dialog.contents

let get_active_dialog_path g =
  (Array.get g.dialogs g.dialog_index).dialog.path

let get_active_dialogf_fallback g =
  (match (Array.get g.dialogs g.dialog_index).dialogf with
  | Some(tlk) -> tlk.contents
  | None -> get_active_dialog g)

let get_active_dialogf_path_fallback g =
  (match (Array.get g.dialogs g.dialog_index).dialogf with
  | Some tlk -> tlk.path
  | None -> get_active_dialog_path g)

let get_active_dialogf_opt g =
  match (Array.get g.dialogs g.dialog_index).dialogf with
  | Some tlk -> Some (tlk.contents)
  | None -> None

let get_active_dialogf_path_opt g =
  (match (Array.get g.dialogs g.dialog_index).dialogf with
  | Some tlk -> Some tlk.path
  | None -> None)

let get_active_dialogs g =
  (Array.get g.dialogs g.dialog_index)

let get_dialogs_by_path game dpath dfpath =
  let tlk_pair : tlk_pair option = Array.fold_left (fun acc tlk_pair ->
    let path_sep_regexp = (Str.regexp "[\\\\/]+") in
    let dpath_regexp = (Str.regexp_case_fold
                          (Str.global_replace path_sep_regexp
                             "[\\\\/]+" dpath)) in
    if Str.string_match dpath_regexp tlk_pair.dialog.path 0 then
      (match tlk_pair.dialogf with
      | None when dfpath = None ->
          Some tlk_pair
      | Some df ->
          (match dfpath with
          | None -> acc
          | Some path ->
              let dfpath_regexp = (Str.regexp_case_fold
                                     (Str.global_replace path_sep_regexp
                                        "[\\\\/]+" path)) in
              if Str.string_match dfpath_regexp df.path 0 then
                Some tlk_pair
              else acc)
      | _ -> acc)
    else acc) None game.dialogs in
  tlk_pair

let create_dialog_search g =
  Array.iteri (fun i t -> Hashtbl.add g.dialog_search t.Tlk.text i)
    (get_active_dialog g)

let pad_tlks game =
  (match (get_active_dialogf_opt game) with
    Some(df) -> begin
      let d = get_active_dialog game in
      let d_pair = get_active_dialogs game in
      let dfl = Array.length df in
      let dl = Array.length d in
      if (dfl > dl) then begin
        let uneven = Array.sub df dl (dfl - dl) in
        log_and_print "*** %s has %d too few entries, padding.\n"
          d_pair.dialog.path (dfl - dl) ;
        d_pair.dialog.contents <- Array.append d uneven ;
        d_pair.dialog_mod <- true ;
      end else if (dfl < dl) then begin
        let uneven = Array.sub d dfl (dl - dfl) in
        let df_record = value_of_option d_pair.dialogf in
        log_and_print "*** %s has %d too few entries, padding.\n"
          df_record.path (dl - dfl) ;
        df_record.contents <- (Array.append df uneven) ;
        d_pair.dialogf_mod <- true ;
      end
    end
  | None -> ())

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
      incr i)
      lse_q ;

    log_or_print "%d characters, %d entries added to DIALOG.TLK\n"
      !char_count num_entries ;
    Queue.clear lse_q ;
    let tlk_pair = get_active_dialogs g in
    tlk_pair.dialog.contents <- (Array.append tlk_pair.dialog.contents ma) ;
    Hashtbl.clear g.dialog_search ;
    create_dialog_search g ;

    (match tlk_pair.dialogf with
    | Some a ->
        a.contents <- (Array.append a.contents fa) ;
        tlk_pair.dialogf_mod <- true
    | None -> ()) ;

    tlk_pair.dialog_mod <- true)
    ()

(* ww: this looks for all case-variants of 'file', so 'CHITIN.KEY' and
 * 'chitin.key' both match! *)
let find_file_in_path path file =
  try
    begin
      let h = Case_ins.unix_opendir path in
      let regexp = Str.regexp_case_fold file in
      let res = ref None in
      (try
        while true do
          let f = Unix.readdir h in
          if (Str.string_match regexp f 0) then
            res := Some(f)
        done
      with e -> ()) ; Unix.closedir h ;
      match !res with
      | Some(e) -> Arch.native_separator (path ^ "/" ^ e)
      | None -> Arch.native_separator (path ^ "/" ^ file)
    end
  with _ -> Arch.native_separator (path ^ "/" ^ file)

let fake_load_dialog gp dialog_path =
  let path = match dialog_path with
  | Some(p) -> p
  | None -> find_file_in_path gp "^dialog\\.tlk$" in
  if file_exists path then begin
    (* (Tlk.load_tlk path),path *)
    ((Tlk.null_tlk ()), path)
  end else begin
    log_and_print
      "\nERROR: Unable to find DIALOG.TLK in:\n\t%s\n" path ;
    log_and_print
      "\nPlease run this program in your Infinity Engine game directory.\n" ;
    failwith "Unable to find DIALOG.TLK"
  end

let fake_load_dialogf gp dialogf_path =
  let path = match dialogf_path with
  | Some(p) -> p
  | None -> find_file_in_path gp "^dialogf\\.tlk$" in
  if file_exists path then begin
    (* let df = Tlk.load_tlk path in
     * Some(df), path *)
    (Some (Tlk.null_tlk ()), path)
  end else
    None, ""

let enhanced_edition_p game =
  (match game.game_type with
  | BGEE
  | BG2EE
  | IWDEE
  | PSTEE -> true
  | GENERIC -> false)

let eep () =
  enhanced_edition_p (the_game ())

let load_dialog_pair path dpath dfpath =
  let dialog, dialog_path = fake_load_dialog path dpath in
  let dialogf, dialogf_path = fake_load_dialogf path dfpath in
  let d = {
    contents = dialog ;
    path = dialog_path
  } in
  let df = (match dialogf with
  | Some(df) -> Some {
      contents = df ;
      path = dialogf_path
    }
  | None -> None) in
  {
   dialog = d ;
   dialog_mod = false ;
   dialogf = df ;
   dialogf_mod = false ;
   loaded = false
 }

let load_default_dialogs game_path =
  let tlk_pair = load_dialog_pair game_path None None in
  (match !dialog_tlk_path with
  | None -> [|tlk_pair|]
  | Some path ->
      let tlkin = load_dialog_pair game_path
          !dialog_tlk_path !dialogf_tlk_path in
      (* to cut down on the work elsewhere, it is assumed that
       * tlkin is always the last tlk pair *)
      [|tlk_pair ; tlkin|])

let load_ee_dialogs game_path =
  let lang_path = game_path ^ "/lang" in
  let lang_dirs =
    (List.fast_sort compare
       (List.map String.lowercase
          (List.filter (fun dir ->
            let dir = Arch.native_separator (lang_path ^ "/" ^ dir) in
            (is_directory dir) &&
            (file_exists (Arch.native_separator (dir ^ "/dialog.tlk"))))
             (Array.to_list (Case_ins.sys_readdir lang_path))))) in
  let languages = (List.map (fun lang ->
    let path = Arch.native_separator (lang_path ^ "/" ^ lang) in
    load_dialog_pair path None None) lang_dirs) in
  (match !dialog_tlk_path with
  | None -> Array.of_list languages
  | Some path ->
      let tlkin = load_dialog_pair game_path
          !dialog_tlk_path !dialogf_tlk_path in
      (* to cut down on the work elsewhere, it is assumed that
       * tlkin is always the last tlk pair *)
      Array.of_list (List.append languages [tlkin]))

let load_dialogs game_path =
  if file_exists (Arch.native_separator
                    (game_path ^ "/lang/en_us/dialog.tlk")) then
    load_ee_dialogs game_path
  else
    load_default_dialogs game_path

let actually_load_tlk_pair game tlk_pair =
  if not tlk_pair.loaded && (file_exists tlk_pair.dialog.path) then begin
    ignore (tlk_pair.dialog.contents <- Tlk.load_tlk tlk_pair.dialog.path) ;
    ignore (match tlk_pair.dialogf with
    | None -> ()
    | Some df -> df.contents <- Tlk.load_tlk df.path) ;
    ignore (tlk_pair.loaded <- true) ;
    ignore (pad_tlks game) ;
    ignore (Hashtbl.clear game.dialog_search) ;
    ignore (create_dialog_search game) ;
  end


exception FoundKey of Key.key * string

let load_null_game () =
  let dialogs =
    (match !dialog_tlk_path with
    | Some(p) -> [|{
                   dialog = {contents = (Tlk.load_tlk p) ; path = p} ;
                   dialog_mod = false ;
                   dialogf = None ;
                   dialogf_mod = false ;
                   loaded = false
                 }|]
    | None -> [|{
                dialog = {contents = (Tlk.null_tlk ()) ;
                          path = " -- NO DIALOG.TLK -- "} ;
                dialog_mod = false ;
                dialogf = None ;
                dialogf_mod = false ;
                loaded = false
              }|]) in
  let dialog_index = 0 in
  let result =
    {
     key = Key.null_key () ;
     game_path = " -- NO GAME -- " ;
     cd_path_list = [] ;
     override_path_list = !override_paths ;
     ids_path_list = !ids_paths ;
     loaded_biffs = Hashtbl.create 1 ;
     dialog_search = Hashtbl.create
       (1 + (Array.length
               (Array.get dialogs dialog_index).dialog.contents * 2)) ;
     str_sets = [] ;
     script_style = BG2 ;
     game_type = GENERIC ;
     dialogs = dialogs ;
     dialog_index = dialog_index ;
   } in
  create_dialog_search result ;
  result

let find_key_file game_paths =
  try
    List.iter (fun path ->
      let keyname = find_file_in_path path "^chitin.key$" in
      if file_exists keyname then begin
        let keybuff = load_file keyname in
        raise (FoundKey((Key.load_key keyname keybuff),path))
      end) game_paths ;
    log_and_print "\nERROR: Unable to find CHITIN.KEY in:\n" ;
    List.iter (fun path -> log_and_print "\t%s\n" path) game_paths ;
    failwith
      "Unable to find CHITIN.KEY: run me in an Infinity Engine game directory"
  with FoundKey(k,gp) -> k, gp

let read_cd_paths gp =
  let paths =
    (try
      let s_d_h = Case_ins.unix_opendir gp in
      let sofar = ref [] in
      begin
        (try
          while true do
            let s = Unix.readdir s_d_h in
            let base,ext = split_resref s in
            if (String.uppercase ext) = "INI" then begin
              let buff = load_file (gp ^ "/" ^ s) in
              (try
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
              with _ -> ())
            end
          done
        with _ -> ())
      end ;
      !sofar
    with _ -> [gp ^ "/CD1" ; gp ^ "/CD2" ; gp ^ "/CD3" ;
               gp ^ "/CD4" ; gp ^ "/CD5" ; gp ^ "/CD6" ; gp])
  in
  if Sys.os_type = "Unix" then
    paths @ [gp ^ "/CD1" ; gp ^ "/CD2" ; gp ^ "/CD3" ;
             gp ^ "/CD4" ; gp ^ "/CD5" ; gp ^ "/CD6"; gp]
  else
    paths

let autodetect_game_type key =
  let starting_assumption = (GENERIC, BG1) in
  let tests = ["SUBRACE", "IDS", GENERIC, IWD2 ;
               "BONES", "IDS", GENERIC, PST ;
               "CLOWNRAN", "IDS", GENERIC, IWD1 ;
               "FLYTHR01", "MVE", GENERIC, BG2 ;
               "OH1000", "ARE", BGEE, BG2 ;
               "OH6000", "ARE", BG2EE, BG2 ;
               "PSTCHAR", "2DA", PSTEE, BG2 ;
               "HOWPARTY", "2DA", IWDEE, BG2] in
  let (game_type, script_style) = List.fold_left
      (fun acc (res, ext, game_type, script_style) ->
        if Key.resource_exists key res ext then begin
          (game_type, script_style)
        end
        else begin
          acc
        end) starting_assumption tests in
  (* Tlk.is_bg2 should be refactored as well, but for now: *)
  ignore (match script_style with
  | BG2 -> Tlk.is_bg2 := true
  | IWD2
  | PST
  | IWD1
  | BG1
  | NONE -> Tlk.is_bg2 := false) ;
  (game_type, script_style)

let have_bgee_lang_dir_p = ref false

let load_game () =
  let key, gp = find_key_file !game_paths in
  let dialogs = load_dialogs gp in
  let dialog_index = 0 in
  let cd_paths = read_cd_paths gp in
  if not (is_directory "override") && (file_exists "chitin.key") then
    Case_ins.unix_mkdir "override" 511 ;
  if (Util.file_exists "gemrb_path.txt") then
    ignore (add_gemrb_path "gemrb_path.txt") ;
  let game_type, script_style = autodetect_game_type key in
  let result =
    {
     key = key ;
     game_path = gp ;
     cd_path_list = cd_paths ;
     override_path_list = [(gp ^ "/override")] @ !override_paths ;
     ids_path_list = !ids_paths @ [(gp ^ "/override")] @ !override_paths ;
     loaded_biffs = Hashtbl.create 5 ;
     dialog_search = Hashtbl.create 100000 ;
     str_sets = [] ; (* and keep it that way! :-) *)
     script_style = script_style ;
     game_type = game_type ;
     dialogs = dialogs ;
     dialog_index = dialog_index
   } in
  ignore (Var.set_game_vars result.game_path result.game_type) ;
  result

let set_additional_bgee_load_paths game dir =
  let gp = game.game_path in
  let more = List.append (if dir <> "en_us" then [gp ^ "/lang/" ^ dir] else [])
      [gp ^ "/lang/en_us"] in
  game.cd_path_list <- (List.append game.cd_path_list more)

let use_bgee_lang_dir game dir =
  let str1 = Str.quote "lang" in
  let str2 = Str.quote dir in
  let regexp = (Str.regexp_case_fold
                  ((Str.quote game.game_path) ^ "[\\\\/]+" ^
                   str1 ^ "[\\\\/]+" ^ str2)) in
  let foundp = ref false in
  ignore (set_additional_bgee_load_paths game dir) ;
  ignore (Var.set_ee_language_var dir) ;
  ignore (Array.iteri (fun index tlk_pair ->
    if Str.string_match regexp tlk_pair.dialog.path 0 then begin
      game.dialog_index <- index ;
      ignore (actually_load_tlk_pair game tlk_pair) ;
      foundp := true ;
    end) game.dialogs) ;
  if not !foundp then begin
    log_and_print
      "ERROR: None of the dialog paths were a match against %s\n" dir ;
    raise Not_found
  end

let set_bgee_lang_dir game dir =
  (match dir with
  | Some d ->
      ignore (use_bgee_lang_dir game d) ;
      have_bgee_lang_dir_p := true ;
  | None -> have_bgee_lang_dir_p := false)

let bgee_language_options game =
  let options = Array.map (fun tlk_pair ->
    let str1 = (Str.quote "lang") in
    let str2 = (Str.quote "dialog.tlk") in
    let regexp = (Str.regexp_case_fold ((Str.quote game.game_path) ^
                                        "[\\\\/]+" ^ str1 ^ "[\\\\/]+" ^
                                        "\\([a-z_]+\\)" ^
                                        "[\\\\/]+" ^ str2)) in
    if Str.string_match regexp tlk_pair.dialog.path 0 then
      Str.matched_group 1 tlk_pair.dialog.path
    else begin
      log_and_print "ERROR: could not match a directory in %s\n"
        tlk_pair.dialog.path ;
      raise Not_found
    end) game.dialogs in
  options

let deal_with_tlkin game =
  (match !dialog_tlk_path, !dialogf_tlk_path with
  | None, None -> ()
  | _, _ ->
      game.dialog_index <- ((Array.length game.dialogs) - 1) ;
      ignore (actually_load_tlk_pair game (get_active_dialogs game)))

let validate_cwd () =
  if not (file_exists "chitin.key") then begin
    log_and_print
      "\nPlease run this program in your Infinity Engine game directory.\n" ;
    failwith "Not a game directory"
  end

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
      (* Check to see if the bif file exists,
       * if it doesn't try for a .CBF file
       *)
      let bf = trial bif_file (game.cd_path_list @
                               [game.game_path ^ "/cache"] ) in
      if file_exists bf then
        bf
      else begin
        let cbf = Case_ins.filename_chop_extension bif_file ^ ".cbf" in
        let cbf_file = trial cbf (game.cd_path_list) in
        let cbf_file = cbf_file in
        if file_exists cbf_file then
          let cache_file = game.game_path ^ "/cache/" ^ bif_file in
          if not (file_exists cache_file) then
            Queue.add cache_file cbifs_to_rem ;
          let sz = Cbif.cbf2bif (Case_ins.fix_name  cbf_file)
              (Case_ins.fix_name cache_file) in
          let _ = log_and_print "[%s] decompressed bif file %d bytes\n"
              cbf_file sz in
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
              raise (FoundRes(load_file path, path)))
            (if ext_up = "IDS" then game.ids_path_list else
            game.override_path_list) ;

        (* Now get it from the BIFs -- look it up in the KEY *)
        let bif_file, bif_index, tis_index, this_biff =
          find_in_key game name ext in
        (* get it out of the BIF *)
        if ext_up = "TIS" then
          (Biff.extract_tis this_biff (tis_index-1))
            skip_this_error,(game.game_path ^ "/" ^ bif_file)
        else
          (Biff.extract_file this_biff bif_index)
            skip_this_error,(game.game_path ^ "/" ^ bif_file)
      with
      | (FoundRes(b,p)) -> b,p
      | Invalid_argument "String.create" ->
          raise (Invalid_argument "String.create")
      | _ ->
          if ok_missing (name ^ "." ^ ext) then
            "","(resource not found)"
          else begin
            if file_exists full then begin
              (load_file full, full)
            end else begin
              if not skip_this_error then begin
                log_and_print
                  "\nERROR locating resource for '%s'\n" for_what ;
                let keypath = game.game_path ^ "/chitin.key" in
                log_and_print
                  "Resource [%s.%s] not found in KEY file:\n\t[%s]\n"
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

let exists_in_overrides game res ext =
  List.fold_left (fun acc dir ->
    if file_exists (dir ^ "/" ^ res ^ "." ^ ext) then
      true
    else acc) false (if (String.uppercase ext) = "IDS" then
      game.ids_path_list else game.override_path_list)

let resource_exists_legacy_check name =
  (* FILE_EXISTS_IN_GAME used to be implemented through load_resource
     This function corresponds to the control-flow
     load_resource > exn > with _ > not ok_missing >
     and aims to preserve legacy behaviour, nonsense though it may be *)
  file_exists name

let resource_exists game res ext =
  let name = res ^ "." ^ ext in
  if (Case_ins.filename_is_implicit res) then begin
    exists_in_overrides game res ext || Key.resource_exists game.key res ext ||
    resource_exists_legacy_check name
  end else
    file_contains_data name

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
      resist_dispel = 0 ;
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
      with e -> { opcode = 0 ; target = 0 ; arg1 = 0 ; arg2 = 0 ;
                  resist_dispel = 0 ; raw_offset = 0 ;})
  with e ->
    log_and_print
      "ERROR: %s\nERROR: Problem Listing Effects (perhaps not a BG2 resource?)\n"
      (printexc_to_string e) ;
    [| |]

let content_name_offset = ref None

let search_biff_contents game o tl sl =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in
  let reg_list = List.map Str.regexp_case_fold sl in
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin (* type match *)
      try
        let buff,path = load_resource "searching BIFF contents"
            game true r.res_name (ext_of_key r.res_type) in
        let matches_one =
          List.fold_left (fun acc r -> acc ||
          try
            let _ = Str.search_forward r buff 0 in
            true
          with _ -> false) false reg_list
        in
        if matches_one then begin
          match !content_name_offset with
          | Some(off) when String.length buff >= off + 4 ->
              let name_id = int_of_str_off buff off in
              o (Printf.sprintf "%8s.%3s in [%s] matches [%s]\n"
                   r.res_name (ext_of_key r.res_type) biff.filename
                   (Tlk.pretty_print (get_active_dialog game) name_id))
          | _ -> o (Printf.sprintf "%8s.%3s in [%s] matches\n"
                      r.res_name (ext_of_key r.res_type) biff.filename)
        end
      with e -> ()
    end) game.key.resource

let search_biff_contents_fun game o tl matches =
  let key_list = List.map (fun ext -> Key.key_of_ext true ext) tl in
  Array.iter (fun r ->
    let biff = game.key.biff.(r.bif_index) in
    if List.mem r.res_type key_list then begin (* type match *)
      try
        let buff,path = load_resource "searching BIFF contents"
            game true r.res_name (ext_of_key r.res_type) in
        if matches buff then begin
          match !content_name_offset with
          | Some(off) when String.length buff >= off + 4 ->
              let name_id = int_of_str_off buff off in
              o (Printf.sprintf "%8s.%3s in [%s] matches [%s]\n"
                   r.res_name (ext_of_key r.res_type) biff.filename
                   (Tlk.pretty_print (get_active_dialog game) name_id))
          | _ -> o (Printf.sprintf "%8s.%3s in [%s] matches\n"
                      r.res_name (ext_of_key r.res_type) biff.filename)
        end
      with e -> ()
    end) game.key.resource

let file_exists_in_game game f =
  let old_allow_missing = !allow_missing in
  allow_missing := [] ;
  let res =
    (try
      let a,b = split_resref f in
      skip_next_load_error := true ;
      let buff,path = load_resource "FILE_EXISTS_IN_GAME" game true a b in
      (String.length buff > 0)
    with _ -> false) in
  allow_missing := old_allow_missing ;
  res

let fix_biff_path filename =
  let sep = (Str.regexp "[\\/:]") in
  let biff_sep = Key.biff_path_separator (enhanced_edition_p (the_game ())) in
  Str.global_replace sep biff_sep filename
