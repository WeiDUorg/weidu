(* list which mod components make changes to the given files *)
open BatteriesInit
open Util

type result = {
    text: string ;
    backup_files: (string * string) list ;
  }

let changelog file_list game =
  Parsewrappers.load_log();
  let backup_lists = Hashtbl.create 1000 in
  let tp2s = Hashtbl.create 100 in
  let tras = Hashtbl.create 100 in
  let parse_tp2 tpfile =
    try
      Hashtbl.find tp2s tpfile
    with _ ->
      (* Hashtbl.clear tp2s; *)
      let x = Parsewrappers.handle_tp2_filename tpfile in
      Hashtbl.add tp2s tpfile x;
      x
  in
  let get_file name =
    try
      Hashtbl.find backup_lists name
    with _ -> []
  in
  let add_tras s =
    let s = Tpstate.decompile_var s in
    let x =
      try Hashtbl.find tras s
      with _ ->
        (* if Hashtbl.length tras > 10 then Hashtbl.clear tras; *)
        let x = Parsewrappers.get_tra_list_filename (Arch.backslash_to_slash s) in
        Hashtbl.add tras s x; x
    in
    Stats.time "adding translation strings" Dc.add_trans_strings x
  in
  let file_list = List.map String.uppercase file_list in
  List.iter (fun cur_mod ->
    let (tpfile,lang,comp,comp_name,status) = cur_mod in
    let tp2 = parse_tp2 tpfile in
    let backup_dir = tp2.Tp.backup in
    List.iter (fun (infile,saveable) ->
      if file_exists infile then
        let infile = Case_ins.perv_open_in infile in
        try
          while true do
            let line = input_line infile in
            let parts = split_log_line line in
            let (a,b) = match parts with
            | a :: b :: _ -> (a,b)
            | a :: [] -> (a,"")
            | [] -> failwith "Empty line in a MAPPINGS file."
            in
            let (a,b) = (String.uppercase a, String.uppercase b) in
            let a_stem = Str.global_replace (Str.regexp "^OVERRIDE[/\\]") "" a in
            if (List.mem a_stem file_list) then begin
              (* log_and_print "%s\n" line; *)
              let m = Tpstate.get_nth_module tp2 comp true in
              Dc.push_trans();
              (try
                let l = List.nth tp2.Tp.languages lang in
                List.iter add_tras l.Tp.lang_tra_files ;
              with _ -> ()
              ) ;
              let comp_str = Dc.single_string_of_tlk_string_safe game m.Tp.mod_name in
              let subcomp_group the_comp =
                let rec walk lst = match lst with
                | Tp.TPM_SubComponents(ts,a,b) :: tl -> Some(ts)
                | hd :: tl -> walk tl
                | [] -> None
                in walk the_comp.Tp.mod_flags
              in
              let subcomp_str = (
                match subcomp_group m with
                | None    -> ""
                | Some(x) -> "" ^ (Dc.single_string_of_tlk_string_safe game x) ^ " -> ") in
              let rec get_version lst = match lst with
              | Tp.Version(lse) :: _ -> ": " ^ Dc.single_string_of_tlk_string_safe game lse
              | _ :: tl -> get_version tl
              | [] -> ""
              in
              let version = get_version tp2.Tp.flags in
              Dc.pop_trans();
              Hashtbl.replace backup_lists a ((tpfile, lang, comp, comp_str, subcomp_str, version, b, saveable) :: get_file a);
            end
          done
        with End_of_file -> close_in infile) [
    (Printf.sprintf "%s/%d/MAPPINGS.%d" backup_dir comp comp,true);
    (Printf.sprintf "%s/%d/OTHER.%d" backup_dir comp comp, false)];) !Tp.the_log;
  let result = List.map (fun file1 ->
    let file1 = String.uppercase file1 in
    let file = if Case_ins.filename_check_suffix file1 ".EXE" || Case_ins.filename_check_suffix file1 ".KEY" then file1 else "OVERRIDE/" ^ file1 in
    let file_log = List.rev (get_file file) in
    let (base,ext) = split file1 in
    let first = Printf.sprintf "\n\n\nMods affecting %s:\n" file1 in
    let item_list = List.mapi (fun i (tpfile,lang,comp,comp_str,subcomp_str,version,backup,saveable) ->
      Printf.sprintf "%05d: %s~%s~ %d %d // %s%s%s\n" i
        (if not saveable then "/* acted upon in an indetectable manner */" else if backup = "" then "/* from game biffs */ "  else "") tpfile lang comp subcomp_str comp_str version
                              ) file_log in
    let backup_files = List.map value_of_option
        (List.filter (function | Some tuple -> true | None -> false)
           (List.mapi (fun i (tpfile, lang, comp, comp_str, subcomp_str, version, backup, saveable) ->
             let out = Printf.sprintf "%s.%05d.%s" base i ext in
             if saveable && file_exists backup then Some (backup, out) else None) file_log)) in
    let text =first ^ (List.fold_left (fun acc item ->
      acc ^ item) "" item_list) in
    (text, backup_files)) file_list
  in
  List.map (fun ((text: string), (files: (string * string) list)) ->
    {
     text = text;
     backup_files = files;
   }) result
