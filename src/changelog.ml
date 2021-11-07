(* list which mod components make changes to the given files *)
open BatteriesInit
open Hashtblinit
open Util

type result = {
    text: string ;
    backup_files: (string * string) list ;
  }

type comp = {
    tpfile: string ;
    lang_num: int ;
    comp_num: int ;
    comp_name: string ;
    subcomp_name: string ;
    version: string ;
    file: string ;
    op: op_type ;
  }

and op_type =
  | UNINSTALL
  | MAPPING
  | MOVE
  | OTHER

let process_log log file_list game =
  let backup_lists = Hashtbl.create (List.length file_list) in
  let tp2_cache = Hashtbl.create 100 in
  let tra_cache = Hashtbl.create 100 in
  let get_list file =
    try
      Hashtbl.find backup_lists file
    with Not_found -> [] in
  let parse_tp2 tpfile =
    try
      Hashtbl.find tp2_cache tpfile
    with Not_found ->
      let tp2 = Parsewrappers.handle_tp2_filename tpfile in
      Hashtbl.add tp2_cache tpfile tp2 ;
      tp2 in
  let add_tras file =
    let file = Var.get_string file in
    let tra = (try Hashtbl.find tra_cache file with Not_found ->
      let tra = Parsewrappers.get_tra_list_filename
          (Arch.backslash_to_slash file) in
      Hashtbl.add tra_cache file tra ;
      tra) in
    ignore (Stats.time "adding translation strings" Dc.add_trans_strings tra) in
  ignore (List.iter (fun cur_mod ->
    let (tpfile, lang_num, comp_num, _, _) = cur_mod in
    let tp2 = parse_tp2 tpfile in
    let backup_dir = tp2.Tp.backup in
    List.iter (fun (backup_file, op_type) ->
      if file_exists backup_file then
        let lines = Util.read_lines backup_file in
        List.iter (fun line ->
          let parts = split_log_line line in
          let (src, dst) = match parts with
          | s :: d :: _ -> (String.uppercase s, String.uppercase d)
          | s :: [] -> (String.uppercase s, "")
          | [] -> failwith "Empty line in backup file" in
          let src_file =
            Str.global_replace (Str.regexp "^OVERRIDE[/\\]") "" src in
          if (List.mem src_file file_list) then begin
            let comp = Tpstate.get_nth_module tp2 comp_num true in
            Dc.push_trans () ;
            (try
              let lang = List.nth tp2.Tp.languages lang_num in
              List.iter add_tras lang.Tp.lang_tra_files ;
            with _ -> ()) ;
            let comp_name =
              Dc.single_string_of_tlk_string_safe game comp.Tp.mod_name in
            let subcomp_name = List.fold_left (fun acc flag -> match flag with
            | Tp.TPM_SubComponents(ts, _, _) ->
                Dc.single_string_of_tlk_string_safe game ts
            | _ -> acc) "" comp.Tp.mod_flags in
            let version = List.fold_left (fun acc flag -> match flag with
            | Tp.Version lse ->
                Dc.single_string_of_tlk_string_safe game lse
            | _ -> acc) "" tp2.Tp.flags in
            Dc.pop_trans () ;
            let record = { tpfile = tpfile ; lang_num = lang_num ;
                           comp_num = comp_num ; comp_name = comp_name ;
                           subcomp_name = subcomp_name ;
                           version = version ; file = dst ; op = op_type } in
            Hashtbl.replace backup_lists src (record :: get_list src) ;
          end) lines)
      [(Printf.sprintf "%s/%d/UNINSTALL.%d"
          backup_dir comp_num comp_num, UNINSTALL) ; (* src *)
       (Printf.sprintf "%s/%d/MAPPINGS.%d"
          backup_dir comp_num comp_num, MAPPING) ; (* src dst *)
       (Printf.sprintf "%s/%d/MOVE.%d"
          backup_dir comp_num comp_num, MOVE) ; (* src dst *)
       (Printf.sprintf "%s/%d/OTHER.%d"
          backup_dir comp_num comp_num, OTHER)] ; (* src *)
            ) log) ;
  backup_lists

    (*
     * Partition a list of mod, component, optype into a list of lists by
     * mod, component (i.e., group optypes); preserves internal order
     *)
let partition log_list =
  let indexed_list = (List.mapi (fun i record -> (i,record)) log_list) in
  let ht = (List.fold_left (fun acc (i,record) ->
    if Hashtbl.mem acc (record.tpfile,record.comp_num) then begin
      Hashtbl.replace acc (record.tpfile,record.comp_num)
        ((i,record) :: Hashtbl.find acc (record.tpfile,record.comp_num)) ;
      acc
    end else begin
      Hashtbl.add acc (record.tpfile,record.comp_num) [(i,record)] ;
      acc end) (Hashtbl.create 5) indexed_list) in
  let unsorted_lists = (Hashtbl.fold (fun _ value acc ->
    value :: acc) ht []) in
  let sorted_lists = (List.stable_sort (fun listl listr ->
    let (il,_) = List.hd listl in
    let (ir,_) = List.hd listr in
    compare il ir) unsorted_lists) in
  List.map (fun lol ->
    List.map (fun (_,list) -> list) lol) sorted_lists

    (*
     * Fold a list of list of records into a list of records based
     * on precedence rules
     *)
let reduce_log_list backup_lists file_log =
  List.map (fun log_list ->
    List.fold_left (fun acc record ->
      (match record.op with
      | MAPPING when acc.op = UNINSTALL ->
          record
      | OTHER when acc.op = UNINSTALL || acc.op = MAPPING || acc.op = MOVE ->
          record
      | _ -> acc)) (List.hd log_list) log_list) (partition file_log)

let prepare_result file_list backup_lists game =
  let get_list file =
    try
      Hashtbl.find backup_lists file
    with Not_found -> [] in
  List.map (fun file1 ->
    let file1 = String.uppercase file1 in
    let file = if Case_ins.filename_check_suffix file1 ".EXE" ||
    Case_ins.filename_check_suffix file1 ".KEY" then file1
    else "OVERRIDE/" ^ file1 in
    let file_log = reduce_log_list backup_lists (List.rev (get_list file)) in
    let (base,ext) = split_resref file1 in
    let first = Printf.sprintf "\n\n\nMods affecting %s:\n" file1 in
    let item_list = List.mapi (fun i record ->
      let optext = (match record.op with
      | UNINSTALL -> "/* created or unbiffed */"
      | MAPPING -> ""
      | MOVE -> ""
      | OTHER -> "/* acted upon in an undefined manner */") in
      Printf.sprintf "%05d: %s ~%s~ %d %d // %s%s%s\n" i
        optext record.tpfile record.lang_num record.comp_num
        record.subcomp_name record.comp_name record.version) file_log in
    let backup_files = List.map value_of_option
        (List.filter (function | Some tuple -> true | None -> false)
           (List.mapi (fun i record ->
             let saveable = (match record.op with
             | UNINSTALL
             | OTHER -> false
             | MAPPING
             | MOVE -> true) in
             let out = Printf.sprintf "%s.%05d.%s" base i ext in
             if saveable && file_exists record.file then
               Some (record.file, out) else
               None) file_log)) in
    let text = first ^ (List.fold_left (fun acc item ->
      acc ^ item) "" item_list) in
    (text, backup_files)) file_list

let changelog file_list game =
  ignore (Parsewrappers.load_log ()) ;
  let file_list = List.map String.uppercase file_list in
  let backup_lists = process_log !Tp.the_log file_list game in
  let result = prepare_result file_list backup_lists game in
  List.map (fun ((text: string), (files: (string * string) list)) ->
    {
     text = text;
     backup_files = files;
   }) result
