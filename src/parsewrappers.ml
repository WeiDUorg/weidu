(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util

let tp2_cache = Hashtbl.create 5

let load_log () =
  try
    let result = parse_file true (File Tp.log_name) "parsing .log files"
        (Dparser.log_file Dlexer.initial) in
    Tp.the_log := List.map (fun (a,b,c,d) ->
      ((String.uppercase a),b,c,d,Tp.Installed)) result
  with e ->
    log_or_print "WARNING: parsing log [%s]: %s\n" Tp.log_name
      (printexc_to_string e) ;
    Tp.the_log := []

let handle_baf_filename filename =
  parse_file true (File filename) "parsing .baf files"
    (Bafparser.baf_file Baflexer.initial)

let compile_baf_filename game filename =
  try
    let script = parse_file true (File filename) "parsing .baf files"
        (Bafparser.baf_file Baflexer.initial) in
    let name,ext = split (Case_ins.filename_basename filename) in
    let out = open_for_writing ("override/" ^ name ^ ".bcs") true in
    Bcs.save_bcs game (Bcs.Save_BCS_OC(out)) script ;
    close_out out
  with e ->
    (log_and_print "ERROR: error compiling [%s]: %s\n"
       filename (printexc_to_string e) ; raise e)

let handle_script_buffer filename buffer =
  match split (String.uppercase filename) with
  | _,"BAF" -> parse_file true (String(filename,buffer)) "parsing .baf files"
        (Bafparser.baf_file Baflexer.initial)
  | _,_ -> parse_file true (String(filename,buffer)) "parsing .bcs files"
        (Bcsparser.bcs_file Bcslexer.initial)

let handle_script_al buffer =
  let result = parse_file false (String("",buffer)) "parsing .baf files"
      (Bafparser.action_list Baflexer.initial) in
  result

let handle_script_tl buffer =
  parse_file false (String("",buffer)) "parsing .baf files"
    (Bafparser.trigger_list Baflexer.initial)
;;

Bcs.parse_al := handle_script_al

let check_file_exists file =
  if Modder.enabled "OVERWRITING_FILE" then begin
    if file_exists file then
      Modder.handle_deb "OVERWRITING_FILE"
        (Printf.sprintf "Overwriting [%s], which already exists\n" file)
    else begin
      let name = Filename.basename file in
      let (a,b) = split name in
      if (try
        Load.skip_next_load_error := true ;
        let buff,path = Load.load_resource "FILE_EXISTS_IN_GAME"
            (Load.the_game()) true a b in
        (String.length buff > 0)
      with Failure _ -> false
      | Invalid_argument "String.create" -> true (* File is > 2^24 bytes *)
      | _ -> false)  then
        Modder.handle_deb "OVERWRITING_FILE"
          (Printf.sprintf
             "Overwriting [%s], which is defined in a biff\n" file) ;
    end
  end
;;

modder_check_file_exists := check_file_exists

let handle_dlg_buffer game filename buffer =
  let emit_from = !Dlg.emit_from in
  let comments = !Dlg.comments in
  let emit_text = !Dlg.emit_text in
  let dlg = Dlg.load_dlg filename buffer in
  Dlg.emit_from := false ;
  Dlg.comments := false ;
  Dlg.emit_text := false ;
  let out_buff = Buffer.create (String.length buffer) in
  Dlg.emit_d dlg filename (Load.get_active_dialog game)
    (Load.get_active_dialogf_opt game)
    out_buff None None (fun str _ -> str) false false ;
  Dlg.emit_from := emit_from ;
  Dlg.comments := comments ;
  Dlg.emit_text := emit_text ;
  Buffer.contents out_buff

let parse_d_buffer filename buffer =
  parse_file true (String(filename,buffer)) "parsing .d files"
    (Dparser.d_file Dlexer.initial)

let handle_d_buffer game filename buffer =
  try
    let result = parse_d_buffer filename buffer in
    (match result with
    | [Dc.Create(dlg) as act] ->
        Dc.dc game [(filename,act)] ;
        let out_buff = Buffer.create (1024 * 32) in
        Dlg.save_dlg dlg out_buff ;
        Dc.clear_state () ;
        Buffer.contents out_buff
    | _ -> failwith "COMPILE_D_TO_DLG -- must simply define a DLG") ;
  with e ->
    Dc.clear_state () ;
    raise e

let handle_tp2_filename_caching filename can_cache =
  if can_cache && Hashtbl.mem tp2_cache filename then
    Hashtbl.find tp2_cache filename
  else
    let res = Tparser.parse_tp2_file (File filename) in
    res.Tp.tp_filename <- filename ;
    if can_cache then
      Hashtbl.add tp2_cache filename res ;
    res

let handle_tp2_filename filename =
  handle_tp2_filename_caching filename false

let handle_tph_filename filename =
  Tparser.parse_tpa_file (File filename)

let handle_tpp_filename filename =
  Tparser.parse_tpp_file (File filename)

let handle_tra_filename filename =
  if file_exists filename || Hashtbl.mem inlined_files filename then begin
    let result = parse_file true (File filename) "parsing .tra files"
        (Dparser.tra_file Dlexer.initial) in
    log_or_print "[%s] has %d translation strings\n" filename
      (List.length result) ;
    Dc.notChanged := false ;
    Stats.time "adding translation strings" Dc.add_trans_strings result
  end else begin
    Modder.handle_msg "SETUP_TRA"
      (Printf.sprintf "%s file not found. Skipping...\n" filename)
  end

let resolve_tra_paths_and_load our_lang tra_l =
(* Resolve %s in the tra path to the directory of the current language,
   evaluate variables and handle_tra_filename *)
  begin
    match our_lang with
    | Some(l) -> List.iter (fun path ->
        let my_regexp = Str.regexp_string "%s" in
        let tra_file = Str.global_replace
            my_regexp l.Tp.lang_dir_name (Var.get_string path) in
        handle_tra_filename tra_file) tra_l
    | _ -> List.iter (fun tra_file ->
        handle_tra_filename (Var.get_string tra_file)) tra_l
  end

let get_tra_list_filename filename =
  if file_exists filename || Hashtbl.mem inlined_files filename then begin
    let result = parse_file true (File filename) "parsing .tra files"
        (Dparser.tra_file Dlexer.initial) in
    Dc.notChanged := false ;
    log_or_print "[%s] has %d translation strings\n" filename
      (List.length result) ;
    result
  end else begin
    log_and_print_modder "%s file not found. Skipping...\n" filename ;
    []
  end

let handle_d_filename filename =
  try
    let result = parse_file true (File filename) "parsing .d files"
        (Dparser.d_file Dlexer.initial) in
    let result = List.map (fun a -> (filename,a)) result in
    Dc.d_action_list := result @ !Dc.d_action_list
  with e ->
    (Dc.clear_state () ; raise e)

let reprint_d_action str pfun = begin
  let lexbuf = lex_init_from_string "printing DLG" str in
  let result = try
    let res = Bafparser.action_list Baflexer.initial lexbuf in
    let buff = Buffer.create (String.length str) in
    Bcs.print_script_text (Load.the_game()) (Bcs.Save_BCS_Buffer(buff))
      (Bcs.BCS_Print_ActionList(res)) false (Some(pfun)) ;
    Buffer.contents buff
  with e -> begin
    str
  end
  in
  pop_context () ;
  result
end
;;

Dlg.reprint_trigger := (fun s ->
  ignore_context_error := true ;
  let ans = try
    let lexbuf = lex_init_from_internal_string "" s in
    let lexbuf = Lexing.from_string (String.copy s) in
    let res = Bafparser.trigger_list Baflexer.initial lexbuf in
    let buff = Buffer.create (String.length s) in
    Bcs.print_script_text (Load.the_game()) (Bcs.Save_BCS_Buffer(buff))
      (Bcs.BCS_Print_TriggerList(res,false)) false None ;
    Buffer.contents buff
  with _ -> s
  in
  ignore_context_error := false ; ans)

let emit_dlg_files game output_dir =
  if (!Dc.d_action_list <> []) then begin
    (try
      Stats.time "process .D files" (Dc.dc game) !Dc.d_action_list ;
    with e ->
      Dc.clear_state () ;
      (*log_and_print "ERROR: problem processing D files: %s\n"
        (printexc_to_string e) ; *)
      raise e) ;
    Hashtbl.iter (fun name dlg ->
      let filename = output_dir ^ "/" ^ name ^ ".dlg" in
      try
        let out_buff = Buffer.create (1024 * 32) in
        Dlg.save_dlg dlg out_buff ;
        Stats.time "saving files" (fun () ->
          let out_chan = open_for_writing filename true in
          Buffer.output_buffer out_chan out_buff ;
          close_out out_chan) ()
      with e ->
        Dc.clear_state () ;
        log_and_print "ERROR: problem saving [%s]: %s\n" filename
          (printexc_to_string e) ;
        raise e) Dc.available_dlgs
  end ;
  Dc.clear_state ()

let tp2_queues: string Queue.t = Queue.create ()

let enqueue_tp2_filename filename =
  log_only "Enqueuing [%s] for TP2 processing.\n" filename ;
  Queue.add filename tp2_queues
