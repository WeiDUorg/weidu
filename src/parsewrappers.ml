open Util

let load_log () =
  try
    let result = parse_file true (File Tp.log_name) "parsing .log files"
	(Dparser.log_file Dlexer.initial) in
    Tp.the_log := List.map (fun (a,b,c,d) -> ((String.uppercase a),b,c,d,Tp.Installed)) result
  with e ->
    log_or_print "WARNING: parsing log [%s]: %s\n" Tp.log_name
      (Printexc.to_string e) ;
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
       filename (Printexc.to_string e) ; raise e)

let handle_script_buffer filename buffer =
  match split (String.uppercase filename) with
  | _,"BAF" -> parse_file true (String(filename,buffer)) "parsing .baf files"
	(Bafparser.baf_file Baflexer.initial)
  | _,_ 		-> parse_file true (String(filename,buffer)) "parsing .bcs files"
	(Bcsparser.bcs_file Bcslexer.initial)

let handle_script_al buffer =
	let result = parse_file false (String("",buffer)) "parsing .baf files"
	(Bafparser.action_list Baflexer.initial) in
	result
;;

Bcs.parse_al := handle_script_al
	
let handle_dlg_buffer game filename buffer =
  let emit_from = !Dlg.emit_from in
  let comments = !Dlg.comments in
  let emit_text = !Dlg.emit_text in
  let dlg = Dlg.load_dlg filename buffer in
  Dlg.emit_from := false;
  Dlg.comments := false;
  Dlg.emit_text := false;
  let out_buff = Buffer.create (String.length buffer) in
  Dlg.emit_d dlg filename game.Load.dialog game.Load.dialogf
    out_buff None None (fun str _ -> str) false false ;
  Dlg.emit_from := emit_from;
  Dlg.comments := comments;
  Dlg.emit_text := emit_text;
  Buffer.contents out_buff

let handle_d_buffer game filename buffer =
  try
    let result = parse_file true (String(filename,buffer)) "parsing .d files"
	(Dparser.d_file Dlexer.initial) in
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


let handle_tp2_filename filename =
  let res = Tparser.parse_tp2_file (File filename)
  in
  res.Tp.tp_filename <- filename ;
  res

let handle_tph_filename filename =
  Tparser.parse_tpa_file (File filename)

let handle_tpp_filename filename =
  Tparser.parse_tpp_file (File filename)

let handle_tra_filename filename =
  if file_exists filename || Hashtbl.mem inlined_files filename then begin
    let result =
      (match split (String.uppercase filename) with
      | _, "TRB" -> begin try
	  let inchan = Case_ins.perv_open_in_bin filename in
 	  let result = Stats.time "parsing .trb files"
	      Marshal.from_channel inchan in
	  close_in inchan ;
	  result
      with e -> log_and_print "TRB trouble: %s\n\n" (Printexc.to_string e);
	raise e
      end
      | _, _ ->
	  let result = parse_file true (File filename) "parsing .tra files"
	      (Dparser.tra_file Dlexer.initial) in
	  result)
    in
    log_or_print "[%s] has %d translation strings\n" filename
      (List.length result);
    Dc.notChanged := false;
    Stats.time "adding translation strings" Dc.add_trans_strings result
  end else begin
    Modder.handle_msg "SETUP_TRA" (Printf.sprintf "%s file not found. Skipping...\n" filename)
  end

let get_tra_list_filename filename =
  if file_exists filename || Hashtbl.mem inlined_files filename then begin
    let result =
      (match split (String.uppercase filename) with
      | _, "TRB" ->
	  let inchan = Case_ins.perv_open_in_bin filename in
	  let result = Stats.time "parsing .trb files"
	      Marshal.from_channel inchan in
	  close_in inchan ;
	  result
      | _, _ ->
	  let result = parse_file true (File filename) "parsing .tra files"
	      (Dparser.tra_file Dlexer.initial) in
	  Dc.notChanged := false;
	  result)
    in
    log_or_print "[%s] has %d translation strings\n" filename
      (List.length result);
    result
  end else begin
    log_and_print_modder "%s file not found. Skipping...\n" filename;
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
  let lexbuf = lex_init_from_internal_string "" s in
  let lexbuf = Lexing.from_string (String.copy s) in
  let res = Bafparser.trigger_list Baflexer.initial lexbuf in
  let buff = Buffer.create (String.length s) in
  Bcs.print_script_text (Load.the_game()) (Bcs.Save_BCS_Buffer(buff))
    (Bcs.BCS_Print_TriggerList(res,false)) false None ;
  Buffer.contents buff
)
;;

let emit_dlg_files game output_dir =
  if (!Dc.d_action_list <> []) then begin
    (try
      Stats.time "process .D files" (Dc.dc game) !Dc.d_action_list ;
    with e ->
      Dc.clear_state () ;
      (*log_and_print "ERROR: problem processing D files: %s\n"
	(Printexc.to_string e) ; *)
      raise e) ;
    Hashtbl.iter (fun name dlg ->
      let filename = output_dir ^ "/" ^ name ^ ".dlg" in
      try
	let out_buff = Buffer.create (1024 * 32) in
	Dlg.save_dlg dlg out_buff ;
	Stats.time "saving files" (fun () ->
	  let out_chan = open_for_writing filename true in
	  Buffer.output_buffer out_chan out_buff ;
	  close_out out_chan ;
				  ) ()
      with e ->
	Dc.clear_state () ;
	log_and_print "ERROR: problem saving [%s]: %s\n" filename
	  (Printexc.to_string e) ;
	raise e
		 ) Dc.available_dlgs ;
  end ;
  Dc.clear_state ()

let tp2_queues: string Queue.t = Queue.create ()

let enqueue_tp2_filename filename =
  log_only "Enqueuing [%s] for TP2 processing.\n" filename ;
  Queue.add filename tp2_queues
;;