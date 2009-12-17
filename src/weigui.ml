(* Note added due to LGPL terms.

This file was created by Valerio Bigiani, AKA The Bigg, starting from
28 April 2006, and taken from the src/main.ml file from Westley Weimer's
WeiDU 185, as subsequently edited by me. *)

open Util
open Version
open Tk

let win = openTk ();;

(* big generic parsing function *)
let parse_buffer filename buffer sort_of_file parse_lex_fun =
  try
    begin
    let lexbuf : Lexing.lexbuf = lex_init_from_string filename buffer in
    try
      let result = Stats.time sort_of_file
        (fun () -> parse_lex_fun lexbuf) () in
      pop_context () ;
      log_or_print "[%s] parsed\n" filename ;
      result
    with e ->
      (try input_error "" (Printexc.to_string e) with _ -> () ) ;
      pop_context () ;
      raise e
    end
  with e ->
    log_and_print "ERROR: parsing [%s]: %s\n"
      filename (Printexc.to_string e) ;
    raise e

(* parse various files *)
let load_log () =
  try
    let result = parse_file true (File Tp.log_name) "parsing .log files"
      (Dparser.log_file Dlexer.initial) in
    List.map (fun (a,b,c,d) -> ((String.uppercase a),b,c,d,Tp.Installed)) result
  with e ->
    log_or_print "WARNING: parsing log [%s]: %s\n" Tp.log_name
      (Printexc.to_string e) ;
    []
;;

let handle_tp2_filename filename =
  let res = Tparser.parse_tp2_file (File filename)
  in
  res.Tp.tp_filename <- filename ;
  res
;;

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
      result)
    in
    result
  end else []
;;

(* Load game to be able to get component names *)
Load.game_paths := Load.registry_game_paths () ;;
let tp2_ht = Hashtbl.create 511 ;;
let tra_ht = Hashtbl.create 511 ;;
let game = Load.load_game () ;;


let get_component_name a b c =
  try
    let tp2 =
      try Hashtbl.find tp2_ht a
      with _ ->
        let res = handle_tp2_filename a in
        Hashtbl.add tp2_ht a res ; res
    in
    Dc.clear_state () ;
    Dc.push_trans ();
    (try
      let l = List.nth tp2.Tp.languages b in
      List.iter (fun s ->
        let x =
          try Hashtbl.find tra_ht s
          with _ ->
            (let x = get_tra_list_filename (Arch.backslash_to_slash s) in
            Hashtbl.add tra_ht s x; x
            )
        in
        Stats.time "adding translation strings" Dc.add_trans_strings x
      ) l.Tp.lang_tra_files ;
    with _ -> ()) ;
    let m = Tpstate.get_nth_module tp2 c true in
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
      | Some(x) -> (Dc.single_string_of_tlk_string_safe game x) ^ " -> ") in
    Dc.clear_state () ;
    Dc.pop_trans ();
    (subcomp_str ^ comp_str)
  with _ ->
    ( "???")
;;


(* some globals *)
let stack_exist stack funz =
  let result = ref false in
  Stack.iter (fun x -> if funz x then result := true) stack;
  !result
;;

let current_stuff = ref [];;
let uninstall_me = ref [];;

let strip_tp2 s =
  let s = Case_ins.filename_basename s in
  let s = Str.global_replace (Str.regexp_case_fold "^.*setup-") "" s in
  let s = Str.global_replace (Str.regexp_case_fold "\\.tp2") "" s in
  String.capitalize (String.lowercase s)
;;

let redraw_mod_list mod_list =
  Listbox.delete mod_list ~first:(`Num(0)) ~last:(`End);
  List.iter (fun (a,b,c,component_name,force_reinst) ->
    Listbox.insert mod_list ~index:`End ~texts:[match component_name with
    | "???" -> Printf.sprintf "%s %d %d (unknown name) %s" a b c (if force_reinst then "*" else "")
    | _ -> (strip_tp2 a) ^ ": " ^ component_name ^  (if force_reinst then " *" else "")];
  ) !current_stuff;
;;

(* Specialistic GUI *)
let uninst_button orig_mod_log () =
  let popup = Toplevel.create win in
  let but_frm = Frame.create popup in
  let temp_undo = Button.create ~text:"Cancel" but_frm in
  let temp_proc = Button.create ~text:"Uninstall these mods" but_frm in
  let temp_frame_log = Frame.create popup in
  let temp_mod_log = Listbox.create temp_frame_log in
  let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
  Listbox.configure temp_mod_log ~selectmode:`Multiple ~yscrollcommand:(Scrollbar.set temp_sb);
  redraw_mod_list temp_mod_log;
  let temp_frame_lab = Frame.create popup in
  let temp_label = Label.create ~text:"Choose the mods to uninstall" popup in
  let temp_f () = destroy popup in
  let temp_g () =
    let new_current_stuff = ref (!current_stuff) in
    let n = List.length !current_stuff in
    current_stuff := [] ;
    let found_force_reinst = ref false in
    for i = 0 to n - 1 do
      let ((a1,a2,a3,a4,a5),b) = (List.hd !new_current_stuff, List.tl !new_current_stuff) in
      new_current_stuff := b;
      if a5 then found_force_reinst := true;
      if List.mem (`Num(i)) (Listbox.curselection temp_mod_log) then found_force_reinst := true;
      if not( List.mem (`Num(i)) (Listbox.curselection temp_mod_log)) then
        current_stuff := (a1,a2,a3,a4,a5 || !found_force_reinst) :: !current_stuff
      else uninstall_me := (a1,a2,a3) :: !uninstall_me;
    done ;
    current_stuff := List.rev !current_stuff;
    redraw_mod_list orig_mod_log;
    destroy popup
  in
  Button.configure temp_undo ~command:temp_f;
  Button.configure temp_proc ~command:temp_g;
  pack ~fill:`X [temp_frame_lab];
  pack ~inside:temp_frame_lab [temp_label];
  pack ~fill:`Both ~expand:true [temp_frame_log];
  pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
  pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
  pack [but_frm];
  pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let reinst_button orig_mod_log () =
  let popup = Toplevel.create win in
  let but_frm = Frame.create popup in
  let temp_undo = Button.create ~text:"Cancel" but_frm in
  let temp_proc = Button.create ~text:"Force Reinstall of this mod" but_frm in
  let temp_frame_log = Frame.create popup in
  let temp_mod_log = Listbox.create temp_frame_log in
  let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
  Listbox.configure temp_mod_log ~yscrollcommand:(Scrollbar.set temp_sb);
  redraw_mod_list temp_mod_log;
  let temp_frame_lab = Frame.create popup in
  let temp_label = Label.create ~text:"Choose the mod to reinstall (simply choose the first one if you need to reinstall multiple ones)" popup in
  let temp_f () = destroy popup in
  let temp_g () =
    let new_current_stuff = ref !current_stuff in
    let n = List.length !current_stuff in
    current_stuff := [] ;
    let found_force_reinst = ref false in
    for i = 0 to n - 1 do
      let ((a1,a2,a3,a4,a5),b) = (List.hd !new_current_stuff, List.tl !new_current_stuff) in
      new_current_stuff := b;
      if a5 then found_force_reinst := true;
      if List.mem (`Num(i)) (Listbox.curselection temp_mod_log) then begin
        found_force_reinst := true;
        uninstall_me := (a1,a2,a3) :: !uninstall_me;
      end;
      current_stuff := (a1,a2,a3,a4,a5 || !found_force_reinst) :: !current_stuff;
    done ;
    current_stuff := (List.rev !current_stuff);
    redraw_mod_list orig_mod_log;
    destroy popup
  in
  Button.configure temp_undo ~command:temp_f;
  Button.configure temp_proc ~command:temp_g;
  pack ~fill:`X [temp_frame_lab];
  pack ~inside:temp_frame_lab [temp_label];
  pack ~fill:`Both ~expand:true [temp_frame_log];
  pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
  pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
  pack [but_frm];
  pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let inst_button_position popup orig_mod_log tp2_file tp2_parsed lang components =
  let but_frm = Frame.create popup in
  let temp_undo = Button.create ~text:"Cancel" but_frm in
  let temp_proc = Button.create ~text:"Apply your settings" but_frm in
  let temp_frame_log = Frame.create popup in
  let temp_mod_log = Listbox.create temp_frame_log in
  let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
  Listbox.configure temp_mod_log ~yscrollcommand:(Scrollbar.set temp_sb);
  redraw_mod_list temp_mod_log;
  Listbox.insert temp_mod_log ~index:`End ~texts:["[At the end]"];
  Listbox.selection_set temp_mod_log ~first:`End ~last:`End;
  Listbox.yview_index temp_mod_log ~index:`End;
  let temp_frame_lab = Frame.create popup in
  let temp_label = Label.create ~text:"Choose the position where to install these components: just before..." popup in
  let temp_f () = destroy popup in
  let temp_g () =
    let x = Listbox.curselection temp_mod_log in
    let x = ref (match x with
      | [`Num(y)] -> y
      | _ -> failwith "Problem decrypting your position choice"
    )
    in
    let uninst_if_pres the_l =
      let a1 = String.uppercase tp2_file in
      let new_current_stuff = ref (!the_l) in
      let n = List.length !the_l in
      the_l := [] ;
      let found_force_reinst = ref false in
      for i = 0 to n - 1 do
        let ((c1,c2,c3,c4,c5),b) = (List.hd !new_current_stuff, List.tl !new_current_stuff) in
        new_current_stuff := b;
        if c5 then found_force_reinst := true;
        let touninst = List.exists (fun c -> c1 = a1 && c3 = c) components in
        if touninst then found_force_reinst := true;
        if not touninst then
          the_l := (c1,c2,c3,c4,c5 || !found_force_reinst) :: !the_l
        else begin
          uninstall_me := (c1,c2,c3) :: !uninstall_me;
          if !x > i then decr x;
        end;
      done ;
      the_l := List.rev !the_l;
    in
    uninst_if_pres current_stuff;
    let new_current_stuff = ref (!current_stuff) in
    let n = List.length !current_stuff in
    current_stuff := [] ;
    let found_force_reinst = ref false in
    let i_did_it = ref false in
    for i = 0 to n - 1 do
      let ((a1,a2,a3,a4,a5),b) = (List.hd !new_current_stuff, List.tl !new_current_stuff) in
      new_current_stuff := b;
      if a5 then found_force_reinst := true;
      if List.mem (`Num(i)) (Listbox.curselection temp_mod_log) then found_force_reinst := true;
      if !x = i then begin
        found_force_reinst := true;
        List.iter (fun y ->
          current_stuff := (String.uppercase tp2_file,lang,y,get_component_name tp2_file lang y,true) :: !current_stuff
        ) components;
        i_did_it := true;
      end;
      current_stuff := (a1,a2,a3,a4,a5 || !found_force_reinst) :: !current_stuff;
    done ;
    if not !i_did_it then begin
      List.iter (fun y ->
        current_stuff := (String.uppercase tp2_file,lang,y,get_component_name tp2_file lang y,true) :: !current_stuff
      ) components;
    end ;
    current_stuff := List.rev !current_stuff;
    redraw_mod_list orig_mod_log;
    destroy popup;
  in
  Button.configure temp_undo ~command:temp_f;
  Button.configure temp_proc ~command:temp_g;
  pack ~fill:`X [temp_frame_lab];
  pack ~inside:temp_frame_lab [temp_label];
  pack ~fill:`Both ~expand:true [temp_frame_log];
  pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
  pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
  pack [but_frm];
  pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let inst_button_comps popup orig_mod_log tp2_file tp2_parsed lang =
  let length = Tpstate.get_last_module_index tp2_parsed in
  let shown_ht = Hashtbl.create 100 in
  let but_frm = Frame.create popup in
  let temp_undo = Button.create ~text:"Cancel" but_frm in
  let temp_proc = Button.create ~text:"Next" but_frm in
  let temp_frame_log = Frame.create popup in
  let temp_mod_log = Listbox.create ~selectmode:`Multiple temp_frame_log in
  let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
  Listbox.configure temp_mod_log ~yscrollcommand:(Scrollbar.set temp_sb);
  let counter = ref 0 in
  for i = 0 to length do
    try
      let the_comp = Tpstate.get_nth_module tp2_parsed i false in
      let isok = List.fold_left (fun acc this -> 
        match this with
          | Tp.TPM_Deprecated(_) -> false
          (* | Tp.TPM_Require_foo(_) -> let condition_matched = some_magic in condition_matched && acc *)
          | _ -> acc
      ) true the_comp.Tp.mod_flags in
      if isok then begin
        let name = get_component_name tp2_file lang i in
        Hashtbl.add shown_ht !counter i;
        incr counter;
        Listbox.insert temp_mod_log ~index:`End ~texts:[match name with
        | "???" -> Printf.sprintf "%s %d %d (unknown name)" tp2_file lang i
        | _ -> (strip_tp2 tp2_file) ^ ": " ^ name];
      end;
    with _ -> ()
  done;
  let temp_frame_lab = Frame.create popup in
  let temp_label = Label.create ~text:"Choose the components(s) to install" popup in
  let temp_f () = destroy popup in
  let temp_g () =
    let x = List.map (fun (`Num(x)) -> Hashtbl.find shown_ht x) (Listbox.curselection temp_mod_log) in
    destroy temp_frame_lab;
    destroy temp_frame_log;
    destroy but_frm;
    inst_button_position popup orig_mod_log tp2_file tp2_parsed lang x;
  in
  Button.configure temp_undo ~command:temp_f;
  Button.configure temp_proc ~command:temp_g;
  pack ~fill:`X [temp_frame_lab];
  pack ~inside:temp_frame_lab [temp_label];
  pack ~fill:`Both ~expand:true [temp_frame_log];
  pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
  pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
  pack [but_frm];
  pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let inst_button_lang popup orig_mod_log tp2_file =
  let tp2 =
    try Hashtbl.find tp2_ht tp2_file
    with _ ->
      let res = handle_tp2_filename tp2_file in
      Hashtbl.add tp2_ht tp2_file res ; res
  in
  match tp2.Tp.languages with
      [] -> inst_button_comps popup orig_mod_log tp2_file tp2 0;
    | [l] -> inst_button_comps popup orig_mod_log tp2_file tp2 0;
    | _ ->
      let but_frm = Frame.create popup in
      let temp_undo = Button.create ~text:"Cancel" but_frm in
      let temp_proc = Button.create ~text:"Next" but_frm in
      let temp_frame_log = Frame.create popup in
      let temp_mod_log = Listbox.create temp_frame_log in
      let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
      Listbox.configure temp_mod_log ~yscrollcommand:(Scrollbar.set temp_sb);
      let arr = Array.of_list tp2.Tp.languages in
        Array.iteri (fun i l ->
          Listbox.insert temp_mod_log ~index:`End ~texts:[l.Tp.lang_name]) arr ;
      let temp_frame_lab = Frame.create popup in
      let temp_label = Label.create ~text:"Choose the language" popup in
      let temp_f () = destroy popup in
      let temp_g () =
        let x = Listbox.curselection temp_mod_log in
        let x = match x with
          | [`Num(x)] -> x
          | _ -> 0
        in
        destroy temp_frame_lab;
        destroy temp_frame_log;
        destroy but_frm;
        inst_button_comps popup orig_mod_log tp2_file tp2 x;
      in
      Button.configure temp_undo ~command:temp_f;
      Button.configure temp_proc ~command:temp_g;
      pack ~fill:`X [temp_frame_lab];
      pack ~inside:temp_frame_lab [temp_label];
      pack ~fill:`Both ~expand:true [temp_frame_log];
      pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
      pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
      pack [but_frm];
      pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let inst_button_tp2 orig_mod_log () =
  let popup = Toplevel.create win in
  let but_frm = Frame.create popup in
  let temp_undo = Button.create ~text:"Cancel" but_frm in
  let temp_proc = Button.create ~text:"Next" but_frm in
  let temp_frame_log = Frame.create popup in
  let temp_mod_log = Listbox.create temp_frame_log in
  let temp_sb = Scrollbar.create temp_frame_log ~command:(Listbox.yview temp_mod_log) in
  Listbox.configure temp_mod_log ~yscrollcommand:(Scrollbar.set temp_sb);
  let dh = Case_ins.unix_opendir "." in
  begin
    try
      while true do
        let x = Unix.readdir dh in
        let y = Case_ins.unix_stat x in
        let check_and_do file =
          if file_size file >= 0 then Listbox.insert ~index:`End ~texts:[file] temp_mod_log;
        in
        match y.Unix.st_kind with
          | Unix.S_REG -> if (String.uppercase(snd (split x))) = "TP2" then check_and_do x;
          | Unix.S_DIR -> check_and_do (x ^ "/" ^ x ^ ".tp2"); check_and_do (x ^ "/setup-" ^ x ^ ".tp2")
          | _ -> ()
      done;
    with End_of_file -> ()
  end;
  let temp_frame_lab = Frame.create popup in
  let temp_label = Label.create ~text:"Choose the mod to install" popup in
  let temp_f () = destroy popup in
  let temp_g () =
    let x = Listbox.get ~index:`Active temp_mod_log in
    destroy temp_frame_lab;
    destroy temp_frame_log;
    destroy but_frm;
    destroy temp_undo;
    destroy temp_proc;
    inst_button_lang popup orig_mod_log x;
  in
  Button.configure temp_undo ~command:temp_f;
  Button.configure temp_proc ~command:temp_g;
  pack ~fill:`X [temp_frame_lab];
  pack ~inside:temp_frame_lab [temp_label];
  pack ~fill:`Both ~expand:true [temp_frame_log];
  pack ~side:`Right ~anchor:`E ~inside:temp_frame_log ~fill:`Y [temp_sb];
  pack ~side:`Right ~inside:temp_frame_log ~fill:`Both ~expand:true [temp_mod_log];
  pack [but_frm];
  pack [temp_undo; temp_proc] ~side:`Right ~inside:but_frm;
;;

let proceed original_stuff skip_readme () =
  let oldname = ref "" in
  let howmany = ref 0 in
  let counter = ref 0 in
  let command = Buffer.create 10000 in
  List.iter (fun(tp2,lang,comp,_,_) ->
    incr counter;
    if (List.exists (fun (a,b,c,d,e) -> (a = tp2) && (c = comp) && e) !current_stuff) ||
       (List.exists (fun (a,b,c) -> (a = tp2) && (c = comp)) !uninstall_me) then howmany := !counter;
  ) (List.rev original_stuff);
  let os = ref (List.rev original_stuff) in
  for i = 1 to !howmany do
    let ((tp2,lang,comp,_,_),b) = (List.hd !os, List.tl !os) in
    os := b;
    if !oldname <> tp2 then Buffer.add_string command (Printf.sprintf "\n%s %d U " tp2 lang);
    Buffer.add_string command (Printf.sprintf "%d %!" comp);
    oldname := tp2;
  done;
  oldname := "";
  List.iter (fun (tp2,lang,comp,_,d) -> if d then begin
    if !oldname <> tp2 then Buffer.add_string command (Printf.sprintf "\n%s %d I " tp2 lang);
    Buffer.add_string command (Printf.sprintf "%d " comp);
    oldname := tp2;
  end) !current_stuff;
  Buffer.output_buffer stdout command;
  let oc = Case_ins.perv_open_out "weidu.command" in
  Buffer.output_buffer oc command;
  close_out oc;

  ignore(Unix.system (Printf.sprintf "%s %s --process-script weidu.command --tlkout dialog.tlk --ftlkout dialogf.tlk --autolog" Case_ins.weidu_executable (if skip_readme then "--skip-at-view" else "")));
  closeTk();
;;

(* basic GUI *)
appname_set (Printf.sprintf "weigui version %s" version);;

let frame_log = Frame.create win;;
let mod_log = Listbox.create frame_log;;
let sb = Scrollbar.create frame_log ~command:(Listbox.yview mod_log);;
Listbox.configure mod_log ~yscrollcommand:(Scrollbar.set sb);;

let frame_lab = Frame.create win;;
let label1 = Label.create ~text:(Printf.sprintf "Welcome to the weigui utility based on WeiDU version %s" version) win;;
let label2 = Label.create ~text:"Mods currently installed:" win;;

let frame_com = Frame.create win;;
let inst = Button.create ~text:"Install another mod" frame_com;;
let uninst = Button.create ~text:"Uninstall an existing mod" frame_com;;
let reinst = Button.create ~text:"Reinstall an existing mod" frame_com;;
let proc = Button.create ~text:"Proceed to setup your game" frame_com;;
let undo = Button.create ~text:"Undo all changes" frame_com;;

pack ~fill:`X [frame_lab];;
pack ~inside:frame_lab [label1; label2];;
pack ~fill:`Both ~expand:true [frame_log];;
pack ~side:`Right ~anchor:`E ~inside:frame_log ~fill:`Y [sb];;
pack ~side:`Right ~inside:frame_log ~fill:`Both ~expand:true [mod_log];;
pack ~fill:`X ~side:`Bottom [frame_com];;
pack ~side:`Left ~anchor:`Center ~inside:frame_com ~fill:`X ~expand:true [inst; uninst; reinst; proc; undo];;

let main () =
  init_log Version.version "weigui.log";
  let no_readme = try Sys.argv.(1) = "--skip-at-view" with _ -> false in
  let original_stuff = load_log () in
  log_and_print "mods currently installed: \n";
  let original_stuff = List.map (fun (a,b,c,_,_) ->
    let component_name = get_component_name a b c in
    (a,b,c,component_name,false)
  ) original_stuff in
  current_stuff := original_stuff;
  Button.configure undo ~command:(let x () = current_stuff := original_stuff; redraw_mod_list mod_log;
                                             uninstall_me := [] in x);
  Button.configure uninst ~command:(uninst_button mod_log); 
  Button.configure reinst ~command:(reinst_button mod_log);
  Button.configure inst ~command:(inst_button_tp2 mod_log);
  Button.configure proc ~command:(proceed original_stuff no_readme);
  redraw_mod_list mod_log;
;;

try
  main ();
  mainLoop();
with e -> log_and_print "uncaught exception: %s\n" (Printexc.to_string e);
;;
