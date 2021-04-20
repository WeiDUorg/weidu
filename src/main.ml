(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   6 November 2005. All changes for this file are listed in
   diffs/src.main.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

open BatteriesInit
open Hashtblinit
open Util
open Myxdiff
open Version
open Parsewrappers

let pause_at_end = ref false
let no_exit_pause = ref false

type output_info = {
    mutable dir : string ;
    mutable file : string ;
    mutable chan : out_channel lazy_t ;
    mutable append : bool ;
  }

  let theout = {
    dir = ".";
    file = "";
    chan = lazy(stdout) ;
    append = false ;
  }

  let print_theout fmt = Printf.fprintf (Lazy.force theout.chan) fmt
  let output_theout s = print_theout "%s" s
  let output_buffer_theout b = print_theout "%s" (Buffer.contents b)
  let flush_theout () = flush (Lazy.force theout.chan)

  let handle_out_boringness file ext_chop =
    try
      if !debug_ocaml then log_and_print "\n\nhandle_out_boringness received %s as a file name.\n" file ;
      if true then begin match theout.dir, theout.file with
      | ".", "" -> if !debug_ocaml then log_and_print "no --out\n"
      | _, "" -> if !debug_ocaml then log_and_print "--out a_dir: %s\n"  theout.dir
      | ".", _ -> if !debug_ocaml then log_and_print "--out a_file: %s\n"  theout.file
      | _ -> if !debug_ocaml then log_and_print "You're a pervert, decide where to put your stuff!\n"
      end ;
      let ext_chop = List.map String.uppercase ext_chop in
      let directory,(name,ext) = (Case_ins.filename_dirname file,split(Case_ins.filename_basename file)) in
      let fullname = match theout.dir, theout.file with
      | ".", "" -> file
      | _, "" -> theout.dir ^
          (match (String.get theout.dir (String.length theout.dir - 1)) with
          | '/'
          | '\\' -> ""
          | _ -> "/") ^ name ^ "." ^ ext
      | ".", _ -> theout.file
      | _ -> file
      in
      let base,ext = split fullname in
      let result =
        if List.mem (String.uppercase ext) ext_chop then
          Case_ins.filename_chop_extension fullname
        else
          fullname
      in
      if !debug_ocaml then log_and_print "handle_out_boringness returns %s\n" result ;
      result
    with _ -> if !debug_ocaml then log_and_print "something happened...\nDefaulting to %s\n" file ; file
;;

  let set_theout app s =
    theout.append <- app ;
    if is_directory s then begin
      theout.dir <- s
    end else begin
      theout.file <- s ;
      if app then
        theout.chan <- lazy (Case_ins.perv_open_out_gen [Open_append ; Open_wronly ; Open_creat ; Open_text ] 511 s)
      else
        theout.chan <- lazy (Case_ins.perv_open_out_gen [ Open_wronly ; Open_creat ; Open_text] 511 s)
    end;
;;

let if_bgee_check_lang_or_fail game =
  if Load.enhanced_edition_p game && not !Load.have_bgee_lang_dir_p then
    failwith "ERROR: this is a multi-language game, \
      but you have not set which language to use. \
  Specify --use-lang at least once."

let force_script_style game forced_script_style exe_name =
  game.Load.script_style <- forced_script_style ;
  ignore (match game.Load.script_style with
  | Load.BG2 -> Tlk.is_bg2 := true
  | Load.PST
  | Load.BG1
  | Load.IWD1
  | Load.IWD2
  | Load.NONE -> Tlk.is_bg2 := false) ;
  let s = match game.Load.script_style with
  | Load.PST -> "PST"
  | Load.BG1 -> "BG1"
  | Load.BG2 -> "BG2"
  | Load.IWD1 -> "IWD1"
  | Load.IWD2 -> "IWD2"
  | Load.NONE -> "NONE (ERROR!!!)"
  in log_and_print "[%s] Using scripting style \"%s\"\n" exe_name s;
;;

let forceify_file forceify game =
  if_bgee_check_lang_or_fail game ;
  (match forceify with
    Some(file) -> begin
      try
        let name,ext = split (String.uppercase file) in
        Dlg.local_string_ht := Some([]) ;
        begin
          match ext with
            "D" -> ignore (
              let inchan = Case_ins.perv_open_in file in
              let lexbuf = lex_init file inchan in
              ignore (Stats.time "parsing .D files"
                        (fun () -> Dparser.d_file Dlexer.initial lexbuf) ());
              close_in inchan ;)
          | "TP2" -> ignore
                (Tparser.parse_tp2_file (File file))
          | "TPA"
          | "TPH" -> ignore
                (Tparser.parse_tpa_file (File file))
          | "TPP" -> ignore
                (Tparser.parse_tpp_file (File file))
          | _ -> log_and_print "ERROR: don't know how to --forceify files with extension [%s]\n" ext ; failwith ext
        end ;
        pop_context ();
        log_or_print "[%s] parsed\n" file ;

        let buf = ref (load_file file) in

        let dout = output_theout in

        let replace lse str =
          let my_regexp = Str.regexp (Str.quote str) in
          try
            let num = Tlk.find_string_fast lse (Load.get_active_dialog game)
                (Load.get_active_dialogf_opt game) game.Load.dialog_search
            in
            let replace_with = Printf.sprintf "!%d %s" num str in
            buf := Str.global_replace my_regexp replace_with !buf ;
          with Not_found ->
            log_and_print "WARNING: cannot find [%s] in dialog.tlk: not --forceifying that string\n" lse.lse_male
        in

        (match !Dlg.local_string_ht with
          Some(lst) ->
            List.iter (fun ls -> match ls with
            | Dlg.Local_String(lse) ->
                if lse.lse_male <> "" then begin
                  replace lse ("~" ^ lse.lse_male ^ "~" );
                  replace lse ("%" ^ lse.lse_male ^ "%" );
                  replace lse ("\"" ^ lse.lse_male ^ "\"" );
                end
            | _ -> failwith "forceify1") (List.rev lst);
        | None -> failwith "forceify2");

        Dlg.local_string_ht := None ;
        dout !buf ;
        ()

      with e ->
        log_and_print "ERROR: problem force-ifying file [%s]: %s\n" file
          (printexc_to_string e) ;
        raise e
    end
  | _ -> ()) ;
;;

let make_tlk_from_file game make_tlk =
  if_bgee_check_lang_or_fail game ;
  begin
  let results : (int * local_string_entry) list =
    List.fold_left (fun acc filename ->
      let result = parse_file true (File filename) "parsing .tra files"
          (Dparser.tra_file Dlexer.initial)
      in
      log_or_print "[%s] has %d translation strings\n" filename (List.length result);
      let result = List.rev_map (fun (i,ts) -> match ts with
        Dlg.Local_String(lse) -> (i,lse)
      | _ -> failwith "make_tlk") result
      in
      List.rev_append acc result) [] make_tlk
  in
  let max = 1 + (List.fold_left (fun acc (i,elt) ->
    if i > acc then i else acc) 0 results)
  in
  log_and_print "New TLK will have %d entries\n" max ;
  let new_tlk = Array.make max ( { Tlk.flags = 7 ;
                                   Tlk.sound_name = "";
                                   Tlk.volume = 0;
                                   Tlk.pitch = 0;
                                   Tlk.text = ""; } )
  in
  let new_tlkf = Array.copy new_tlk in
  List.iter (fun (i,lse) ->
    let male, female = Tlk.lse_to_tlk_string lse in
    new_tlk.(i) <- male ;
    new_tlkf.(i) <- female) results ;
  let d_pair = Load.get_active_dialogs game in
  d_pair.Load.dialog_mod <- true ;
  d_pair.Load.dialog.Load.contents <- new_tlk ;
  (match d_pair.Load.dialogf with
  | None -> ()
  | Some f -> f.Load.contents <- new_tlkf ;
      d_pair.Load.dialogf_mod <- true)
  end

let extract_tlk_to_file game user_min user_max strfind_list traify_num =
  if_bgee_check_lang_or_fail game ;
  begin
    let tlk = (Load.get_active_dialog game) in
    let ftlk = (Load.get_active_dialogf_opt game) in
    let my_min = match user_min with
      Some(i) -> i
    | None -> 0
    in
    let my_max = match user_max with
      Some(i) -> i
    | None -> (Array.length tlk) - 1
    in
    let reg_list = List.map Str.regexp_case_fold strfind_list in
    for i = my_min to my_max do
      let matches = reg_list = [] ||
      List.fold_left (fun acc r -> acc ||
      try
        let _ = Str.search_forward r tlk.(i).Tlk.text 0 in
        true
      with _ -> false
          ) false reg_list
      in
      if matches then begin
        print_theout "@%-5d =" (i + traify_num);
        let display ts = begin
          print_theout " %s" (Tlk.weidu_quotify ts.Tlk.text) ;
          if ts.Tlk.sound_name <> "" then
            print_theout " [%s]" ts.Tlk.sound_name ;
        end in
        display tlk.(i) ;
        (match ftlk with
          None -> ()
        | Some(a) ->
            if a.(i).Tlk.text <> tlk.(i).Tlk.text ||
            a.(i).Tlk.sound_name <> tlk.(i).Tlk.sound_name then
              display a.(i) )  ;
        print_theout "\n" ;
      end
    done
  end ;
;;

let cmp_binary_file cmp_dest cmp_src game =
  (match cmp_dest with
    Some(d) ->
      let b1,s = match cmp_src with
      | Some (x) -> (load_file x,x)
      | None ->
          let (base,ext) = split (Case_ins.filename_basename d) in
          (fst (Load.load_resource "cmp-from" game true base ext),Case_ins.filename_basename d)
      in
      let b2 = load_file d in
      let l1 = String.length b1 in
      let l2 = String.length b2 in
      if cmp_src = None then
        print_theout "COPY_EXISTING ~%s~ ~override~\n" s
      else
        print_theout "COPY ~%s~ ~%s~\n" s d;
      print_theout "// patches to turn [%s] into [%s]\n" s d ;
      if (l1 <> l2) then begin
        print_theout "\t//[%s] is %d bytes while [%s] is %d bytes\n"
          s l1 d l2
      end;
      let (b1,b2) =
        if (l1 < l2) then begin
          print_theout "\tINSERT_BYTES 0x%x 0x%x\n" l1 (l2 - l1);
          b1 ^ String.make (l2 - l1) '\000',b2
        end else if (l1 > l2) then begin
          print_theout "\tDELETE_BYTES 0x%x 0x%x\n" l2 (l1 - l2);
          (String.sub b1 0 l2), b2
        end else begin
          b1,b2
        end
      in
      for i = 0 to l2 - 1 do
        if b1.[i] <> b2.[i] then begin
          print_theout "\tWRITE_BYTE 0x%x %d // 0x%02x"
            i (Char.code b2.[i]) (Char.code b2.[i]);
          if Char.code b2.[i] > 0x20 && Char.code b2.[i] < 0x80 then
            print_theout " == %c" b2.[i];
          print_theout "\n";
        end
      done
  | _ -> ()) ;
;;

let cmp_d_file dcmp_dest dcmp_src game =
  if_bgee_check_lang_or_fail game ;
  (match dcmp_dest with
    Some(d) ->
      let buff,s = match dcmp_src with
      | Some (x) -> (load_file x,x)
      | None ->
          let (base,ext) = split (Case_ins.filename_basename d) in
          (fst (Load.load_resource "cmp-from" game true base ext),Case_ins.filename_basename d)
      in
      let b,e = split s in
      let imp_base = Case_ins.filename_basename b in
      let s_dlg = Dlg.load_dlg imp_base buff in

      let b,e = split d in
      let buff, final_path = Load.load_resource "DLG compare command" game true b e in
      let imp_base = Case_ins.filename_basename b in
      let d_dlg = Dlg.load_dlg imp_base buff in

      let new_buffer = Buffer.create (1024 * 32) in
      Dlg.dlg_compare new_buffer s_dlg d_dlg (Load.get_active_dialog game) (Load.get_active_dialogf_opt game) reprint_d_action ;
      print_theout "<<<<<<<< .../%s\n" s;
      output_buffer_theout new_buffer;
      print_theout ">>>>>>>>\nCOMPILE ~.../%s~\n" s;

  | _ -> ()) ;
;;

let cmp_text_file textcmp_dest textcmp_src game =
  (match textcmp_dest with
    Some(d) ->
      let src_buff,s = match textcmp_src with
      | Some (x) -> (load_file x,x)
      | None ->
          let (base,ext) = split (Case_ins.filename_basename d) in
          (fst (Load.load_resource "cmp-from" game true base ext),Case_ins.filename_basename d)
      in
      let out_name = ".../" ^ d ^ ".patch" in
      let b,e = split d in
      let dest_buff, final_path = Load.load_resource "BCS patch command" game true b e in begin
        try begin
          let res = Diff.get_patch src_buff dest_buff 20 in
          print_theout "<<<<<<<< %s\n%s>>>>>>>>\n" out_name res;
          print_theout "// TP2 patch to turn %s into %s. For example using:\n" s d;
          if textcmp_src = None then
            print_theout "COPY_EXISTING ~%s~ ~override~\n" s
          else
            print_theout "COPY ~%s~ ~%s~\n" s d;
          print_theout "\tAPPLY_BCS_PATCH ~%s~\n" out_name
        end
        with e ->
          log_and_print "Failed to create patch for [%s] to [%s] : %s\n" s d
            (printexc_to_string e)
      end
  | _ -> ()) ;
;;

let cmp_bcs_file bcmp_dest bcmp_src game =
  (match bcmp_dest with
    Some(d) ->
      let decompile (buff,patch_filename) =
        let bcs = handle_script_buffer (patch_filename ^ ".BCS") buff in
        let out_buff = Buffer.create 40960 in
        Bcs.print_script_text game (Bcs.Save_BCS_Buffer(out_buff))
          (Bcs.BCS_Print_Script(bcs)) false None ;
        Buffer.contents out_buff
      in
      let src_buff,s = match bcmp_src with
      | Some (x) -> (decompile (load_file x,x),x)
      | None ->
          let (base,ext) = split (Case_ins.filename_basename d) in
          (decompile(Load.load_resource "cmp-from" game true base ext),Case_ins.filename_basename d)
      in
      let out_name = ".../" ^ d ^ ".patch" in
      let b,e = split d in
      let dest_buff = decompile (Load.load_resource "BCS patch command" game true b e) in
      begin
        try begin
          let res = Diff.get_patch src_buff dest_buff 20 in
          print_theout "<<<<<<<< %s\n%s>>>>>>>>\n" out_name res;
          print_theout "// TP2 patch to turn %s into %s. For example using:\n" s d;
          if bcmp_src = None then
            if String.uppercase e = "BS" then
              print_theout "COPY ~scripts/%s~ ~scripts~\n" s
            else
              print_theout "COPY_EXISTING ~%s~ ~override~\n" s
          else
            print_theout "COPY ~%s~ ~%s~\n" s d;
          print_theout "\tDECOMPILE_BCS_TO_BAF\n";
          print_theout "\tAPPLY_BCS_PATCH ~%s~\n" out_name;
          print_theout "\tCOMPILE_BAF_TO_BCS\n";
        end
        with e ->
          log_and_print "Failed to create patch for [%s] to [%s] : %s\n" s d
            (printexc_to_string e)
      end
  | _ -> ()) ;
;;

let rcmp_file game rcmp_src rcmp_dest =
  (match rcmp_src, rcmp_dest with
  | Some (s), Some(d) ->
      let load file =
        let a,b = split (Filename.basename file) in
        let buff = try load_file file with e -> fst (Load.load_resource "--rcmp" game true a b) in
        match String.uppercase b with
        | "DLG" -> buff
        | "BCS"
        | "BS" -> buff
        | _ -> buff
      in
      Diff.compare_rt print_theout (load s) (load d)
  | _ -> ()) ;
;;

let diff_patch_file bcmp_orig bcmp_patch game =
  (match bcmp_orig,bcmp_patch with
    Some(s),Some(d) ->
      let b,e = split s in
      let orig_buff, final_path = Load.load_resource "BCS patch compare command" game true b e in
      let b,e = split d in
      let patch_buff, final_path = Load.load_resource "BCS patch compare command" game true b e in begin
        try begin
          let new_buff, bad_chunks, app_chunks = Diff.do_patch orig_buff patch_buff true in begin
            if ( bad_chunks > 0 ) then begin
              log_and_print "ERROR: Cannot apply patch %s (%d bad chunks).\n" d bad_chunks ;
              failwith "Cannot Apply Patch"
            end ;
            if ( app_chunks > 0 ) then begin
              log_and_print "WARNING: %d chunks in patch file %s already applied.\n" app_chunks d
            end ;
            if (bad_chunks == 0) then
              if (new_buff = orig_buff) then
                log_and_print "File %s unchanged by patch %s.\n" s d
              else
                let out_name = s ^ ".new" in
                let out = Case_ins.perv_open_out_bin out_name in begin
                  log_and_print "Saving new file to %s\n" out_name ;
                  output_string out new_buff ;
                  close_out out
                end
          end
        end
        with e ->
          log_and_print "Failed to patch file [%s] with patch [%s] : %s\n" s d
            (printexc_to_string e)
      end
  | _,_ -> ()) ;
;;

let cmp_tlk_file tlkcmp_src tlkcmp_dest tlkcmp_strings user_min user_max =
  (match tlkcmp_src,tlkcmp_dest with
    Some(s),Some(d) ->
      let stlk = Tlk.load_tlk s in
      let dtlk = Tlk.load_tlk d in
      if Array.length stlk <> Array.length dtlk then begin
        log_and_print "WARNING: %s has %d entries, %s has %d entries\n"
          s (Array.length stlk) d (Array.length dtlk)
      end ;
      let my_min = match user_min with
        Some(i) -> i
      | None -> 0
      in
      let my_max = match user_max with
        Some(i) -> i
      | None -> (min (Array.length stlk) (Array.length dtlk)) - 1
      in
      print_theout "<<<<<<<< .../tlkcmp.tra\n" ;
      let stlk_cmp = Array.map (fun entry -> {entry with Tlk.sound_name = String.uppercase entry.Tlk.sound_name}) stlk in
      let dtlk_cmp = Array.map (fun entry -> {entry with Tlk.sound_name = String.uppercase entry.Tlk.sound_name}) dtlk in
      for i = my_min to my_max do
        if (stlk_cmp.(i).Tlk.text <> dtlk_cmp.(i).Tlk.text) ||
        (stlk_cmp.(i).Tlk.sound_name <> dtlk_cmp.(i).Tlk.sound_name) then
          print_theout "@%d = ~%s~ [%s]\n"
            (1000000 + i) dtlk.(i).Tlk.text dtlk.(i).Tlk.sound_name
      done ;
      print_theout ">>>>>>>>\n\nLOAD_TRA ~.../tlkcmp.tra~\n";
      for i = my_min to my_max do
        if (stlk_cmp.(i).Tlk.text <> dtlk_cmp.(i).Tlk.text) ||
        (stlk_cmp.(i).Tlk.sound_name <> dtlk_cmp.(i).Tlk.sound_name) then begin
          if tlkcmp_strings then
            print_theout "\tSTRING_SET %s @%d\n"
              (Tlk.weidu_quotify stlk.(i).Tlk.text) (1000000 + i)
          else
            print_theout "\tSTRING_SET %d @%d\n" i (1000000 + i)
        end
      done ;
      flush_theout ();
  | _,_ -> ()) ;
;;

let cmp_tra_file tcmp_src tcmp_dest game =
  (match tcmp_src,tcmp_dest with
    Some(s),Some(d) ->
      let tracompare s d =
        try
          let schan = Case_ins.perv_open_in s in
          let lexbuf = lex_init s schan in
          let sresult = Stats.time "parsing .TRA files" (fun () -> Dparser.tra_file Dlexer.initial lexbuf) () in
          log_or_print "[%s] parsed (%d translation strings)\n" s (List.length sresult);
          close_in schan ;
          pop_context ();

          let dchan = Case_ins.perv_open_in d in
          let lexbuf = lex_init d dchan in
          let dresult = Stats.time "parsing .TRA files" (fun () -> Dparser.tra_file Dlexer.initial lexbuf) () in
          log_or_print "[%s] parsed (%d translation strings)\n" d (List.length dresult);
          close_in dchan ;
          pop_context ();

          (* int * dlg.string list *)
          let left_out = ref [] in
          List.iter (fun (si,sv) ->
            let found = List.fold_left (fun acc (di,_) -> acc || si = di)
                false dresult in
            if not found then
              left_out := (si, sv) :: !left_out) sresult ;
          let left_out = List.sort compare !left_out in
          if left_out = [] then begin
            print_theout "\n// All Strings in [%s] are also in [%s]\n" s d ;
          end else begin
            print_theout
              "\n// Strings in [%s] that are not in [%s]:\n" s d ;
            List.iter (fun (i,v) ->
              print_theout "@%-7d = ~%s~\n" i (Dc.single_string_of_tlk_string_safe game v)) left_out ;
            print_theout "\n\n"
          end
        with e ->
          begin
            print_theout "\nThe ENTIRE FILE [%s] is missing:\n\t%s\n"
              d (printexc_to_string e) ;
            log_and_print "Skipping [%s] and [%s] : %s\n" s d
              (printexc_to_string e)
          end
      in
      if (Case_ins.unix_stat s).Unix.st_kind <> Unix.S_REG then begin
        let s_d_h = Case_ins.unix_opendir s in
        try
          while true do
            let s' = Unix.readdir s_d_h in
            let one = (s ^ "/" ^ s') in
            let two = (d ^ "/" ^ s') in
            if (Case_ins.unix_stat one).Unix.st_kind = Unix.S_REG then
              tracompare one two
          done
        with e -> (Unix.closedir s_d_h )
      end else begin
        tracompare s d
      end

  | _,_ -> ()) ;
;;

let display_string i game =
  let male = Tlk.pretty_print (Load.get_active_dialog game) i in
  let female = Tlk.pretty_print_opt (Load.get_active_dialogf_opt game) i in
  if (female = "" || male = female) then
    log_and_print "String #%d is %s\n" i male
  else
    log_and_print "String #%d is %s (MALE)\nString #%d is %s (FEMALE)\n" i male i female
;;

let display_string_by_number ds_list user_min user_max game =
  if_bgee_check_lang_or_fail game ;
  begin
    let my_min = match user_min with
      Some(i) -> i
    | None -> 0
    in
    let my_max = match user_max with
      Some(i) -> i
    | None -> (Array.length (Load.get_active_dialog game)) - 1
    in
    for i = my_min to my_max do
      display_string i game
    done
  end
;;

let display_string_by_content strfind_list game =
  if_bgee_check_lang_or_fail game ;
  begin
    let reg_list = List.map Str.regexp_case_fold strfind_list in
    Array.iteri (fun i s ->
      let matches_one =
        List.fold_left (fun acc r -> acc ||
        try
          let _ = Str.search_forward r s.Tlk.text 0 in
          true
        with _ -> false) false reg_list
      in
      if matches_one then
        log_and_print "String #%d is %s\n" i (Tlk.pretty_print (Load.get_active_dialog game) i)) (Load.get_active_dialog game)
  end ;
;;

let list_languages list_lang =
  begin match list_lang with
  | None -> ()
  | Some(x) ->
      let tp2 = handle_tp2_filename x in
      let i = ref 0 in
      List.iter (fun lang ->
        output_theout (Printf.sprintf "%d:%s\n%!" !i lang.Tp.lang_name);
        incr i) tp2.Tp.languages
  end;
;;

let list_components list_comp list_comp_lang game =
  begin match list_comp with
  | None -> ()
  | Some(x) ->
      let tp2 = handle_tp2_filename x in
      let lang = list_comp_lang in
      let tp2_ht = Hashtbl.create 511 in
      let tra_ht = Hashtbl.create 511 in
      let fake_log = ref [] in
      for i = 0 to Tpstate.get_highest_module_number tp2.Tp.module_list + 1 do
        try
          ignore (Tpstate.get_nth_module tp2 i false);
          fake_log := (x,lang,i,None,Tp.Installed) :: !fake_log;
        with _ -> ()
      done;
      output_theout (Tpstate.sprintf_log game handle_tp2_filename
                       handle_tra_filename get_tra_list_filename
                       (List.rev !fake_log) tp2_ht tra_ht false false);
  end

let list_components_json list_comp list_comp_lang game =
  begin match list_comp with
  | None -> ()
  | Some(x) ->
      let tp_file = handle_tp2_filename x in
      let lang = list_comp_lang in
      let comp_list = (List.filter (fun comp ->
        not comp.Tp.deprecated) (Tpstate.get_component_list tp_file)) in
      Dc.clear_state () ;
      Dc.push_trans () ;
      Var.var_clear_push () ;
      (try
        ignore (Tpstate.set_prelang_tp2_vars tp_file) ;
        ignore (Arch2.associate_these ());
        let tra_files = (try
          List.nth tp_file.Tp.languages list_comp_lang
        with _ -> List.nth tp_file.Tp.languages 0) in
        ignore (List.iter (fun tra_file ->
          Parsewrappers.handle_tra_filename (Var.get_string tra_file))
                  tra_files.Tp.lang_tra_files) ;
        ignore (Tpstate.set_postlang_tp2_vars tp_file) ;
      with _ -> ()) ;
      output_theout (Printf.sprintf "%s\n"
                       (Json.stringify_component_list comp_list)) ;
      Dc.clear_state () ;
      Dc.pop_trans () ;
      Var.var_pop () ;
  end

let save_component_name game =
  let old_tp_quick_log = !Tp.quick_log in
  Tp.quick_log := false;
  load_log();
  Tpstate.save_log game handle_tp2_filename handle_tra_filename get_tra_list_filename;
  Tp.quick_log := old_tp_quick_log
;;

let biff_get bg_list game =
  let files_in_chitin = Key.list_of_key_resources game.Load.key false in
  let try_to_load str = begin
    try begin
      let base,ext = split (String.uppercase str) in
      let path = theout.dir ^ "/" ^ str in
      let out = open_for_writing path true in
      if ext <> "IDS" && ext <> "2DA" then begin
        let fullpath : string = Load.copy_resource game base ext out in
        close_out out ;
        log_and_print "[%s] created from [%s]\n" path fullpath
      end else begin
        let buff, fullpath = Load.load_resource "--biff-get" game false  base ext in
        output_string out buff ;
        close_out out ;
        log_and_print "[%s] created from [%s]\n" path fullpath
      end
    end with e ->
      log_and_print "[%s] --biff-get error: %s\n" str (printexc_to_string e)
  end in

  List.iter (fun str ->
    try
      let any_matches = ref false in
      let regexp = Str.regexp_case_fold str in
      List.iter (fun possible ->
        if Str.string_match regexp possible 0 then begin
          any_matches := true ;
          try_to_load possible
        end
            ) files_in_chitin ;
      if not !any_matches then
        log_and_print "\nNo matches for: %s\n" str
    with e ->
      log_and_print "\nERROR: %s\n" (printexc_to_string e)
        ) bg_list
;;

let make_biff_from_dir make_biff game =
  (match make_biff with
  | None -> ()
  | Some(s) -> begin
      let file_list = ref [] in
      let s_d_h = Case_ins.unix_opendir s in
      (try
        while true do
          let s' = Unix.readdir s_d_h in
          if ((Case_ins.unix_stat (s ^ "/" ^ s')).Unix.st_kind =
              Unix.S_REG) then file_list := (s ^ "/" ^ s') :: !file_list
        done
      with _ -> () ) ;
      if !file_list <> [] then begin
        let data = if game.Load.script_style = Load.PST then "" else "data/" in
        let filename = data ^ s ^ ".bif" in
        let new_key = Biff.save_biff game.Load.key filename
            (Load.fix_biff_path filename) !file_list in
        let oc = open_for_writing "CHITIN.KEY" true in
        Key.save_key new_key oc ;
        close_out oc ;
      end
  end) ;
;;

let remove_biff_from_key remove_biff game =
  (match remove_biff with
  | None -> ()
  | Some(f) ->
      let new_key = Key.remove_biff game.Load.key f in
      let oc = open_for_writing "CHITIN.KEY" true in
      Key.save_key new_key oc ;
      close_out oc) ;
;;

let append_strings strapp_list game =
  if_bgee_check_lang_or_fail game ;
  let lse_strapp_list = List.map (fun s ->
    Dlg.Local_String( { lse_male = s; lse_female = s;
                        lse_male_sound = "" ; lse_female_sound = ""; })) strapp_list in
  if (lse_strapp_list <> []) then begin
    let _ = List.map (Dc.resolve_tlk_string game) lse_strapp_list in
    ()
  end ;
;;

let decompile_dlg dlg_list transitive two_pass use_trans d_headers d_toplevel game =
  if_bgee_check_lang_or_fail game ;
  let loaded_dlgs = List.map (fun (b,e) ->
    try
      let buff, final_path = Load.load_resource "DLG decompile command" game true b e in
      let imp_base = Case_ins.filename_basename b in
      let dlg =
        try Dlg.load_dlg imp_base buff
        with e -> log_and_print "ERROR: problem loading [%s]: %s\n" b
            (printexc_to_string e) ; raise e
      in
      let out_name =
        if theout.file = "" then
          theout.dir ^ "/" ^ imp_base ^ ".d"
        else
          theout.dir ^ "/" ^ theout.file
      in
      let transout_name = (Case_ins.filename_chop_extension out_name ^ ".tra" ) in
      (dlg,out_name,transout_name,b,e,final_path)
    with e -> log_and_print "ERROR: problem handling [%s]: %s\n" b
        (printexc_to_string e) ; raise e
        ) dlg_list
  in
  for i = 1 to if transitive || two_pass then 2 else 1 do
    List.iter (fun (dlg,out_name,transout_name,b,e,final_path) ->
      try
        let out_chan =
          if theout.append then Lazy.force theout.chan
          else open_for_writing out_name false
        in
        let out_trans_chan = match use_trans with
          true ->
            log_and_print "[%s] created as translation file\n" transout_name ;
            Some(open_for_writing transout_name false)
        | false -> None
        in
        if (d_headers) && !Dlg.comments then begin
          Printf.fprintf out_chan "// creator  : %s (version %s)\n" Sys.argv.(0) version;
          Printf.fprintf out_chan "// argument : %s.%s\n" b e ;
          Printf.fprintf out_chan "// game     : %s\n" game.Load.game_path;
          Printf.fprintf out_chan "// source   : %s\n" final_path ;
          Printf.fprintf out_chan "// dialog   : %s\n" (Load.get_active_dialogs game).Load.dialog.Load.path ;
          Printf.fprintf out_chan "// dialogF  : %s\n\n"
            (match (Load.get_active_dialogs game).Load.dialogf with
            | None -> "(none)"
            | Some tlk -> tlk.Load.path) ;
        end ;
        let new_buff = Buffer.create (1024 * 32) in
        Dlg.emit_d dlg out_name (Load.get_active_dialog game)
          (Load.get_active_dialogf_opt game) new_buff out_trans_chan None
          reprint_d_action transitive d_toplevel;
        Buffer.output_buffer out_chan new_buff ;
        if not theout.append then close_out out_chan ;
      with e ->
        log_and_print "ERROR: problem creating [%s] from [%s]: %s\n" out_name
          b (printexc_to_string e) ;
        raise e
          ) loaded_dlgs ;
  done ;
;;

let untraify game untraify_d untraify_tra =
  if_bgee_check_lang_or_fail game ;
  ( match (untraify_d,untraify_tra) with
  | (Some(d),Some(tra)) ->
      let result = parse_file true (File tra) "parsing .tra files" (Dparser.tra_file Dlexer.initial) in
      log_or_print "[%s] has %d translation strings\n" tra
        (List.length result);
      let base,ext = split d in
      let base = handle_out_boringness base [ext] in
      if !debug_ocaml then log_and_print "I'm trying to save to %s.%s\n\n" base ext ;
      let filebuff = load_file d in
      let filebuff = List.fold_left (fun acc (i,ls) ->
        match ls with
        | Dlg.Local_String(lse) ->
            let buff =
              (if lse.lse_male <> "" then begin
                "~" ^ lse.lse_male ^ "~" ^ (
                                           if lse.lse_male_sound <> "" then begin
                                             " [" ^ lse.lse_male_sound ^ "]";
                                           end else "" )
              end else "") ^
              (if (lse.lse_female <> "") &&
                (lse.lse_female <> lse.lse_male) then begin
                  " ~" ^ lse.lse_female ^ "~" ^
                  (if lse.lse_female_sound <> "" then begin
                    " [" ^ lse.lse_female_sound ^ "]";
                  end else "")
                end else "" )
            in
            Str.global_replace (Str.regexp (Printf.sprintf "@%d\\([^0-9]\\)" i)) (Printf.sprintf "%s\\1" buff) acc;
        | _ -> failwith "traify1"
              ) filebuff result
      in
      let out = open_for_writing (base ^ "." ^ ext) true in
      output_string out filebuff;
      close_out out;
  | _ -> ()) ;
;;

let traify_file game traify traify_num traify_comment traify_old_tra =
  if_bgee_check_lang_or_fail game ;
  (match traify with
  | Some(file) -> begin
      try
        let name,ext = split (String.uppercase file) in

        let buf = ref (load_file file) in

        let base = handle_out_boringness file
            ["d"; "tra"; "tp2"; "baf"; "tpa"; "tpp"; "tph"] in
        if !debug_ocaml then
          log_and_print "I'm trying to save to %s.%s and %s.tra\n\n"
            base (String.lowercase ext) base ;
        let transout_name = base ^ ".tra" in
        let dout_name = base ^ "." ^ ext in

        let counter = ref !traify_num in
        let old_tra = ref None in

        let replace index my_regexp comment =
          let replace_with = (Printf.sprintf "@%d" index) ^ comment in
          buf := Str.global_replace my_regexp replace_with !buf ;
        in
        let remove str =
          let my_regexp = Str.regexp (Str.quote str) in
          buf := Str.global_replace my_regexp "" !buf ;
        in
        let remove_empty_sound string =
          let regexp = Str.regexp ("\\(" ^ string ^ "\\)[ \t]+" ^
                                   (Str.quote "[]")) in
          buf := Str.global_replace regexp "\\1" !buf ;
        in
        let fix_suckage index comment =
          let my_regexp = Str.regexp (Str.quote
                                        (Printf.sprintf " /* @%d%s */" index comment)) in
          let replace_with = Printf.sprintf "%s" comment in
          buf := Str.global_replace my_regexp replace_with !buf ;
        in
        let add_refer index comment =
          let my_regexp = Str.regexp (Printf.sprintf "@%d\\([^0-9]\\)" index) in
          let replace_with = Printf.sprintf "@%d%s\\1" index comment in
          buf := Str.global_replace my_regexp replace_with !buf;
          let my_regexp = Str.regexp (Str.quote (comment ^ comment)) in
          let replace_with = comment in
          buf := Str.global_replace my_regexp replace_with !buf;
        in
        let make_comment lse =
          if traify_comment then
            (Printf.sprintf " /* " ^ lse.lse_male ^ " */")
          else ""
        in

        let sound_str sound =
          if sound <> "" then
            Printf.sprintf " [%s]" sound
          else ""
        in
        let make_male_tra_string index lse =
          if lse.lse_male <> "" then begin
            (Printf.sprintf "@%-4d = ~%s~" index lse.lse_male) ^
            (sound_str lse.lse_male_sound)
          end
          else ""
        in
        let make_female_tra_string lse =
          if lse.lse_female <> "" && lse.lse_female <> lse.lse_male then begin
            (Printf.sprintf " ~%s~" lse.lse_female) ^
            (sound_str lse.lse_female_sound)
          end
          else ""
        in
        let make_tra_string index lse =
          (make_male_tra_string index lse) ^ (make_female_tra_string lse) ^ "\n"
        in

        let do_traification index lse =
          (* 'tis a thing of beauty *)
          let delimiters = ["~"; "%"; "\""] in
          let comment = make_comment lse in

          begin match (lse.lse_male, lse.lse_male_sound, lse.lse_female, lse.lse_female_sound) with
          | ("", "", "", "") -> ()
          | (male, "", female, "") -> (* male = female unless there is a female string *)
              begin
                List.iter (fun md ->
                  List.iter (fun fd ->
                    let male = Str.quote (md ^ male ^ md) in
                    let female = Str.quote (fd ^ female ^ fd) in
                    ignore (remove_empty_sound male) ;
                    ignore (remove_empty_sound female) ;
                    let regexp = Str.regexp (male ^ "[ \t]+" ^ female) in
                    ignore (replace index regexp comment) ;
                    if female = male then begin
                      let regexp = Str.regexp (male) in
                      ignore (replace index regexp comment)
                    end) delimiters) delimiters ;
                ()
              end
          | (male, male_sound, female, "") ->
              begin
                List.iter (fun md ->
                  List.iter (fun fd ->
                    let male = Str.quote (md ^ male ^ md) in
                    let female = Str.quote (fd ^ female ^ fd) in
                    let male_sound = Str.quote ("[" ^ male_sound ^ "]") in
                    ignore (remove_empty_sound female) ;
                    let regexp = Str.regexp (male ^ "[ \t]+" ^ male_sound ^ "[ \t]+" ^ female) in
                    ignore (replace index regexp comment)) delimiters) delimiters ;
                ()
              end

          | (male, "", female, female_sound) ->
              begin
                List.iter (fun md ->
                  List.iter (fun fd ->
                    let male = Str.quote (md ^ male ^ md) in
                    let female = Str.quote (fd ^ female ^ fd) in
                    let female_sound = Str.quote ("[" ^ female_sound ^ "]") in
                    ignore (remove_empty_sound male) ;
                    let regexp = Str.regexp (male ^ "[ \t]+" ^ female ^ "[ \t]+" ^ female_sound) in
                    ignore (replace index regexp comment)) delimiters) delimiters ;
                ()
              end

          | (male, male_sound, female, female_sound) ->
              begin
                List.iter (fun md ->
                  List.iter (fun fd ->
                    let male = Str.quote (md ^ male ^ md) in
                    let female = Str.quote (fd ^ female ^ fd) in
                    let male_sound = Str.quote ("[" ^ male_sound ^ "]") in
                    let female_sound = Str.quote ("[" ^ female_sound ^ "]") in
                    let regexp = Str.regexp (male ^ "[ \t]+" ^ male_sound ^ "[ \t]+" ^ female ^ "[ \t]+" ^ female_sound) in
                    ignore (replace index regexp comment) ;
                    if female = male && female_sound = male_sound then begin
                      let regexp = Str.regexp (male ^ "[ \t]+" ^ male_sound) in
                      ignore (replace index regexp comment) ;
                    end) delimiters) delimiters ;
                ()
              end
          end ;

          let tra_str = (make_tra_string index lse) in
          (index, tra_str)
        in

        (match traify_old_tra with
        | None -> ()
        | Some(x) ->
            let max = ref !counter in
            let inchan = Case_ins.perv_open_in_bin x in
            let lexbuf = lex_init file inchan in
            let tra_file = Stats.time "parsing.TRA files" (fun () ->
              Dparser.tra_file Dlexer.initial lexbuf) () in
            let sorted = Dlg.sort_list_for_traify (List.map (fun (i, ts) ->
              (i, (Dlg.lse_of_ts ts))) tra_file) in
            let tra = List.fold_left (fun acc (reference, ls) ->
              ignore (if !max <= reference then max := reference + 1) ;
              let comment = make_comment ls in
              ignore (fix_suckage reference comment) ;
              ignore (if traify_comment then add_refer reference comment) ;
              List.append acc [(do_traification reference ls)]) [] sorted in
            counter := !max ;
            old_tra := Some(tra) ;
            close_in inchan) ;

        Dlg.local_string_ht := Some([]) ;
        let old_ok = !Dc.ok_to_resolve_strings_while_loading in
        Dc.ok_to_resolve_strings_while_loading := None ;
        Dc.doing_traify := true ;

        begin
          match ext with
          | "D" ->
              let lexbuf = lex_init_from_string file !buf in
              ignore (Stats.time "parsing .D files"
                        (fun () -> Dparser.d_file Dlexer.initial lexbuf) ());
              pop_context ();
          | "TP2" -> ignore (Tparser.parse_tp2_file (String (file,!buf)))
          | "TPA"
          | "TPH" -> ignore (Tparser.parse_tpa_file (String (file,!buf)))
          | "TPP" -> ignore (Tparser.parse_tpp_file (String (file,!buf)))
          | "BAF" ->
              let lexbuf = lex_init_from_string file !buf in
              ignore (Stats.time "parsing .D files"
                        (fun () ->
                          Bafparser.baf_file Baflexer.initial lexbuf) ());
              pop_context ();
          | _ -> log_and_print
                "ERROR: don't know how to --traify files with extension [%s]\n"
                ext ; failwith ext
        end ;
        log_or_print "[%s] parsed for --traify\n" file ;

        Dc.ok_to_resolve_strings_while_loading := old_ok ;
        Dc.doing_traify := false;

        let fodder = (Dlg.make_list_for_traify counter) in
        let traified = (List.map (fun (index, ls) ->
          do_traification index ls) fodder) in

        let result = (List.fast_sort (fun (index1,str1) (index2,str2) ->
          compare index1 index2) (match !old_tra with
          | Some(l) -> List.append l traified
          | None -> traified)) in

        let traout = open_for_writing transout_name true in
        ignore (List.iter (fun (index, str) ->
          output_string traout str) result) ;

        Dlg.local_string_ht := None ;

        let dout = open_for_writing dout_name true in
        Printf.fprintf dout "%s" !buf ;

        close_out traout ;
        close_out dout ;
        ()

      with e ->
        log_and_print "ERROR: problem tra-ifying file [%s]: %s\n" file
          (printexc_to_string e) ;
        raise e
    end
  | None -> ())

let compile_baf baf_list game =
  if_bgee_check_lang_or_fail game ;
  List.iter (fun str ->
    try
      let script = handle_baf_filename str in
      let name,ext = split (Case_ins.filename_basename str) in
      let out = Case_ins.perv_open_out_bin (theout.dir ^ "/" ^ name ^ ".bcs") in
      Bcs.save_bcs game (Bcs.Save_BCS_OC(out)) script ;
      close_out out
    with e -> log_and_print "ERROR: problem loading [%s]: %s\n" str
        (printexc_to_string e) ; raise e
        ) baf_list ;
;;

let test_output_tlk game pause_at_end =
  let test path =
    (match path with
    | Some path when (file_exists path) -> begin
        (try Unix.access path [Unix.W_OK] ;
          log_or_print "[%s] claims to be writeable.\n" path ;
          if (Case_ins.unix_stat path).Unix.st_kind <> Unix.S_REG then
            failwith (path ^ " is a not a regular file") ;
          log_or_print "[%s] claims to be a regular file.\n" path ;
          ()
        with e ->
          log_and_print "\nERROR: The file [%s] cannot be written to. Perhaps it is in use by another process (close ShadowKeeper, all Infinity Engine games and editors, etc.). It may also be naturally read-only: use Windows Explorer and right-click on the file to pull up its properties. Make sure that the \"read-only\" box is NOT checked. Please fix this problem and try again.\n" path ;
          pause_at_end := true ;
          raise e)
    end
    | _ -> ()) in
  List.iter test (Array.fold_left (fun acc tlk_pair ->
    List.append acc [Some (tlk_pair.Load.dialog.Load.path); (match tlk_pair.Load.dialogf with
    | None -> None
    | Some df -> Some (df.Load.path))]) [] game.Load.dialogs)

let do_tp2_files tp_list force_install_these_main force_uninstall_these_main pause_at_end game =
  pause_at_end := true ;
  if !Tp.always_uninstall then pause_at_end := false ;
  if !Tp.always_yes then pause_at_end := false ;
  if !Tp.sometimes_reinstall then pause_at_end := false ;
  Tp.force_install_these := force_install_these_main ;
  Tp.force_uninstall_these := force_uninstall_these_main ;
  if !Tp.force_install_these <> [] then pause_at_end := false ;
  if !Tp.force_uninstall_these <> [] then pause_at_end := false ;
  load_log () ;
  List.iter (fun tp_file -> Queue.add tp_file tp2_queues) tp_list ;
  while not (Queue.is_empty tp2_queues) do
    let tp_file = Queue.take tp2_queues in
    try
      if file_exists tp_file then begin
        let result = handle_tp2_filename tp_file in
        Tpwork.handle_tp game tp_file result;
      end
    with e ->
      log_and_print "ERROR: problem parsing TP file [%s]: %s\n" tp_file
        (printexc_to_string e) ;
      exit_status := StatusParseError ;
      raise e
  done
;;

let do_script process_script pause_at_end game =
  pause_at_end := false ;
  load_log () ;
  let buff = try load_file process_script with _ -> "" in
  let lines = Str.split many_newline_or_cr_regexp buff in
  List.iter (fun x -> Queue.add x tp2_queues) lines;
  while not (Queue.is_empty tp2_queues) do
    let this = Queue.take tp2_queues in
    let parts = Array.of_list (Str.split many_whitespace_regexp this) in
    let tp_file = parts.(0) in
    let action = ref "" in
    Tp.always_yes := false;
    Tp.always_uninstall := false;
    Tp.sometimes_reinstall := false;
    Tp.forced_language := int_of_string parts.(1);
    Tp.force_install_these := [];
    Tp.force_uninstall_these := [];
    Tp.specified_specific_components := true;
    let toproc = ref [] in
    Array.iteri (fun i s -> if i > 1 then begin
      match String.uppercase s with
      | "U"
      | "I" -> action := String.uppercase s
      | _ -> begin
          let num = int_of_string s in
          match !action with
          | "U"
          | "I" -> toproc := (num, !action) :: !toproc ;
          | _ -> failwith "wrong installation script"
      end
    end) parts ;
    List.iter (fun (a,b) -> log_and_print "%s %d\n" b a) !toproc;
    try
      if file_exists tp_file then begin
        let result = handle_tp2_filename tp_file in
        List.iter (fun (a,b) ->
          begin
            match b with
            | "U" ->
                Tp.force_install_these := [];
                Tp.force_uninstall_these := [a];
            | "I" ->
                Tp.force_uninstall_these := [];
                Tp.force_install_these := [a];
            | _ -> ()
          end ;
          Tpwork.handle_tp game tp_file result
            ) (List.rev !toproc);
        List.iter (fun c -> match c with
        | Command (s,e) ->
            log_or_print "Executing: [%s]\n" s ;
            ignore (exec_command s e)
        | Fn f ->
            Lazy.force f
              )
          !execute_at_exit;
        execute_at_exit := [];
      end
    with e ->
      log_and_print "ERROR: problem parsing TP file [%s]: %s\n" tp_file
        (printexc_to_string e) ;
      raise e
  done
;;

let decompile_bcs bcs_list game =
  if_bgee_check_lang_or_fail game ;
  List.iter (fun str ->
    let b,e = split str in
    try
      let buff, _ =
        if file_exists str then (load_file str),"" else
        Load.load_resource "decompile BCS command" game true b (String.uppercase e)
      in
      let script = handle_script_buffer str buff in
      let base = Case_ins.filename_basename b in
      let out_name = theout.dir ^ "/" ^ base ^ ".baf" in
      let out = Case_ins.perv_open_out out_name in
      (try
        Bcs.print_script_text game (Bcs.Save_BCS_OC(out))
          (Bcs.BCS_Print_Script(script)) (!Dlg.comments) None ;
        close_out out
      with e ->
        log_and_print "ERROR: problem printing script [%s]: %s\n" b
          (printexc_to_string e) ; close_out out
          )
    with e -> log_and_print "ERROR: problem handling [%s]: %s\n" b
        (printexc_to_string e)
        ) bcs_list ;
;;

let merge_tlk tlk_merge game =
  if_bgee_check_lang_or_fail game ;
  List.iter (fun str ->
    let name,ext = split (String.uppercase str) in
    let tlk = Tlk.load_tlk str in
    let dialog = Load.get_active_dialog game in
    let max =
      if Array.length tlk > Array.length dialog then
        Array.length dialog
      else
        Array.length tlk
    in
    for i = 0 to max - 1 do
      dialog.(i) <- tlk.(i)
    done ;
    (Load.get_active_dialogs game).Load.dialog_mod <- true
        ) tlk_merge ;
;;

let parse_check file kind =
  no_exit_pause := true ;
  if (not (file_exists file)) then
    failwith (Printf.sprintf "[%s] No such file" file) ;
  (try
    (match (String.uppercase_ascii kind) with
    | "D" ->
        let buff = load_file file in
        let old_ok = !Dc.ok_to_resolve_strings_while_loading in
        Dc.ok_to_resolve_strings_while_loading := None ;
        Dc.doing_traify := true ;
        ignore (Parsewrappers.parse_d_buffer file buff) ;
        Dc.ok_to_resolve_strings_while_loading := old_ok ;
        Dc.doing_traify := false ;
    | "BAF" ->
        let buff = load_file file in
        let old_ok = !Dc.ok_to_resolve_strings_while_loading in
        Dc.ok_to_resolve_strings_while_loading := None ;
        Dc.doing_traify := true ;
        ignore (Parsewrappers.handle_script_buffer file buff) ;
        Dc.ok_to_resolve_strings_while_loading := old_ok ;
        Dc.doing_traify := false ;
    | "TP2" ->
        ignore (Parsewrappers.handle_tp2_filename file) ;
    | "TPA" ->
        ignore (Parsewrappers.handle_tph_filename file) ;
    | "TPP" ->
        ignore (Parsewrappers.handle_tpp_filename file) ;
    | _ -> exit_status := StatusArgumentInvalid ;
        log_and_print
          "--parse-check does not know what to do with files of type [%s]\n"
          kind ;
        failwith "Unknown file type") ;
    log_or_print "File [%s] was successfully parsed as type [%s]\n"
      file kind ;
  with Parsing.Parse_error -> exit_status := StatusParseError ;
    log_or_print "ERROR: File [%s] was NOT successfully parsed as type [%s]\n"
      file kind ;
    raise Parsing.Parse_error)

let main () =

  let user_min = ref None in
  let user_max = ref None in

  let cmp_src = ref None in
  let cmp_dest = ref None in

  let list_lang = ref None in
  let list_comp = ref None in
  let list_comp_json = ref None in
  let list_comp_lang = ref 0 in
  let save_comp_name = ref false in

  let dcmp_src = ref None in
  let dcmp_dest = ref None in

  let tcmp_src = ref None in
  let tcmp_dest = ref None in

  let bcmp_src = ref None in
  let bcmp_dest = ref None in

  let rcmp_src = ref None in
  let rcmp_dest = ref None in

  let textcmp_src = ref None in
  let textcmp_dest = ref None in

  let d_toplevel = ref false in

  let bcmp_orig = ref None in
  let bcmp_patch = ref None in

  let tlkcmp_src = ref None in
  let tlkcmp_dest = ref None in
  let tlkcmp_strings = ref false in

  let make_biff = ref None in
  let remove_biff = ref None in
  let make_tlk = ref [] in

  let no_game = ref false in

  let transitive = ref false in
  let two_pass = ref false in

  let dlg_list = ref [] in
  let d_list = ref [] in
  let ds_list = ref [] in
  let strapp_list = ref [] in
  let bc_list = ref [] in
  let bg_list = ref [] in
  let bcs_list = ref [] in
  let baf_list = ref [] in

  let no_auto_update = ref false in
  let auto_update_all = ref false in
  let exit_now = ref false in

  let force_install_these_main   = ref [] in
  let force_uninstall_these_main = ref [] in

  let change_log = ref [] in

  let process_script = ref "" in

  let automate_list = ref [] in
  let automate_file = ref None in
  let automate_min = ref None in

  let tlk_merge = ref [] in

  let extract_tlk = ref false in
  let extract_kits = ref (0) in

  let tp_list = ref [] in

  let strfind_list = ref [] in

  let trans_list = ref [] in

  let d_headers = ref true in

  let list_biff = ref false in
  let list_files = ref false in


  let bs_type_list = ref [] in
  let bs_str_list = ref [] in

  let biff_short = ref 0 in
  let biff_short_at = ref 0 in

  let output_dialog = ref None in
  let output_dialogf = ref None in
  let test_output_tlk_p = ref false in

  let traify = ref None in
  let traify_old_tra = ref None in
  let traify_comment = ref false in
  let traify_num = ref 0 in

  let untraify_d = ref None in
  let untraify_tra = ref None in

  let forceify = ref None in

  let use_trans = ref false in
  let test_trans = ref false in

  let no_auto_tp2 = ref false in

  let ee_use_lang = ref None in

  let parse_check_file = ref "" in
  let parse_check_kind = ref "" in

  let argv0_base, argv0_ext = split (String.uppercase (Case_ins.filename_basename Sys.argv.(0))) in

  let auto () = begin
    pause_at_end := true ;
    if not !no_game then
      Load.validate_cwd () ;
    if is_directory "debugs" then
      init_log Version.version ("debugs/" ^ argv0_base ^ ".DEBUG")
    else
      init_log Version.version (argv0_base ^ ".DEBUG") ;
    (try
      if (Arch.do_auto_update) then begin
        if not !no_auto_update then
          Autoupdate.verify_latest true
      end else
        log_and_print "[On this architecture, WeiDU does not auto-update.\n  You must ensure that you have the most recent version.]\n"
    with e ->
      begin
        log_and_print "ERROR: Cannot perform auto-update, going ahead anyway!\n\t%s\n"
          (printexc_to_string e) ;
      end ) ;
    if List.exists (fun arg -> let a,b = split arg in (String.uppercase b) = "TP2")
        (Array.to_list Sys.argv) then
      () (* setup-solaufein.exe foo.tp2
          * runs foo.tp2, not setup-solaufein.tp2 *)
    else begin
      let rec try_it file_list = match file_list with
      | file :: lst ->
          if file_exists file then
            tp_list := file :: !tp_list
          else try_it lst
      | [] ->
          log_and_print "\n\n** ERROR ** [%s.TP2] not found.\nMake sure that you have unpacked the archive correctly and\nthat you are not trying to run this file from inside an archive." argv0_base
      in
      try_it (Util.all_possible_tp2s (tp2_name argv0_base))
    end
  end in

  let forced_script_style = ref Load.NONE in
  let counter = ref 0 in

  let usageMsg = Printf.sprintf "\t\tWeiDU (version %s: \"%s\")\n\nusage: WeiDU [options] BAF,BCS,D,DLG,TRA,TP,TP2-files" version comment in
  let argDescr = [
    "", Myarg.Unit (fun a -> a), "\nGeneral Input Options:\n" ;

    "--game", Myarg.String Load.add_game_path, "X\tset main game directory to X" ;
    "--game-by-type", Myarg.String (fun x -> Load.add_game_path(Arch.game_path_by_type x)), "X\tset main game directory to the one where X is installed (BG,BG2,IWD,IWD2,PST)";
    "--nogame", Myarg.Set no_game,"\tdo not load any default game files" ;
    "--search", Myarg.String Load.add_override_path, "X\tlook in X for input files (cumulative)" ;
    "--search-ids", Myarg.String Load.add_ids_path, "X\tlook in X for input IDS files (cumulative)" ;
    "--tlkin", Myarg.String Load.set_dialog_tlk_path,"X\tuse X as DIALOG.TLK" ;
    "--ftlkin", Myarg.String Load.set_dialogf_tlk_path,"X\tuse X as DIALOGF.TLK";
    "--use-lang", Myarg.String (fun s -> ee_use_lang := Some (String.lowercase s)), "X\ton games with multiple languages, use files in lang/X/";
    "--tlkmerge", Myarg.String (fun s -> tlk_merge := !tlk_merge @ [s]; test_output_tlk_p := true),
    "X\tmerge X into loaded DIALOG.TLK" ;
    "--yes", Myarg.Set Tp.always_yes,"\tanswer all TP2 questions with 'Yes'";
    "--uninstall", Myarg.Set Tp.always_uninstall,"\tanswer all TP2 questions with 'Uninstall'" ;
    "--reinstall", Myarg.Set Tp.sometimes_reinstall,"\treinstall all installed TP2 components" ;
    "--language", Myarg.Int (fun d -> Tp.forced_language := d), "X\tSet the language to X" ;
    "--force-install", Myarg.Int (fun d -> force_install_these_main     := d :: !force_install_these_main;
      Tp.specified_specific_components := true), "\tX installs component X number (cumulative)" ;
    "--force-uninstall", Myarg.Int (fun d -> force_uninstall_these_main := d :: !force_uninstall_these_main;
      Tp.specified_specific_components := true), "\tX uninstalls component X number (cumulative)" ;
    "--force-install-rest", Myarg.Rest (Myarg.Int (fun d -> force_install_these_main := d :: !force_install_these_main;
      Tp.specified_specific_components := true)), "\tX Y... installs component number X, Y... (cumulative)" ;
    "--force-install-list", Myarg.List (Myarg.Int (fun d -> force_install_these_main := d :: !force_install_these_main;
      Tp.specified_specific_components := true)), "\tX Y... installs component number X, Y... (cumulative)" ;
    "--force-uninstall-rest", Myarg.Rest (Myarg.Int (fun d -> force_uninstall_these_main := d :: !force_uninstall_these_main;
      Tp.specified_specific_components := true)), "\tX Y... uninstalls component number X, Y... (cumulative)" ;
    "--force-uninstall-list", Myarg.List (Myarg.Int (fun d -> force_uninstall_these_main := d :: !force_uninstall_these_main;
      Tp.specified_specific_components := true)), "\tX Y... uninstalls component number X, Y... (cumulative)" ;
    "--quick-menu", Myarg.Int (fun d -> Tp.chosen_quick_menu := Some d), "\tX installs the quick menu selection X";
    "--process-script", Myarg.String (fun s -> process_script := s; Tp.skip_at_view := true; Tp.quick_log := true; test_output_tlk_p := true), "\tX process installation script X";
    "--skip-at-view", Myarg.Set Tp.skip_at_view, "\tkills AT_* ~VIEW this~";
    "--quick-log", Myarg.Set Tp.quick_log, "\tDoesn't print the name of components in weidu.log (much faster)";
    "--safe-exit", Myarg.Set Tpstate.safe_exit, "\tPrints weidu.log after starting the installation of every component";
    "--version", Myarg.Set exit_now, "\tprint version number and exit";
    "--exit", Myarg.Set exit_now, "\tprint version number and exit";
    "--no-exit-pause", Myarg.Set no_exit_pause, "\tDon't ask to press enter to exit";
    "--ask-every", Myarg.Set Tp.ask_all, "\task about every TP2 component" ;
    "--ask-only", Myarg.List (Myarg.Int (fun i -> Tp.ask_only := i ::
      !Tp.ask_only)), "\tX Y... limits the interactive installer to asking only about the specified components (cumulative)" ;
    "--list-languages", Myarg.String (fun s -> list_lang := Some s), "\tX lists the languages in X";
    "--list-components", Myarg.Tuple [
    Myarg.String (fun s -> list_comp := Some s);
    Myarg.Int (fun s -> list_comp_lang := s);
  ], "\tX Y lists all components in X using language Y";
    "--list-components-json", Myarg.Tuple [
    Myarg.String (fun s -> list_comp_json := Some s) ;
    Myarg.Int (fun i -> list_comp_lang := i) ;
  ], "\tX Y lists all components in X using language Y with JSON output EXPERIMENTAL!" ;
    "--save-components-name", Myarg.Set save_comp_name, "\trewrites weidu.log, printing every component name";
    "--change-log",Myarg.String (fun s -> change_log := s :: !change_log), "\tgenerates a changelog for the given resource (cumulative)";
    "--change-log-list",Myarg.List (Myarg.String (fun s -> change_log := s :: !change_log)), "\tgenerates a changelog for the given resource (cumulative)";
    "--change-log-rest",Myarg.Rest (Myarg.String (fun s -> change_log := s :: !change_log)), "\tgenerates a changelog for the given resource (cumulative)";
    "--noautoupdate", Myarg.Set no_auto_update,"\tdo not auto-update WeiDU setup files" ;
    "--no-auto-tp2", Myarg.Set no_auto_tp2, "\tdo not run setup-mymod.tp2 even if argv[0] is setup-mymod.exe" ;
    "--noselfupdatemsg", Myarg.Clear Autoupdate.self_update_message,"\tdo not print any self-updating messages" ;
    "--update-all", Myarg.Set auto_update_all,"\tauto-update all WeiDU setup files";
    "--args", Myarg.String (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter),
    "\tX X will be stored in the %argv[x]% variable (cumulative)";
    "--args-rest", Myarg.Rest (Myarg.String (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter)),
    "\tX Y... X, Y... will be stored in the %argvx% variables (cumulative)";
    "--args-list", Myarg.List (Myarg.String (fun s -> Var.set_string ("argv[" ^ (string_of_int !counter) ^ "]") s; incr counter)),
    "\tX Y... X, Y... will be stored in the %argvx% variables (cumulative)";
    "--case-exact", Myarg.Unit (fun () -> Case_ins.case_exact := true), "\tapply no case transformations to file-system IO" ;
    "--print-backtrace", Myarg.Unit (fun () -> print_backtrace := true; Printexc.record_backtrace true),"\tprints OCaml stack trace when reporting an exception (rarely of interest to end-users)";
    "--debug-ocaml", Myarg.Set Util.debug_ocaml,"\tenables random debugging information for the Ocaml source (rarely of interest to end-users)" ;
    "--debug-boiic", Myarg.Set Tp.debug_boiic,"\tprints out which files have been changed by BUT_ONLY_IF_IT_CHANGES" ;
    "--debug-change", Myarg.Set Tp.debug_change,"\tprints a warning if a file is being COPY_EXISTED without receiving a change." ;
    "--modder", Myarg.List (Myarg.TwoStrings (fun a b -> Modder.set_modder [a, b, 10])), "\tX Y... enables the MODDER mode and sets the MODDER option X to Y (cumulative)";
    "--clear-memory", Myarg.Set Tpstate.clear_memory,"\tcalls CLEAR_MEMORY after every action evaluation.";
    "--script-style", Myarg.String (fun s ->
      let n = match String.uppercase s with
      | "BG"
      | "BG2" -> Load.BG2
      | "BG1" -> Load.BG1
      | "PST" -> Load.PST
      | "IWD"
      | "IWD1" -> Load.IWD1
      | "IWD2" -> Load.IWD2
      | _ -> parse_error "unknown SCRIPT_STYLE"
      in forced_script_style := n),"X\tuse BCS/BAF style X (BG, PST, IWD1, IWD2)" ;
    "--min", Myarg.Int (fun i -> user_min := Some(i)), "X\tlower range for some commands (like --tlkcmp)" ;
    "--max", Myarg.Int (fun i -> user_max := Some(i)), "X\tupper range for some commands (like --string)" ;
    "--parse-check", Myarg.TwoStrings (fun kind file ->
      parse_check_kind := kind; parse_check_file := file),
    "\tX Y parses file Y as file type X and returns 0 if the file was parsed without errors; X must be one of D, BAF, TP2, TPA or TPP" ;

    "", Myarg.Unit (fun a -> a), "\nGeneral Output Options:\n" ;

    "--out", Myarg.String (set_theout false), "X\temit to file or directory X" ;
    "--append", Myarg.String (set_theout true), "X\tappend to file or directory X" ;
    "--backup", Myarg.String (fun s -> backup_dir := Some(s)), "X\tbackup files to directory X before overwriting" ;
    "--extract-kits", Myarg.Int (fun d -> extract_kits := d), "X\textract all kits starting with kit #X";
    "--tlkout", Myarg.String (fun s -> output_dialog := Some(s) ; test_output_tlk_p := true), "X\temit X as new DIALOG.TLK" ;
    "--ftlkout", Myarg.String (fun s -> output_dialogf := Some(s) ; test_output_tlk_p := true), "X\temit X as new DIALOGF.TLK" ;

    "", Myarg.Unit (fun a -> a), "\nD Options:\n" ;

    "--transin", Myarg.String (fun s -> trans_list := !trans_list @ [s]), "X\tuse translation file X (cumulative)" ;
    "--testtrans", Myarg.Set test_trans, "\ttest all translations files" ;
    "--noheader", Myarg.Clear d_headers, "\tdo not emit .D header comments" ;
    "--nofrom", Myarg.Clear Dlg.emit_from, "\tdo not emit .D \"// from:\" comments" ;
    "--full-from", Myarg.Set two_pass, "\tGenerate complete \"// from:\" comments";
    "--nocom", Myarg.Clear Dlg.comments, "\tdo not emit ANY .D / .BAF comments" ;
    "--transitive", Myarg.Set transitive, "\tFollow EXTERN links when making D files" ;
    "--toplevel", Myarg.Set d_toplevel, "\tEmit top-level DLG states only" ;
    "--text", Myarg.Set Dlg.emit_text, "\temit string text with refs in comments" ;
    "--traify", Myarg.String (fun s -> traify := Some(s)), "X\tconvert .D file X to use TRAs (use with --out)" ;
    "--traify-old-tra", Myarg.String (fun s -> traify_old_tra := Some(s)), "X\tthe given .TRA file contains the initial strings to traify" ;
    "--traify#", Myarg.Int (fun d -> traify_num := d), "X\tstart --traify .TRA file at @X" ;
    "--traify-comment", Myarg.Set traify_comment, "\toutput @1 /* ~Hello~ */ rather than @1 when traifying" ;
    "--untraify-d", Myarg.String (fun s -> untraify_d := Some(s)), "X\tconvert .D file X to use hardcoded strings...";
    "--untraify-tra", Myarg.String (fun s -> untraify_tra := Some(s)), "X\t...from TRA file X";
    "--forceify", Myarg.String (fun s -> forceify := Some(s)), "X\tconvert .D file X to use forced strrefs (use with --dout)" ;
    "--transref", Myarg.Set Dlg.use_trans_ref, "\temit string reference numbers in TRA files" ;
    "--trans", Myarg.Set use_trans, "\temit coupled .D and .TRA files\n\nTLK String Options:\n" ;
    "--traify-tlk", Myarg.Set extract_tlk, "\temit a .TRA file for the given .TLK file (see --out, --min, --traify#)" ;
    "--make-tlk", Myarg.String (fun s -> make_tlk := s :: !make_tlk ; test_output_tlk_p := true), "X\tmake a .TLK file from .TRA file X (cumulative, see --tlkout)" ;
    "--string", Myarg.Int (fun i -> ds_list := i :: !ds_list), "X\tdisplay string reference #X (cumulative)" ;
    "--strfind", Myarg.String (fun s -> strfind_list := s :: !strfind_list), "X\tdisplay strings that contain X (cumulative, regexp allowed)" ;
    "--strapp", Myarg.String (fun s -> strapp_list := s :: !strapp_list ; test_output_tlk_p := true), "X\tappend string X to DIALOG.TLK (cumulative)" ;

    "", Myarg.Unit (fun a -> a), "\nBIFF Options:\n" ;

    "--list-biffs", Myarg.Set list_biff, "\tenumerate all BIFF files in CHITIN.KEY" ;
    "--list-files", Myarg.Set list_files, "\tenumerate all resource files in CHITIN.KEY";
    "--biff", Myarg.String (fun s -> bc_list := (String.uppercase s) :: !bc_list), "X\tenumerate contents of BIFF file X (cumulative)" ;
    "--biff-type", Myarg.String (fun s -> bs_type_list := s :: !bs_type_list), "X\texamine all BIFF resources of extension X ... (cumulative)" ;
    "--biff-str", Myarg.String (fun s -> bs_str_list := s :: !bs_str_list), "X\t... and list those containing X (cumulative, regexp allowed)" ;
    "--biff-name", Myarg.Int (fun i -> Load.content_name_offset := Some(i)),
    "X\tassume matching items have a strref name at offset X" ;
    "--biff-value", Myarg.Int (fun i -> biff_short := i), "X\t... or list those containing value X ..." ;
    "--biff-value-at", Myarg.Int (fun i -> biff_short_at := i), "X\t... at offset X" ;
    "--biff-get", Myarg.String (fun s -> bg_list := s :: !bg_list), "X\textract resource X from game BIFFs (cumulative, regexp allowed)" ;
    "--biff-get-rest", Myarg.Rest (Myarg.String (fun s -> bg_list := s :: !bg_list)), "X, Y, ...\textract resources X, Y, ... from game BIFFs (regexp allowed)" ;
    "--biff-get-list", Myarg.List (Myarg.String (fun s -> bg_list := s :: !bg_list)), "X, Y, ...\textract resources X, Y, ... from game BIFFs (regexp allowed)" ;
    "--make-biff", Myarg.String (fun s -> make_biff := Some(s)), "X\tmake data\\X.bif from all files in folder X, update CHITIN.KEY" ;
    "--remove-biff", Myarg.String (fun s -> remove_biff := Some(s)), "X\tremove references to biff X and its resources, update CHITIN.KEY" ;

    "", Myarg.Unit (fun a -> a), "\nARE/ITM/SPL/CRE Options:\n" ;

    "--automate", Myarg.String (fun s -> automate_list := s ::
      !automate_list), "X\tautomatically make a TP2 file for ARE/ITM/SPL/CRE/EFF/STO files in X" ;
    "--automate-file", Myarg.String (fun s -> automate_file := Some s), "X\tautomatically make a TP2 snippet for ARE/ITM/SPL/CRE/EFF/STO file X" ;
    "--automate-min", Myarg.Int (fun i -> automate_min := Some i), "X\tminimum strref # for --automate (default is SoA)" ;

    "", Myarg.Unit (fun a -> a), "\nComparison Options:\n" ;

    "--cmp-from", Myarg.String (fun s -> cmp_src := Some(s)), "X\temit WRITE_BYTEs to turn this file ..." ;
    "--cmp-to", Myarg.String (fun s -> cmp_dest := Some(s)), "X\t... into this one";
    "--dcmp-from", Myarg.String (fun s -> dcmp_src := Some(s)), "X\temit REPLACEs to turn this DLG file ..." ;
    "--dcmp-to", Myarg.String (fun s -> dcmp_dest := Some(s)), "X\t... into this one";
    "--tcmp-from", Myarg.String (fun s -> tcmp_src := Some(s)), "X\tcompare this TRA file (or directory of TRA files)..." ;
    "--tcmp-to", Myarg.String (fun s -> tcmp_dest := Some(s)), "X\t... with this one (or this directory)";
    "--bcmp-from", Myarg.String (fun s -> bcmp_src := Some(s)), "X\temit APPLY_BCS_PATCH to turn this BCS file..." ;
    "--bcmp-to", Myarg.String (fun s -> bcmp_dest := Some(s)), "X\t... into this one" ;
    "--rcmp-from",Myarg.String (fun s -> rcmp_src := Some(s)), "X\temit REPLACE_TEXTUALLY patches to turn this file..." ;
    "--rcmp-to",Myarg.String (fun s -> rcmp_dest := Some(s)), "X\t... into this one" ;
    "--textcmp-from", Myarg.String (fun s -> textcmp_src := Some(s)), "X\temit APPLY_BCS_PATCH to turn this textual file..." ;
    "--textcmp-to", Myarg.String (fun s -> textcmp_dest := Some(s)), "X\t... into this one" ;
    (* For debugging patch/diff: *)
    "--bcmp-orig", Myarg.String (fun s -> bcmp_orig := Some(s)), "X\toriginal file to apply ..." ;
    "--bcmp-patch", Myarg.String (fun s -> bcmp_patch := Some(s)), "X\t... this patch to" ;
    "--tlkcmp-from", Myarg.String (fun s -> tlkcmp_src := Some(s)), "X\temit STRING_SETs to convert this TLK file ..." ;
    "--tlkcmp-to", Myarg.String (fun s -> tlkcmp_dest := Some(s)), "X\t... into this one";
    "--tlkcmp-use-strings", Myarg.Set tlkcmp_strings, "\tmodifies --tlkcmp behavior";

    "", Myarg.Unit (fun a -> a), "\nLog Options:\n" ;

    "--log", Myarg.String (fun s -> init_log Version.version s),"X\tlog output and details to X" ;
    "--autolog", Myarg.Unit (fun () -> init_log Version.version "WSETUP.DEBUG"), "\tlog output and details to WSETUP.DEBUG" ;
    "--logapp", Myarg.Set append_to_log,"\tappend to log instead of overwriting" ;
    "--log-extern", Myarg.Set log_extern,"\talso log output from commands invoked by WeiDU " ;
    "--debug-assign", Myarg.Set Var.debug_assign,"\tPrint out all values assigned to TP2 variables" ;
    "--debug-value", Myarg.Set Tp.debug_pe,"\tPrint out all value expressions" ;
    "--continue", Myarg.Set Tp.continue_on_error,"\tcontinue despite TP2 action errors" ;

    "", Myarg.Unit (fun a -> a), "\nHelp Options:\n" ;

  ] in
  let give_help () =
    Myarg.usage argDescr usageMsg ;
    exit (return_value StatusArgumentInvalid)
  in
  let handleArg str = begin
    let base,ext = split (String.uppercase str) in
    match ext with
    | "D" -> test_output_tlk_p := true ; d_list := str :: !d_list
    | "DLG" -> dlg_list := (base,ext) :: !dlg_list
    | "TLK" -> test_output_tlk_p := true ; Load.set_dialog_tlk_path str
    | "TP"
    | "TP2" -> test_output_tlk_p := true ; tp_list := !tp_list @ [str]
    | "TRA" -> trans_list := !trans_list @ [str]
    | "BCS" | "BS" -> bcs_list := !bcs_list @ [str]
    | "BAF" -> test_output_tlk_p := true ; baf_list := !baf_list @ [str]
    | "" ->
        begin
          let do_state state_num =
            Hashtbl.add Dlg.emit_these_states state_num () ;
            Dlg.emit_some_states_only := true
          in try
            Scanf.sscanf str "%d-%d" (fun a b ->
              for i = a to b do do_state i done) ;
          with _ ->
            begin try
              Scanf.sscanf str "%d" (fun d -> do_state d)
            with _ ->
              log_and_print "Unknown argument: [%s]\n\n" str ;
              give_help ()
            end
        end
    | _ -> log_and_print "Unknown argument: [%s]\n\n" str ; give_help ()
  end in

  log_and_print "[%s] WeiDU version %s\n" Sys.argv.(0) version ;
  let version_int = int_of_string Version.version in
  if version_int mod 100 <> 0 then
    log_and_print "This is a non-stable version. Unless you're sure about what you're doing, consider downgrading.\n" ;

  (* see if AUTOUPDATE is in our base name *)
  (try
    Autoupdate.self()
  with _ -> ()) ;

  Load.game_paths := Load.registry_game_paths () ;

  Myarg.parse argDescr handleArg usageMsg  ;
  if !exit_now then exit 0;

  if (!auto_update_all) then begin
    (if (Arch.do_auto_update) then
      Autoupdate.verify_latest true);
    exit (return_value StatusSuccess) ;
  end ;

  (* see if SETUP is in our base name *)
  let setup_regexp = Str.regexp_case_fold "setup" in
  if not !no_auto_tp2 then begin
    (try
      let _ = Str.search_forward setup_regexp argv0_base 0 in
      auto () ;
    with Not_found ->
      if Array.length Sys.argv <= 1 then begin
        Myarg.usage argDescr usageMsg ;
        flush_all () ;
        log_and_print "\nEnter arguments: " ;
        let mystr = read_line () in
        if mystr = "" then exit (return_value StatusArgumentInvalid)
        else exit ( Sys.command (Sys.executable_name ^ " " ^ mystr))
      end) ;
  end else
    test_output_tlk_p := false ;

  let game =
    if !no_game then
      Load.load_null_game ()
    else
      Load.load_game ()
  in

  Load.saved_game := Some(game) ;

  if (!forced_script_style <> Load.NONE) then
    force_script_style game !forced_script_style Sys.argv.(0);

  if Load.enhanced_edition_p game then begin
    (match !ee_use_lang with
    | None -> Load.set_bgee_lang_dir game
              (attempt_to_load_bgee_lang_dir game.Load.game_path)
    | Some s -> Load.set_bgee_lang_dir game (Some s) ;
        write_bgee_lang_dir game.Load.game_path s) ;
  end
  else begin
    ignore (Load.actually_load_tlk_pair game (Load.get_active_dialogs game)) ;
  end ;

  ignore (Load.deal_with_tlkin game) ;

  (* todo: fix this mess *)
  Dc.cur_index := Array.length (Load.get_active_dialog game) ;


  let automate_min = lazy(match !automate_min with
  | Some x -> x
  | None -> begin
      let rec walk lst = match lst with
      | (s,i) :: _ when Tppe.is_true (Tppe.eval_pe "" game (Tp.PE_GameIs(s,false))) -> i
      | hd :: tl -> walk tl
      | [] -> 0
      in walk ["BG2",62169; "TOB",74107; "BG1",22186; "TOTSC",24124; "IWD1",34502; "HOW",34502; "TOTLM",34502; "PST",106497; "IWD2",41422]
  end)
  in
  if !automate_list <> [] then begin
    if_bgee_check_lang_or_fail game ;
    Automate.automate game !automate_list (Lazy.force automate_min) (output_string (Lazy.force theout.chan)) ;
  end;

  (match !automate_file with
  | Some(x) -> if_bgee_check_lang_or_fail game ;
      Automate.automate_file game x (Lazy.force automate_min) (output_string (Lazy.force theout.chan));
  | None -> ());


  if (!forceify <> None) then
    forceify_file !forceify game ;


  if !make_tlk <> [] then
    make_tlk_from_file game !make_tlk ;


  (if !extract_tlk then begin
    extract_tlk_to_file game !user_min !user_max !strfind_list !traify_num ;
    strfind_list := [] ;
  end);


  (if !extract_kits > 0 then Kit.extract game (output_string (Lazy.force theout.chan)) theout.dir
      !extract_kits) ;


  if !cmp_dest <> None then
    cmp_binary_file !cmp_dest !cmp_src game ;


  if !dcmp_dest <> None then
    cmp_d_file !dcmp_dest !dcmp_src game ;


  if !textcmp_dest <> None then
    cmp_text_file !textcmp_dest !textcmp_src game ;


  if !bcmp_dest <> None then
    cmp_bcs_file !bcmp_dest !bcmp_src game ;


  if !rcmp_src <> None && !rcmp_dest <> None then
    rcmp_file game !rcmp_src !rcmp_dest ;


  (* For debugging patch/diff: *)
  if !bcmp_orig <> None && !bcmp_patch <> None then
    diff_patch_file !bcmp_orig !bcmp_patch game ;


  if !tlkcmp_src <> None && !tlkcmp_dest <> None then
    cmp_tlk_file !tlkcmp_src !tlkcmp_dest !tlkcmp_strings !user_min !user_max ;


  if !tcmp_src <> None && !tcmp_dest <> None then
    cmp_tra_file !tcmp_src !tcmp_dest game ;


  (* Display Strings *)
  if !ds_list <> [] && (!user_min <> None || !user_max <> None) then
    display_string_by_number !ds_list !user_min !user_max game
  else if !ds_list <> [] then
    List.iter (fun i -> display_string i game) !ds_list ;


  (* display strings that match *)
  if (!strfind_list <> []) then
    display_string_by_content !strfind_list game ;


  (* List all BIFFs *)
  if (!list_biff) then begin
    Key.list_biff game.Load.key output_theout
  end ;


  (* List all files *)
  if (!list_files) then begin
    Key.list_key game.Load.key output_theout
  end ;


  (* List BIFF contents *)
  if (!bc_list <> []) then begin
    Key.list_biff_contents game.Load.key output_theout !bc_list
  end ;


  (* List languages in a tp2 *)
  if !list_lang <> None then
    list_languages !list_lang ;


  (* list components of a tp2 *)
  if !list_comp <> None then
    list_components !list_comp !list_comp_lang game ;


  if !list_comp_json <> None then
    list_components_json !list_comp_json !list_comp_lang game ;


  if !save_comp_name then
    save_component_name game ;

  (* Regex on BIFF contents *)
  if (!bs_type_list <> [] && !bs_str_list <> []) then begin
    Load.search_biff_contents game output_theout !bs_type_list !bs_str_list
  end else if (!bs_type_list <> [] && !biff_short_at <> 0) then begin
    let size = if !biff_short > 65535 then 4
    else if !biff_short > 255 then 2
    else 1
    in
    Load.search_biff_contents_fun game output_theout !bs_type_list
      (fun buff ->
        (String.length buff) >= (!biff_short_at + size) &&
        let i = (match size with
        | 2 -> short_of_str_off
        | 1 -> byte_of_str_off
        | _ -> int_of_str_off
              ) buff !biff_short_at in
        i = !biff_short)
  end else if (!bs_type_list <> [] || !bs_str_list <> []) then begin
    log_and_print "WARNING: Please specify both --biff-type EXT and (--biff-str STRING or --biff-short-at OFFSET)\n"
  end  ;

  (* Grab resources from BIFFs *)
  if (!bg_list <> []) then
    biff_get !bg_list game ;


  if !make_biff <> None then
    make_biff_from_dir !make_biff game ;


  if !remove_biff <> None then
    remove_biff_from_key !remove_biff game ;


  (* Append Strings *)
  if !strapp_list <> [] then
    append_strings !strapp_list game ;


  (* Handle DLG files *)
  if !dlg_list <> [] then
    decompile_dlg !dlg_list !transitive !two_pass !use_trans !d_headers !d_toplevel game ;


  (* Handle TRA files *)
  Dc.ok_to_resolve_strings_while_loading := Some(game) ;
  List.iter handle_tra_filename !trans_list ;

  if !test_trans then begin
    Dc.test_trans output_theout game
  end ;

  if !untraify_d <> None && !untraify_tra <> None then
    untraify game !untraify_d !untraify_tra ;


  if !traify <> None then
    traify_file game !traify traify_num !traify_comment !traify_old_tra ; (* traify_num is intentionally not dereferenced *)


  if !baf_list <> [] then
    compile_baf !baf_list game ;


  (* Handle D files *)
  List.iter (handle_d_filename ) !d_list ;
  Dc.ok_to_resolve_strings_while_loading := None;

  (* Emit DLG files *)
  emit_dlg_files game theout.dir ;


  (* Check that we can write to the given TLK file(s) *)
  if !test_output_tlk_p then begin
    test_output_tlk game pause_at_end ; (* pause_at_end is intentionllay not derefenenced *)
  end ;


  if !tp_list <> [] then begin
    do_tp2_files !tp_list !force_install_these_main !force_uninstall_these_main pause_at_end game ; (* pause_at_end is intentionally not dereferenced *)
  end ;


  if !process_script <> "" then begin
    do_script !process_script pause_at_end game; (*pause_at_end is intentionally not dereferenced *)
  end ;


  if !change_log <> [] then begin
    let results = Changelog.changelog !change_log game in
    ignore (List.iter (fun result ->
      output_theout result.Changelog.text ;
      List.iter (fun (source, new_name) ->
        let destination = Printf.sprintf "%s/%s" theout.dir new_name in
        copy_large_file source destination "--change-log") result.Changelog.backup_files) results) ;
  end ;


  if !parse_check_file <> "" && !parse_check_kind <> "" then
    parse_check !parse_check_file !parse_check_kind ;


  (* Handle BCS files *)
  if !bcs_list <> [] then
    decompile_bcs !bcs_list game ;


  (match !backup_list_chn with
    Some(c) -> close_out c ; backup_list_chn := None
  | None -> () ) ;
  backup_dir := None ;

  (* make sure we add all those strings! *)
  if not (Queue.is_empty !Dc.strings_to_add) then begin
    let dc_lse_strapp_list = !Dc.strings_to_add in
    Load.append_strings game dc_lse_strapp_list
  end ;

  if not (game.Load.str_sets = []) then begin
    log_or_print "WARNING: %d SET_STRINGs were executed but no uninstall information was created.\n" (List.length game.Load.str_sets)
  end ;


  if !tlk_merge <> [] then
    merge_tlk !tlk_merge game ;


  (* Emit all changed dialogs *)
  let save_tlk path contents =
    let outchan = open_for_writing path true in
    Tlk.save_tlk path contents outchan in
  (* Emit all but the acitve dialogs *)
  Array.iteri (fun index tlk_pair ->
    if not (index = game.Load.dialog_index) then begin
      if tlk_pair.Load.dialog_mod then begin
        save_tlk tlk_pair.Load.dialog.Load.path
          tlk_pair.Load.dialog.Load.contents
      end ;
      if tlk_pair.Load.dialogf_mod then begin
        (match tlk_pair.Load.dialogf with
        | None -> ()
        | Some df -> save_tlk df.Load.path df.Load.contents)
      end
    end) game.Load.dialogs ;
  (* Emit the active dialogs to their original paths or to --tlkout *)
  let tlk_pair = Load.get_active_dialogs game in
  if tlk_pair.Load.dialog_mod then begin
    (match !output_dialog with
    | None -> save_tlk tlk_pair.Load.dialog.Load.path
          tlk_pair.Load.dialog.Load.contents
    | Some dpath -> save_tlk dpath tlk_pair.Load.dialog.Load.contents) ;
  end ;
  if tlk_pair.Load.dialogf_mod then begin
    (match !output_dialogf, tlk_pair.Load.dialogf with
    | None, Some df -> save_tlk df.Load.path df.Load.contents
    | Some dfpath, Some df -> save_tlk dfpath df.Load.contents
    | _, _ -> ()) ;
  end ;

  Hashtbl.iter (fun a x -> Unix.close x.Biff.fd) game.Load.loaded_biffs;
  Queue.iter my_unlink Load.cbifs_to_rem;

  List.iter (fun (a,b) -> log_and_print "\n%s %s\n" a b) (List.rev !Tpstate.strings_to_print_at_exit) ;

  ()
;; (* main ends here *)


(try
  Stats.time "stuff not covered elsewhere" main ();
with e ->
  log_and_print "\nFATAL ERROR: %s\n" (printexc_to_string e) ) ;

(match !Util.log_channel with
  Some(o) -> Stats.print o "\n\t\tWeiDU Timings\n" ; flush o
| None -> () ) ;

List.iter (fun c -> match c with
| Command (s,e) ->
    log_or_print "Executing: [%s]\n" s ;
    ignore (exec_command s e)
| Fn f ->
    Lazy.force f)
    !execute_at_exit ;
if file_exists "override/add_spell.ids" && not (file_exists "override/spell.ids.installed") && not (
  let files = Sys.readdir "override" in
  let r = Str.regexp_case_fold "$spell\\.ids\\..*\\.marker^" in
  List.exists (fun f -> Str.string_match r f 0) (Array.to_list files)
    ) then
  my_unlink "override/add_spell.ids" ;

(match !Util.log_channel with
  Some(o) -> close_out o
| None -> () ) ;

Util.log_channel := None;

if not !no_exit_pause && (!pause_at_end ||
(!exit_status <> StatusSuccess)) then begin
  log_and_print "\nPress ENTER to exit.\n" ;
  try ignore (read_line () ) with _ -> ()
end ;

exit (return_value !exit_status) ;
