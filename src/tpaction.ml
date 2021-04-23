(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util
open Diff
open Tp
open Parsewrappers
open Tpstate
open Tphelp
open Tppe
open Tppatch
open Tpuninstall


(*************************************************************************
 * process_action
 *************************************************************************)
let rec process_action_real our_lang game this_tp2_filename tp a =

  let get_next_col_number file =
    let (a,b) = split file in
    let buff,path = Load.load_resource "getting 2DA columnns" game true a b in
    try
      let lst = Str.split many_newline_or_cr_regexp buff in
      let elt = List.nth lst 5 in
      let cols = split_apart elt in
      let num_cols = List.length cols in
      let last_col = num_cols - 2 in
      last_col + 1
    with e ->
      log_and_print "ERROR: cannot find col numbers in %s\n" file ;
      raise e
  in

  let get_next_line_number file =
    let (a,b) = split file in
    let buff,path = Load.load_resource "getting 2DA lines" game true a b in
    try
      let idx = Str.search_backward (Str.regexp "[\r\n][0-9]") buff
          ((String.length buff) - 1) in
      let minibuff = Str.string_after buff (idx+1) in
      let lst = split_apart minibuff in
      let elt = List.hd lst in
      let last_number = int_of_string elt in
      last_number + 1
    with e ->
      log_and_print "ERROR: cannot find line numbers in %s\n" file ;
      raise e
  in

  let when_exists file when_list existing game =
    if List.mem TP_IfExists when_list then begin
      if not existing then
        bigg_file_exists file game.Load.key
      else is_true (eval_pe "" game (Pred_File_Exists_In_Game (PE_LiteralString(file)))) ;
    end else true in

  let process_action = (process_action_real our_lang game this_tp2_filename) in
  let process_patch2 = process_patch2_real process_action tp our_lang in

  let run_patch x = ignore (process_patch2 "" game "" x) in
  let pl_of_al x = [TP_PatchInnerAction x] in

  let str = action_to_str a in
  Stats.time str (fun () ->
    try
      (match a with

      | TP_ActionBashFor(where,al) ->
          run_patch (TP_PatchBashFor (where,pl_of_al al))

      | TP_ActionDefineArray(arr,vals) ->
          run_patch (TP_PatchDefineArray(arr,vals))

      | TP_ActionDefineAssociativeArray(arr,vals) ->
          run_patch (TP_DefineAssociativeArray(arr,vals))

      | TP_ActionSortArrayIndices(array,sort_type) ->
          run_patch (TP_PatchSortArrayIndices(array,sort_type))

      | TP_Action_For_Each(var,sl,al) ->
          run_patch (TP_PatchForEach(var,sl,pl_of_al al))

      | TP_ActionPHPEach(var,invar,outvar,al) ->
          run_patch (TP_PatchPHPEach(var,invar,outvar,pl_of_al al))

      | TP_Outer_For(init,guard,inc,body) ->
          run_patch (TP_PatchFor(init,guard,inc,pl_of_al body))

      | TP_Outer_While(guard,body) ->
          run_patch (TP_PatchWhile(guard,pl_of_al body))

      | TP_Outer_Inner_Buff(buff_var,pl) ->
          run_patch (TP_PatchInnerBuff(buff_var,pl))

      | TP_Outer_Inner_Buff_Save(store_var,buff_var,pl) ->
          run_patch (TP_PatchInnerBuffSave(store_var,buff_var,pl))

      | TP_Outer_Set(name,value) ->
          run_patch (TP_PatchSet(name,value))

      | TP_Outer_Sprint(name,msg) ->
          run_patch (TP_PatchSprint(name,msg))

      | TP_Outer_Text_Sprint (var,str) ->
          run_patch (TP_PatchTextSprint(var,str))

      | TP_Outer_Snprint(size,name,msg) ->
          run_patch (TP_PatchSnprint(size,name,msg))

      | TP_Print(msg) ->
          run_patch(TP_PatchPrint(msg))

      | TP_Warn(msg) ->
          run_patch(TP_PatchWarn(msg))

      | TP_Log(msg) ->
          run_patch(TP_PatchLog(msg))

      | TP_Fail(msg) ->
          run_patch(TP_PatchFail(msg))

      | TP_Abort(msg) ->
          run_patch(TP_PatchAbort(msg))

      | TP_Reraise ->
          run_patch(TP_PatchReraise)

      | TP_If(p,al1,al2) ->
          run_patch(TP_PatchIf(p,pl_of_al al1, pl_of_al al2))

      | TP_ActionMatch(str,opts) ->
          run_patch(TP_PatchMatch(str, List.map (fun (a,b,c) -> a,b,pl_of_al c) opts))

      | TP_ActionTry(al,opts) ->
          run_patch(TP_PatchTry(pl_of_al al, List.map (fun (a,b,c) -> a,b,pl_of_al c) opts))

      | TP_Define_Action_Macro(str,decl,al) ->
          Hashtbl.replace macros (str,false) (decl, pl_of_al al)

      | TP_Define_Patch_Macro(str,decl,al) ->
          Hashtbl.replace macros (str,true) (decl, al)

      | TP_Define_Action_Function (name,ints,strs,rets,retas,body) ->
          Hashtbl.replace functions (name,false) (ints,strs,rets,retas,pl_of_al body)

      | TP_Define_Dimorphic_Function (name,ints,strs,rets,retas,body) ->
          Hashtbl.replace functions (name,false) (ints,strs,rets,retas,pl_of_al body) ;
          Hashtbl.replace functions (name,true) (ints,strs,rets,retas,pl_of_al body)

      | TP_Define_Patch_Function (name,ints,strs,rets,retas,body) ->
          Hashtbl.replace functions (name,true) (ints,strs,rets,retas,body)

      | TP_Launch_Action_Function (name,ints,strs,rets,retas) ->
          run_patch (TP_Launch_Patch_Function (name,false,ints,strs,rets,retas))

      | TP_Launch_Action_Macro(str) ->
          run_patch (TP_Launch_Patch_Macro (str,false))

      | TP_Action_ReadLN(x) ->
          run_patch (TP_PatchReadLN x)

      | TP_RandomSeed(i) ->
          run_patch (TP_PatchRandomSeed i)

      | TP_ActionClearArray(arr) ->
          run_patch (TP_PatchClearArray arr)

      | TP_Silent ->
          run_patch (TP_PatchSilent)

      | TP_Verbose ->
          run_patch (TP_PatchVerbose)

      | TP_ActionToLower x ->
          run_patch (TP_PatchToLower x)

      | TP_ActionToUpper x ->
          run_patch (TP_PatchToUpper x)

      | TP_ActionGetStrRef (a,b,c,d) ->
          run_patch (TP_PatchGetStrRef(a,b,c,d))

      | TP_Delete(filelist, do_backup) ->
          let directories = ref [] in
          let rec delete list =
            match list with
            | [] -> ()
            | head :: tail -> begin
                if file_exists head && not (is_directory head) then begin
                  if do_backup then
                    backup_if_extant head ;
                  ignore (record_other_file_op head) ;
                  Sys.remove head ;
                  delete tail
                end else if is_directory head then begin
                  let head = Str.global_replace (Str.regexp "/$") ""
                      (Arch.backslash_to_slash head) in
                  let contents = List.map (fun x ->
                    String.concat "/" [head;x]) (Array.to_list
                                                   (Case_ins.sys_readdir head)) in
                  directories := head :: !directories ;
                  delete (List.append contents tail)
                end else begin
                  log_or_print "DELETE [%s]: does not exist" head ;
                  delete tail
                end
            end
          in
          delete (List.map (fun x ->
            Case_ins.fix_name (Var.get_string (eval_pe_str x))) filelist) ;
          List.iter (fun dir -> Case_ins.unix_rmdir dir) !directories

      | TP_Move(filelist, do_backup) ->
          let move src dst =
            let ok = ref true in
            (try ignore (String.index src ' '); ok := false with _ -> ());
            (try ignore (String.index dst ' '); ok := false with _ -> ());
            if do_backup && not !ok then
              failwith
                (Printf.sprintf "MOVE [%s] [%s]: source and destination can't contain spaces"
                   src dst);
            let dst = if is_directory dst then
              dst ^ "/" ^ (Case_ins.filename_basename src)
            else
              dst in
            if not (is_directory src) then
              if (file_exists dst) then begin
                log_or_print "MOVE [%s] [%s]: destination exists, falling back to COPY_LARGE\n"
                  src dst ;
                process_action tp (TP_CopyLarge{
                                   copy_large_use_glob = false;
                                   copy_large_file_list = [ (src,dst) ];
                                   copy_large_backup = do_backup;
                                 }); end
              else begin
                log_or_print "Moving %s to %s\n" src dst;
                if not (file_exists src) then
                  log_and_print "ERROR: cannot locate %s\n" src ;
                Case_ins.unix_rename src dst;
                if do_backup then
                  (match !move_list_chn with
                  | Some(chn) -> output_string chn (src ^ log_line_separator ^ dst ^ "\n") ;
                      flush chn
                  | None -> ());
                if !ok then
                  ignore (List.iter record_other_file_op [src;dst]) end
            else log_and_print "MOVE [%s] [%s]: source is a directory\n" src dst;
          in
          List.iter (fun (src,dst) ->
            (match src with
            | TP_File src ->
                let src = Var.get_string src in
                let dst = Var.get_string dst in
                if is_directory src then begin
                  if not (is_directory dst) then
                    failwith
                      (Printf.sprintf "MOVE [%s] [%s]: can't move a directory into a file, or destination directory not existing"
                         src dst);
                  Array.iter (fun file ->
                    move (src ^ "/" ^ file) dst) (Sys.readdir src)
                end else
                  move src dst
            | TP_Directory_Regexp (dir, match_exact, regexp) ->
                let dir = Var.get_string dir in
                let regexp = Var.get_string regexp in
                let dst = Var.get_string dst in
                if not (is_directory dst) then
                  failwith
                    (Printf.sprintf "MOVE ([%s] [%s]) [%s]: can't move a directory into a file, or destination directory not existing"
                       dir regexp dst);
                let dh = Case_ins.unix_opendir dir in
                let reg =
                  (match match_exact with
                  | Some(true) ->
                      Str.regexp_string_case_fold
                  | _ ->
                      Str.regexp_case_fold)
                    regexp in
                (try
                  while true do
                    let next = Unix.readdir dh in
                    if ((Case_ins.unix_stat (dir ^ "/" ^ next)).Unix.st_kind = Unix.S_REG) &&
                      (Str.string_match reg next 0) then begin
                        let file = (String.uppercase (dir ^ "/" ^ next)) in
                        let filespec = Case_ins.filename_basename file in
                        move file (dst ^ "/" ^ filespec)
                      end
                  done ;
                with End_of_file -> ());
                Unix.closedir dh ;)) filelist

      | TP_DisableFromKey(file_lst) ->
          let file_lst = List.map Var.get_string (List.map eval_pe_str file_lst) in
          let file_lst = List.map String.uppercase file_lst in
          let new_key = Key.remove_files game.Load.key file_lst in
          let oc = open_for_writing "CHITIN.KEY" true in
          Key.save_key new_key oc ;
          close_out oc ;
          let keybuff = load_file "chitin.key" in
          game.Load.key <- Key.load_key "chitin.key" keybuff ;
          Hashtbl.iter (fun name biff ->
            (try
              Unix.close biff.Biff.fd
            with e ->
              log_and_print "ERROR: DISABLE_FROM_KEY failed to close %s\n" name ;
              raise e)) game.Load.loaded_biffs;
          game.Load.loaded_biffs <- Hashtbl.create 5 ;

      | TP_Require_File(file,error_msg) ->
          log_and_print "Checking for required files ...\n" ;
          let file = Arch.backslash_to_slash file in
          let test = ref false in
          test := bigg_file_exists file game.Load.key ;
          if !test then begin
            log_or_print "[%s] found\n" file ;
          end
          else begin
            log_and_print "[%s] not found\n" file ;
            log_and_print "\n%s\n" (Dc.single_string_of_tlk_string game error_msg) ;
            failwith file ;
          end

      | TP_Inlined_File(name,contents) ->
          let name = Var.get_string (Arch.backslash_to_slash name) in
          log_only_modder "Defined Inlined File [%s] (length %d)\n"
            name (String.length contents) ;
          Hashtbl.add inlined_files (Arch.backslash_to_slash name) contents

      | TP_Biff(dest,what) ->
          let find_list = ref [] in
          let dest = Var.get_string dest in
          List.iter (fun (directory,exact_match,regexp_string) ->
            (try
              let directory = Var.get_string directory in
              let regexp_string = Var.get_string regexp_string in
              if !debug_ocaml then log_and_print "MAKE_BIFF %s %s\n" directory regexp_string;
              let dh = Case_ins.unix_opendir directory in
              let reg = begin match exact_match with
              | Some(true) ->
                  Str.regexp_string_case_fold
              | _ ->
                  Str.regexp_case_fold
              end regexp_string
              in
              (try
                while true do
                  let next = Unix.readdir dh in
                  if !debug_ocaml then log_and_print "%s\n" next;
                  if ((Case_ins.unix_stat (directory ^ "/" ^ next)).Unix.st_kind =
                      Unix.S_REG) && (Str.string_match reg next 0) then
                    (if !debug_ocaml then log_and_print "  match!\n";
                     find_list := (String.uppercase (directory ^ "/" ^ next)) :: !find_list;
                     ignore (record_other_file_op ("override/" ^ next))) ;
                done
              with End_of_file -> ());
              Unix.closedir dh ;
            with _ -> ())) what ;
          if !debug_ocaml then log_and_print "MAKE_BIFF: got file list\n";
          if !find_list <> [] then begin
            let data = if game.Load.script_style = Load.PST then "" else "data/" in
            let filename = data ^ dest ^ ".bif" in
            let new_key = Biff.save_biff game.Load.key filename
                (Load.fix_biff_path filename) !find_list in
            if !debug_ocaml then log_and_print "MAKE_BIFF: Calculated new key\n";
            let oc = open_for_writing "CHITIN.KEY" true in
            Key.save_key new_key oc ;
            close_out oc ;
            if !debug_ocaml then log_and_print "MAKE_BIFF: Saved the key\n";
            (* re-load the chitin *)
            let keybuff = load_file "chitin.key" in
            if !debug_ocaml then log_and_print "MAKE_BIFF: Loaded the key file\n";
            game.Load.key <- Key.load_key "chitin.key" keybuff ;
            Hashtbl.iter (fun name biff ->
              (try
                Unix.close biff.Biff.fd
              with e ->
                log_and_print "ERROR: MAKE_BIFF failed to close %s\n" name ;
                raise e)) game.Load.loaded_biffs;
            game.Load.loaded_biffs <- Hashtbl.create 5 ;
            if !debug_ocaml then log_and_print "Unmarshaled the key\n";
          end

      | TP_Forbid_File(file,error_msg) ->
          log_and_print "Checking for forbidden files ...\n" ;
          let file = Arch.backslash_to_slash file in
          let test = ref false in
          test := bigg_file_exists file game.Load.key ;
          if !test then begin
            log_and_print "[%s] found\n" file ;
            log_and_print "\n%s\n" (Dc.single_string_of_tlk_string game
                                      error_msg) ;
            failwith file
          end else begin
            log_or_print "[%s] not found (as desired)\n" file ;
          end

      | TP_Reinclude(string_list) ->
          let string_list = List.map Var.get_string string_list in
          List.iter (fun file ->
            let tph_parsed =
              if !debug_ocaml then log_and_print "Loading %s...\n" file ;
              let x = handle_tph_filename file in
              Hashtbl.replace loaded_tph file x ;
              x
            in
            List.iter (process_action tp) tph_parsed ;) string_list ;

      | TP_Include(string_list) ->
          let string_list = List.map Var.get_string string_list in
          List.iter (fun file ->
            let tph_parsed = if Hashtbl.mem loaded_tph file
            then begin
              if !debug_ocaml then log_and_print "%s is already loaded, restoring the image...\n" file ;
              Hashtbl.find loaded_tph file
            end
            else begin
              if !debug_ocaml then log_and_print "Loading %s...\n" file ;
              let x = handle_tph_filename file in
              Hashtbl.replace loaded_tph file x ;
              x
            end
            in
            List.iter (process_action tp) tph_parsed ;) string_list ;

      | TP_Uninstall_Now(name,comp) ->
          let comp = Int32.to_int (eval_pe "" game comp) in
          let name = Var.get_string name in
          if already_installed name comp then begin
            if (!safe_exit) then failwith "cannot UNINSTALL when in --safe-exit mode.";
            if uninstall game handle_tp2_filename name comp false then
              ()
            else
              failwith
                (Printf.sprintf "unable to uninstall %s component #%d"
                   name comp)
          end else
            log_or_print "%s component #%d not present, good.\n"
              name comp

      | TP_ClearMemory ->
          log_and_print "Clearing the variables.\n" ;
          let temp_author = Var.get_string "%TP2_AUTHOR%" in
          let temp_lang = Var.get_string "%LANGUAGE%" in
          let temp_name = Var.get_string "%TP2_FILE_NAME%" in
          let temp_bname = Var.get_string "%TP2_BASE_NAME%" in
          let temp_modf = Var.get_string "%MOD_FOLDER%" in
          let temp_modv = Var.get_string "%MOD_VERSION%" in
          let temp_num = Var.get_string "%COMPONENT_NUMBER%" in
          let temp_save = Var.get_string "%SAVE_DIRECTORY%" in
          let temp_mpsave = Var.get_string "%MPSAVE_DIRECTORY%" in
          let temp_user = Var.get_string "%USER_DIRECTORY%" in
          let temp_ee_lang = Var.get_string "%EE_LANGUAGE%" in
          Var.clear_var () ;
          Var.all_the_assoc () ;
          Var.set_string "TP2_AUTHOR" temp_author ;
          Var.set_string "LANGUAGE" temp_lang ;
          Var.set_string "TP2_FILE_NAME" temp_name ;
          Var.set_string "TP2_BASE_NAME" temp_bname ;
          Var.set_string "MOD_FOLDER" temp_modf ;
          Var.set_string "MOD_VERSION" temp_modv ;
          Var.set_string "COMPONENT_NUMBER" temp_num ;
          Var.set_string "SAVE_DIRECTORY" temp_save ;
          Var.set_string "MPSAVE_DIRECTORY" temp_mpsave ;
          Var.set_string "USER_DIRECTORY" temp_user ;
          Var.set_string "EE_LANGUAGE" temp_ee_lang ;
          Arch2.associate_these () ;

      | TP_ClearArrays ->
          log_and_print "Clearing the arrays.\n" ;
          Var.clear_arr () ;

      | TP_ClearCodes ->
          log_and_print "Clearing the macros.\n" ;
          clear_codes () ;
          process_action_real our_lang game this_tp2_filename tp
            (TP_Include Tph.builtin_definitions);

      | TP_ClearInlined ->
          log_and_print "Clearing the inlined files.\n" ;
          clear_inlined () ;

      | TP_Clear_Ids_Map ->
          log_and_print "Clearing the IDS map.\n" ;
          Bcs.clear_ids_map game ;

      | TP_ClearEverything ->
          List.iter (process_action_real our_lang game this_tp2_filename tp)
            [ TP_ClearArrays; TP_ClearMemory; TP_ClearInlined;
              TP_ClearCodes; TP_Clear_Ids_Map ]

      | TP_CopyRandom(slistlist,plist,wlist) ->
          List.iter (fun slist ->
            let slist = List.filter (fun file -> when_exists file wlist true game) slist in
            log_and_print "Randomizing %d file(s) ...\n" (List.length slist) ;
            let final_dlist = ref [] in
            let finished = ref false in
            while not !finished do
              let dlist = List.map (fun s -> (Random.bits (), s) ) slist in
              let dlist = List.sort (fun (a,b) (a',b') -> compare a a') dlist in
              final_dlist := dlist ;
              finished := true ;
              if List.length dlist > 1 then
                List.iter2 (fun (a,b) c -> if b = c then finished := false)
                  dlist slist
            done ;
            let dlist = List.map (fun (a,b) -> "override/" ^ b) !final_dlist in
            let sdlist = List.combine slist dlist in
            let copy_args = {
              copy_get_existing = true;
              copy_use_regexp = false;
              copy_use_glob = false;
              copy_file_list = sdlist ;
              copy_patch_list = plist ;
              copy_constraint_list = wlist ;
              copy_backup = true;
              copy_at_end = true;
              copy_save_inlined = false;
            } in
            process_action tp (TP_Copy(copy_args))) slistlist

      | TP_CopyAllGamFiles(pl,wl) ->
          (* Patch the baldur.gam in the biff *)
          let my_copy_args = {
            copy_get_existing = true;
            copy_use_regexp = false;
            copy_use_glob = false ;
            copy_file_list = [ ("baldur.gam", "override") ] ;
            copy_patch_list = pl ;
            copy_constraint_list = wl ;
            copy_backup = true ;
            copy_at_end = false ;
            copy_save_inlined = false ;
          } in
          process_action tp (TP_Copy(my_copy_args)) ;
          let file_list = ref [] in
          let get_file_list base_dir =
            let dlist = list_of_files_in_directory base_dir in
            List.iter (fun filename ->
              let filename = base_dir ^ "/" ^ filename in
              if is_directory filename &&
                filename <> (base_dir ^ "/.") &&
                filename <> (base_dir ^ "/..")
              then
                file_list := !file_list @
                  [(filename ^ "/baldur.gam", filename ^ "/baldur.gam")] ;) dlist ;
            ()
          in
          ignore (get_file_list (Var.get_string "%SAVE_DIRECTORY%")) ;
          ignore (get_file_list (Var.get_string "%MPSAVE_DIRECTORY%")) ;
          let gam_list = List.filter (fun (src, dst) ->
            Sys.file_exists src) !file_list in
          let my_copy_args = {
            copy_get_existing = false;
            copy_use_regexp = false;
            copy_use_glob = false ;
            copy_file_list = gam_list ;
            copy_patch_list = pl ;
            copy_constraint_list = wl ;
            copy_backup = false ;
            copy_at_end = false ;
            copy_save_inlined = false ;
          } in
          process_action tp (TP_Copy(my_copy_args)) ;

      | TP_Copy(copy_args) ->
          let get_existing = copy_args.copy_get_existing in
          let use_reg = copy_args.copy_use_regexp in
          let use_glob = copy_args.copy_use_glob in
          let slist = copy_args.copy_file_list in
          let plist = copy_args.copy_patch_list in
          let clist = copy_args.copy_constraint_list in
          let make_a_backup = copy_args.copy_backup in
          let copy_at_end = copy_args.copy_at_end in
          let save_inlined = copy_args.copy_save_inlined in
          if true then begin
            let worklist = ref [] in

            let bts1, bts2 = match get_existing, use_reg with
            | false, false -> true,  true
            | true,  false -> false, true
            | true,  true  -> false, false
            | _ -> failwith "COPY type internal"
            in
            let slist = List.map (fun (x,y) -> (Var.get_string x, Var.get_string y)) slist in

            let slist = List.map (fun (x,y) ->
              (if bts1 then Arch.backslash_to_slash x else x),
              (if bts2 then Arch.backslash_to_slash y else y)) slist
            in

            let slist = List.filter (fun (src,dst) -> when_exists src clist get_existing game)
              (if get_existing = true && use_reg = true then begin
                let files_in_chitin = Key.list_of_key_resources game.Load.key use_glob in
                let new_list = List.map (fun (s,p) ->
                  let regexp = Str.regexp_case_fold s in
                  let matches = ref [] in
                  List.iter (fun possible ->
                    if Str.string_match regexp possible 0 then begin
                      (try
                        let dest = (Arch.backslash_to_slash
                                      (Str.replace_matched p possible)) in
                        let file_pattern = (Str.regexp ".+\\..+$") in
                        if not (Str.string_match file_pattern dest 0) ||
                        is_directory dest then begin
                          matches :=
                            (possible, dest ^ "/" ^ possible) :: !matches
                        end else begin
                          matches := (possible, dest) :: !matches
                        end
                      with | Failure s ->
                        failwith (Printf.sprintf
                                    "COPY_EXISTING_REGEXP failed: %s" s)) ;
                    end) files_in_chitin;
                  let matches = List.sort compare !matches in
                  if (matches = []) then
                    [(s,p)]
                  else
                    matches) slist in
                let new_list = List.flatten new_list in
                List.sort_unique compare new_list
              end else if use_glob then begin
                let res = ref [] in
                List.iter (fun (s,p) ->
                  let myfun (glob_s : string) = begin
                    log_only "Callback from Arch.glob: %s\n" glob_s;
                    res := (glob_s, p) :: !res
                  end in
                  log_only "Calling Arch.glob: %s\n" s;
                  let res = (Arch.glob s myfun) in
                  log_only "Arch.glob returned.\n";
                  res) slist ;
                !res
              end else
                slist) in

            let len = List.length slist in
            log_and_print "Copying%s %d file%s ...\n"
              (if List.length plist > 0 then " and patching" else "")
              len
              (if len = 1 then "" else "s") ;

            let copy_one_file src dest = begin
              let dest =
                if use_reg || use_glob then
                  dest
                else
                  Var.get_string dest in
              let src =
                if use_reg || use_glob then
                  src
                else
                  Var.get_string src in
              if String.uppercase src = "DIALOG.TLK" then
                log_and_print_modder "\n\nUse COPY_LARGE rather than COPY on dialog.tlk!\n\n\n" ;

              let buff =
                if not get_existing then
                  load_file src
                else
                  let a,b = split src in
                  let buff,path = Load.load_resource "COPY" game true a b in
                  buff in
              let orig_buff = String.copy buff in
              ignore (set_copy_vars src dest (Some(buff))) ;

              (* if (buff <> "") then *) begin
                if (!has_if_eval_bug || List.exists (fun x ->
                  match x with
                    TP_Eval(_) -> true | _ -> false) clist) then begin
                      try
                        List.iter (fun p -> process_patch1 src game buff p) plist
                      with _ -> ()
                    end;
                let ok_to_copy = List.fold_left (fun acc elt -> acc &&
                  match elt with
                  | TP_Contains(s) -> begin
                      let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
                      try
                        let _ = Str.search_forward my_regexp buff 0 in
                      true
                      with _ ->
                        log_only_modder "Not copying [%s] to [%s] because it does NOT contain [%s]\n"
                          src dest (Var.get_string (eval_pe_str s)) ;
                        false
                  end
                  | TP_NotContains(s) -> begin
                      let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
                      try
                        let _ = Str.search_forward my_regexp buff 0 in
                        log_only_modder "Not copying [%s] to [%s] because it DOES contain [%s]\n"
                          src dest (Var.get_string (eval_pe_str s)) ;
                        false
                      with _ ->
                        true
                  end
                  | TP_IfSizeIs(size) ->
                      if String.length buff = size then
                        true
                      else begin
                        log_only_modder "Not copying [%s] to [%s] because size is %d, NOT %d\n"
                          src dest (String.length buff) size ;
                        false
                      end
                  | TP_Eval(pe) ->
                      let v = eval_pe buff game pe in
                      if v = Int32.zero then begin
                        log_only_modder "Not copying [%s] to [%s] because condition evaluates to %ld\n"
                          src dest v ; false
                      end else true
                  | TP_ButOnlyIfItChanges
                  | TP_IfExists -> true) true clist in
                if ok_to_copy then begin
                  let result_buff =
                    List.fold_left (fun acc elt ->
                      try process_patch2 src game acc elt
                      with e -> log_and_print
                          "ERROR: [%s] -> [%s] Patching Failed (COPY) (%s)\n"
                          src dest (printexc_to_string e); raise e)
                      buff plist in
                  let dest =
                    if is_directory dest then
                      dest ^ "/" ^ (Case_ins.filename_basename src)
                    else
                      dest in
                  let it_changed =
                    Stats.time "BUT_ONLY" (fun () ->
                      if List.mem TP_ButOnlyIfItChanges clist then
                        begin
                          let changed =
                            ((String.length result_buff) <>
                             (String.length orig_buff)) ||
                             ((String.compare orig_buff result_buff) <> 0) in
                          let newsrc, newdes =
                            (Case_ins.filename_basename src, Case_ins.filename_basename dest) in
                          if !debug_boiic then log_and_print "[%s]%s->[%s]%s did%s change\n"
                              newsrc (String.make (12 - (String.length newsrc)) ' ')
                              newdes (String.make (12 - (String.length newdes)) ' ')
                              (if changed then "" else "n't") ;
                          changed
                        end else begin
                          if (!debug_change) && (plist <> []) then begin
                            let changed =
                              ((String.length result_buff) <>
                               (String.length orig_buff)) ||
                               ((String.compare orig_buff result_buff) <> 0) in
                            let newsrc, newdes =
                              (Case_ins.filename_basename src, Case_ins.filename_basename dest) in
                            if !debug_change && not changed then
                              log_and_print "Patches on [%s]%s->[%s]%s don't alter the file\n"
                                newsrc (String.make (12 - (String.length newsrc)) ' ')
                                newdes (String.make (12 - (String.length newdes)) ' ') ;
                            true
                          end else true ;
                          (* always copy *)
                        end) () in
                  if (it_changed) then begin
                    let doit () = Stats.time "saving files" (fun () ->
                      if not (save_inlined) then begin
                        let out =
                          try open_for_writing_internal make_a_backup dest true
                          with e -> log_and_print
                              "ERROR: COPY ~%s~ ~%s~ FAILED: cannot open target\n"
                              src dest ;
                            raise e in
                        output_string out result_buff ;
                        close_out out ;
                        begin (* handle read-only files! *)
                          try
                            Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
                          with e -> ()
                              (* log_or_print "WARNING: chmod %s : %s\n" filename
                                 (printexc_to_string e) *)
                        end ;
                        if make_a_backup then
                          log_only "Copied [%s] to [%s]\n" src dest
                        else
                          log_only "Copied [%s] to [%s] (NO BACKUP MADE!)\n" src dest
                      end else begin
                        log_only_modder "Defined Inlined File [%s] (length %d)\n"
                          dest (String.length result_buff) ;
                        Hashtbl.add inlined_files (Arch.backslash_to_slash dest) result_buff
                      end) ()
                    in
                    if copy_at_end then worklist := doit :: !worklist
                    else doit ()
                  end else begin
                    log_only_modder
                      "Not copying [%s] to [%s] because it did not change\n"
                      src dest
                  end
                end (* end: if ok_to_copy *)
              end (* (* end: if buff <> "" *)
                     else log_or_print "Not copying [%s] to [%s] because it is empty\n"
                     src dest
                   *)
            end (* end: let copy_one_file = begin *)
            in
            List.iter (fun (src,dest) ->
              let src = Var.get_string src in
              if is_directory src then begin
                let dh = Case_ins.unix_opendir src in
                (try
                  while true do
                    let base = Unix.readdir dh in
                    let source = src ^ "/" ^ base in
                    if not (is_directory source) then
                      copy_one_file source (dest ^ "/" ^ base)
                  done
                with _ -> () ) ;
                Unix.closedir dh
              end else if (is_directory dest) then
                copy_one_file src (dest ^ "/" ^
                                   (Case_ins.filename_basename src))
              else
                copy_one_file src dest) slist ;
            List.iter (fun f -> f ()) !worklist
          end

      | TP_CopyLarge(copy_large_args) ->
          let use_glob = copy_large_args.copy_large_use_glob in
          let slist = copy_large_args.copy_large_file_list in
          let make_a_backup = copy_large_args.copy_large_backup in
          let worklist = ref [] in

          let slist =
            List.map (fun (x,y) ->
              (Arch.backslash_to_slash x, Arch.backslash_to_slash y)) slist in

          let slist =
            if use_glob then begin
              let res = ref [] in
              List.iter (fun (s,p) ->
                let myfun (glob_s : string) = begin
                  log_only "Callback from Arch.glob: %s\n" glob_s;
                  res := (glob_s, p) :: !res
                end in
                log_only "Calling Arch.glob: %s\n" s;
                let res = (Arch.glob s myfun) in
                log_only "Arch.glob returned.\n";
                res) slist ;
              !res
            end else
              slist in

          let len = List.length slist in
          log_and_print "Copying%s %d file%s ...\n" "" len
            (if len = 1 then "" else "s") ;

          let copy_one_file src dest = begin
            let dest =
              if use_glob then
                dest
              else
                Var.get_string dest in
            let src =
              if use_glob then
                src
              else
                Var.get_string src in
            let dest =
              if is_directory dest then
                dest ^ "/" ^ (Case_ins.filename_basename src)
              else
                dest in
            (match String.uppercase (snd (split dest)) with
            | ".IDS" -> Bcs.clear_ids_map game
            | _ -> ()) ;
            ignore (set_copy_vars src dest None) ;
            Stats.time "saving files" (fun () ->
              if Hashtbl.mem inlined_files (Arch.backslash_to_slash src) then begin
                let copy_args = {
                  copy_get_existing = false;
                  copy_use_regexp = false;
                  copy_use_glob = use_glob;
                  copy_file_list = [(src,dest)] ;
                  copy_patch_list = [] ;
                  (* C_L is <> [] to avoid looping *)
                  copy_constraint_list = [TP_Eval(get_pe_int "1")] ;
                  copy_backup = make_a_backup ;
                  copy_at_end = false ;
                  copy_save_inlined = false ;
                } in
                (* we came here from tp_copy, now we go back *)
                process_action tp (TP_Copy(copy_args))
              end else begin
                if make_a_backup then
                  backup_if_extant dest
                else
                  record_other_file_op dest ;
                copy_large_file src dest "doing a COPY_LARGE" ;
                if make_a_backup then
                  log_only "Copied [%s] to [%s]\n" src dest
                else
                  log_only "Copied [%s] to [%s] (NO BACKUP MADE!)\n" src dest
              end) ()
          end (* end: let copy_one_file = begin *)
          in
          List.iter (fun (src,dest) ->
            let src = Var.get_string src in
            if is_directory src then begin
              let dh = Case_ins.unix_opendir src in
              (try
                while true do
                  let base = Unix.readdir dh in
                  let source = src ^ "/" ^ base in
                  if not (is_directory source) then
                    copy_one_file source (dest ^ "/" ^ base)
                done
              with _ -> ()) ;
              Unix.closedir dh
            end else copy_one_file src dest) slist ;
          List.iter (fun f -> f ()) !worklist

      | TP_Add_Music(m) -> begin
          let mus_file = Var.get_string m.music_file in
          let mus_base_file = Case_ins.filename_basename mus_file in
          let mus_name = Var.get_string m.music_name in
          if is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString "SONGLIST.2DA",
                            PE_LiteralString
                              ("[ %TAB%%LNL%%WNL%]" ^ mus_base_file ^ "[ %TAB%%LNL%%WNL%]"))))
          then begin
            let buff,path =
              Load.load_resource "ADD_MUSIC" game true "SONGLIST" "2DA" in
            let number = find_table_row buff 2 (Str.regexp_case_fold ("^" ^ mus_base_file ^ "$")) in
            let number = Int32.sub number 3l in
            Var.set_int32 mus_name number ;
            log_and_print "\n\nMUS [%s] already present! Skipping!\n\n"
              mus_base_file
          end else begin
            log_and_print "Adding %s Music ...\n" mus_name ;
            let this_music_number = get_next_line_number "SONGLIST.2DA" in
            if this_music_number > 100 then begin
              if not (check_enhanced_engine game (Some "tb#music") (Some 20) true true) then begin
                failwith "The game requires tob_hacks, ToBEx or GemRB to support more than 100 musics."
              end
            end;
            let str_to_append = Printf.sprintf "%d %s %s"
                this_music_number mus_name mus_base_file in

            let a1 = TP_Append("SONGLIST.2DA",str_to_append,[],true,false,0) in

            let dest_music_file = "music/" ^ mus_base_file in
            let a2 = TP_Copy(
              {copy_get_existing = false;
                copy_use_regexp = false;
                copy_use_glob = false;
                copy_file_list = [(mus_file,dest_music_file)] ;
                copy_patch_list = [] ;
                copy_constraint_list = [] ;
                copy_backup = true;
                copy_at_end = false;
                copy_save_inlined = false;} ) in
            let action_list = [ a1 ; a2 ] in
            List.iter (process_action tp) action_list ;
            Var.set_int32 mus_name (Int32.of_int this_music_number) ;
            log_and_print "Added %s Music\n" mus_name ;
          end
      end

      | TP_Add_AreaType(flag) -> begin
          let flag = Var.get_string (eval_pe_str flag) in
          if is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString "AREATYPE.iDS",
                            PE_LiteralString
                              ("[ \t\n\r]" ^ flag ^ "[ \t\n\r]"))))
          then begin
            Var.set_int32 (flag) (Bcs.int_of_sym game "AREATYPE" flag) ;
            log_and_print "\n\nArea Type [%s] already present! Skipping!\n\n" flag
          end else begin
            let a,b = split "AREATYPE.IDS" in
            let buff,path =
              Load.load_resource "ADD_AREA_TYPE" game true a b in
            let rec trynumber i =
              if i = 16 then
                failwith (Printf.sprintf "No space in areatype.ids for %s" flag);
              let number = 1 lsl i in
              if is_true (eval_pe "" game
                            (PE_ResourceContains
                               (PE_LiteralString "AREATYPE.iDS",
                                PE_LiteralString
                                  ("[ \t\n\r]" ^ string_of_int number ^ "[ \t\n\r]"))))
              then trynumber (i + 1)
              else i
            in
            let number = trynumber 0 in
            let a1 = TP_Append
                ("areatype.ids",
                 (Printf.sprintf "%d %s" (1 lsl number) flag),[],true,false,0) in
            process_action tp a1;
            Var.set_int32 flag (Int32.of_int number) ;
            log_and_print "Added Area Type %s\n" flag;
          end
      end

      | TP_Add_2DA(f,s,r) -> begin
          let s = Var.get_string (eval_pe_str s) in
          if is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString f,
                            PE_LiteralString
                              ("[ \t\n\r]" ^ s ^ "[ \t\n\r]"))))
          then begin
            let a,b = split f in
            let buff,path =
              Load.load_resource "ADD_2DA" game true a b in
            let number = find_table_row buff 0
                (Str.regexp_case_fold ("^" ^ s ^ "$")) in
            let number = Int32.sub number 3l in
            Var.set_int32 s number ;
            log_and_print "\n\n%s [%s] already present! Skipping!\n\n" f s
          end else begin
            let number = (get_line_count f game) in
            let t = match Dc.resolve_tlk_string game r with
              Dlg.TLK_Index(i) -> i
            | _ -> log_and_print "ERROR: cannot resolve SAY patch\n" ;
                failwith "resolve"
            in
            let a1 = TP_Append
                (f,(Printf.sprintf "%s %d" s t),[],true,false,0) in
            process_action tp a1;
            Var.set_int32 s (Int32.of_int number) ;
            log_and_print "Added %s %s\n" f s;
          end
      end

      | TP_Add_Projectile(p) -> begin
          let p = {p with pro_file = Var.get_string p.pro_file;} in
          let this_pro_name =
            Case_ins.filename_chop_extension
              (Case_ins.filename_basename p.pro_file) in
          if is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString "PROJECTL.IDS",
                            PE_LiteralString
                              ("[ %TAB%%LNL%%WNL%]" ^ this_pro_name ^ "[ %TAB%%LNL%%WNL%]"))))
          then begin
            Var.set_int32 (this_pro_name) (Int32.add
                                             (Bcs.int_of_sym game "PROJECTL" this_pro_name)
                                             (Int32.of_int 1)) ;
            log_and_print "\n\nPRO [%s] already present! Skipping!\n\n"
              this_pro_name
          end else begin
            log_and_print "Adding projectile file %s ...\n" p.pro_file;
            let this_missile_name = Var.get_string p.missile_ids_name in
            let a1 = TP_Include [".../WEIDU_NAMESPACE/lc_fix_missile_ids.tpa"] in
            List.iter (process_action tp) [a1];
            let this_pro_number = get_next_line_number "PROJECTL.IDS" in
            let a1 =
              TP_Append ("PROJECTL.IDS",
                         (Printf.sprintf "%d %s"
                            this_pro_number this_pro_name),[],true,false,0) in
            let dest_pro_file = "override/" ^ (Case_ins.filename_basename p.pro_file) in
            let a2 = TP_Copy(
              {copy_get_existing = false;
                copy_use_regexp = false;
                copy_use_glob = false;
                copy_file_list = [(p.pro_file, dest_pro_file)] ;
                copy_patch_list = [] ;
                copy_constraint_list = [] ;
                copy_backup = true;
                copy_at_end = false;
                copy_save_inlined = false;
              }) in
            process_action tp a1;
            if Load.file_exists_in_game game "missile.ids" then begin
              let this_miss_number = get_next_line_number "MISSILE.IDS" in
              let a1a =
                TP_Append("MISSILE.IDS",
                          (Printf.sprintf "%d %s"
                             this_miss_number this_missile_name),[],true,false,0) in
              process_action tp a1a;
            end;
            process_action tp a2;
            Var.set_int32 (this_pro_name) (Int32.of_int (this_pro_number + 1)) ;
            log_and_print "Added projectile file %s\n" p.pro_file;
          end
      end

      | TP_Add_Spell(file,kind,level,ids_name,pl,ple,pld) ->
          log_and_print "Adding spell %s\n" ids_name;
          let file = Var.get_string file in
          let ids_name = Var.get_string ids_name in
          process_action tp (TP_Include [".../WEIDU_NAMESPACE/add_spell.tpa"]);
          let use_ple = if_true (match ple with
          | None -> false
          | Some x ->
              process_action tp (TP_Define_Patch_Macro ("TB#ADD_SPELL_PLE", [], x));
              true) in
          let use_pld = if_true (match pld with
          | None -> false
          | Some x ->
              process_action tp (TP_Define_Patch_Macro ("TB#ADD_SPELL_PLD", [], x));
              true) in
          process_action tp (TP_Define_Patch_Macro ("TB#ADD_SPELL_PL", [], pl));
          Var.set_int32 "tb#type" (eval_pe "" game kind);
          Var.set_int32 "tb#level" (eval_pe "" game level);
          Var.set_int32 "tb#use_pld" (eval_pe "" game (PE_Int32 use_pld));
          Var.set_int32 "tb#use_ple" (eval_pe "" game (PE_Int32 use_ple));
          Var.set_string "tb#identifier" (eval_pe_str (PE_LiteralString ids_name));
          Var.set_string "tb#source_file" (eval_pe_str (PE_LiteralString file));
          process_action tp (TP_Launch_Action_Macro("TB#ADD_SPELL"));
          Var.set_string ids_name (Var.get_string_exact "%tb#newcode%");

          log_and_print "Added spell %s\n" ids_name;


      | TP_CopyKit(oldString,newString,patches_list) ->
          let oldString = Var.get_string oldString in
          let newString = Var.get_string newString in
          log_and_print "Preparing to copy kit %s to %s\n" oldString newString ;
          let patches_list = List.map (fun(a,b)->
            (Var.get_string a),(Var.get_string
                                  b)) patches_list in
          let get_line file =
            let (a,b) = split file in
            let buff,path = Load.load_resource "getting 2DA columnns" game true a b in
            let my_regexp = Str.regexp_case_fold
                (Printf.sprintf "%s%s[\t ].*$"
                   (if file = "kitlist.2da" then " " else "^") oldString) in
            ignore(Str.search_forward my_regexp buff 0);
            Str.matched_string buff
          in
          let get_column file =
            let (a,b) = split file in
            let buff,path = Load.load_resource "getting 2DA columnns" game true a b in
            let lines = Str.split many_newline_or_cr_regexp buff in
            let cells = List.map (Str.split many_whitespace_regexp) lines in
            let headers = List.nth cells 2 in
            let rec get_where lst cnt = match lst with
            | hd :: tl -> if String.uppercase hd = String.uppercase oldString then cnt else get_where tl (cnt + 1)
            | [] -> failwith (Printf.sprintf "Unknown kit: %s" oldString)
            in
            let column = get_where headers 1 in
            List.fold_left (fun acc elt ->
              Printf.sprintf "%s%s " acc (List.nth elt
                                            column))
              (if file = "weapprof.2da" then oldString ^ " " else "")
              (List.tl (List.tl (List.tl cells)))
          in
          let get_entry file which =
            let line = get_line file in
            let split = Str.split many_whitespace_regexp line in
            List.nth split which
          in
          let fix_clab which =
            let a = TP_Copy(
              {copy_get_existing = true ;
                copy_use_regexp = false;
                copy_use_glob = false;
                copy_file_list = [(which ^ ".2da","override")] ;
                copy_patch_list = [] ;
                copy_constraint_list = [] ;
                copy_backup = true ;
                copy_at_end = false;
                copy_save_inlined = false;
              }) in
            process_action tp a;
            "override/" ^ which ^ ".2da"
          in
          let patches = Hashtbl.create 5 in
          List.iter (fun (a,b) -> Hashtbl.add patches (String.lowercase a) b)
            patches_list;
          let get_clasweap file =
            try get_line file
            with _ -> oldString ^ " 1 1 1 1 1 1 1 1"
          in
          let get_it file how =
            let o =
              if !debug_ocaml then log_and_print "%s ... " file;
              if Hashtbl.mem patches file then Hashtbl.find patches file else
              match how with
              | "line" -> get_line (file ^ ".2da")
              | "column" -> get_column (file ^ ".2da")
              | "clasweap" -> get_clasweap (file ^ ".2da")
              | "clab" -> fix_clab (get_entry "kitlist.2da" 4)
              | "lower" -> get_entry "kitlist.2da" 1
              | "mixed" -> get_entry "kitlist.2da" 2
              | "help" -> get_entry "kitlist.2da" 3
              | "luabbr" -> get_entry "luabbr.2da" 1
              | "unused" -> (get_entry "kitlist.2da" 6) ^ " " ^
                  (get_entry "kitlist.2da" 7)
              | _ -> failwith (Printf.sprintf "CopyKit: %s" how)
            in
            if !debug_ocaml then log_and_print "~%s~\n" o;
            Str.global_replace (Str.regexp ("^" ^ oldString^"[ \t]"))
              (newString ^ " ") o
          in
          if not (is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString "kitlist.2da",
                            PE_LiteralString
                              ("[ %TAB%%LNL%%WNL%]" ^ oldString ^ "[ %TAB%%LNL%%WNL%]")))))
          then
            failwith (Printf.sprintf "ERROR: Could not find kit %s" oldString) ;
          let copy_kit = {
            kit_name = newString;
            clasweap = get_it "clasweap" "clasweap";
            weapprof = get_it "weapprof" "column";
            abclasrq = get_it "abclasrq" "line";
            abclsmod = get_it "abclsmod" "line";
            abdcdsrq = get_it "abdcdsrq" "line";
            abdcscrq = get_it "abdcscrq" "line";
            dualclas = get_it "dualclas" "line";
            alignmnt = get_it "alignmnt" "line";
            ability_file = get_it "clab" "clab";
            include_in = "";
            lower = Dlg.TLK_Index (int_of_string (get_it "lower" "lower"));
            mixed = Dlg.TLK_Index (int_of_string (get_it "mixed" "mixed"));
            help        = Dlg.TLK_Index (int_of_string (get_it "help"  "help"));
            tob_abbr = get_it "luabbr" "luabbr";
            tob_start = begin
              let str = get_it "25stweap" "column" in
              let lst = Str.split (Str.regexp "[ \t]+") str in
              List.map (fun elt -> if elt = "$" then "" else elt) lst
            end;
            unused_class = get_it "unusabilities" "unused";

            (*                          unused_class : string ;
              tob_start : string list ;*)
          } in
          process_action tp (TP_Add_Kit(copy_kit))

      | TP_Add_Kit(k) -> begin
          let k = { k with
                    kit_name = (Var.get_string k.kit_name) ;
                  } in
          log_and_print "Adding %s Kit ...\n" k.kit_name ;

          if is_true (eval_pe "" game
                        (PE_ResourceContains
                           (PE_LiteralString "kitlist.2da",
                            PE_LiteralString
                              ("[ %TAB%%LNL%%WNL%]" ^ k.kit_name ^ "[ %TAB%%LNL%%WNL%]"))))
          then begin
            Var.set_int32 (k.kit_name) (Int32.sub (Bcs.int_of_sym game "KIT" k.kit_name) 0x4000l) ;
            log_and_print "\n\nKit [%s] already present! Skipping!\n\n"
              k.kit_name
          end else begin
            let a1 = TP_Append("CLASWEAP.2DA",k.clasweap,[],true,false,0) in
            let a2 = TP_Append_Col("WEAPPROF.2DA",k.weapprof,Tp.get_pe_int "2",[],true,0) in
            let a3 = TP_Append("ABCLASRQ.2DA",k.abclasrq,[],true,false,0) in
            let a4 = TP_Append("ABDCDSRQ.2DA",k.abdcdsrq,[],true,false,0) in
            let a5 = TP_Append("ABDCSCRQ.2DA",k.abdcscrq,[],true,false,0) in
            let a_e1 = TP_Append("ABCLSMOD.2DA",k.abclsmod,[],true,false,0) in
            let a_e2 = TP_Append("DUALCLAS.2DA",k.dualclas,[],true,false,0) in
            let a6 = TP_Append("ALIGNMNT.2DA",k.alignmnt,[],true,false,0) in
            let abil_file = String.uppercase (Case_ins.filename_basename k.ability_file) in
            if !debug_ocaml then log_and_print "%s\n" abil_file;
            let abil_file_no_ext = Case_ins.filename_chop_extension abil_file in
            let dest_abil_file = "override/" ^ abil_file in
            let a7 = TP_Copy(
              {copy_get_existing = false ;
                copy_use_regexp = false;
                copy_use_glob = false;
                copy_file_list = [(k.ability_file,dest_abil_file)] ;
                copy_patch_list = [] ;
                copy_constraint_list = [] ;
                copy_backup = true ;
                copy_at_end = false;
                copy_save_inlined = false;
              }) in
            let include_list = split_apart k.include_in in
            let lower_index = match Dc.resolve_tlk_string game k.lower with
              Dlg.TLK_Index(i) -> i
            | _ -> log_and_print "ERROR: cannot resolve KIT lower string\n" ;
                failwith "resolve"
            in
            let mixed_index = match Dc.resolve_tlk_string game k.mixed with
              Dlg.TLK_Index(i) -> i
            | _ -> log_and_print "ERROR: cannot resolve KIT mixed string\n" ;
                failwith "resolve" in
            let help_index = match Dc.resolve_tlk_string game k.help with
              Dlg.TLK_Index(i) -> i
            | _ -> log_and_print "ERROR: cannot resolve KIT help string\n" ;
                failwith "resolve" in
            let this_kit_number = get_next_line_number "KITLIST.2DA" in
            if this_kit_number > 0x500 then begin
              failwith ("The game cannot currently support more than 1280 kits.\n" ^
                        "Ask Ascension64 to further increase the limit in ToBEx");
            end;
            if this_kit_number > 0x100 then begin
              if not (check_enhanced_engine game (None) (Some 20) true false) then begin
                failwith "The game requires ToBEx or GemRB to support more than 256 kits."
              end
            end;
            let this_kit_prof_number = get_next_col_number "WEAPPROF.2DA" in
            let append_to_kitlist = Printf.sprintf
                "%d  %s %d %d %d %s %d %s"
                this_kit_number k.kit_name
                lower_index mixed_index help_index
                abil_file_no_ext this_kit_prof_number
                k.unused_class in
            let a8 = TP_Append("KITLIST.2DA",append_to_kitlist,[],true,false,0) in
            let include_actions = List.map (fun file ->
              let num = get_next_line_number (file ^ ".2DA" ) in
              let str = Printf.sprintf "%d      %d" num this_kit_number in
              TP_Append(file ^ ".2DA",str,[],true,false,0)
                ) include_list in
            let abbr = Printf.sprintf  "%s               %s" k.kit_name k.tob_abbr in
            let a9 = TP_Append("LUABBR.2DA",abbr,[],true,false,0) in
            let a10 = TP_Set_Col("25STWEAP.2DA",
                                 ("" :: "" :: k.kit_name ::
                                  (List.map Var.get_string k.tob_start)),
                                 this_kit_prof_number+1) in
            let a11 = TP_Append("KIT.IDS",
                                (Printf.sprintf "0x%x %s" (0x4000 + this_kit_number)
                                   k.kit_name),[],true,false,0) in
            let fix2da1 =
              TP_Copy ({copy_get_existing = true;
                         copy_use_regexp = false;
                         copy_use_glob = false;
                         copy_file_list = ["weapprof.2da", "override";
                                            "25stweap.2da", "override";] ;
                         copy_patch_list =
                         [TP_PatchIf
                             (PE_GT
                                (get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
                              TP_Read2DA
                                (get_pe_int "2", get_pe_int "0", get_pe_int "0",get_pe_string "tb#kit_temp2");
                              TP_Patch2DA
                                (get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_int "%tb#kit_temp2% %tb#kit_temp2%");
                              TP_PatchSet
                                (get_pe_string "tb#kit_this_is_a_temp_var",get_pe_int "0");
                              TP_Patch2DANow
                                ("tb#kit_this_is_a_temp_var", get_pe_int "0");],[])] ;
                         copy_constraint_list = [] ;
                         copy_backup = true;
                         copy_at_end = false;
                         copy_save_inlined = false;
                       }) in
            let fix2da1a =
              TP_Copy ({copy_get_existing = true;
                         copy_use_regexp = false;
                         copy_use_glob = false;
                         copy_file_list = ["kitlist.2da", "override"] ;
                         copy_patch_list =
                         [TP_PatchIf
                             (PE_GT
                                (get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
                              TP_Read2DA
                                (get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_string "tb#kit_temp2a");
                              TP_Patch2DA
                                (get_pe_int "2", get_pe_int "0", get_pe_int "0", get_pe_int "%tb#kit_temp2a% %tb#kit_temp2a%");
                              TP_PatchSet
                                (get_pe_string "tb#kit_this_is_a_temp_var",get_pe_int "0");
                              TP_Patch2DANow
                                ("tb#kit_this_is_a_temp_var", get_pe_int "0");],[])] ;
                         copy_constraint_list = [] ;
                         copy_backup = true;
                         copy_at_end = false;
                         copy_save_inlined = false;
                       }) in
            let action_list = a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 ::
              a9 :: a10 :: a11 :: a_e1 :: a_e2 :: fix2da1 :: fix2da1a :: include_actions in
            let old_allow_missing = !Load.allow_missing in
            Load.allow_missing :=
              "LUABBR.2DA" :: "25STWEAP.2DA" :: old_allow_missing ;
            (* actually do it! *)
            List.iter (process_action tp) action_list ;
            let fix2da2 =
              TP_Copy ({copy_get_existing = true;
                         copy_use_regexp = false;
                         copy_use_glob = false;
                         copy_file_list = ["weapprof.2da", "override";
                                            "25stweap.2da", "override";] ;
                         copy_patch_list =
                         [TP_PatchIf
                             (PE_GT
                                (get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
                              TP_PatchStringTextually
                                (None,None,"^%tb#kit_temp2%",
                                 String.make
                                   (String.length
                                      (Var.get_string_exact "%tb#kit_temp2%")) ' ',None);],[])] ;
                         copy_constraint_list = [] ;
                         copy_backup = true;
                         copy_at_end = false;
                         copy_save_inlined = false;
                       }) in
            let fix2da2a =
              TP_Copy ({copy_get_existing = true;
                         copy_use_regexp = false;
                         copy_use_glob = false;
                         copy_file_list = ["kitlist.2da", "override"] ;
                         copy_patch_list =
                         [ TP_PatchIf
                             (PE_GT
                                (get_pe_int "%SOURCE_SIZE%", get_pe_int "0"),[
                              TP_PatchStringTextually
                                (None,None,"^%tb#kit_temp2a%",
                                 String.make
                                   (String.length
                                      (Var.get_string_exact "%tb#kit_temp2a%")) ' ',None);],[])] ;
                         copy_constraint_list = [] ;
                         copy_backup = true;
                         copy_at_end = false;
                         copy_save_inlined = false;
                       }) in
            process_action tp fix2da2;
            process_action tp fix2da2a;
            Load.allow_missing := old_allow_missing ;
            Var.set_int32 (k.kit_name) (Int32.of_int this_kit_number) ;
            log_and_print "Added %s Kit\n" k.kit_name ;
            Bcs.clear_ids_map game ;
          end
      end

      | TP_String_Set_Range(min,max,tra_file) ->
          begin
            Dc.push_copy_trans_modder ();
            try
              let tra_file = Var.get_string tra_file in
              handle_tra_filename (Arch.backslash_to_slash tra_file) ;
              let min = Int32.to_int (eval_pe "" game min) in
              let max = Int32.to_int (eval_pe "" game max) in
              for i = min to max do
                Dc.set_string game i (Dlg.Trans_String(Dlg.Int i)) false
              done ;
              Dc.pop_trans ()
            with e ->
              (Dc.pop_trans (); raise e)
          end

      | TP_String_Set(lst,tra_file_opt) -> begin
          Dc.push_copy_trans_modder () ;
          (try
            (match tra_file_opt with
            | None -> ()
            | Some(tra_file) ->
                begin
                  let tra_file = Var.get_string tra_file in
                  handle_tra_filename (Arch.backslash_to_slash tra_file)
                end) ;
            List.iter (fun (s1,str) ->
              let i = (try [int_of_string s1]
              with _ -> begin
                try
                  Hashtbl.find_all game.Load.dialog_search s1
                with _ ->
                  log_and_print "ERROR: Cannot find string [%s]\n" s1 ;
                  failwith "ERROR: STRING_SET"
              end) in
              List.iter (fun i -> Dc.set_string game i str false) i) lst ;
            Dc.pop_trans () ;
          with e -> Dc.pop_trans () ; raise e)
      end

      | TP_String_Set_Evaluate(lst,tra_file_opt) ->
          (* eval pe to string and wrap around TP_String_Set *)
          let eval_lst = List.map (fun (pe,str) ->
            let eval_s = match pe with
            | PE_String(s) ->
                begin try
                  Int32.to_string (eval_pe "" game pe)
                with _ ->
                  Var.get_string (eval_pe_str s)
                end
            | _ -> Int32.to_string (eval_pe "" game pe) in
            (eval_s,str)) lst in
          process_action tp (TP_String_Set(eval_lst,tra_file_opt))

      | TP_Alter_TLK (pl) ->
          let b = get_pe_int "0" in
          let e = get_pe_int (string_of_int
                                (Array.length (Load.get_active_dialog game) +
                                   (Queue.length !Dc.strings_to_add - 1))) in
          process_action tp (TP_Alter_TLK_Range(b,e,pl))

      | TP_Alter_TLK_Range (b,e,pl) ->
          let b = Int32.to_int (eval_pe "" game b) in
          let e = Int32.to_int (eval_pe "" game e) in
          let rec get i= match i with
          | i when i = e -> [i]
          | _ -> i :: (get (i+1))
          in
          process_action tp (TP_Alter_TLK_List
                               (List.map (fun i ->
                                 get_pe_int (string_of_int i)) (get b),pl))

      | TP_Alter_TLK_List(lst,pl) -> begin
          List.iter (fun x ->
            let i = Int32.to_int (eval_pe "" game x) in
            let male = if (i < Array.length (Load.get_active_dialog game)) then
              (Load.get_active_dialog game).(i)
            else
              fst (Tlk.lse_to_tlk_string (Hashtbl.find Dc.strings_added_ht i)) in
            let newmale = List.fold_left (fun acc elt ->
              process_patch2 "dialog.tlk" game acc elt) male.Tlk.text pl in
            let soundmale = male.Tlk.sound_name in
            let newfemale, soundfemale = match Load.get_active_dialogf_opt game with
              Some dialogf ->
                let female = if (i < Array.length (Load.get_active_dialog game)) then
                  dialogf.(i)
                else
                  snd (Tlk.lse_to_tlk_string (Hashtbl.find Dc.strings_added_ht i))
                in
                (List.fold_left (fun acc elt ->
                  process_patch2 "dialog.tlk" game acc elt)
                   female.Tlk.text pl, female.Tlk.sound_name)
            | None -> newmale, soundmale in
            Dc.set_string game i (Dlg.Local_String {
                                  lse_male = newmale;
                                  lse_male_sound = soundmale;
                                  lse_female = newfemale;
                                  lse_female_sound = soundfemale;
                                }) false;)
            lst
      end

      | TP_Load_Tra(str_l) -> begin
          let str_l = List.map (fun x -> Arch.backslash_to_slash (Var.get_string x)) str_l in
          let numtra = List.length str_l in
          log_and_print "loading %d tra file%s\n" numtra
            (if numtra = 1 then "" else "s") ;
          List.iter (fun str ->
            handle_tra_filename (Var.get_string str)) str_l
      end

      | TP_GetFileArray(toArr,path,pattern,doDirectories) -> begin
          let path = Var.get_string (eval_pe_str path) in
          let dh = Case_ins.unix_opendir path in
          let pattern = Var.get_string (eval_pe_str pattern) in
          let pattern = Str.regexp_case_fold (match pattern with
          | "" -> "^\\(\\|[^.]\\|\\.\\...*\\|[^.]..*\\|\\.[^.].*\\)$"
                (* the above means "everything, save `.' and `..'" *)
          | _ -> pattern) in
          let my_type = match doDirectories with
          | true -> Unix.S_DIR
          | false -> Unix.S_REG in
          let i = ref 0 in
          try
            while true do
              let filep = (Unix.readdir dh) in
              if        Str.string_match pattern filep 0 then begin
                let file = path ^ "/" ^ filep in
                let stats = Case_ins.unix_stat file in
                if stats.Unix.st_kind = my_type then begin
                  Var.set_string
                    (eval_pe_str
                       (PE_Dollars(toArr,[get_pe_string(string_of_int !i)],
                                   false,true))) file;
                  incr i
                end
              end
            done;
          with End_of_file -> Unix.closedir dh
      end

      | TP_Mkdir(str_l) -> begin
          let str_l = List.map (fun x -> Arch.backslash_to_slash x) str_l in
          let numdir = List.length str_l in
          log_and_print "Creating %d director%s\n" numdir
            (if numdir = 1 then "y" else "ies") ;
          List.iter (fun str ->
            try
              recursive_mkdir (Var.get_string str) 511 (* 511 = octal 0777 = a+rwx *)
            with e ->
              (log_and_print "Problem %s on %s : tp.ml\n"
                 (printexc_to_string e) str)) str_l
      end

      | TP_Compile(eval,dlg_l,pl,tra_l) -> begin
          let dlg_l =  List.map (fun x -> Var.get_string x) dlg_l in
          let tra_l =  List.map (fun x -> Var.get_string x) tra_l in
          let dlg_l =  List.map (fun x -> Arch.backslash_to_slash x) dlg_l in
          let tra_l =  List.map (fun x -> Arch.backslash_to_slash x) tra_l in
          let numd = ref 0 in
          let nums = ref 0 in
          let handle_one_d_file filespec = match split
              (String.uppercase filespec) with
          | _,"BAF" -> incr nums
          | _,"D" -> incr numd
          | _,_ -> ()
          in
          List.iter (fun filespec ->
            if is_directory filespec then begin
              let dh = Case_ins.unix_opendir filespec in
              (try
                while true do
                  let base = Unix.readdir dh in
                  let source = filespec ^ "/" ^ base in
                  if not (is_directory source) then
                    handle_one_d_file source
                done
              with _ -> ()) ;
              Unix.closedir dh
            end else handle_one_d_file filespec) dlg_l ;
          log_and_print "Compiling " ;
          (if (!numd > 0) then begin
            log_and_print "%d dialogue file%s " !numd
              (if (!numd > 1) then "s" else "") ;
            if (!nums > 0) then
              log_and_print "and "
          end) ;
          (if (!nums > 0) then begin
            log_and_print "%d script%s " !nums
              (if (!nums > 1) then "s" else "") ;
          end) ;
          log_and_print "...\n" ;

          Dc.push_copy_trans_modder ();
          Dc.notChanged := true;

          resolve_tra_paths_and_load !our_lang tra_l;

          let handle_one_d_file d =
            (* handle AUTO_TRA "solarom/%s" *)
            begin try
              Dc.push_copy_trans () ;
              List.iter (fun f ->
                match f,!our_lang with
                  Auto_Tra(path),Some(l) ->
                    let my_regexp = Str.regexp_string "%s" in
                    let tra_file_dir = Str.global_replace
                        my_regexp l.lang_dir_name path in
                    let d_base,_ = split (Case_ins.filename_basename d) in
                    let tra_file = tra_file_dir ^ "/" ^ d_base ^ ".TRA" in
                    handle_tra_filename tra_file ;
                | Auto_Tra(path),None ->
                    let d_base,_ = split (Case_ins.filename_basename d) in
                    let tra_file = path ^ "/" ^ d_base ^ ".TRA" in
                    handle_tra_filename tra_file
                | _ -> ()) tp.flags ;
              if !Dc.notChanged then
                Modder.handle_msg "SETUP_TRA"
                  (Printf.sprintf "WARNING: COMPILE %s with strings from setup.tra\n" d);

              Dc.ok_to_resolve_strings_while_loading := Some(game) ;
              let newd = if eval then "tb#_compile_eval_buffer/" ^ d else d in
              if eval then begin
                let contents = Var.get_string (load_file d) in
                log_only_modder "Defined Inlined File [%s] (length %d)\n"
                  newd (String.length contents) ;
                Hashtbl.add inlined_files (Arch.backslash_to_slash newd) contents
              end;
              let newd1 = if pl = [] then newd else "tb#compile_patches/" ^ newd in
              if pl <> [] then begin
                let contents = load_file newd in
                let contents = List.fold_left (fun acc elt ->
                  process_patch2 d game acc elt) contents pl in
                log_only_modder "Defined Inlined File [%s] (length %d)\n"
                  newd1 (String.length contents) ;
                Hashtbl.add inlined_files (Arch.backslash_to_slash newd1) contents
              end;
              (if Modder.enabled "MISSING_EVAL" then
                check_missing_eval ("COMPILE " ^ d) (load_file newd1);
               match split (String.uppercase (Case_ins.filename_basename d)) with
               | _,"BAF" -> compile_baf_filename game newd1
               | _,"D" -> handle_d_filename newd1
               | _,_ -> ())
            with e ->
              Dc.pop_trans();
              if !debug_ocaml then log_and_print "Dc.pop_trans 5253\n";
              Dc.ok_to_resolve_strings_while_loading := None ;
              log_and_print "ERROR: compiling [%s]!\n" d ;
              raise e
            end;
            Dc.pop_trans();
            if !debug_ocaml then log_and_print "Dc.pop_trans 5253\n";
            Dc.ok_to_resolve_strings_while_loading := None ;
          in
          try
            List.iter (fun filespec ->
              if is_directory filespec then begin
                let dh = Case_ins.unix_opendir filespec in
                (try
                  while true do
                    let base = Unix.readdir dh in
                    let source = filespec ^ "/" ^ base in
                    if not (is_directory source) then
                      handle_one_d_file source
                  done
                with End_of_file -> ()) ;
                Unix.closedir dh ;
              end else handle_one_d_file filespec) dlg_l ;
            log_or_print "Processing %d dialogues/scripts ...\n"
              (List.length dlg_l) ;
            emit_dlg_files game "override" ;
            Dc.pop_trans () ;
          with e -> Dc.pop_trans ();
            raise e
      end

      | TP_Set_Col(file,new_col_list,col_num) ->
          log_and_print_modder "Setting game text column-wise ...\n" ;
          let eight,three = split (String.uppercase file) in
          let buff,loaded_path = Load.load_resource "SET_COLUMN" game true eight three in
          if buff = "" then
            log_or_print "[%s]: empty or does not exist\n" file
          else begin
            let dest = "override/" ^ file in
            let buff_as_lines = Str.split many_newline_or_cr_regexp buff in
            if List.length buff_as_lines <> List.length new_col_list then begin
              log_and_print "Cannot set column-wise because there are %d lines in %s but I was only given %d things to append\n"
                (List.length buff_as_lines) file (List.length new_col_list)    ;
              failwith ("cannot set column-wise to " ^ file)
            end ;
            Stats.time "saving files" (fun () ->
              let out = open_for_writing dest true in
              let linecount = ref 0 in
              List.iter2 (fun orig_line new_col ->
                let line_as_cols = Str.split many_whitespace_regexp orig_line in
                let i = ref 0 in
                if !linecount = 2 then incr i; (* 2nd coloumn (counting from 0) has one less entry *)
                incr linecount;
                List.iter (fun orig_col ->
                  (if !i = col_num then
                    Printf.fprintf out "%-20s%-20s" new_col orig_col
                  else
                    Printf.fprintf out "%-20s" orig_col) ;
                  incr i;) line_as_cols ;
                (if (!i <= col_num) then
                  Printf.fprintf out "%-20s" new_col) ;
                output_string out "\r\n") buff_as_lines new_col_list ;
              close_out out ;
              begin (* handle read-only files! *)
                try
                  Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
                with e -> ()
                    (* log_or_print "WARNING: chmod %s : %s\n" filename
                       (printexc_to_string e) *)
              end ;) () ;
            log_or_print "Set text in [%s] column-wise\n" file
          end

      | TP_Append_Col(file,src,count_prepend,con_l,frombif,do_backup) ->
          let file = Arch.backslash_to_slash (Var.get_string file) in
          if when_exists file con_l frombif game then begin (* Ugly-code blues *)
            let temp = Var.get_string src in
            let src_list = Str.split (Str.regexp "[ \t]+") temp in
            let src_list = List.map (fun elt -> if elt = "$" then "" else elt) src_list in
            let count_prepend = Int32.to_int (eval_pe "" game count_prepend) in
            let rec prepend i string_list =
              if i = 1 then
                ("" :: string_list)
              else begin
                if i > 1 then
                  "" :: (prepend (i-1) string_list)
                else      (* i <= 0 *)
                  string_list
              end
            in

            let src_list = prepend count_prepend src_list in
            log_and_print "Appending to files column-wise ...\n" ;
            let buff = if frombif then
              let eight,three = split (String.uppercase file) in
              let buff,loaded_path =
                Load.load_resource "APPEND_COLUMN" game true eight three in
              buff
            else
              load_file file
            in
            if buff = "" then
              log_or_print "[%s]: empty or does not exist\n" file
            else begin
              let okay = List.fold_left (fun acc elt -> acc &&
                match elt with
                  TP_Contains(s) -> begin
                    let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
                    try let _ = Str.search_forward my_regexp buff 0 in
                    log_only "Appending cols to [%s] because it DOES contain [%s]\n"
                      file (Var.get_string (eval_pe_str s))  ;
                    true
                    with _ ->
                      log_only "Not appending cols to [%s] because it does NOT contain [%s]\n"
                        file (Var.get_string (eval_pe_str s)) ;
                      false
                  end
                | TP_NotContains(s) -> begin
                    let my_regexp = Str.regexp_case_fold (Var.get_string (eval_pe_str s)) in
                    try let _ = Str.search_forward my_regexp buff 0 in
                    log_only "Not appending cols to [%s] because it DOES contains [%s]\n"
                      file (Var.get_string (eval_pe_str s)) ;
                    false
                    with _ ->
                      log_only "Appending cols to [%s] because it does NOT contain [%s]\n"
                        file (Var.get_string (eval_pe_str s)) ;
                      true
                end
                | TP_IfSizeIs(size) -> String.length buff = size
                | TP_ButOnlyIfItChanges
                | TP_IfExists -> true
                | TP_Eval(pe) ->
                    let v = eval_pe buff game pe in
                    if v = Int32.zero then begin
                      log_only "Not appending cols to [%s] because condition evaluates to %ld\n"
                        file v ; false
                    end else begin
                      log_only "Appending cols to [%s] because condition evaluates to %ld\n"
                        file v ; true
                    end) true con_l in
              if okay then begin (* do the append *)
                let dest = if frombif then
                  "override/" ^ file
                else
                  file in
                let buff_as_lines = Str.split many_newline_or_cr_regexp buff in
                if List.length buff_as_lines <> List.length src_list then begin
                  log_and_print "Cannot append column-wise because there are %d lines in %s but I was only given %d things to append\n"
                    (List.length buff_as_lines) file (List.length src_list)  ;
                  failwith ("cannot append column-wise to " ^ file)
                end ;
                Stats.time "saving files" (fun () ->
                  if do_backup = 2 then begin
                    let out_buff = Buffer.create 1000 in
                    List.iter2 (fun orig app ->
                      Buffer.add_string out_buff orig ;
                      Buffer.add_string out_buff " " ;
                      Buffer.add_string out_buff app ;
                      Buffer.add_string out_buff "\r\n") buff_as_lines src_list ;
                    let result_buff = Buffer.contents out_buff in
                    log_only_modder "Defined Inlined File [%s] (length %d)\n"
                      dest (String.length result_buff) ;
                    Hashtbl.add inlined_files (Arch.backslash_to_slash dest) result_buff
                  end else begin
                    let out = open_for_writing_internal (do_backup = 0) dest true in
                    List.iter2 (fun orig app ->
                      output_string out orig ;
                      output_string out " " ;
                      output_string out app ;
                      output_string out "\r\n") buff_as_lines src_list ;
                    close_out out ;
                    begin (* handle read-only files! *)
                      try
                        Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
                      with e -> ()
                          (* log_or_print "WARNING: chmod %s : %s\n" filename
                             (printexc_to_string e) *)
                    end
                  end) () ;
                log_or_print "Appended text to [%s] column-wise\n" file
              end
            end
          end else log_or_print "Not appending to [%s] because the file does not exist\n" file

      | TP_Append(file,src,con_l,frombif,keep_crlf,do_backup) ->
          let file = Arch.backslash_to_slash (Var.get_string file) in
          if when_exists file con_l frombif game then begin
            if Case_ins.filename_check_suffix(String.lowercase file) "ids" then
              Bcs.clear_ids_map game ;
            log_and_print "Appending to files ...\n" ;
            let src = Var.get_string src in
            let buff = if frombif then begin
              let eight,three = split (String.uppercase file) in
              let the_buff,loaded_path =
                Load.load_resource "APPEND" game true eight three in
              the_buff
            end else begin
              load_file file
            end in
            let okay = List.fold_left (fun acc elt -> acc &&
              match elt with
                TP_Contains(s) -> begin
                  let my_regexp = Str.regexp_case_fold
                      (Var.get_string (eval_pe_str s)) in
                  try let _ = Str.search_forward my_regexp buff 0 in
                  log_only "Appending [%.10s...] to [%s] because it DOES contain [%s]\n"
                    src file (Var.get_string (eval_pe_str s))  ;
                  true
                  with _ ->
                    log_only "Not appending [%.10s...] to [%s] because it does NOT contain [%s]\n"
                      src file (Var.get_string  (eval_pe_str s)) ;
                    false
                end
              | TP_NotContains(s) -> begin
                  let my_regexp = Str.regexp_case_fold
                      (Var.get_string (eval_pe_str s)) in
                  try let _ = Str.search_forward my_regexp buff 0 in
                  log_only "Not appending [%.10s...] to [%s] because it DOES contains [%s]\n"
                    src file (Var.get_string (eval_pe_str s)) ;
                  false
                  with _ ->
                    log_only "Appending [%.10s...] to [%s] because it does NOT contain [%s]\n"
                      src file (Var.get_string (eval_pe_str s)) ;
                    true
              end
              | TP_IfSizeIs(size) -> String.length buff = size
              | TP_ButOnlyIfItChanges
              | TP_IfExists -> true
              | TP_Eval(pe) ->
                  let v = eval_pe buff game pe in
                  if v = Int32.zero then begin
                    log_only "Not appending [%.10s] to [%s] because condition evaluates to %ld\n"
                      src file v ; false
                  end else begin
                    log_only "Appending [%.10s] to [%s] because condition evaluates to %ld\n"
                      src file v ; true
                  end) true con_l in
            if okay then begin (* do the append *)
              let dest =
                if frombif then "override/" ^ file
                else file
              in
              Stats.time "saving files" (fun () ->
                let save_to fn =
                  if keep_crlf then begin
                    fn buff;
                    if !debug_ocaml then log_and_print "%s\n" buff;
                    if String.length buff < 2 || Str.last_chars buff 2 <> "\r\n" then
                      fn "\r\n";
                    let src = if String.lowercase dest = "quests.ini" &&
                      Arch.view_command = "start" then
                      (Str.global_replace (Str.regexp "\\([^\r]\\)\n") "\\1\r\n" src)
                    else src in
                    fn src;
                    if !debug_ocaml then log_and_print "%s\n" src;
                    if Str.last_chars src 2 <> "\r\n" then fn "\r\n";
                  end else begin
                    let nice_newlines = Str.global_replace
                        many_newline_or_cr_regexp "\r\n" (buff ^ "\r\n") in
                    fn nice_newlines ;
                    fn src ;
                    fn "\r\n" ;
                  end;
                in
                if do_backup = 2 then begin
                  let out_buff = Buffer.create 1000 in
                  save_to (Buffer.add_string out_buff);
                  let result_buff = Buffer.contents out_buff in
                  log_only_modder "Defined Inlined File [%s] (length %d)\n"
                    dest (String.length result_buff) ;
                  Hashtbl.add inlined_files (Arch.backslash_to_slash dest) result_buff
                end else begin
                  let out = open_for_writing_internal (do_backup = 0) dest true in
                  save_to (output_string out);
                  close_out out ;
                  begin (* handle read-only files! *)
                    try
                      Case_ins.unix_chmod dest 511 ; (* 511 = octal 0777 = a+rwx *)
                    with e -> ()
                        (* log_or_print "WARNING: chmod %s : %s\n" filename
                           (printexc_to_string e) *)
                  end ;
                end;) () ;
              log_or_print "Appended text to [%s]\n" file
            end
          end else log_or_print "Not appending to [%s] because the file does not exist\n" file

      | TP_Extend_Top(use_reg,dest,src,pl,tra_l)
      | TP_Extend_Bottom(use_reg,dest,src,pl,tra_l) -> begin
        let dest = if not use_reg then Arch.backslash_to_slash dest else dest in
        let tra_l = List.map (fun x -> Arch.backslash_to_slash x) tra_l in
        log_and_print "Extending game scripts ...\n" ;
        let dest = (Var.get_string dest) in
        let src = (Var.get_string src) in
        let src = Arch.backslash_to_slash src in
        let dlist =
          if use_reg = false then
            [dest]
          else begin
            let files_in_chitin = Key.list_of_key_resources game.Load.key true in
            let regexp = Str.regexp_case_fold dest in
            let matches = ref [] in
            List.iter (fun possible ->
              if Str.string_match regexp possible 0 then begin
                matches := (possible) :: !matches
              end) files_in_chitin ;
            if (!matches = []) then
              [dest]
            else
              !matches
          end
        in
        Dc.push_copy_trans_modder () ;
        (try
          Dc.notChanged := true ;
          (* handle AUTO_TRA "solarom/%s" *)
          List.iter (fun f ->
            (match f,!our_lang with
            | Auto_Tra(path),Some(l) ->
                let my_regexp = Str.regexp_string "%s" in
                let tra_file_dir = Str.global_replace
                    my_regexp l.lang_dir_name path in
                let d_base,_ = split (Case_ins.filename_basename src) in
                let tra_file = tra_file_dir ^ "/" ^ d_base ^ ".TRA" in
                handle_tra_filename tra_file ;
            | Auto_Tra(path),None ->
                let d_base,_ = split (Case_ins.filename_basename src) in
                let tra_file = path ^ "/" ^ d_base ^ ".TRA" in
                handle_tra_filename tra_file
            | _ -> ())) tp.flags ;

          resolve_tra_paths_and_load !our_lang tra_l ;

          if !Dc.notChanged then
            Modder.handle_msg "SETUP_TRA"
              (Printf.sprintf "WARNING: EXTEND* %s with strings from setup.tra\n" src);
          let src_script =
            let src_buff = load_file src in
            if (!has_if_eval_bug) then begin
              (try
                List.iter (fun p -> process_patch1 src game src_buff p) pl ;
              with _ -> ())
            end ;
            let src_buff = List.fold_left (fun acc elt ->
              (try
                process_patch2 src game acc elt
              with e ->
                log_and_print "ERROR: [%s] -> [%s] Patching Failed (EXTEND_TOP/BOTTOM)\n"
                  src dest ; raise e))
                src_buff pl in
            Dc.ok_to_resolve_strings_while_loading := Some(game) ;
            (try
              let res = handle_script_buffer src src_buff in
              Dc.ok_to_resolve_strings_while_loading := None ;
              res
            with e ->
              begin
                Dc.ok_to_resolve_strings_while_loading := None ;
                raise e
              end) ; in
          List.iter (fun dest ->
            let base,ext = split (String.uppercase dest) in
            let dest_script =
              let old_a_m = !Load.allow_missing in
              Load.allow_missing := dest :: old_a_m ;
              let dest_buff, dest_path =
                (try
                  Load.load_resource "EXTEND_TOP/EXTEND_BOTTOM" game true base ext
                with _ ->
                  begin
                    log_only "[%s] not found, treating as empty.\n" dest ;
                    "",""
                  end)
              in
              Load.allow_missing := old_a_m ;
              handle_script_buffer dest dest_buff in

            let destpath =
              (try
                ignore(String.index dest '/'); dest
              with _ -> "override/" ^ dest) in
            Stats.time "saving files" (fun () ->
              let out = open_for_writing destpath true in
              Bcs.save_bcs game (Bcs.Save_BCS_OC(out))
                (match a with
                  TP_Extend_Top(_,_,_,_,_) -> src_script @ dest_script
                | _ -> dest_script @ src_script) ;
              close_out out ;
              begin (* handle read-only files! *)
                (try
                  Case_ins.unix_chmod destpath 511 ; (* 511 = octal 0777 = a+rwx *)
                with e -> ())
                  (* log_or_print "WARNING: chmod %s : %s\n" filename
                     (printexc_to_string e) *)
              end ;) () ;
            log_or_print "Extended script [%s] with [%s]\n" dest src) dlist ;
          Dc.pop_trans () ;
        with e -> Dc.pop_trans () ; raise e)
      end

      | TP_At_Interactive_Exit(str,exact) ->
          if !interactive then process_action tp (TP_At_Exit(str,exact))
      | TP_At_Interactive_Now(retvar,str,exact) ->
          if !interactive then process_action tp (TP_At_Now(retvar,str,exact))

      | TP_At_Exit(str,exact) ->
          begin
            let str = Var.get_string str in
            let a,b = split (String.uppercase str) in
            match b with
            | "TP2" -> (enqueue_tp2_filename) str
            | _ ->
                let str = if exact then str else Arch.handle_view_command str !skip_at_view in
                if List.mem (Command (str,exact)) !execute_at_exit then
                  ()
                else
                  execute_at_exit := (Command (str,exact)) :: !execute_at_exit
          end

      | TP_At_Now(retvar,str,exact) ->
          begin
            let retvar = match retvar with
            | None -> None
            | Some str -> Some (Var.get_string (eval_pe_str str)) in
            let str = Var.get_string str in
            let a,b = split (String.uppercase str) in
            match b with
            | "TP2" -> (enqueue_tp2_filename) str
            | _ ->
                let str = if exact then str else Arch.handle_view_command str !skip_at_view in
                (match retvar with
                | None -> ignore (exec_command str exact)
                | Some var ->
                    let retval = match exec_command str exact with
                    | Unix.WEXITED i
                    | Unix.WSIGNALED i
                    | Unix.WSTOPPED i -> i in
                    Var.set_int var retval)
          end

      | TP_At_Interactive_Uninstall(_)
      | TP_At_Uninstall(_)
      | TP_At_Interactive_Uninstall_Exit(_)
      | TP_At_Uninstall_Exit(_) -> ()

      | TP_DecompressBiff(sl) ->
          let sl = List.map Var.get_string (List.map eval_pe_str sl) in
          Hashtbl.iter (fun name biff ->
            (try
              Unix.close biff.Biff.fd
            with e ->
              log_and_print "ERROR: DECOMPRESS_BIFF failed to close %s\n" name ;
              raise e)) game.Load.loaded_biffs;
          game.Load.loaded_biffs <- Hashtbl.create 5 ;
          let ensure_terminal_separator path =
            if Str.string_match (Str.regexp ".*[\\\\/]$") path 0 then
                path
            else path ^ "/" in
          let biff_path_list = List.sort_unique compare
              (List.flatten (List.map (fun s ->
                let s = String.lowercase s in
                if file_exists s then begin
                  log_and_print "WARNING: giving biffs as %s to \
                    DECOMPRESS_BIFF is deprecated\n" s ;
                  [s]
                end else begin
                  let paths = List.filter file_exists
                      (List.map (fun path ->
                        let guess = (ensure_terminal_separator path) ^
                          "data/" ^ s in
                        ignore (log_only "Looking for biff %s at %s\n" s guess) ;
                        guess) game.Load.cd_path_list) in
                  (match paths with
                  | [] -> failwith (Printf.sprintf "DECOMPRESS_BIFF could not \
                                      find biff %s" s) ;
                  | list -> list)
                end) sl)) in
          let backup_filename biff suffix =
            let backup_prefix = "fl#biffbackup" in
            let basename = Case_ins.filename_basename biff in
            let dirname = Case_ins.filename_dirname biff in
            dirname ^ "/" ^ backup_prefix ^ suffix ^ "_" ^ basename in
          let backup biff suffix =
            let backup = backup_filename biff suffix in
            if file_exists backup then begin
              Unix.unlink backup end ;
            ignore (Case_ins.unix_rename biff backup) ;
            backup in
          let backdown biff suffix =
            let backup = backup_filename biff suffix in
            if file_exists biff then
              Unix.unlink biff ;
            ignore (Case_ins.unix_rename backup biff) ;
            biff in
          let decompress biff =
            let fd = Case_ins.unix_openfile biff [Unix.O_RDONLY] 0 in
            let buff = Bytes.create 8 in
            ignore (Unix.read fd buff 0 8) ;
            ignore (Unix.close fd) ;
            (match buff with
            | "BIFFV1  " -> ignore (log_and_print "[%s] already decompressed\n" biff) ; ()
            | "BIFCV1.0" ->
                let backed_up = backup biff "" in
                (try
                  let sz = Biff.bifc2biff (Case_ins.fix_name backed_up) (Case_ins.fix_name biff) in
                  ignore (log_and_print "[%s] decompressed biff file: %d bytes\n" biff sz) ;
                with e ->
                  ignore (backdown biff "") ;
                  ignore (log_and_print "ERROR: could not decompress biff %s [%s]\n" biff (printexc_to_string e)) ;
                  raise e)
            | "BIF V1.0" ->
                let biff = backup biff "" in
                (try
                  let new_bif = (Case_ins.filename_chop_extension biff) ^ ".bif" in
                  if file_exists new_bif then ignore (Unix.unlink new_bif) ;
                  let sz =  Cbif.cbf2bif (Case_ins.fix_name biff) (Case_ins.fix_name new_bif) in
                  ignore (log_and_print "[%s] decompressed biff file: %d bytes\n" biff sz) ;
                with e ->
                  ignore (backdown biff "") ;
                  ignore (log_and_print "ERROR: could not decompress biff %s [%s]\n" biff (printexc_to_string e)) ;
                  raise e)
            | _ -> failwith (Printf.sprintf "%s is not a valid BIFF file (wrong sig)" biff)) in
          let length = (List.length biff_path_list) in
          ignore (log_and_print "Decompressing %i biff file%s\n" length (if length = 1 then "" else "s")) ;
          ignore (List.iter decompress biff_path_list) ;

      | TP_AddJournal(existing,managed,title,ref_list,tra_list) ->
          (match game.Load.game_type with
          | Util.BGEE
          | Util.BG2EE
          | Util.IWDEE
          | Util.PSTEE -> begin

              log_and_print "Processing quests and journals\n" ;

              let resolve_string ref =
                match Dc.resolve_tlk_string game ref with
                  Dlg.TLK_Index i -> i
                | _ -> failwith "ERROR: ADD_JOURNAL cannot resolve reference"
              in
              let isolate_title_and_resolve index =
                let isolate_title string =
                  if Str.string_match (Str.regexp "\\(.+\\)$") string 0 then begin
                    let title = Str.matched_group 1 string in
                    let trim = (Str.regexp "[ \t\\.]+$") in
                    Str.global_replace trim "" title
                  end
                  else
                    failwith "ERROR: ADD_JOURNAL was unable to isolate a journal title and none was provided"
                in

                let male = Dc.pretty_print_no_quote
                    (Load.get_active_dialog game)
                    index false false in
                let female = Dc.pretty_print_no_quote
                    (Load.get_active_dialogf_fallback game)
                    index true false in
                let lse = Dlg.Local_String(
                  {lse_male = (isolate_title male);
                    lse_male_sound = "";
                    lse_female = (isolate_title female);
                    lse_female_sound = "";}) in
                resolve_string lse
              in

              let tra_list = List.map Arch.backslash_to_slash tra_list in

              Dc.push_copy_trans_modder ();
              (try
                Dc.notChanged := true;

                resolve_tra_paths_and_load !our_lang tra_list;

                if !Dc.notChanged then
                  Modder.handle_msg "SETUP_TRA"
                    (Printf.sprintf "WARNING: ADD_JOURNAL with strings from setup.tra\n");

                let indices = List.map resolve_string ref_list in

                if is_true (eval_pe "" game (Pred_File_Exists_In_Game (PE_LiteralString "bgee.lua"))) then begin
                  try
                    Var.var_push () ;

                    ignore (if existing then
                      Var.set_int32 "existing" (Int32.of_int 1)
                    else Var.set_int32 "existing" (Int32.of_int 0)) ;

                    ignore (match title with
                    | None -> Var.set_int32 "title" (Int32.of_int (-1))
                    | Some s -> begin
                        let strref = resolve_string s in
                        Var.set_int32 "title" (Int32.of_int strref) ;
                        if not existing &&
                          is_true (eval_pe "" game
                                     (PE_ResourceContains
                                        ((PE_LiteralString "bgee.lua"),
                                         (PE_LiteralString
                                            (Printf.sprintf "createQuest[ %%TAB%%]*([ %%TAB%%]*%d[ %%TAB%%]*)" strref)))))
                        then begin
                          let strref = eval_pe "" game PE_NextStrref in
                          Dc.set_string game (Int32.to_int strref) s true ;
                          Var.set_int32 "title" strref ;
                        end ;
                    end) ;

                    ignore (process_action tp
                              (TP_ActionDefineArray
                                 (PE_LiteralString "entries",
                                  (List.map (fun x -> Printf.sprintf "%d" x) indices)))) ;

                    ignore (process_action tp (TP_Include
                                                 [".../WEIDU_NAMESPACE/fl#add_journal_lua.tpa"])) ;
                    ignore (process_action tp (TP_Launch_Action_Macro "fl#ADD_JOURNAL_LUA")) ;
                    Var.var_pop () ;
                  with e -> Var.var_pop () ; raise e
                end else begin (* code path for legacy SQL format *)

                  let titled_indices = (match title with
                    Some(t) ->
                      let title = resolve_string t in
                      List.map (fun index ->
                        title,index) indices
                  | None ->
                      List.map (fun index ->
                        let title = isolate_title_and_resolve index in
                        title,index) indices) in

                  let quests,journals_quests = Sql.get_quests_data "ADD_JOURNAL" in
                  let highest_quest_id = ref (List.fold_left (fun acc record ->
                    max acc record.Sql.quest_id) 1 quests) in

                  let quest_id_table = Hashtbl.create
                      (if not existing then (List.length titled_indices)
                      else ((List.length quests) + (List.length titled_indices))) in

                  let journal_id_table = Hashtbl.create
                      (if not existing then (List.length titled_indices)
                      else ((List.length journals_quests) + (List.length titled_indices))) in

                  if existing then begin
                    ignore (List.iter (fun record ->
                      Hashtbl.add quest_id_table record.Sql.quest_strref record.Sql.quest_id)
                              quests) ;
                    ignore (List.iter (fun record ->
                      Hashtbl.add journal_id_table (record.Sql.journal_id,
                                                    record.Sql.journal_quest_id) 0)
                              journals_quests) ;
                  end;

                  let get_quest_id strref =
                    if not (Hashtbl.mem quest_id_table strref) then begin
                      let id = !highest_quest_id + 1 in
                      Hashtbl.add quest_id_table strref id ;
                      highest_quest_id := id ;
                      id ;
                    end
                    else
                      Hashtbl.find quest_id_table strref ;
                  in

                  let new_quests = List.fold_left (fun acc (title,index) ->
                    if not (Hashtbl.mem quest_id_table title) then
                      List.append acc
                        [(Sql.make_quests_record (get_quest_id title) "" title 0 0 0
                            ~quest_MC1:(Some (-1)) ())]
                    else
                      acc) [] titled_indices in

                  (* this can be optimised *)
                  let highest_quest_group = ref (List.fold_left (fun acc record ->
                    (match record.Sql.journal_quest_group with
                      None -> (-1)
                    | Some i -> max acc i))
                                                   0 journals_quests) in

                  let new_journals = List.fold_left (fun acc (title,index) ->
                    let quest_id = get_quest_id title in
                    if not (Hashtbl.mem journal_id_table (index,quest_id)) then begin
                      Hashtbl.add journal_id_table (index,quest_id) 0 ;
                      List.append acc
                        [(Sql.make_journals_record index quest_id 0
                            ~quest_group:(if managed then
                              (Some (!highest_quest_group + 1))
                            else
                              (Some 0)) ~date:(Some "") ~journal_MC1:(Some (-1)) ())]
                    end
                    else
                      acc) [] titled_indices in

                  if existing && managed then
                    ignore (List.iter (fun (title,index) ->
                      List.iter (fun record ->
                        (try
                          if (Hashtbl.mem quest_id_table title) &&
                            (Hashtbl.find quest_id_table title) = record.Sql.journal_quest_id &&
                            (match record.Sql.journal_quest_group with
                              Some 0 -> true
                            | _ -> false) then
                            record.Sql.journal_quest_group <- (Some (!highest_quest_group + 1))
                        with Not_found -> ())) journals_quests)
                              (match title,titled_indices with
                                Some t, [] -> [(resolve_string t),0]
                              | _,_ -> titled_indices)) ;

                  let quests = List.append new_quests quests in
                  let journals_quests = List.append new_journals journals_quests in
                  ignore (Sql.set_quests_data (quests,journals_quests)) ;
                end ; (* end legacy SQL path *)

                Dc.pop_trans ();
              with e -> Dc.pop_trans () ; raise e)
            end
          | Util.GENERIC -> ())

      | TP_Create(filetype, version, resref, patch_list) ->
          begin
            let filetype = Var.get_string filetype in
            let resref = Var.get_string resref in
            let version = (match version with
            | None -> ""
            | Some s -> Var.get_string s) in

            Var.set_string "FL#CREATE#TYPE" (eval_pe_str (PE_LiteralString filetype));
            Var.set_string "FL#CREATE#RESREF" (eval_pe_str (PE_LiteralString resref));
            Var.set_string "FL#CREATE#VERSION" (eval_pe_str (PE_LiteralString version));

            process_action tp (TP_Define_Patch_Macro ("FL#CREATE#PATCH_LIST", [], patch_list));
            process_action tp (TP_Include [".../WEIDU_NAMESPACE/fl#create.tpa"]);
            process_action tp (TP_Launch_Action_Macro("FL#CREATE"));
          end

      | TP_WithTra(tra_list, action_list) ->
          begin
            run_patch (TP_PatchWithTra (tra_list, pl_of_al action_list))
          end

      | TP_WithVarScope (action_list) ->
          begin
            run_patch (TP_PatchWithVarScope (pl_of_al action_list))
          end

      | TP_ActionTime(name, action_list) ->
          begin
            run_patch (TP_PatchTime (name, pl_of_al action_list))
          end
      );
      if !clear_memory then begin
        clear_memory := false;
        process_action tp TP_ClearMemory;
        clear_memory := true;
      end;
      with
     | Abort msg -> raise (Abort msg)
     | e -> (* from: let rec process_action = try *)
      (if !continue_on_error then begin
        log_and_print "WARNING: Continuing despite [%s]\n"
          (printexc_to_string e);

        (try assert false with Assert_failure(file,line,col) -> set_errors file line);
      end else begin
        exit_status := StatusInstallFailure ;
        log_and_print "Stopping installation because of error.\n" ;
        raise e
      end)) ()
