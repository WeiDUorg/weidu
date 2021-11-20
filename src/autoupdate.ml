(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util
open Version

let self_update_message = ref true

let get_version_list () =
  let weidu_list = ref [] in
  let digest_ht = Hashtbl.create 255 in
  let argv_0 = Case_ins.filename_basename Sys.argv.(0) in
  let this,ext = try split_resref argv_0 with _ -> argv_0,"" in
  let my_real_name = this^(if ext = "" then "" else ".exe") in
  let this_digest = Digest.file my_real_name in
  Hashtbl.add digest_ht this_digest (int_of_string version) ;
  (if not (Str.string_match
             (Str.regexp_case_fold "setup-.*\.exe$") my_real_name 0) then
    weidu_list := (my_real_name,int_of_string version) :: !weidu_list) ;
  (try begin
    let d_h = Case_ins.unix_opendir "." in
    try
      while true do
        let f = Unix.readdir d_h in
        if Arch.is_weidu_executable f then begin
          (try
            let f_digest = Digest.file f in
            let version =
              if Hashtbl.mem digest_ht f_digest then
                Hashtbl.find digest_ht f_digest
              else begin
                let version = (try
                  Arch.get_version f
                with Failure "invalid pid" ->
                  Printf.printf
                    "{%s} could not get version; if this is WeiDU, you need to update it yourself\n%!" f ;
                  failwith "moving on"
                | Failure "not weidu" ->
                    Printf.printf
                      " but does not appear to be WeiDU; if it is, you need to update it yourself\n" ;
                    failwith "moving on") in
                log_and_print " version = %d\n" version ;
                Hashtbl.add digest_ht f_digest version ;
                version
              end in
            weidu_list := (f,version) :: !weidu_list
          with _ -> ())
        end done
    with e -> ()
  end with e -> ()) ;
  List.sort (fun (f1,v1) (f2,v2) -> v2 - v1) !weidu_list

let verify_latest can_spawn = begin
  let sorted = get_version_list () in
  if !debug_ocaml then List.iter (fun (f,v) -> log_and_print "%s %d\n" f v) sorted;
  let argv_0 = Case_ins.filename_basename Sys.argv.(0) in
  let this,ext = try split_resref argv_0 with _ -> argv_0,"" in
  let my_real_name = this^(if ext = "" then "" else ".exe") in

  (* head of list is newest element *)

  if (List.length sorted) > 1 then begin

    let newest,newest_t = List.hd sorted in
    let oldest,oldest_t = List.hd (List.rev sorted) in

    if (newest_t <> oldest_t) then begin
      (* out-of-synch: time to do updates *)
      log_and_print "Newest WeiDU is version %d, updating!\n" newest_t ;
      log_and_print "WeiDU files in version order:\n" ;
      List.iter (fun (f,v) -> log_and_print "  [%s] version %d\n" f v ) sorted ;

      let newest_buff = load_file newest in

      List.iter (fun (target,target_t) ->
        if (target <> this) && (target_t <> newest_t) then begin
          (* log_and_print "\tUnlinking [%s]: " target ;        *)
          let unlink_worked = (try Case_ins.unix_unlink target ; true
          with _ -> false) in
          (* log_and_print "%b\n" unlink_worked ; *)
          log_and_print "\tCopying [%s] -> [%s]: " newest target ;
          let copy_worked = try
            let out = open_for_writing target true in
            output_string out newest_buff ;
            close_out out ;
            if Arch.view_command <> "start"
            then Unix.chmod target 0o755 (* rwxr-xr-x *);
            true
          with _ -> false in
          log_and_print "%b\n" copy_worked ;
        end
                ) sorted ;

      if newest_t <> (int_of_string version) then begin
        let not_this = Case_ins.filename_basename
            (let file,time = (List.find (fun (f,v) -> f <> this) sorted) in
            file)
        in

        Sys.argv.(0) <- not_this ;

        let cmd = Array.fold_left (fun acc elt -> acc ^ " " ^ elt)
            Sys.argv.(0) (Array.sub Sys.argv 1 ((Array.length Sys.argv)-1)) in

        let env =
          Array.append [|Printf.sprintf "weiduautoupdate=%s" my_real_name ;
                         Printf.sprintf "weiduautoupdatesilent=%d"
                           (if !self_update_message then 1 else 0)
                       |]
            (Unix.environment ())
        in

        if (can_spawn) then begin
          let _ = Unix.execve not_this Sys.argv env in
          exit (return_value StatusSuccess) ;
        end
      end
    end ;
  end
end

let self () =
  (* let update_regexp = Str.regexp_case_fold "weiduautoupdate" in      *)
  let target = Unix.getenv "weiduautoupdate" in
  let silent =
    try
      let waus = Unix.getenv "weiduautoupdatesilent" in
      if !debug_ocaml then log_and_print "weiduautoupdatesilent = %s\n"
          waus ;
      if waus = "0" then false else true ;
    with _ -> (
      if !debug_ocaml then log_and_print "weiduautoupdatesilent is not set\n" ;
      false)
  in
  log_and_print "Auto-Updating on behalf of [%s]\n" target ;
  let this_buff = load_file Sys.argv.(0) in
  (* in this case we can always just copy ourselves over the target *)
  let unlink_worked = (try Case_ins.unix_unlink target ; true with _ -> false) in
  log_and_print "\tCopying [%s] -> [%s]: " Sys.argv.(0) target ;
  let copy_worked = try
    let out = open_for_writing target true in
    output_string out this_buff ;
    close_out out ;
    if Arch.view_command <> "start" then Unix.chmod target 0o755 (* rwxr-xr-x *);
    true
  with _ -> false in
  log_and_print "\nAuto-Updating on behalf of [%s] (done)\n" target ;

  try
    Sys.argv.(0) <- (Printf.sprintf "\"%s\"" target) ;
    let is_wau = Str.regexp "weiduautoupdate" in
    let env = Array.of_list (
      List.filter (fun e ->
        not (Str.string_match is_wau e 0)
                  )
        (Array.to_list (Unix.environment()))
     ) in
    Unix.execve target Sys.argv env;
  with _ ->
    if not silent then begin
      log_and_print
        "\n\n\t***********************************************************\n\tWeiDU has finished auto-updating all copies of itself\n\tin this directory. Please RE-RUN %s\n\tto actually install the mod.\n\t(sorry, I can't do it for you, Windows won't let me)\n" target ;
      (if not Myarg.good_terminal_p then (try ignore (read_line () ) with _ -> ()))
    end;
    exit (return_value StatusAutoUpdateRetry) ;
