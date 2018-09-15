(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit

let log_and_print fmt =
  let k result = begin
    output_string stdout result ; flush stdout ;
  end
  in
  Printf.kprintf k fmt

let quote str =
  Printf.sprintf "\"%s\"" str

let main () =
  if Array.length Sys.argv = 1 then begin
    log_and_print "Usage: %s name-of-tp2 otheroptions\n" Sys.argv.(0) ;
  end else begin
    let buff = Buffer.create 1000 in
    Sys.argv.(1) <- String.lowercase Sys.argv.(1) ;
    if Case_ins.filename_check_suffix Sys.argv.(1) ".tp2" then
      Sys.argv.(1) <- Case_ins.filename_chop_suffix Sys.argv.(1) ".tp2" ;
    if Case_ins.filename_check_suffix Sys.argv.(1) "/" then
      Sys.argv.(1) <- Case_ins.filename_chop_suffix Sys.argv.(1) "/" ;
    let debug_where = try
      let h = Unix.stat "debugs" in
      if h.Unix.st_kind = Unix.S_DIR then
        quote (Printf.sprintf "debugs/%s.debug" Sys.argv.(1))
      else quote (Printf.sprintf "setup-%s.debug" Sys.argv.(1)) ;
    with _ -> quote (Printf.sprintf "setup-%s.debug" Sys.argv.(1)) ;
    in
    let we = Case_ins.weidu_executable in
    let fast = try
      let s = Sys.argv.(0) in
      String.uppercase (Str.string_before (Filename.basename s) 4) = "FAST"
    with _ -> false in
    let weidu_executable = try
      let s = Sys.argv.(0) in
      let s = Str.string_after s (String.index s '-') in
      let s = Str.string_before s (String.rindex s '.') in
      let we = Str.string_before we (String.rindex we '.') in
      we ^ s ^ ".exe"
    with _ -> we in
    Buffer.add_string buff (Printf.sprintf "%s --log %s "
                              weidu_executable debug_where) ;
    let x = Sys.argv.(1) in
    let tp2s = List.filter Sys.file_exists
        [(x ^ "/" ^ x ^ ".tp2") ;
         (x ^ "/" ^ "setup-" ^ x ^ ".tp2") ;
         (x ^ ".tp2") ;
         ("setup-" ^ x ^ ".tp2")] in
    let tp2 = (try List.hd tp2s with _ ->
      failwith "ERROR: TP2 file not found.") in
    Buffer.add_string buff (quote tp2) ;
    if (fast) then
      Buffer.add_string buff
        " --quick-log --skip-at-view --safe-exit --no-exit-pause " ;
    for i = 2 to Array.length Sys.argv - 1 do
      Buffer.add_string buff (match Sys.argv.(i) with
      | "--force-install" -> " --force-install-list "
      | "--force-uninstall" -> " --force-uninstall-list "
      | x -> " " ^ x ^ " ") ;
    done ;
    let cmd = Buffer.contents buff in
    print_endline cmd ;
    let int_of_ps x = match x with
    | Unix.WEXITED(i)
    | Unix.WSIGNALED(i)
    | Unix.WSTOPPED(i)-> i
    in
    exit (int_of_ps (Unix.system cmd)) ;
  end
;;

try
  main ()
with e -> log_and_print "Error: %s\n" (Printexc.to_string e)
