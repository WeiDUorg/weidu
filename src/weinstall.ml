let log_and_print fmt =
  let k result = begin
    output_string stdout result ; flush stdout ;
  end
  in
  Printf.kprintf k fmt

let main () =
  if Array.length Sys.argv = 1 then begin
    log_and_print "Usage: %s name-of-tp2 otheroptions\n" Sys.argv.(0);
  end else begin
    let buff = Buffer.create 1000 in
    Sys.argv.(1) <- String.lowercase Sys.argv.(1);
    if Case_ins.filename_check_suffix Sys.argv.(1) ".tp2" then Sys.argv.(1) <- Case_ins.filename_chop_suffix Sys.argv.(1) ".tp2";
    let debug_where = try
      let h = Unix.stat "debugs" in
      if h.Unix.st_kind = Unix.S_DIR then Printf.sprintf "debugs/%s.debug" Sys.argv.(1)
      else Printf.sprintf "setup-%s.debug" Sys.argv.(1);
    with _ -> Printf.sprintf "setup-%s.debug" Sys.argv.(1);
    in
    let we = Case_ins.weidu_executable in
    let weidu_executable, fast = try
      let s = Sys.argv.(0) in
      let fast = String.uppercase (Str.string_before (Filename.basename s) 4) = "FAST" in
      let s = Str.string_after s (String.index s '-') in
      let s = Str.string_before s (String.rindex s '.') in
      let we = Str.string_before we (String.rindex we '.') in
      we ^ s ^ ".exe", fast
    with _ -> we, false in
    Buffer.add_string buff (Printf.sprintf "%s --tlkout dialog.tlk --ftlkout dialogf.tlk --log %s "
                              weidu_executable debug_where);
    let x = Sys.argv.(1) in
    Buffer.add_string buff (Printf.sprintf " %s.tp2 setup-%s.tp2 %s/%s.tp2 %s/setup-%s.tp2 "
                              x            x      x  x      x        x);
    if (fast) then
      Buffer.add_string buff " --quick-log --skip-at-view --safe-exit ";
    for i = 2 to Array.length Sys.argv - 1 do
      Buffer.add_string buff (
      match Sys.argv.(i) with
      | "--force-install" -> " --force-install-list "
      | "--force-uninstall" -> " --force-uninstall-list "
      | x -> " " ^ x ^ " "
     );
    done;
    let x = Buffer.contents buff in
    print_endline x;
    let int_of_ps x = match x with
    | Unix.WEXITED(i)
    | Unix.WSIGNALED(i)
    | Unix.WSTOPPED(i)
      -> i
    in
    exit (int_of_ps (Unix.system x));
  end

;;

try
  main ()
with e -> log_and_print "Error: %s\n" (Printexc.to_string e)
;;
