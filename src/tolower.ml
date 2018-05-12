(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(*
 * Module to reduce all files in the base directory to lowercase
 * and other Linux-only boringness
 *)

open BatteriesInit

(*
 * Bare-bones interface
 *)
exception Good_exit ;;

let force = ref false

let askfor func mess =
  print_string mess ;
  try
    while true do
      print_string "[Y]es or [N]o\n" ;
      let x = read_line () in
      match String.uppercase x with
      | "Y" ->
          func () ;
          raise Good_exit ;
      | "N" ->
          raise Good_exit ;
      | "Y!" ->
          force := true ;
          func () ;
          raise Good_exit ;
      | _ ->
          ()
    done
  with Good_exit -> ()

(*
 * Turn all files to lowercase, recursively.
 *)
let rec find_and_lower cur_dir () =
  if not !force && Sys.file_exists (cur_dir ^ "/lang") &&
    Sys.file_exists (cur_dir ^ "/BaldursGate") then begin
    output_string stdout
      "This looks like an EE-type game. Tolower would break it.\n" ;
    failwith "EE"
  end ;
  let dh = Unix.opendir cur_dir in
  let dirlist = ref [] in
  let done_ht = Hashtbl.create 5 in
  try
    while true do
      let element = Unix.readdir dh in
      if not (Hashtbl.mem done_ht element) then begin
        let implicit = element.[0] = '.' in
        let element = cur_dir ^ "/" ^ element in

        let is_a_symlink =
          let stats = Unix.lstat element in
          stats.Unix.st_kind = Unix.S_LNK
        in
        let is_a_dir =
          let stats = Unix.lstat element in
          stats.Unix.st_kind = Unix.S_DIR
        in
        let lowercase = element = (String.lowercase element) in
        (* Even if directory itself is already lowercased, it may contain
           not-lowercased items and we consequently need to process it anyway.
         *)
        if not implicit && not is_a_symlink && (not lowercase || is_a_dir) then
          begin
            let exists = Hashtbl.mem done_ht (String.lowercase element) in
            if exists && is_a_dir then begin
              dirlist := (element, true) :: !dirlist ;
            end else begin
              Unix.rename element (String.lowercase element) ;
              if is_a_dir then begin
                dirlist := (String.lowercase element, false) :: !dirlist ;
              end
            end
          end ;
        Hashtbl.add done_ht (String.lowercase element) true ;
      end ;
    done
  with End_of_file ->
    Unix.closedir dh ;
    List.iter (fun (x, do_del) ->
      find_and_lower x () ;
      if do_del then Unix.rmdir x) !dirlist

(*
 * Generate linux.ini from baldur.ini using /unix/style/paths rather than
 * W:\indows\style\paths
 *)

let get_wine_cfg () =
  (* first, figure out what c:\ (etc) means in Unix style *)
  let home = Sys.getenv "HOME" in
  let winepath = home ^ "/.wine/dosdevices" in
  let winelst = Sys.readdir winepath in
  let allpaths = Hashtbl.create 21 in
  Array.iter (fun this ->
    let thisdest = Unix.readlink (winepath ^ "/" ^ this) in
    let relative = Str.regexp "..?/" in
    let thisdest = if Str.string_match relative thisdest 0 then
      (winepath ^ "/" ^ thisdest)
    else thisdest in
    Hashtbl.add allpaths (String.uppercase this) thisdest) winelst ;
  (* Read all directory list in baldur.ini and create linux.ini from it *)
  let linuxini = open_out "linux.ini" in
  Array.iter (fun file ->
    if (Filename.check_suffix file ".ini") then begin
      let baldurini = open_in file in
      let cdregex = Str.regexp "[ \t]*[HC]D.:=" in
      try
        while true do
          let line = input_line baldurini in
          if Str.string_match cdregex line 0 then begin
            let split = Str.split (Str.regexp "[=;]") line in
            List.iter (fun this ->
              let path = Str.string_before this 2 in
              let path = Hashtbl.find allpaths (String.uppercase path) in
              let otherpart = Str.string_after this 2 in
              let newpath = path ^ otherpart in
              let newpath = Str.global_replace (Str.regexp "\\\\")
                                                  "/" newpath in
              Printf.fprintf linuxini "CD1:=%s\n%!" newpath) (List.tl split)
          end ;
        done
      with End_of_file -> () ;
        close_in baldurini
    end) (Sys.readdir ".") ;
  close_out linuxini
;;

(*
 * Ask the user which functions to run
 *)
askfor (find_and_lower ".") "Do you want to lowercase everything?
  (run if you extracted some mods since the last time you ran this utility)\n" ;
askfor get_wine_cfg "Do you want to generate linux.ini from baldur.ini?
(needed once per installation)\n"
