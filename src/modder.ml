open Util

exception Modder_error of string

type modder_level =
  | Fail
  | Warn
  | None

let mode = Hashtbl.create 5

let options = ["SETUP_TRA"     ;"AREA_VARIABLES";"MISSING_EXTERN";"MISSING_RESREF";"ICT2_ACTIONS"]

let set_modder str_l =
  debug_modder := true;
  List.iter (fun x -> Hashtbl.replace mode x Warn) options;
  List.iter (fun (s1,s2) ->
    let s2 = match String.uppercase s2 with
    | "NONE" -> None
    | "WARN" -> Warn
    | "FAIL" -> Fail
    | _ -> failwith (Printf.sprintf "Unknown MODDER mode: %s" s2)
    in
    let s1 = String.uppercase s1 in
    (try
      ignore (Hashtbl.find mode s1);
    with _ -> failwith (Printf.sprintf "Unknown MODDER option: %s" s1));
    Hashtbl.replace mode s1 s2
	    ) str_l
;;

let get x = if !debug_modder then try Hashtbl.find mode (String.uppercase x) with _ -> Warn else None

let handle_deb test str =
  if get test <> None then log_and_print "%s" str;
  if get test =  Warn then (try assert false with Assert_failure(file,line,col) -> set_errors file line);
  if get test =  Fail then raise (Modder_error str)

let handle_msg test str =
  if get test <> None then log_and_print "%s" str;

