open BatteriesInit
open Hashtblinit
open Util

exception Modder_error of string

type modder_level =
  | Fail
  | Warn
  | None

let level_of_string s2 =
  match String.uppercase s2 with
  | "NONE" -> None
  | "WARN" -> Warn
  | "FAIL" -> Fail
  | _ -> failwith (Printf.sprintf "Unknown MODDER mode: %s" s2)

let string_of_level s2 =
  match s2 with
  | None -> "NONE"
  | Warn -> "WARN"
  | Fail -> "FAIL"

let mode = Hashtbl.create 5

let options = ["SETUP_TRA" ; "AREA_VARIABLES" ; "MISSING_EXTERN" ;
               "MISSING_RESREF" ; "ICT2_ACTIONS" ;
               "MISSING_EVAL" ; "OVERWRITING_FILE" ;
               "FUN_ARGS"]

let print_msg str =
  let old_be_silent = !be_silent in
  be_silent := false ; log_and_print "%s" str ; be_silent := old_be_silent

let set_modder str_l =
  if not !debug_modder then begin
    debug_modder := true ;
    List.iter (fun x -> match x with
    | "OVERWRITING_FILE" -> Hashtbl.replace mode x (None,100)
    | _ -> Hashtbl.replace mode x (Warn,100)) options ;
  end ;
  List.iter (fun (s1,s2,prio) ->
    let s2 = level_of_string s2 in
    let s1 = String.uppercase s1 in
    let old_prio = try
      snd (Hashtbl.find mode s1) ;
    with
    | _ ->
        if List.mem s1 options then
          log_and_print "Known but unassigned MODDER option: %s" s1
        else
          failwith (Printf.sprintf "Unknown MODDER option: %s" s1) ;
        1000
    in
    if prio <= old_prio then Hashtbl.replace mode s1 (s2, prio)) str_l

let get x = if !debug_modder then try
  fst (Hashtbl.find mode (String.uppercase x))
with _ -> Warn else None

let handle_deb test str =
  (match get test with
  | None -> ()
  | Warn -> print_msg ("WARNING: " ^ str) ;
      (try assert false with
      | Assert_failure(file,line,col) -> set_errors file line)
  | Fail -> print_msg ("ERROR: " ^ str) ; raise (Modder_error str))

let handle_msg test str =
  if get test <> None then print_msg str

let enabled test =
  get test <> None
