(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

open BatteriesInit
open Hashtblinit
open Util
open Tlk

let read_unsetstr filename : Load.str_set_record list =
  try
    let inchan = Case_ins.perv_open_in_bin filename in
    let ans : Load.str_set_record list = Marshal.from_channel inchan in
    close_in inchan;
    ans
  with _ ->
    parse_file true (File (filename ^ ".TEXT")) "parsing unsetstr files"
      (Dparser.unsetstr_file Dlexer.initial)
;;

let write_unsetstr filename unsetstr =
  let outchan = Case_ins.perv_open_out_bin filename in
  Marshal.to_channel outchan unsetstr [];
  close_out outchan;
  let outchan = Case_ins.perv_open_out (filename ^ ".TEXT") in
  List.iter (fun item ->
    let (idx,old,old_f) = item in
    Printf.fprintf outchan "#%d ~~~~~%s~~~~~ [%s] #%d #%d #%d ~~~~~%s~~~~~ [%s] #%d #%d #%d\n" 
      idx old.text old.sound_name old.flags old.volume old.pitch
      old_f.text old_f.sound_name old_f.flags old_f.volume old_f.pitch
    ;
  ) unsetstr;
  Printf.fprintf outchan "%!";
  close_out outchan
;;

let read_tlkpath filename : string * string option =
  parse_file true (File filename) "parsing tlk-path files"
    (Dparser.tlk_path_file Dlexer.initial)

let write_tlkpath filename dialog_path dialogf_path =
  let out = Case_ins.perv_open_out filename in
  ignore (output_string out ("~" ^ dialog_path ^ "~")) ;
  (match dialogf_path with
  | None -> ignore (output_string out "\n")
  | Some p -> ignore (output_string out (" ~" ^ p ^ "~\n"))) ;
  ignore (close_out out)

let read_cli_vars filename : string list =
  try
    let infile = Case_ins.perv_open_in_bin filename in
    let record : string list =
      Marshal.from_channel infile
    in
    close_in infile ;
    record
  with _ ->
    parse_file true (File (filename ^ ".TEXT")) "parsing args files"
      (Dparser.args_file Dlexer.initial)
;;

let write_cli_vars filename cli_vars =
  let outchan = Case_ins.perv_open_out_bin filename in
  Marshal.to_channel outchan cli_vars [] ;
  close_out outchan ;
  let outchan = Case_ins.perv_open_out (filename ^ ".TEXT") in
  List.iter (fun s ->
    Printf.fprintf outchan "~~~~~%s~~~~~\n" s;
  ) cli_vars;
  Printf.fprintf outchan "%!";
  close_out outchan;
;;

let read_readln filename : string list =
  try
    let infile = Case_ins.perv_open_in_bin filename in
    let record : (Tp.tp_pe_string * string) list =
      Marshal.from_channel infile
    in
    close_in infile ;
    List.map snd record
  with _ ->
    parse_file true (File (filename ^ ".TEXT")) "parsing readln files"
      (Dparser.args_file Dlexer.initial)

let write_readln filename writeln =
  let outchan = Case_ins.perv_open_out_bin filename in
  Marshal.to_channel outchan (List.map (fun s -> (Tp.PE_LiteralString "",s)) writeln) [] ;
  close_out outchan ;
  let outchan = Case_ins.perv_open_out (filename ^ ".TEXT") in
  List.iter (fun b ->
    Printf.fprintf outchan "~~~~~%s~~~~~\n" b;
  ) writeln;
  Printf.fprintf outchan "%!";  close_out outchan ;
