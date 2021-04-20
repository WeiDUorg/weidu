(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* automatically make a TP2 file section for all files in the given
 * directory *)
open BatteriesInit
open Hashtblinit
open Util

let process file game min o =
  let consider buff off off_name =
    let i = int_of_str_off buff off in
    if (i > min) && (i > 0) &&
      (i <= Array.length (Load.get_active_dialog game)) then begin
        let male = Tlk.pretty_print (Load.get_active_dialog game) i in
        let female = Tlk.pretty_print_opt (Load.get_active_dialogf_opt game) i in
        o (Printf.sprintf "  SAY %s %s %s\n"
             off_name  male female)
      end
  in
  let _,ext = split file in
  let ext = String.uppercase ext in
  try begin
    match ext with
    | "SPL"
    | "ITM"
    | "CRE" -> begin
        let buff = load_file file in
        consider buff 0x8 "NAME1" ;
        consider buff 0xc "NAME2" ;
        if ext = "ITM" then begin
          consider buff 0x50 "UNIDENTIFIED_DESC" ;
          consider buff 0x54 "DESC" ;
        end ;
        if ext = "SPL" then begin
          consider buff 0x50 "UNIDENTIFIED_DESC" ;
        end;
        if ext = "ITM" || ext = "SPL" then begin
          let eff_list = Load.eff_of_spl_itm buff in
          Array.iter (fun eff ->
            if eff.Load.opcode = 139 (* display string *) then
              let offset = eff.Load.raw_offset + 4 in
              consider buff offset (Printf.sprintf "0x%x" offset)
                     ) eff_list
        end ;
        if ext = "CRE" then begin
          let i = ref 0xa4 in
          while !i <= 0x230 do
            consider buff !i (Printf.sprintf "0x%x" !i) ;
            i := !i + 4;
          done ;
        end
    end
    | "EFF" -> begin
        let buff = load_file file in
        let eff_list = Load.eff_of_eff buff in
        Array.iter (fun eff ->
          if eff.Load.opcode = 139 (* display string *) then
            let offset = eff.Load.raw_offset + 0x1c in
            consider buff offset (Printf.sprintf "0x%x" offset)
                   ) eff_list
    end
    | "ARE" -> begin
        begin
          let buff = load_file file in
          let signature = String.sub buff 0 8 in
          let num_info_points, info_point_offset =
            match signature with
            | "AREAV1.0" ->
                (short_of_str_off buff 0x5a , int_of_str_off buff 0x5c )
            | "AREAV9.1" ->
                (short_of_str_off buff 0x6a , int_of_str_off buff 0x6c )
            | _ ->
                o (Printf.sprintf "  // %s files not supported\n" signature) ;
                (0, 0)
          in
          for i = 0 to (num_info_points - 1) do
            let base_offset = info_point_offset + (196 * i) in
            let offset = base_offset + 100 in
            consider buff offset (Printf.sprintf "0x%x" offset)
          done
        end
    end
    | "STO" -> begin
        let buff = load_file file in
        consider buff 0xc "NAME2";
    end
    | _ -> log_and_print "Not automating [%s]\n" file
  end with e -> begin
    log_and_print "Error automating [%s] : %s \n" file
      (printexc_to_string e)
  end

let automate_file game file_name min o =
  process file_name game min o

let automate game dir_l min o =
  List.iter (fun dir ->
    if (Case_ins.unix_stat dir).Unix.st_kind <> Unix.S_REG then begin
      let d_h = Case_ins.unix_opendir dir in
      try
        while true do
          let s = Unix.readdir d_h in
          let full = dir ^ "/" ^ s in
          if file_exists full then begin
            let src = Printf.sprintf "~%s~" full in
            let dst = Printf.sprintf "~override/%s~" s in
            o (Printf.sprintf "\nCOPY %-30s %-30s\n" src dst) ;
            process full game min o
          end
        done
      with e -> (Unix.closedir d_h )
    end
            ) dir_l
