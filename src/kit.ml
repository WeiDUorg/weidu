(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Kit Extraction Code *)

open BatteriesInit
open Hashtblinit
open Util

let lines buffer = Str.split (Str.regexp "[\r\n]+") buffer
let words buffer = Str.split (Str.regexp "[ \t]+") buffer

let fix_me = "!XXX YOU MUST FIX ME XXX!"

type table = {
  resource : string;
  default : string;
  headers : string array;
  rows : string array list;
}

type extracted_kit = {
  name : string;
  lower : string;
  mixed : string;
  help : string;
  abilities : string;
  ability_buffer : string;
  clasweap : string;
  weapprof : string;
  abclasrq : string;
  abclsmod : string;
  abdcdsrq : string;
  abdcscrq : string;
  alignmnt : string;
  dualclas : string;
  include_in : string list;
  unusable : string;
  klass : string;
  luabbr : string;
  tfstweap : string list;
}

let equal_ci a b = String.lowercase a = String.lowercase b

let int_of_string_opt string =
  try Some (int_of_string string) with Failure _ -> None

let parse_table resource buffer =
  let token_lines =
    List.filter (fun line -> line <> [])
      (List.map words (lines buffer))
  in
  match token_lines with
  | signature :: default :: headers :: rows
      when signature <> [] && equal_ci (List.hd signature) "2DA" &&
           default <> [] && headers <> [] ->
      { resource = resource;
        default = List.hd default;
        headers = Array.of_list headers;
        rows = List.map Array.of_list rows; }
  | _ ->
      failwith (Printf.sprintf
        "ERROR: --extract-kits cannot parse %s.2DA: invalid 2DA structure"
        resource)

let load_table game resource =
  let buffer, _ =
    Load.load_resource "extracting kits" game true resource "2DA"
  in
  parse_table resource buffer

let load_required_table game resource =
  if not (Load.resource_exists game resource "2DA") then
    failwith (Printf.sprintf
      "ERROR: --extract-kits requires %s.2DA, but that resource is not present"
      resource);
  load_table game resource

let load_optional_table game resource =
  if Load.resource_exists game resource "2DA" then
    Some (load_table game resource)
  else
    None

let find_row table name =
  List.find (fun row ->
    Array.length row > 0 && equal_ci row.(0) name) table.rows

let row_or_fix table name =
  try String.concat " " (Array.to_list (find_row table name))
  with Not_found ->
    log_and_print
      "Kit.extract: cannot find row ~%s~ in %s.2DA\n"
      name table.resource;
    fix_me

let find_header table name =
  let result = ref None in
  Array.iteri (fun index header ->
    if !result = None && equal_ci header name then result := Some index)
    table.headers;
  !result

let column_at table index =
  if index < 0 || index >= Array.length table.headers then
    failwith (Printf.sprintf
      "ERROR: --extract-kits: column %d is outside %s.2DA's %d-column header"
      index table.resource (Array.length table.headers));
  List.map (fun row ->
    let value_index = index + 1 in
    if value_index < Array.length row then row.(value_index)
    else table.default) table.rows

let write_file path buffer =
  let channel = Case_ins.perv_open_out_bin path in
  try
    output_string channel buffer;
    close_out channel
  with e ->
    close_out_noerr channel;
    raise e

let extract game o output_dir min_num =
  if min_num < 1 then
    failwith "ERROR: --extract-kits requires a positive starting kit number";

  (* These resources hold fields required by ADD_KIT. The remaining tables
     are optional because ADD_KIT itself updates them only IF_EXISTS. *)
  let kitlist = load_required_table game "KITLIST" in
  let clasweap = load_required_table game "CLASWEAP" in
  let abclasrq = load_required_table game "ABCLASRQ" in
  let abclsmod = load_required_table game "ABCLSMOD" in
  let abdcdsrq = load_required_table game "ABDCDSRQ" in
  let abdcscrq = load_required_table game "ABDCSCRQ" in
  let alignmnt = load_required_table game "ALIGNMNT" in
  let dualclas = load_required_table game "DUALCLAS" in
  let weapprof = load_required_table game "WEAPPROF" in
  let kittable = load_optional_table game "KITTABLE" in
  let tfstweap = load_optional_table game "25STWEAP" in
  let luabbr = load_optional_table game "LUABBR" in

  let kittable_list = ref [] in
  begin match kittable with
  | None -> ()
  | Some table ->
      let seen = Hashtbl.create 31 in
      List.iter (fun row ->
        for index = 1 to Array.length row - 1 do
          let resource = row.(index) in
          let key = String.lowercase resource in
          if not (Hashtbl.mem seen key) then begin
            Hashtbl.add seen key true;
            if Load.resource_exists game resource "2DA" then
              kittable_list :=
                (resource, load_table game resource) :: !kittable_list
          end
        done) table.rows
  end;

  let kittable_mentions id =
    let mentions_id (_, table) =
      List.exists (fun row ->
        Array.length row > 1 && int_of_string_opt row.(1) = Some id)
        table.rows
    in
    List.map fst (List.filter mentions_id (List.rev !kittable_list))
  in

  let selected_rows =
    List.fold_left (fun selected row ->
      if Array.length row = 0 then selected
      else match int_of_string_opt row.(0) with
      | None -> selected
      | Some id when id < min_num -> selected
      | Some id when Array.length row < 9 ->
          failwith (Printf.sprintf
            "ERROR: --extract-kits: kit %d in KITLIST.2DA has %d fields; expected at least 9"
            id (Array.length row))
      | Some _ -> row :: selected) [] kitlist.rows
    |> List.rev
  in

  let abilities = Hashtbl.create 31 in
  let spells = Hashtbl.create 511 in
  let process_ability buffer =
    List.iter (fun line ->
      List.iter (fun word ->
        if String.length word > 4 && word.[2] = '_' then
          Hashtbl.replace spells (Str.string_after word 3) true)
        (words line)) (lines buffer)
  in

  let prepare_kit row =
    let id = int_of_string row.(0) in
    let name = row.(1) in
    let ability = row.(5) in
    let proficiency = match int_of_string_opt row.(6) with
    | Some value -> value
    | None -> failwith (Printf.sprintf
        "ERROR: --extract-kits: kit %s has invalid PROFICIENCY value ~%s~ in KITLIST.2DA"
        name row.(6))
    in
    if proficiency < 0 || proficiency >= Array.length weapprof.headers then
      failwith (Printf.sprintf
        "ERROR: --extract-kits: kit %s refers to missing WEAPPROF.2DA column %d"
        name proficiency);
    let ability_buffer =
      try Hashtbl.find abilities (String.lowercase ability)
      with Not_found ->
        if not (Load.resource_exists game ability "2DA") then
          failwith (Printf.sprintf
            "ERROR: --extract-kits: kit %s refers to missing ability table %s.2DA"
            name ability);
        let buffer, _ =
          Load.load_resource "extracting a kit ability table" game true
            ability "2DA"
        in
        Hashtbl.add abilities (String.lowercase ability) buffer;
        process_ability buffer;
        buffer
    in
    let profile_name = weapprof.headers.(proficiency) in
    let tfstweap_values = match tfstweap with
    | None -> []
    | Some table ->
        let column = match find_header table name with
        | Some index -> Some index
        | None -> find_header table profile_name
        in
        begin match column with
        | Some index -> column_at table index
        | None ->
            log_and_print
              "Kit.extract: cannot find kit ~%s~ or proficiency profile ~%s~ in %s.2DA; using its default column\n"
              name profile_name table.resource;
            List.map (fun _ -> table.default) table.rows
        end
    in
    let luabbr_value = match luabbr with
    | None -> ""
    | Some table ->
        try
          let row = find_row table name in
          if Array.length row > 1 then row.(1) else table.default
        with Not_found ->
          log_and_print
            "Kit.extract: cannot find row ~%s~ in %s.2DA\n"
            name table.resource;
          fix_me
    in
    { name = name;
      lower = row.(2);
      mixed = row.(3);
      help = row.(4);
      abilities = ability;
      ability_buffer = ability_buffer;
      clasweap = row_or_fix clasweap name;
      weapprof = String.concat " " (name :: column_at weapprof proficiency);
      abclasrq = row_or_fix abclasrq name;
      abclsmod = row_or_fix abclsmod name;
      abdcdsrq = row_or_fix abdcdsrq name;
      abdcscrq = row_or_fix abdcscrq name;
      alignmnt = row_or_fix alignmnt name;
      dualclas = row_or_fix dualclas name;
      include_in = kittable_mentions id;
      unusable = row.(7);
      klass = row.(8);
      luabbr = luabbr_value;
      tfstweap = tfstweap_values; }
  in

  (* Prepare every kit, referenced table and string before creating output. *)
  let kits = List.map prepare_kit selected_rows in
  let spell_files = Hashtbl.fold (fun spell _ files ->
    if Load.resource_exists game spell "SPL" then
      let buffer, _ =
        Load.load_resource "extracting a kit ability" game true spell "SPL"
      in
      (spell, buffer) :: files
    else files) spells []
  in
  let output = Buffer.create 4096 in
  let add format = Printf.bprintf output format in
  List.iter (fun kit ->
    log_and_print "Kit.extract: Processing %s\n" kit.name;
    add "ADD_KIT ~%s~\n" kit.name;
    add "~%s~\n" kit.clasweap;
    add "~%s~\n" kit.weapprof;
    List.iter (fun row -> add "~%s~\n" row)
      [ kit.abclasrq; kit.abclsmod; kit.abdcdsrq; kit.abdcscrq;
        kit.alignmnt; kit.dualclas ];
    add "~%s/%s.2DA~\n" output_dir kit.abilities;
    add "~%s~\n" (String.concat " " kit.include_in);
    add "~%s %s~\n" kit.unusable kit.klass;
    add "~%s~\n" kit.luabbr;
    add "~%s~\n" (String.concat " " kit.tfstweap);
    List.iter (fun string_ref ->
      let index = match int_of_string_opt string_ref with
      | Some value -> value
      | None -> failwith (Printf.sprintf
          "ERROR: --extract-kits: kit %s has invalid string reference ~%s~ in KITLIST.2DA"
          kit.name string_ref)
      in
      let male = Tlk.pretty_print (Load.get_active_dialog game) index in
      let female =
        Tlk.pretty_print_opt (Load.get_active_dialogf_opt game) index
      in
      if female = "" || male = female then add "SAY %s\n" male
      else add "SAY %s %s\n" male female)
      [ kit.lower; kit.mixed; kit.help ];
    add "\n\n/**************************************************************************/\n\n")
    kits;

  List.iter (fun kit ->
    write_file
      (Printf.sprintf "%s/%s.2DA" output_dir kit.abilities)
      kit.ability_buffer) kits;
  List.iter (fun (spell, buffer) ->
    write_file (Printf.sprintf "%s/%s.SPL" output_dir spell) buffer)
    spell_files;
  o (Buffer.contents output);
  Automate.automate game [output_dir] 0 o
