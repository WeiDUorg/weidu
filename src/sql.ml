open BatteriesInit
open Util

(* Record holding entries from the quests table *)
type bgee_quest = {
    quest_id : int ;
    quest_name : string ;
    quest_strref : int ;
    quest_show_children : int ;
    quest_state : int ;
    quest_chapter : int ;
    quest_MC1 : int option ;
  }

(* Record holding entries from the journals_quests table *)
and bgee_journal = {
    journal_id : int ;
    journal_quest_id : int ;
    journal_state : int ;
    mutable journal_quest_group : int option ;
    journal_date : string option ;
    journal_MC1 : int option ;
  }
(* script_version = ONE: quests: 6 columns; journals: 3 columns
 * script_version = TWO: quests: 6 columns; journals: 4 columns, quest_group
 * script_version = THREE: quests: 6 columns;
 * journals: 5 columns, quest_group && date
 * script_version = FOUR: quests: 7 columns, quest_MC1;
 * journals: 6 columns, quest_group && date && journal_MC1
*)
and bgee_script_version = ONE | TWO | THREE | FOUR

let script_version = ref ONE
let cached_buffer = ref ""

let load_bgee_sql for_what =
  let buff,path = (Load.load_resource for_what
                     (Load.the_game ()) true "bgee" "sql") in
  buff

let body_match_string = "\\([ \t]*[0-9]+,.+[\r\n]+\\)+"

let quests_match_string =
  (".*INSERT[ \t\r\n]+INTO[ \t\r\n]+quests[ \t\r\n]+ROWS[ \t\r\n]*([ \t\r\n]*" ^
    body_match_string ^
   ".*)[ \t\r\n]*;.*")

let journals_match_string =
  (".*INSERT[ \t\r\n]+INTO[ \t\r\n]+journals_quests[ \t\r\n]+ROWS[ \t\r\n]*([ \t\r\n]*" ^
   body_match_string ^
   ".*)[ \t\r\n]*;.*")

let extract_quests_section for_what buff =
  (try
    ignore (Str.search_forward (Str.regexp quests_match_string) buff 0) ;
    let whole = Str.matched_string buff in
    ignore (Str.search_forward (Str.regexp body_match_string) whole 0) ;
    Str.matched_string whole ;
  with Not_found -> failwith
      (Printf.sprintf "%s was unable to match a quests section in BGEE.SQL"
         for_what) ;)

let extract_journals_quests_section for_what buff =
  (try
    ignore (Str.search_forward (Str.regexp journals_match_string) buff 0) ;
    let whole = Str.matched_string buff in
    ignore (Str.search_forward (Str.regexp body_match_string) whole 0) ;
    Str.matched_string whole
  with Not_found -> failwith
      (Printf.sprintf
         "%s was unable to match a journals_quests section in BGEE.SQL"
         for_what) ;)

let make_quests_record id name strref sc state chapter ?(quest_MC1 = None) () =
  {quest_id = id;
   quest_name = name;
   quest_strref = strref;
   quest_show_children = sc;
   quest_state = state;
   quest_chapter = chapter;
   quest_MC1 = (match !script_version with
   | ONE | TWO | THREE -> None
   | FOUR -> quest_MC1);
 }

let make_journals_record journal_id quest_id state ?(quest_group = None)
    ?(date = None) ?(journal_MC1 = None) () =
  {journal_id = journal_id;
   journal_quest_id = quest_id;
   journal_state = state;
   journal_quest_group = (match !script_version with
   | ONE -> None
   | TWO | THREE | FOUR -> quest_group);
   journal_date = (match !script_version with
   | ONE | TWO -> None
   | THREE | FOUR -> date);
   journal_MC1 = (match !script_version with
   | ONE | TWO | THREE -> None
   | FOUR -> journal_MC1);
 }

let strip_comments buff =
  Str.global_replace (Str.regexp "[ \t]*//.*") "" buff

let compact_string buff =
  Str.global_replace (Str.regexp "[ \t\r\n();]+") "" buff

let normalise_newlines buff =
  Str.global_replace (Str.regexp "\r\n?") "\n" buff

let strip_empty_lines buff =
  Str.global_replace (Str.regexp "^[ \t]*\n") "" buff

let get_first_line buff =
  List.hd (Str.split (Str.regexp "\n") buff)

let ensure_terminal_comma buff =
  if not (Str.string_match (Str.regexp ".+,$") buff 0) then
    buff ^ ","
  else buff

let sanitise_string buff =
  (ensure_terminal_comma
     (compact_string
        (strip_comments buff)))

let parse buff fmt receiver =
  let buff = sanitise_string buff in
  let chan = Scanf.Scanning.from_string buff in
  let get_record () =
    Scanf.bscanf chan fmt receiver
  in
  let acc = ref [] in
  (try
    while not (Scanf.Scanning.end_of_input chan) do
      acc := List.append [ (get_record ()) ] !acc ;
    done
  with e -> failwith (Printf.sprintf "Failure to parse BGEE.SQL [%s]"
                        (printexc_to_string e))) ;
  !acc

let parse_quests_section buff =
  (match !script_version with
  | ONE | TWO | THREE -> parse buff "%i,'%s@',%i,%i,%i,%i,"
        (fun a b c d e f -> make_quests_record a b c d e f ())
  | FOUR -> parse buff "%i,'%s@',%i,%i,%i,%i,%i,"
        (fun a b c d e f g -> make_quests_record a b c d e f
            ~quest_MC1:(Some g) ()))

let parse_journals_quests_section buff =
  (match !script_version with
  | ONE -> parse buff "%i,%i,%i,"
        (fun a b c -> make_journals_record a b c ())
  | TWO -> parse buff "%i,%i,%i,%i,"
        (fun a b c d -> make_journals_record a b c
            ~quest_group:(Some d) ())
  | THREE -> parse buff "%i,%i,%i,%i,'%s@',"
        (fun a b c d e -> make_journals_record a b c
            ~quest_group:(Some d) ~date:(Some e) ())
  | FOUR -> parse buff "%i,%i,%i,%i,'%s@',%i,"
        (fun a b c d e f -> make_journals_record a b c
            ~quest_group:(Some d) ~date:(Some e) ~journal_MC1:(Some f) ()))

let get_column_count buff =
  (Str.split (Str.regexp ",")
     (compact_string
        (get_first_line
           (strip_empty_lines
              (strip_comments
                 (normalise_newlines buff))))))

let determine_script_version quests journals_quests =
  let quests_columns = List.length (get_column_count quests) in
  let journals_columns = List.length (get_column_count journals_quests) in
  (match quests_columns, journals_columns with
  | 6, 3 -> ONE
  | 6, 4 -> TWO
  | 6, 5 -> THREE
  | 7, 6 -> FOUR
  | _, _ -> failwith "Cannot parse BGEE.SQL due to unrecognised format")

let get_quests_data for_what =
  let buff = load_bgee_sql for_what in
  ignore (cached_buffer := buff) ;
  let quests = extract_quests_section for_what buff in
  let journals_quests = extract_journals_quests_section for_what buff in
  let version = determine_script_version quests journals_quests in
  ignore (script_version := version) ;
  let parsed_quests = parse_quests_section quests in
  let parsed_journals_quests = parse_journals_quests_section journals_quests in
  parsed_quests,parsed_journals_quests

let sort_quests quests =
  (* first by id, then by strref *)
  let cmp_by_strref r1 r2 =
    compare r1.quest_strref r2.quest_strref
  in
  let cmp_by_id r1 r2 =
    compare r1.quest_id r2.quest_id
  in
  (List.stable_sort cmp_by_id (List.sort cmp_by_strref quests))

let sort_journals_quests journals =
  (* first by quest_id, then by journal_id *)
  let cmp_by_journal r1 r2 =
    compare r1.journal_id r2.journal_id
  in
  let cmp_by_quest r1 r2 =
    compare r1.journal_quest_id r2.journal_quest_id
  in
  (List.stable_sort cmp_by_quest (List.sort cmp_by_journal journals))

let quests_to_string quests =
  let open_statement = "INSERT INTO quests ROWS\n(\n" in
  let close_statement = ");" in
  let body = List.fold_left (fun acc r ->
    (acc ^ "\t" ^ (string_of_int r.quest_id) ^ ",'" ^ r.quest_name ^ "'," ^
     (string_of_int r.quest_strref) ^ "," ^
     (string_of_int r.quest_show_children) ^
     "," ^ (string_of_int r.quest_state) ^ "," ^
     (string_of_int r.quest_chapter) ^
     (match r.quest_MC1 with
     | None -> ""
     | Some i -> "," ^ (string_of_int i)) ^ ",\n")) "" quests in
  (open_statement ^ body ^ close_statement)

let journals_to_string journals =
  let open_statement = "INSERT INTO journals_quests ROWS\n(\n" in
  let close_statement = ");" in
  let body = List.fold_left (fun acc r ->
    (acc ^ "\t" ^ (string_of_int r.journal_id) ^ "," ^
     (string_of_int r.journal_quest_id)
     ^ "," ^ (string_of_int r.journal_state) ^
     (match r.journal_quest_group with
     | None -> ""
     | Some g -> ("," ^ (string_of_int g))) ^
     (match r.journal_date with
     | None -> ""
     | Some s -> (",'" ^ s ^ "'")) ^
     (match r.journal_MC1 with
     | None -> ""
     | Some i -> "," ^ (string_of_int i)) ^ ",\n")) "" journals in
  (open_statement ^ body ^ close_statement)

let swap_in_new buff quests journals =
  let buff = Str.global_replace (Str.regexp quests_match_string) quests buff in
  let buff = Str.global_replace (Str.regexp journals_match_string)
      journals buff in
  let buff = normalise_newlines buff in
  buff

let set_quests_data (quests,journals_quests) =
  let quests = sort_quests quests in
  let journals_quests = sort_journals_quests journals_quests in
  let quests_string = quests_to_string quests in
  let journals_string = journals_to_string journals_quests in
  let new_buff = swap_in_new !cached_buffer quests_string journals_string in
  if new_buff <> !cached_buffer then
    (try
      Stats.time "saving files" (fun () ->
        let chan = open_for_writing "override/bgee.sql" true in
        output_string chan new_buff ;
        close_out chan) () ;
    with e ->
      failwith (Printf.sprintf "Unable to write to BGEE.SQL because: %s"
                  (printexc_to_string e)))
  else
    log_only "BGEE.SQL was not saved because it did not change\n" ;
  ()
