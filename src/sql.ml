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
  }

(* Record holding entries from the journals_quests table *)
and bgee_journal = {
    journal_id : int ;
    journal_quest_id : int ;
    journal_state : int ;
    mutable journal_quest_group : int option ;
    journal_date : string option ;
  }

(* script_version = 1: 3 columns
 * script_version = 2: 4 columns, quest_group
 * script_version = 3: 5 columns, quest_group && date
*)
let script_version = ref 1
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
      (Printf.sprintf "%s was unable to match a journals_quests section in BGEE.SQL"
         for_what) ;)

let make_quests_record id name strref sc state chapter () =
  {quest_id = id;
    quest_name = name;
    quest_strref = strref;
    quest_show_children = sc;
    quest_state = state;
    quest_chapter = chapter;}

let make_journals_record journal_id quest_id state ?(quest_group = None) ?(date = None) () =
  {journal_id = journal_id;
    journal_quest_id = quest_id;
    journal_state = state;
    journal_quest_group = (match !script_version with
      1 -> None
    | 2 | 3 -> quest_group
    | _ -> failwith "Internal error: bad version for BGEE.SQL");
    journal_date = (match !script_version with
      1 | 2 -> None
    | 3 -> date
    | _ -> failwith "Internal error: bad version for BGEE.SQL");}

let strip_comments buff =
  Str.global_replace (Str.regexp "[ \t]*//.*") "" buff

let compact_string buff =
  Str.global_replace (Str.regexp "[ \t\r\n();]") "" buff

let parse buff fmt receiver =
  let buff = (compact_string (strip_comments buff)) in
  let chan = Scanf.Scanning.from_string buff in
  let get_record () =
    Scanf.bscanf chan fmt receiver
  in
  let acc = ref [] in
  (try
    while true do
      acc := List.append [ (get_record ()) ] !acc ;
    done
  with End_of_file -> ()) ;
  !acc

let parse_quests_section buff =
  parse buff "%i,'%s@',%i,%i,%i,%i,"
    (fun a b c d e f -> make_quests_record a b c d e f ())

let parse_journals_quests_section buff =
  let tmp = (Str.split (Str.regexp ",")
               (compact_string
                  (strip_comments
                     (List.hd
                        (Str.split (Str.regexp "\n")
                           (Str.global_replace (Str.regexp "\r\n?") "\n" buff)))))) in
  (match (List.length tmp) with
    3 -> script_version := 1 ; parse buff "%i,%i,%i,"
        (fun a b c -> make_journals_record a b c ())
  | 4 -> script_version := 2 ; parse buff "%i,%i,%i,%i,"
        (fun a b c d -> make_journals_record a b c ~quest_group:(Some d) ())
  | 5 -> script_version := 3 ; parse buff "%i,%i,%i,%i,'%s@',"
        (fun a b c d e -> make_journals_record a b c ~quest_group:(Some d) ~date:(Some e) ())
  | _ -> failwith "Cannot parse BGEE.SQL due to unrecognised format")

let get_quests_data for_what =
  let buff = load_bgee_sql for_what in
  ignore (cached_buffer := buff) ;
  let quests = extract_quests_section for_what buff in
  let journals_quests = extract_journals_quests_section for_what buff in
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
     (string_of_int r.quest_strref) ^ "," ^ (string_of_int r.quest_show_children) ^
     "," ^ (string_of_int r.quest_state) ^ "," ^ (string_of_int r.quest_chapter) ^ ",\n")
      ) "" quests in
  (open_statement ^ body ^ close_statement)

let journals_to_string journals =
  let open_statement = "INSERT INTO journals_quests ROWS\n(\n" in
  let close_statement = ");" in
  let body = List.fold_left (fun acc r ->
    (acc ^ "\t" ^ (string_of_int r.journal_id) ^ "," ^ (string_of_int r.journal_quest_id)
     ^ "," ^ (string_of_int r.journal_state) ^
     (match r.journal_quest_group with
       None -> ""
     | Some g -> ("," ^ (string_of_int g))) ^
     (match r.journal_date with
       None -> ",\n"
     | Some s -> (",'" ^ s ^ "',\n"))
       )) "" journals in
  (open_statement ^ body ^ close_statement)

let swap_in_new buff quests journals =
  let buff = Str.global_replace (Str.regexp quests_match_string) quests buff in
  let buff = Str.global_replace (Str.regexp journals_match_string) journals buff in
  let buff = Str.global_replace (Str.regexp "\r\n?") "\n" buff in
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
      failwith (Printf.sprintf "Unable to write to BGEE.SQL because: %s" (printexc_to_string e)))
  else
    log_only "BGEE.SQL was not saved because it did not change\n" ;
  ()
