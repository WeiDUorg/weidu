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
    journal_quest_group : int option ;
  }

let have_quest_group = ref false
let cached_buffer = ref ""

let load_bgee_sql for_what =
  let buff,path = (Load.load_resource for_what
                     (Load.the_game ()) true "bgee" "sql") in
  buff

let extract_quests_section for_what buff =
  let opening = ".*INSERT[ \t\r\n]+INTO[ \t\r\n]+quests[ \t\r\n]+ROWS[ \t\r\n]*([ \t\r\n]*" in
  let body = "\\([ \t]*[0-9]+,.*[ \t\r\n]*\\)+" in
  let closing = ".*)[ \t\r\n]*;.*" in
  (try
    ignore (Str.search_forward (Str.regexp (opening ^ body ^ closing)) buff 0) ;
    let whole = Str.matched_string buff in
    ignore (Str.search_forward (Str.regexp body) whole 0) ;
    Str.matched_string whole ;
  with
    Not_found ->
      failwith (Printf.sprintf
                  "%s was unable to match a quests section in bgee.sql"
                  for_what) ;)

let extract_journals_quests_section for_what buff =
  let opening = ".*INSERT[ \t\r\n]+INTO[ \t\r\n]+journals_quests[ \t\r\n]+ROWS[ \t\r\n]*([ \t\r\n]*" in
  let body = "\\([ \t]*[0-9]+,.*[ \t\r\n]*\\)+" in
  let closing = ".*)[ \t\r\n]*;.*" in
  (try
    ignore (Str.search_forward (Str.regexp (opening ^ body ^ closing)) buff 0) ;
    let whole =Str.matched_string buff in
    ignore (Str.search_forward (Str.regexp body) whole 0) ;
    Str.matched_string whole ;
  with
    Not_found ->
      failwith (Printf.sprintf
                  "%s was unable to match a journals_quests section in bgee.sql"
                  for_what) ;)

let make_quests_record id name strref sc state chapter =
  {quest_id = id;
    quest_name = name;
    quest_strref = strref;
    quest_show_children = sc;
    quest_state = state;
    quest_chapter = chapter;}

let make_journals_record journal_id quest_id state quest_group =
  {journal_id = journal_id;
    journal_quest_id = quest_id;
    journal_state = state;
    journal_quest_group = quest_group;}

let parse buff fmt receiver =
  let buff = Str.global_replace (Str.regexp "[ \t\r\n();]") "" buff in
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
    (fun a b c d e f -> {quest_id = a;
                          quest_name = b;
                          quest_strref = c;
                          quest_show_children = d;
                          quest_state = e;
                          quest_chapter = f}) ;
;;

let parse_journals_quests_section buff =
  let tmp = (Str.split (Str.regexp ",")
               (Str.global_replace (Str.regexp "[ \t\r\n();]") ""
                  (List.hd
                     (Str.split (Str.regexp "\n")
                        (Str.global_replace (Str.regexp "\r\n?") "\n" buff))))) in
  if (List.length tmp) = 3 then begin
    have_quest_group := false ;
    parse buff "%i,%i,%i," (fun a b c -> {journal_id = a;
                                           journal_quest_id = b;
                                           journal_state = c;
                                           journal_quest_group = None})
  end
  else begin
    have_quest_group := true ;
    parse buff "%i,%i,%i,%i," (fun a b c d -> {journal_id = a;
                                                journal_quest_id = b;
                                                journal_state = c;
                                                journal_quest_group = Some d})
  end ;
;;

let get_quests_data for_what =
  let buff = load_bgee_sql for_what in
  ignore (cached_buffer := buff) ;
  let quests = extract_quests_section for_what buff in
  let journals_quests = extract_journals_quests_section for_what buff in
  let parsed_quests = parse_quests_section quests in
  let parsed_journals_quests = parse_journals_quests_section journals_quests in
  parsed_quests,parsed_journals_quests

let sort_quests quests =
  quests

let sort journals_quests journals =
  journals

let set_quests_data (quests,journals_quests) =
  let quests = sort_quests quests in
  let journals_quests = sort_journals_quest journals_quests in
  ()

(* sort the lists *)
(* make a string out of the list of records *)
(* regexp-swap the list back into the bgee.sql buffer *)
(* write the modified buffer to disk *)
