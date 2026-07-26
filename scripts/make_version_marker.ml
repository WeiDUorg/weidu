let read_file filename =
  let channel = open_in_bin filename in
  let length = in_channel_length channel in
  let contents = really_input_string channel length in
  close_in channel ;
  contents

let find_version contents =
  let regexp =
    Str.regexp "let[ \t]+version[ \t]*=[ \t]*\"\\([0-9]+\\)\"" in
  try
    ignore (Str.search_forward regexp contents 0) ;
    let version = Str.matched_group 1 contents in
    let next = Str.match_end () in
    (try
      ignore (Str.search_forward regexp contents next) ;
      failwith "multiple version definitions"
    with Not_found -> version)
  with Not_found ->
    failwith "version definition not found"

let () =
  if Array.length Sys.argv <> 3 then
    failwith "usage: make_version_marker.ml VERSION_ML OUTPUT_ML" ;
  let version = find_version (read_file Sys.argv.(1)) in
  let output = open_out_bin Sys.argv.(2) in
  Printf.fprintf output
    "(* Generated from %s. Do not edit. *)\n\
     let marker = \"\\000WEIDU_AUTOUPDATE_VERSION:%s\\000\"\n"
    Sys.argv.(1) version ;
  close_out output
