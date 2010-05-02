open Util

type sav = sav_file Queue.t

and sav_file =
    {
     filename : string;
     contents  : string;
   }

let sav_of_str str =
  match Str.string_before str 8 with
  | "SAV V1  "
  | "SAV V1.0" -> begin
      let pos = ref 8 in
      let acc = Queue.create () in
      let i i3 = Int32.to_int i3 in
      let i3 i = Int32.of_int i in
      try
	while true do
	  let name_l = i (int32_of_str_off str !pos) in
	  pos := !pos + 4;
	  let name = get_string_of_size str !pos name_l in
	  pos := !pos + name_l;
	  let u_l = i (int32_of_str_off str !pos) in
	  pos := !pos + 4;
	  let c_l = i (int32_of_str_off str !pos) in
	  pos := !pos + 4;
	  let com_str = String.sub str !pos c_l in
	  pos := !pos + c_l;
	  let unc_str = Cbif.uncompress com_str 0 c_l u_l in
	  Queue.push {
	  filename = name;
	  contents = unc_str;
	  } acc;
	  if !debug_ocaml then log_and_print "%s:\tu %d\tc %d\n" name u_l c_l;
	done;
	Queue.create ()
      with
      | Invalid_argument "String.sub"
      | Invalid_argument "index out of bounds" -> acc
  end
  | _ -> failwith "Unknown SAV signature"
;;

let str_of_sav lvl sav =
  let b = Buffer.create 1000000 in
  Buffer.add_string b "SAV V1.0";
  while not (Queue.is_empty sav) do
    let file = Queue.pop sav in
    Buffer.add_string b (str_of_int (String.length file.filename + 1));
    Buffer.add_string b file.filename;
	Buffer.add_char b '\000';
    Buffer.add_string b (str_of_int (String.length file.contents));
    let c = Cbif.compress (Int32.to_int lvl) file.contents 0 (String.length file.contents) in
    Buffer.add_string b (str_of_int (String.length c));
    Buffer.add_string b c;
    if !debug_ocaml then log_and_print "%s:\tu %d\tc%d\n" file.filename
	(String.length file.contents) (String.length c);
  done;
  if !debug_ocaml then log_and_print "str_of_sav done\n";
  let x = Buffer.contents b in
  if !debug_ocaml then log_and_print "Buffer flushed\n";
  x
;;
