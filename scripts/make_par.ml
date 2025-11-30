let main () =
  let i = Printf.sprintf "%s/trealparserin.ml" Sys.argv.(1) in
  let o = Printf.sprintf "%s/trealparser.ml" Sys.argv.(1) in
  let i = open_in i in
  let o = open_out_bin o in
  let   gotoTable_reg = Str.regexp " *gotoTable = \\[\\| *(\\* [0-9]* elements \\*)" in
  let actionTable_reg = Str.regexp " *actionTable = \\[\\| *(\\* [0-9]* elements \\*)" in
  try
    let in_array = ref false in
    let array_length = ref 0 in
    let sub_cnt = ref 0 in
    let this_arr = ref "" in
    while true do
      let line = input_line i in
      if Str.string_match gotoTable_reg line 0 || Str.string_match actionTable_reg line 0 then begin
	in_array :=  true;
	if Str.string_match gotoTable_reg line 0 then
	  this_arr := "gotoTable"
	else
	  this_arr := "actionTable"
	      ;
	sub_cnt := 0;
	array_length := 0;
	Printf.fprintf o "%s = (\n" !this_arr;
	Printf.fprintf o "let %s%d = [|" !this_arr !sub_cnt;
      end else begin
	if line = "  |];" && !in_array then begin
	  in_array :=  false;
	  let buffer = Buffer.create 80 in
	  let par_cnt = ref 0 in
	  let rec doIt i =
	    if i = !sub_cnt then begin
	      Buffer.add_string buffer (Printf.sprintf " %s%d" !this_arr i);
	      for j = 0 to !par_cnt - 1 do Buffer.add_char buffer ')' done
	    end else begin
	      if i = !sub_cnt - 1 then begin
		Buffer.add_string buffer ( Printf.sprintf "Array.append %s%d" !this_arr i);
		doIt (i+1)
	      end else begin
		Buffer.add_string buffer (Printf.sprintf "Array.append %s%d (" !this_arr i);
		incr par_cnt;
		doIt (i+1)
	      end
	    end
	  in
	  doIt 0;
	  sub_cnt := 0;
	  array_length := 0;
	  Printf.fprintf o "  |] in\n";
	  Printf.fprintf o "%s\n" (Buffer.contents buffer);
	  Printf.fprintf o ");\n";
	end else begin
	  if !in_array then begin
	    let parts = Str.split (Str.regexp ";") line in
	    array_length := !array_length + (List.length parts);
	    if !array_length > 60000 then begin
	      incr sub_cnt;
	      array_length := (List.length parts);
	      Printf.fprintf o "  |] in\n";
	      Printf.fprintf o "let %s%d = [|\n" !this_arr !sub_cnt;
	      Printf.fprintf o "%s\n" line
	    end else Printf.fprintf o "%s\n" line
	  end else
	    Printf.fprintf o "%s\n" line
	end;
      end;
    done;
  with End_of_file -> (close_in i; close_out o)
;;

main ()
