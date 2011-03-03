let zero = Char.code '0'

let read_num str pos =
  try
    while str.[!pos] <> '-' && (str.[!pos] < '0' || str.[!pos] > '9') do
      incr pos
    done;
    let isneg = if str.[!pos] = '-' then (incr pos; true) else false in
    let res = ref 0 in
    while str.[!pos] >= '0' && str.[!pos] <= '9' do
      res := !res * 10 + Char.code str.[!pos] - zero;
      incr pos;
    done;
    if isneg then 0 - !res else !res
  with e -> min_int
;;

let output o i s =
  let byte0 = i land 0xff in
  let byte1 = (i land 0xff00) lsr 8 in
  Printf.fprintf o "\\%03d\\%03d" byte0 byte1;
  if !s > 1000 then begin
    output_string o "\\\n";
    s := 0
  end else begin
    incr s
  end


let output_int o i s =
  if i < 0 - 0x8000 || i > 0x4000 then failwith "output_int: WRITE_SSHORT out of bounds\n";
  output o i s
  
let output_def o i s =
  if i >= 0x4000 || i <= 0 then failwith "output_def: WRITE_SSHORT out of bounds\n";
  let i = i + 0x4000 in
  output o i s

let restructure_table i o name def =
  Printf.fprintf o "%s_val = \"" name;
  let finished = ref false in
  let val_cnt = ref 0 in
  let length = ref 0 in
  let s = ref 0 in
  while not !finished do
    let line = input_line i in
    if String.sub line 2 2 = "|]" then begin
      finished := true;
      if !val_cnt > 0 then begin
        output_def o !val_cnt s
      end;
      output_string o "\";\n";
      Printf.fprintf o "%s_use = Array.make %d %d;\n" name (!length) def;
    end else begin
      let pos = ref 0 in
      while line.[!pos] <> ')' do incr pos done;
      let scanned = ref false in
      while not !scanned do
        let ans = read_num line pos in
        if ans = min_int then scanned := true else begin
          incr length;
          if ans = def then begin
            incr val_cnt
          end else begin
            if !val_cnt > 0 then begin
              output_def o !val_cnt s;
              val_cnt := 0;
            end;
            output_int o ans s;
          end
        end
      done;
    end
  done

let main () =
  let i = open_in      Sys.argv.(1) in
  let o = open_out_bin Sys.argv.(2) in
  try
    while true do
      let line = input_line i in
      let sub11 = try String.sub line 2 11 with Invalid_argument _ -> "" in
      begin match sub11 with
        | "actionTable" ->
          restructure_table i o "actionTable" 0
        | "gotoTable =" ->
          restructure_table i o "gotoTable" 65535
        | _ ->
          output_string o line;
          output_string o "\n";
      end;
    done
  with End_of_file ->
    close_in i;
    close_out o
;;

try
  main ()
with e ->
  print_endline (Printexc.to_string e)
