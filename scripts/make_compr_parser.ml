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

let str_of_int32 o i =
  let d = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let c = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let b = Int32.to_int (Int32.logand i 255l) in
  let i = Int32.shift_right_logical i 8 in
  let a = Int32.to_int (Int32.logand i 255l) in
  Printf.fprintf o "\\%03d\\%03d\\%03d\\%03d" d c b a

let output_int o i s =
  str_of_int32 o (Int32.of_int i);
  if !s > 1000 then begin
    output_string o "\\\n";
    s := 0
  end else begin
    incr s
  end

let restructure_table i o name def =
  Printf.fprintf o "%s_val = \"" name;
  let finished = ref false in
  let length = ref 0 in
  let s = ref 0 in
  while not !finished do
    let line = input_line i in
    if String.sub line 2 2 = "|]" then begin
      finished := true;
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
          output_int o ans s;
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
