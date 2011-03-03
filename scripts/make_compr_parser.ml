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

let main () =
  let i = open_in      Sys.argv.(1) in
  let o = open_out_bin Sys.argv.(2) in
  try
    let seeking = ref true in
    let vals = Buffer.create 5000 in
    let cnts = Buffer.create 5000 in
    let val_cnt = ref 0 in
    let subarr_cnt = ref 0 in
    let val_old = ref min_int in
    while true do
      let line = input_line i in
      if !seeking then begin
        List.iter (fun (str, len) ->
          try if String.sub line 2 len = str then begin
            seeking := false;
            Printf.fprintf o "%s_use = [| |];\n" str;
            Printf.bprintf vals "%s_val = [| [|\n" str;
            Printf.bprintf cnts "%s_cnt = [| [|\n" str;
          end
          with Invalid_argument _ -> ()
        ) ["actionTable", 11; "gotoTable",9];
        if !seeking then begin
          output_string o line;
          output_string o "\n";
        end
      end else begin
        if String.sub line 2 2 = "|]" then begin
          seeking := true;
          if !val_cnt > 0 then begin
            Printf.bprintf vals "%d; " !val_old;
            Printf.bprintf cnts "%d; " !val_cnt
          end;
          val_old := min_int;
          val_cnt := 0;
          Buffer.output_buffer o vals;
          output_string o "|] |];\n";
          Buffer.output_buffer o cnts;
          output_string o "|] |];\n";
          Buffer.reset vals;
          Buffer.reset cnts;
        end else begin
          let pos = ref 0 in
          while line.[!pos] <> ')' do incr pos done;
          let finished = ref false in
          while not !finished do
            let ans = read_num line pos in
            if ans = min_int then finished := true else begin
              if ans = !val_old then incr val_cnt else begin
                if !val_cnt > 0 then begin
                  Printf.bprintf vals "%d; " !val_old;
                  Printf.bprintf cnts "%d; " !val_cnt;
                  if !subarr_cnt = 100 then begin
                    Buffer.add_string vals "|]; [|";
                    Buffer.add_string cnts "|]; [|";
                    subarr_cnt := 0;
                  end else incr subarr_cnt
                end;
                val_cnt := 1;
                val_old := ans;
              end
            end
          done;
        end
      end
    done
  with End_of_file ->
    close_in i;
    close_out o
;;

try
  main ()
with e ->
  print_endline (Printexc.to_string e)
