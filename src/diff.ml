(* Diff routines using xdiff library *)
open Util
open Xdiff
open Str

(* Usage: new_buff, bad_chunks, app_chunks = do_patch orig_buff patch_buff

   Applies patch_buff to orig_buff to create new_buff.  The number
   of chunks that couldn't be applied is returned in bad_chunks.
   The number of chunks that are already applied is returned in app_chunks.
 *)

let fixdouble s =
  Str.global_replace (Str.regexp "\r") "" s

let fixnl s =
  let s = fixdouble s in
  if s.[String.length s - 1] = '\n' then s else s ^ "\n"

let do_patch orig_buff patch_buff vb = begin
  let orig_buff = fixnl orig_buff in
  let patch_buff = fixnl patch_buff in
  let newf, rejf = Xdiff.patch orig_buff patch_buff in begin
    (* RE which deliminates rejected chunks from patch *)
    let delimre = Str.regexp "@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" in
    let rej_chunks = Str.full_split delimre rejf in
    let bad_chunks = ref 0 in
    let app_chunks = ref 0 in
    let rec check_chunk = function
        [] -> ()
      | Delim(stra) :: Text(strb) :: rem ->
          let chunk = stra ^ strb in
          let newff, rejff = Xdiff.revpatch orig_buff chunk in
          let begre = Str.regexp "^" in
          let chunk = Str.global_replace begre "    " chunk in
          let lfre = Str.regexp_string "\r" in
          let chunk = Str.global_replace lfre "" chunk in
          let _ = 
            if (rejff <> "") then begin
              incr bad_chunks;
              log_or_print "!!! FAILED PATCH CHUNK:\n";
              if vb then log_or_print "%s\n" chunk;
            end
            else begin
              incr app_chunks;
              log_or_print "*** CHUNK ALREADY APPLIED:\n";
              if vb then log_or_print "%s\n" chunk;
            end
          in
          check_chunk rem;
      | _ -> (log_and_print "do_patch: chunk from Xdiff.diff has wrong format.";
              raise Not_found)
    in
    check_chunk rej_chunks;
    if !bad_chunks > 0 then begin
      log_and_print "The following patch:\n\n%s\n\nfailed on the following text:\n\n%s\n\n" patch_buff orig_buff
    end;
    (newf, !bad_chunks, !app_chunks) ;
  end
end

let get_patch orig_buff new_buff ncont =begin
  let orig_buff = fixnl orig_buff in
  let new_buff = fixnl new_buff in
  try
    fixdouble (Xdiff.diff orig_buff new_buff ncont)
  with e -> begin
    log_and_print "create_patch: couldn't create patch: %s\n" (printexc_to_string e);
    raise Not_found
  end
end

let compare_rt (o : ('a, out_channel, unit) format -> 'a) old_buff new_buff =
  let lines_i = Array.of_list (Str.split many_newline_or_cr_regexp old_buff) in
  let lines_o = Array.of_list (Str.split many_newline_or_cr_regexp new_buff) in
  let cnt_i = Array.length lines_i in
  let cnt_o = Array.length lines_o in
  let i = ref 0 in
  let j = ref 0 in
  let saved_i = Hashtbl.create 5 in
  let saved_o = Hashtbl.create 5 in
  let saved_a = Hashtbl.create 5 in
  let mismatch = Hashtbl.create 5 in
  while !i < cnt_i && !i < cnt_o do
    let l1,l2,i',j' = lines_i.(!i),lines_o.(!j),1,1 in
    i := !i + i';
    j := !j + j';
    if l1 <> l2 then
      o "REPLACE_TEXTUALLY CASE_SENSITIVE EXACT_MATCH ~%s~ ~%s~" l1 l2
    ;
    if Hashtbl.mem saved_i l1 && not (Hashtbl.mem saved_a (l1,l2)) then
      Hashtbl.add mismatch l1 true;
    o "%s%s\n" "" "";
    Hashtbl.add saved_i l1 true;
    Hashtbl.add saved_o l2 true;
    Hashtbl.add saved_a (l1,l2) true;
  done;
  if Hashtbl.length mismatch <> 0 then o "\n%s%s" "" "";
  Hashtbl.iter (fun k v ->
    o "// will mismatch when a source line is ~%s~\n%s" k ""
  ) mismatch
;;
