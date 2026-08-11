(*
 * PCRE2 support for explicitly opted-in TP2 regular expressions.
 *
 * WeiDU's established regexp language is OCaml Str.  This module deliberately
 * does not attempt to translate or replace that language: callers select it
 * only after the parser has seen PCRE2_REGEXP.  Keeping the implementation in
 * a separate module also prevents PCRE2's match data from depending on Str's
 * process-global "last match" state.
 *)

type code

external compile_raw : string -> bool -> code = "weidu_pcre2_compile"

external exec_raw :
  code -> string -> int -> bool -> bool -> int array option
  = "weidu_pcre2_exec"

external capture_names_raw : code -> (string * int) array
  = "weidu_pcre2_capture_names"

external utf_raw : code -> bool = "weidu_pcre2_utf"

type regexp = {
  code : code;
  capture_names : (string * int) array;
  utf : bool;
}

type match_result = {
  regexp : regexp;
  subject : string;
  offsets : int array;
}

let compile ~case_sensitive pattern =
  let code = compile_raw pattern case_sensitive in
  { code; capture_names = capture_names_raw code; utf = utf_raw code }

let search ?(anchored = false) ?(notempty_at_start = false)
    regexp subject start =
  match exec_raw regexp.code subject start anchored notempty_at_start with
  | None -> None
  | Some offsets -> Some { regexp; subject; offsets }

let capture_count result = (Array.length result.offsets / 2) - 1

let valid_group_number result number =
  number >= 0 && ((number * 2) + 1) < Array.length result.offsets

let group result number =
  if not (valid_group_number result number) then
    invalid_arg (Printf.sprintf "PCRE2 capture group %d does not exist" number)
  else
    let first = result.offsets.(number * 2) in
    let last = result.offsets.((number * 2) + 1) in
    if first < 0 || last < 0 then None
    else Some (String.sub result.subject first (last - first))

let named_group result name =
  let found = ref false in
  let value = ref None in
  Array.iter
    (fun (candidate, number) ->
      if !value = None && candidate = name then begin
        found := true;
        value := group result number
      end)
    result.regexp.capture_names;
  if not !found then
    invalid_arg (Printf.sprintf "PCRE2 named capture group %s does not exist" name)
  else
    !value

let named_groups result =
  let seen = Hashtbl.create (Array.length result.regexp.capture_names) in
  let groups = ref [] in
  Array.iter
    (fun (name, _) ->
      if not (Hashtbl.mem seen name) then begin
        Hashtbl.add seen name ();
        groups := (name, named_group result name) :: !groups
      end)
    result.regexp.capture_names;
  List.rev !groups

let match_start result = result.offsets.(0)
let match_end result = result.offsets.(1)

let advance_after_empty regexp subject offset =
  let subject_length = String.length subject in
  if offset >= subject_length then offset
  else if not regexp.utf then offset + 1
  else begin
    let next = ref (offset + 1) in
    (* PCRE2 has already validated the UTF-8 subject. Continuation bytes have
       the binary prefix 10, so skip them to the next code-point boundary. *)
    while !next < subject_length &&
          (Char.code subject.[!next] land 0xc0) = 0x80 do
      incr next
    done;
    !next
  end

let add_capture buffer result number =
  match group result number with
  | None -> ()
  | Some value -> Buffer.add_string buffer value

let add_named_capture buffer result name =
  match named_group result name with
  | None -> ()
  | Some value -> Buffer.add_string buffer value

let all_digits value =
  let rec loop index =
    index = String.length value ||
    (match value.[index] with
    | '0' .. '9' -> loop (index + 1)
    | _ -> false)
  in
  String.length value > 0 && loop 0

(*
 * PCRE2 replacements use $0, $1, ... and ${name}; $$ emits a literal dollar
 * sign.  Captures that exist but did not participate expand to the empty
 * string.  Invalid capture references are errors rather than silent data
 * corruption.
 *)
let expand_replacement result replacement =
  let length = String.length replacement in
  let buffer = Buffer.create length in
  let rec loop index =
    if index >= length then ()
    else if replacement.[index] <> '$' then begin
      Buffer.add_char buffer replacement.[index];
      loop (index + 1)
    end else if index + 1 >= length then begin
      Buffer.add_char buffer '$';
      loop (index + 1)
    end else
      match replacement.[index + 1] with
      | '$' ->
          Buffer.add_char buffer '$';
          loop (index + 2)
      | '0' .. '9' ->
          let last = ref (index + 2) in
          while !last < length &&
                replacement.[!last] >= '0' && replacement.[!last] <= '9' do
            incr last
          done;
          let number = int_of_string
              (String.sub replacement (index + 1) (!last - index - 1)) in
          add_capture buffer result number;
          loop !last
      | '{' ->
          let closing = try String.index_from replacement (index + 2) '}'
            with Not_found ->
              invalid_arg "unterminated ${...} PCRE2 replacement reference" in
          let reference =
            String.sub replacement (index + 2) (closing - index - 2) in
          if reference = "" then
            invalid_arg "empty ${...} PCRE2 replacement reference"
          else if all_digits reference then
            add_capture buffer result (int_of_string reference)
          else
            add_named_capture buffer result reference;
          loop (closing + 1)
      | _ ->
          (* A dollar sign has no special meaning unless it starts one of the
             documented forms above.  This keeps ordinary TP2 text intact. *)
          Buffer.add_char buffer '$';
          loop (index + 1)
  in
  loop 0;
  Buffer.contents buffer

let replace_match result replacement =
  let first = match_start result in
  let last = match_end result in
  let expanded = expand_replacement result replacement in
  let buffer = Buffer.create
      (String.length result.subject - (last - first) + String.length expanded) in
  Buffer.add_substring buffer result.subject 0 first;
  Buffer.add_string buffer expanded;
  Buffer.add_substring buffer result.subject last
    (String.length result.subject - last);
  Buffer.contents buffer

(*
 * PCRE2's documented global-matching algorithm retries an empty match at the
 * same offset with ANCHORED|NOTEMPTY_ATSTART.  If that fails, it advances one
 * character. WeiDU buffers are byte strings, but when a pattern explicitly
 * enables UTF mode the retry advances to the next UTF-8 code-point boundary.
 *)
let iter_matches regexp subject callback =
  let subject_length = String.length subject in
  let rec loop start retry_nonempty =
    let result = search ~anchored:retry_nonempty
        ~notempty_at_start:retry_nonempty regexp subject start in
    match result with
    | Some matched ->
        callback matched;
        let first = match_start matched in
        let last = match_end matched in
        loop last (first = last)
    | None when retry_nonempty && start < subject_length ->
        loop (advance_after_empty regexp subject start) false
    | None -> ()
  in
  loop 0 false

let global_replace regexp replacement subject =
  let buffer = Buffer.create (String.length subject) in
  let copied_until = ref 0 in
  iter_matches regexp subject (fun matched ->
    let first = match_start matched in
    let last = match_end matched in
    Buffer.add_substring buffer subject !copied_until (first - !copied_until);
    Buffer.add_string buffer (expand_replacement matched replacement);
    copied_until := last);
  Buffer.add_substring buffer subject !copied_until
    (String.length subject - !copied_until);
  Buffer.contents buffer

let count regexp subject =
  let result = ref 0 in
  iter_matches regexp subject (fun _ -> incr result);
  !result

(* PCRE2 has no reverse-search API. Advance from each match's start rather
   than its end so the forward scan preserves RINDEX's overlapping-match
   semantics (for example, /aa/ in "aaa") without probing every byte. *)
let search_backward regexp subject start =
  if start < 0 || start > String.length subject then
    invalid_arg "PCRE2 reverse-search offset is outside the subject"
  else
    let rec loop position best =
      if position > start || position > String.length subject then best
      else match search regexp subject position with
        | None -> best
        | Some result when match_start result > start -> best
        | Some result ->
            let next = advance_after_empty regexp subject
                (match_start result) in
            if next = match_start result then Some result
            else loop next (Some result)
    in
    loop 0 None
