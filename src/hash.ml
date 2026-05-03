(* Centralized hashing helpers (SHA-256 + fallback digest utilities). *)

open BatteriesInit
open Hashtblinit

let many_whitespace_regexp = Str.regexp "[ \t]+"
let normalize_slashes p = Str.global_replace (Str.regexp "\\\\") "/" p

type sha256_backend =
  | ShaCertUtil
  (* PowerShell Get-FileHash — available on all modern Windows versions (8+/Server 2012+)
     as a fallback when certutil is absent or misbehaves. *)
  | ShaPS
  | Sha256sum
  | Shasum256

let is_sha256_hex s =
  let s = String.lowercase_ascii (String.trim s) in
  let rec all_hex i =
    if i >= String.length s then true
    else
      let c = s.[i] in
      (((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) && all_hex (i + 1)) in
  String.length s = 64 && all_hex 0

let sha256_default_backends () =
  if Sys.os_type = "Win32" then
    (* certutil first (always present on Windows); PowerShell as safety net *)
    [ShaCertUtil; ShaPS]
  else
    [Sha256sum; Shasum256]

let run_command_capture_lines cmd =
  try
    let proc = Unix.open_process_in cmd in
    let lines = ref [] in
    (try while true do lines := input_line proc :: !lines done
     with End_of_file -> ()) ;
    let status = Unix.close_process_in proc in
    Some (List.rev !lines, status)
  with _ -> None

let parse_sha256_from_lines backend lines =
  let normalize_line line =
    String.lowercase_ascii
      (Str.global_replace (Str.regexp "[ \t\r]") "" (String.trim line)) in
  let first_token line =
    match Str.split many_whitespace_regexp (String.trim line) with
    | hd :: _ -> String.lowercase_ascii hd
    | [] -> "" in
  (* Backend-specific parsing keeps us strict and avoids matching random 64-hex blobs. *)
  match backend with
  | ShaCertUtil ->
      (* certutil prints the hash on a line by itself, surrounded by header/footer lines *)
      let rec scan = function
        | [] -> None
        | hd :: tl ->
            let n = normalize_line hd in
            if is_sha256_hex n then Some n else scan tl
      in scan lines
  | ShaPS ->
      (* PowerShell `(Get-FileHash ...).Hash` outputs a single uppercase hex string;
         we trim and lowercase it before validation *)
      let rec scan = function
        | [] -> None
        | hd :: tl ->
            let tok = String.lowercase_ascii (String.trim hd) in
            if is_sha256_hex tok then Some tok else scan tl
      in scan lines
  | Sha256sum
  | Shasum256 ->
      let rec scan = function
        | [] -> None
        | hd :: tl ->
            let tok = first_token hd in
            if is_sha256_hex tok then Some tok else scan tl
      in scan lines

let sha256_file_with_backends path backends =
  (* Filename.quote uses OS double-quotes, suitable for certutil/sha256sum/shasum.
     PowerShell -Command "..." is already a double-quoted string, so nesting
     OS double-quotes would break paths that contain spaces.  We therefore
     quote the path with PowerShell single-quotes for the ShaPS backend. *)
  let quoted    = Filename.quote path in
  let ps_quoted =
    (* Single-quote escaping for PowerShell: wrap in '' and escape any literal
       single quote in the path as ''. *)
    "'" ^ Str.global_replace (Str.regexp_string "'") "''" path ^ "'" in
  let command_of_backend = function
    | ShaCertUtil -> Printf.sprintf "certutil -hashfile %s SHA256" quoted
    | ShaPS ->
        (* -NoProfile speeds up startup; output is one lowercase hex line.
           ps_quoted uses single-quote escaping to avoid nesting double quotes
           inside the PowerShell -Command "..." wrapper. *)
        Printf.sprintf
          "powershell -NoProfile -Command \"(Get-FileHash -Path %s -Algorithm SHA256).Hash.ToLower()\""
          ps_quoted
    | Sha256sum -> Printf.sprintf "sha256sum %s" quoted
    | Shasum256 -> Printf.sprintf "shasum -a 256 %s" quoted in
  (* Only accept output from commands that exited successfully. *)
  let rec go = function
    | [] -> None
    | backend :: tl ->
        begin match run_command_capture_lines (command_of_backend backend) with
        | Some (lines, Unix.WEXITED 0) ->
            begin match parse_sha256_from_lines backend lines with
            | Some digest -> Some digest
            | None -> go tl
            end
        | _ -> go tl
        end
  in
  go backends

let sha256_file path = sha256_file_with_backends path (sha256_default_backends ())

let sha256_string s =
  (* We intentionally hash via the same external SHA-256 backends used for files,
     so behavior is consistent across all hash call sites in this codebase. *)
  let tmp = Filename.temp_file "weidu-sha256-" ".tmp" in
  try
    let ch = open_out_bin tmp in
    output_string ch s ;
    close_out ch ;
    let res = sha256_file tmp in
    (try Sys.remove tmp with _ -> ()) ;
    res
  with _ ->
    (try Sys.remove tmp with _ -> ()) ;
    None

let canonicalize_for_key ?base path =
  (* Canonicalization here is lexical (no filesystem access), matching security checks
     where path traversal should be normalized without requiring files to exist. *)
  let p = normalize_slashes path in
  let full =
    if Filename.is_relative p then
      let b = match base with
        | Some b when b <> "" -> normalize_slashes b
        | _ -> normalize_slashes (Sys.getcwd ()) in
      b ^ "/" ^ p
    else p in
  let parts = String.split_on_char '/' full in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "" :: rest when acc <> [] -> resolve acc rest
    | "." :: rest -> resolve acc rest
    | ".." :: rest ->
        (match acc with
        | [] | [""] | [_] -> resolve acc rest
        | _ :: prev -> resolve prev rest)
    | part :: rest -> resolve (part :: acc) rest
  in
  String.lowercase_ascii (String.concat "/" (resolve [] parts))

let tp2_hash_seen = Hashtbl.create 97

let register_tp2_hash ?game_dir path digest =
  let key = canonicalize_for_key ?base:game_dir path in
  if Hashtbl.mem tp2_hash_seen key then begin
    let old_digest = Hashtbl.find tp2_hash_seen key in
    old_digest <> digest
  end else begin
    Hashtbl.add tp2_hash_seen key digest ;
    false
  end

let audit_chain_prev = ref ""
let audit_chain_sha256_available = ref (None : bool option)

let next_audit_chain ts ctx result =
  (* Tamper-evident chain:
     chain_n = hash(chain_{n-1} || ts || ctx || msg).
     We prefer SHA-256 and degrade to Digest only if SHA-256 backends are unavailable. *)
  let chain_seed = !audit_chain_prev ^ "|" ^ ts ^ "|" ^ ctx ^ "|" ^ result in
  let chain =
    match !audit_chain_sha256_available with
    | Some false -> Digest.to_hex (Digest.string chain_seed)
    | _ ->
        begin match sha256_string chain_seed with
        | Some digest ->
            audit_chain_sha256_available := Some true ;
            digest
        | None ->
            audit_chain_sha256_available := Some false ;
            Digest.to_hex (Digest.string chain_seed)
        end in
  audit_chain_prev := chain ;
  chain
