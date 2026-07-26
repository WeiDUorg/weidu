(* Native path-authority enforcement for operations initiated by TP2 code.
 *
 * Paths are resolved through the filesystem before they are compared with
 * the configured authorities.  This prevents an in-root symbolic link from
 * turning an apparently safe path into access outside an approved root.
 *)

type operation = Read | Write

type context = {
  tp2 : string ;
  component : int option ;
  action : string option ;
}

type grant = {
  declared : string ;
  canonical : string ;
  recursive : bool ;
}

type denial = {
  operation : operation ;
  requested : string ;
  resolved : string option ;
  context : context ;
  authorities : grant list ;
}

exception Denied of denial

external canonical_existing_path : string -> string =
  "weidu_canonical_existing_path"

let grant_option = "--allow-file-root"

let explicit_roots = ref []
let explicit_read_roots = ref []
let read_grants = ref []
let write_grants = ref []
let configured = ref false
let current_context = ref None
let pending_denial = ref None

let normalize_separators path =
  String.map (fun c -> if c = '\\' then '/' else c) path

let comparison_path path =
  let path = normalize_separators path in
  if Sys.os_type = "Win32" then String.lowercase_ascii path else path

let rec append_components base = function
| [] -> base
| "" :: rest
| "." :: rest -> append_components base rest
| ".." :: rest -> append_components (Filename.dirname base) rest
| component :: rest ->
    append_components (Filename.concat base component) rest

let canonicalize path =
  let path = normalize_separators path in
  let absolute =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path in
  let rec find_existing candidate suffix =
    try
      ignore (Unix.lstat candidate) ;
      let canonical = canonical_existing_path candidate in
      if canonical = "" then
        failwith ("cannot resolve existing path [" ^ candidate ^ "]") ;
      append_components (normalize_separators canonical) suffix
    with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
        let parent = Filename.dirname candidate in
        if parent = candidate then
          failwith ("cannot find an existing ancestor of [" ^ path ^ "]")
        else
          find_existing parent (Filename.basename candidate :: suffix)
  in
  find_existing absolute []

let starts_with string prefix =
  let string_length = String.length string in
  let prefix_length = String.length prefix in
  string_length >= prefix_length &&
  String.sub string 0 prefix_length = prefix

let contains root path =
  let root = comparison_path root in
  let path = comparison_path path in
  let root_length = String.length root in
  path = root ||
  (root_length > 0 && root.[root_length - 1] = '/' &&
   starts_with path root) ||
  (String.length path > root_length &&
   starts_with path root &&
   path.[root_length] = '/')

let make_grant recursive declared =
  { declared ; canonical = canonicalize declared ; recursive }

let deduplicate grants =
  List.fold_left (fun result grant ->
    if List.exists (fun existing ->
      existing.recursive = grant.recursive &&
      comparison_path existing.canonical = comparison_path grant.canonical)
        result then
      result
    else
      result @ [grant]) [] grants

let add_explicit_root path =
  explicit_roots := !explicit_roots @ [path]

let add_explicit_read_root path =
  explicit_read_roots := !explicit_read_roots @ [path]

let configure ~read_roots ~write_roots ~read_files ~write_files =
  let explicit = List.map (make_grant true) !explicit_roots in
  let explicit_read = List.map (make_grant true) !explicit_read_roots in
  let make_roots paths = List.map (make_grant true) paths in
  let make_files paths = List.map (make_grant false) paths in
  write_grants := deduplicate
      (make_roots write_roots @ make_files write_files @ explicit) ;
  read_grants := deduplicate
      (make_roots read_roots @ make_files read_files @
       !write_grants @ explicit @ explicit_read) ;
  configured := true

let path_is_within ~root path =
  contains (canonicalize root) (canonicalize path)

let operation_name = function
| Read -> "read"
| Write -> "write"

let context_description context =
  let component = match context.component with
  | None -> ""
  | Some number -> Printf.sprintf ", component %d" number in
  let action = match context.action with
  | None -> ""
  | Some name -> ", action " ^ name in
  Printf.sprintf "TP2 [%s]%s%s" context.tp2 component action

let grant_description grant =
  Printf.sprintf "[%s] (%s)"
    grant.canonical (if grant.recursive then "root" else "file")

let denial_description denial =
  let resolved = match denial.resolved with
  | Some path -> Printf.sprintf ", resolved as [%s]" path
  | None -> ", which could not be resolved safely" in
  let authorities =
    match denial.authorities with
    | [] -> "(none configured)"
    | grants -> String.concat ", " (List.map grant_description grants) in
  Printf.sprintf
    "FILE ACCESS DENIED: %s requested %s access to [%s]%s.\n\
     Authorized %s authorities: %s.\n\
     To authorize another installation root, rerun WeiDU with %s PATH."
    (context_description denial.context)
    (operation_name denial.operation)
    denial.requested
    resolved
    (operation_name denial.operation)
    authorities
    grant_option

let () =
  Printexc.register_printer (function
  | Denied denial -> Some (denial_description denial)
  | _ -> None)

let raise_pending () =
  match !pending_denial with
  | None -> ()
  | Some denial -> raise (Denied denial)

let authorized grant path =
  if grant.recursive then contains grant.canonical path
  else comparison_path grant.canonical = comparison_path path

let require operation requested =
  match !current_context with
  | None -> ()
  | Some context ->
      raise_pending () ;
      let authorities =
        if operation = Write then !write_grants else !read_grants in
      let resolved =
        try Some (canonicalize requested)
        with _ -> None in
      let permitted =
        !configured &&
        match resolved with
        | None -> false
        | Some path -> List.exists (fun grant -> authorized grant path)
                         authorities in
      if not permitted then begin
        let denial =
          { operation ; requested ; resolved ; context ; authorities } in
        pending_denial := Some denial ;
        raise (Denied denial)
      end

let require_read path = require Read path
let require_write path = require Write path
let raise_if_denied = raise_pending

let require_glob pattern =
  let pattern = normalize_separators pattern in
  let wildcard_position =
    let position = ref (String.length pattern) in
    String.iteri (fun index character ->
      if index < !position &&
         (character = '*' || character = '?' || character = '[') then
        position := index) pattern ;
    !position in
  let literal_prefix = String.sub pattern 0 wildcard_position in
  let root =
    if literal_prefix = "" then
      "."
    else if literal_prefix.[String.length literal_prefix - 1] = '/' then
      Filename.dirname (literal_prefix ^ "__weidu_glob_authority__")
    else
      Filename.dirname literal_prefix in
  require_read (if root = "" then "." else root)

let reraise_if_denied exception_value =
  match exception_value with
  | Denied _ -> raise exception_value
  | _ -> raise_pending ()

let with_context update callback =
  let previous_context = !current_context in
  let outermost = previous_context = None in
  let next_context = update previous_context in
  if outermost then pending_denial := None ;
  current_context := Some next_context ;
  try
    let result = callback () in
    raise_pending () ;
    current_context := previous_context ;
    if outermost then pending_denial := None ;
    result
  with exception_value ->
    current_context := previous_context ;
    if outermost then pending_denial := None ;
    raise exception_value

let with_tp2 tp2 callback =
  with_context (fun previous ->
    match previous with
    | None -> { tp2 ; component = None ; action = None }
    | Some context -> { context with tp2 }) callback

let with_component component callback =
  with_context (fun previous ->
    match previous with
    | None ->
        { tp2 = "(unknown)" ; component = Some component ; action = None }
    | Some context ->
        { context with component = Some component ; action = None }) callback

let with_action action callback =
  with_context (fun previous ->
    match previous with
    | None -> { tp2 = "(unknown)" ; component = None ;
                action = Some action }
    | Some context -> { context with action = Some action }) callback

let with_recovery callback =
  let original_denial = !pending_denial in
  pending_denial := None ;
  try
    let result = callback () in
    raise_pending () ;
    pending_denial := original_denial ;
    result
  with exception_value ->
    pending_denial := original_denial ;
    raise exception_value
