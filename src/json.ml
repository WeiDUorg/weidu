let quote = "\""
let quote_regexp = (Str.regexp quote)
let esc_quote = "\\\""
let backslash = "\\\\"
let backslash_regexp = (Str.regexp backslash)
let esc_backslash = "\\\\\\\\"
let newlines = "[\n\r]"
let newlines_regexp = (Str.regexp newlines)

let escape_string s =
  (* Can't use String.escaped because it messes up non-ASCII *)
  (Printf.sprintf "\"%s\""
     (Str.global_replace quote_regexp esc_quote
        (Str.global_replace backslash_regexp esc_backslash
           (Str.global_replace newlines_regexp "" s))))

let export_string ts =
  escape_string (Dc.single_string_of_tlk_string_safe (Load.the_game ()) ts)

let stringify_labels labels =
  let body = String.concat "," (List.map (fun string ->
    Printf.sprintf "%s" (escape_string string)) labels) in
  String.concat "" [ "[" ; body ; "]" ]

let stringify_component_group group =
  let body = String.concat "," (List.map (fun string ->
    Printf.sprintf "%s" string) (List.map (fun ts ->
    export_string ts) group)) in
  String.concat "" [ "[" ; body ; "]" ]

let stringify_component component =
  let index = Printf.sprintf "\"index\":%d" component.Tp.index in
  let number = Printf.sprintf "\"number\":%d" component.Tp.number in
  let forced = Printf.sprintf "\"forced\":%B" component.Tp.forced in
  let name = Printf.sprintf "\"name\":%s" (export_string component.Tp.name) in
  let label = (match component.Tp.label with
  | Some s -> Printf.sprintf "\"label\":%s" (stringify_labels s)
  | None -> "") in
  let subgroup = (match component.Tp.subgroup with
  | Some ts -> Printf.sprintf "\"subgroup\":%s" (export_string ts)
  | None -> "") in
  let group = Printf.sprintf "\"group\":%s"
      (stringify_component_group component.Tp.group) in

  let body = String.concat "," (List.filter (fun string ->
    string <> "") [ index ; number ; forced ; name ; label ;
                    subgroup ; group ]) in
  String.concat "" ["{" ; body ; "}"]

let stringify_component_list components =
  let body = String.concat "," (List.map stringify_component components) in
  String.concat "" ["[" ; body ; "]" ]
