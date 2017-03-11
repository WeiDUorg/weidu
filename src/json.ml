let export_string ts =
  String.escaped (Dc.single_string_of_tlk_string
                    (Load.the_game ()) ts)

let stringify_component_group group =
  let body = String.concat "," (List.map (fun string ->
    Printf.sprintf "\"%s\"" string) (List.map (fun ts ->
    export_string ts) group)) in
  String.concat "" [ "[" ; body ; "]" ]

let stringify_component component =
  let index = Printf.sprintf "\"index\":\"%d\"" component.Tp.index in
  let number = Printf.sprintf "\"number\":\"%d\"" component.Tp.number in
  let forced = Printf.sprintf "\"forced\":\"%B\"" component.Tp.forced in
  let name = Printf.sprintf "\"name\":\"%s\""
      (export_string component.Tp.name) in
  let subgroup = (match component.Tp.subgroup with
  | Some ts -> Printf.sprintf "\"subgroup\":\"%s\""
        (export_string ts)
  | None -> "") in
  let group = Printf.sprintf "\"group\":%s"
      (stringify_component_group component.Tp.group) in

  let body = String.concat "," (List.filter (fun string ->
    string <> "") [ index ; number ; forced ; name ; subgroup ; group ]) in
  String.concat "" ["{" ; body ; "}"]

let stringify_component_list components =
  let body = String.concat "," (List.map stringify_component components) in
  String.concat "" ["[" ; body ; "]" ]
