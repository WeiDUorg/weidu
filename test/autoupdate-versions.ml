#mod_use "../batteries-lite/batList.ml";;
#mod_use "../hashtbl-4.03.0/myhashtbl.ml";;
#mod_use "../batteries-lite/batteriesInit.ml";;
#mod_use "../hashtbl-4.03.0/hashtblinit.ml";;
#mod_use "../src/version.ml";;

let fail label expected actual =
  Printf.eprintf "%s: expected %s, got %s\n" label expected actual;
  exit 1

let string_of_version = function
  | Some version -> string_of_int version
  | None -> "none"

let expect_source label expected versions =
  let actual = Version.latest_stable_version versions in
  if actual <> expected then
    fail label (string_of_version expected) (string_of_version actual)

let expect_outdated label expected ~latest versions =
  let actual = List.filter (Version.is_outdated ~latest) versions in
  if actual <> expected then
    fail label
      (String.concat "," (List.map string_of_int expected))
      (String.concat "," (List.map string_of_int actual))

let () =
  expect_source "stable beats a numerically newer release candidate"
    (Some 25100) [24900; 25100; 25106];
  expect_source "selection is independent of discovery order"
    (Some 25100) [25106; 24900; 25100];
  expect_source "the highest stable version is selected"
    (Some 25200) [25201; 25100; 25200; 25206];
  expect_source "a development version is not a stable source"
    None [25101; 25106];
  expect_outdated "only versions older than the stable source are replaced"
    [24900] ~latest:25100 [24900; 25100; 25101; 25106];
  print_endline "Autoupdate version-selection tests passed"
