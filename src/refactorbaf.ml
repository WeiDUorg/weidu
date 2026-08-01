open BatteriesInit
open Hashtblinit
open Util

type trigger =
  | Or of trigger list
  | Trigger of string * string option
  | NotTrigger of string * string option

let do_refactor : (Str.regexp * string) option ref = ref None

let parse_triggers : (string -> trigger list) ref = ref (fun s ->
  failwith "parse_triggers not loaded")

let refactor_ht = Hashtbl.create 5

let set_refactor x = do_refactor := match x with
  Some(a,b, case_sens, exact_m) -> begin
    if Hashtbl.mem refactor_ht (a,b, case_sens, exact_m) then
      Hashtbl.find refactor_ht (a,b, case_sens, exact_m)
    else begin
      let a_reg = match case_sens, exact_m with
      | true , true  -> Str.regexp_string a
      | true , false -> Str.regexp a
      | false, true  -> Str.regexp_string_case_fold a
      | false, false -> Str.regexp_case_fold a
      in
      let ans = Some(a_reg, b) in
      Hashtbl.add refactor_ht (a,b, case_sens, exact_m) ans ;
      ans
    end
  end
| None -> None

let rec print_t t =
  match t with
  | Or [] -> ""
  | Or [t] -> print_t t
  | Or tl -> "OR(" ^ string_of_int (List.length tl) ^ ") " ^ print_tl tl
  | Trigger (s,None) -> s
  | Trigger (s,Some a) -> Printf.sprintf "TriggerOverride(%s,%s)" a s
  | NotTrigger (s,None) -> "!" ^ s
  | NotTrigger (s,Some a) -> Printf.sprintf "!TriggerOverride(%s,%s)" a s

and print_tl tl =
  List.fold_left (fun acc elt -> acc^ " " ^ print_t elt) "" tl

let rec enforce_actor tl a =
  List.map (fun t ->
    match t with
    | Trigger (s,None) -> Trigger(s,a)
    | NotTrigger(s,None) -> NotTrigger(s,a)
    | Or tl1 -> Or (enforce_actor tl1 a)
    | Trigger(s,Some a)
    | NotTrigger(s,Some a) ->
        let msg = "REFACTOR_*_TRIGGER tries to add TriggerOverride to a trigger that already uses TriggerOverride" in
        failwith msg) tl

let sub pre post s a =
  let tl = !parse_triggers (Str.global_replace pre post s) in
  match a with
  | None -> tl
  | Some _ -> enforce_actor tl a

(* A trigger list is a conjunction of clauses. A plain trigger is a
   one-element clause and [Or] contains the alternatives in a larger clause. *)
let clause_of_trigger = function
| Or tl -> tl
| t -> [t]

let trigger_of_clause = function
| [t] -> t
| tl -> Or tl

let cnf_of_triggers tl = List.map clause_of_trigger tl

let matches pre s =
  Str.string_match pre s 0 && Str.matched_string s = s

(* Applying the matched source's negation to an already-negated replacement
   keeps one negation rather than turning [!A] into [A]. *)
let ensure_negated = function
| Trigger (s,a)
| NotTrigger (s,a) -> NotTrigger (s,a)
| Or _ -> failwith "Nested OR()"

(* Negate a replacement in CNF. Each source clause contributes one choice to
   every resulting clause; this is the distributive product required by De
   Morgan's laws. The choices and the generated clauses retain source order. *)
let negate_cnf cnf =
  List.fold_left (fun clauses source_clause ->
    List.concat (List.map (fun clause ->
      List.map (fun t -> clause @ [ensure_negated t]) source_clause
    ) clauses)
  ) [[]] cnf

(* Disjoin two CNF expressions. False is represented by one empty clause and
   true by an empty clause list, which also makes empty replacements behave
   consistently during distribution. *)
let or_cnf left right =
  if left = [] || right = [] then []
  else
    List.concat (List.map (fun left_clause ->
      List.map (fun right_clause -> left_clause @ right_clause) right
    ) left)

let substitute_atom pre post = function
| Trigger (s,a) as t ->
    if matches pre s then cnf_of_triggers (sub pre post s a) else [[t]]
| NotTrigger (s,a) as t ->
    if matches pre s then
      negate_cnf (cnf_of_triggers (sub pre post s a))
    else [[t]]
| Or _ -> failwith "Nested OR()"

let substitute_trigger pre post t =
  let clause = clause_of_trigger t in
  let cnf = List.fold_left (fun acc atom ->
    or_cnf acc (substitute_atom pre post atom)
  ) [[]] clause in
  List.map trigger_of_clause cnf

let refactor tl = match !do_refactor with
| None -> tl
| Some(pre, post) ->
    List.concat (List.map (substitute_trigger pre post) tl)
