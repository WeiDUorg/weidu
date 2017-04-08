open BatteriesInit
open Util

type trigger =
  | Or of trigger list
  | Trigger of string * string option
  | NotTrigger of string * string option

let do_refactor : (Str.regexp * string * bool) option ref = ref None

let parse_triggers : (string -> trigger list) ref = ref (fun s ->
  failwith "parse_triggers not loaded")

let spacer = "tb#refactor_baf_must_certainly_not_match_against_me"
let spacer_r = Str.regexp spacer

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
      let parts = Str.split many_whitespace_or_nl_regexp b in
      let any = ref false in
      let parts = List.map (fun s ->
        let is_neg = s.[0] = '!' in
        let s = if is_neg then Str.string_after s 1 else s in
        if Str.string_match a_reg s 0 && Str.matched_string s = s then begin
          any := true ;
          (if is_neg then "!" else "") ^ spacer ^ s
        end else (if is_neg then "!" else "") ^ s) parts in
      let ans = Some(a_reg, String.concat " " parts, !any) in
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

let rec find pre tl =
  List.exists (fun t -> match t with
  | Trigger (s,_)
  | NotTrigger (s,_) -> Str.string_match pre s 0 && Str.matched_string s = s
  | Or tl -> find pre tl) tl

let fix_trigger acc tl =
  List.iter (fun t -> acc := t :: !acc) (List.rev tl)

let rec invert and_acc or_acc t = match t with
| Trigger (s,a) -> or_acc := NotTrigger (s,a) :: !or_acc
| NotTrigger (s,a) -> or_acc := Trigger (s,a) :: !or_acc
| Or tl' -> List.iter (invert or_acc and_acc) (List.rev tl')

let rec fix_nottrigger acc tl =
  let or_acc = ref [] in
  let and_acc = ref [] in
  List.iter (invert and_acc or_acc) tl ;
  if List.length !and_acc = 0 then
    acc := Or(!or_acc) :: !acc
  else
    List.iter (fun t ->
      acc := Or(t :: !or_acc) :: !acc) !and_acc

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
  | Some x -> enforce_actor tl a

let fix_or_internal pre post acc tl =
  let old_tl = ref [] in
  let new_tl = ref [] in
  let found = ref false in
  List.iter (fun t -> match t with
  | Or _ -> failwith "Nested OR()"
  | Trigger (s,a) -> if not !found && find pre [t] then begin
      found := true ;
      new_tl := List.append (sub pre post s a) !new_tl
  end
  else old_tl := t :: !old_tl
  | NotTrigger (s,a) -> if not !found && find pre [t] then begin
      found := true ;
      List.iter (invert new_tl old_tl) (sub pre post s a)
  end
  else old_tl := t :: !old_tl) tl ;
  let flatten_or tl =
    let a = ref [] in
    List.iter (fun t -> match t with
    | Or tl' -> List.iter (fun t -> a := t :: !a) tl'
    | _ -> a := t :: !a) tl ;
    Or !a
  in
  if !new_tl = [] then acc := flatten_or !old_tl :: !acc
  else begin
    List.iter (fun t ->
      acc := flatten_or (t :: !old_tl) :: !acc) !new_tl
  end


let rec subst pre post acc tl =
  List.iter (fun t -> if find pre [t] then match t with
  | Trigger (s,a) -> fix_trigger acc (sub pre post s a)
  | NotTrigger (s,a) -> fix_nottrigger acc (sub pre post s a)
  | Or tl -> fix_or pre post acc tl
  else acc := t :: !acc) tl ;

and fix_or pre post acc tl =
  let acc' = ref [] in
  fix_or_internal pre post acc' tl ;
  if find pre !acc' then begin
    subst pre post acc !acc' ;
  end else acc := !acc' @ !acc

let rec remove_spacer tl = List.map (fun t -> match t with
| Trigger (s,a) -> Trigger (Str.global_replace spacer_r "" s,a)
| NotTrigger (s,a) -> NotTrigger (Str.global_replace spacer_r "" s,a)
| Or tl' -> Or (remove_spacer tl')) tl

let refactor tl = match !do_refactor with
| None -> tl
| Some(pre, post, any) -> begin
    let acc = ref [] in
    let tl = subst pre post acc (List.rev tl) in
    if any then remove_spacer !acc else !acc
end
