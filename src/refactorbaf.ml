open Util

type trigger =
	| Or of trigger list
	| Trigger of string
	| NotTrigger of string

let do_refactor : (Str.regexp * string) option ref = ref None

let parse_triggers : (string -> trigger list) ref = ref (fun s -> failwith "parse_triggers not loaded")

let set_refactor x = do_refactor := match x with
	Some(a,b) -> Some(Str.regexp_case_fold a, b)
|	None -> None

let rec print_t t =
	match t with
	| Or [] -> ""
	| Or [t] -> print_t t
	| Or tl -> "OR(" ^ string_of_int (List.length tl) ^ ") " ^ print_tl tl
	| Trigger s -> s
	| NotTrigger s -> "!" ^ s

and print_tl tl =
	List.fold_left (fun acc elt -> acc^ " " ^ print_t elt) "" tl

let rec find pre tl =
	List.exists (fun t -> match t with
	| Trigger s
	| NotTrigger s -> Str.string_match pre s 0 && Str.matched_string s = s
	| Or tl -> find pre tl
	) tl
	
let fix_trigger acc tl =
	List.iter (fun t -> acc := t :: !acc) (List.rev tl)

let rec invert and_acc or_acc t = match t with 
	| Trigger s -> or_acc := NotTrigger s :: !or_acc
	| NotTrigger s -> or_acc := Trigger s :: !or_acc
	| Or tl' -> List.iter (invert or_acc and_acc) (List.rev tl')

let rec fix_nottrigger acc tl =
	let or_acc = ref [] in
	let and_acc = ref [] in
	List.iter (invert and_acc or_acc) tl;
	if List.length !and_acc = 0 then
		acc := Or(!or_acc) :: !acc
	else
		List.iter (fun t ->
			acc := Or(t :: !or_acc) :: !acc
		) !and_acc
;;

let sub pre post s =
	!parse_triggers (Str.global_replace pre post s)

let rec subst pre post tl =
	let acc = ref [] in
	List.iter (fun t -> if find pre [t] then match t with
	|	Trigger s -> fix_trigger acc (sub pre post s)
	|	NotTrigger s -> fix_nottrigger acc (sub pre post s)
	|	Or tl -> fix_or pre post acc tl
	else acc := t :: !acc
	) tl;
	!acc

and fix_or pre post acc tl =
	let old_tl = ref [] in
	let new_tl = ref [] in
	List.iter (fun t -> match t with
	|	Or _ -> failwith "Nested OR()"
	|	Trigger s -> if find pre [t] then
		new_tl := List.append (sub pre post s) !new_tl
		else old_tl := t :: !old_tl
	|	NotTrigger s -> if find pre [t] then
		List.iter (invert new_tl old_tl) (sub pre post s)
		else old_tl := t :: !old_tl
	) tl;
	let flatten_or tl =
		let a = ref [] in
		List.iter (fun t -> match t with
		|	Or tl' -> List.iter (fun t -> a := t :: !a) tl'
		|	_ -> a := t :: !a
		) tl;
		Or !a
	in
	if !new_tl = [] then acc := flatten_or !old_tl :: !acc
	else begin
		List.iter (fun t ->
			acc := flatten_or (t :: !old_tl) :: !acc
		) !new_tl
	end

	
let refactor tl = match !do_refactor with
| None -> tl
| Some(pre, post) -> begin
		let tl = subst pre post (List.rev tl) in
		tl
	end
