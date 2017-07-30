(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Disjoint Timings *)
open BatteriesInit

type t = { mutable start : float;
           mutable above_me : float ;
         }

      (* Create the top level *)
let top = { start = 0.0; above_me = 0.0 ; }

    (* The stack of current path through
     * the hierarchy. The first is the
     * leaf. *)
let stack : t list ref = ref [top]

let record = Hashtbl.create 127
let extra = Hashtbl.create 10

let update_table table name duration =
  let sofar =
    if Hashtbl.mem table name then
      Hashtbl.find table name
    else
      0.0
  in
  Hashtbl.replace table name (sofar +. duration)

let time name f a =
  stack := { start = (Unix.times ()).Unix.tms_utime ; above_me = 0.0}
    :: !stack ;
  try
    let result = f a in
    let later = (Unix.times ()).Unix.tms_utime in
    let r = List.hd !stack in
    stack := List.tl !stack ;
    let above_me = List.hd !stack in
    let duration = later -. (r.start +. r.above_me) in
    above_me.above_me <- above_me.above_me +. r.above_me +. duration ;
    update_table record name duration ;
    result
  with e -> begin
    let later = (Unix.times ()).Unix.tms_utime in
    let r = List.hd !stack in
    stack := List.tl !stack ;
    let above_me = List.hd !stack in
    let duration = later -. (r.start +. r.above_me) in
    above_me.above_me <- above_me.above_me +. r.above_me +. duration ;
    update_table record name duration ;
    raise e
  end

let inclusive_time name f a =
  let now = (Unix.times ()).Unix.tms_utime in
  try
    let result = f a in
    let later = (Unix.times ()).Unix.tms_utime in
    let duration = later -. now in
    update_table extra name duration ;
    result
  with e -> begin
    let later = (Unix.times ()).Unix.tms_utime in
    let duration = later -. now in
    update_table extra name duration ;
    raise e
  end

let print chn msg =
  let l = ref [] in
  let total = ref 0.0 in
  Hashtbl.iter (fun name time ->
    total := !total +. time ;
    l := (name,time) :: !l) record ;
  let l = List.sort (fun (a,b) (c,d) -> compare b d) !l in
  if (Hashtbl.length extra) > 0 then begin
    Printf.fprintf chn "\n\t\tMod Timings\n" ;
    Hashtbl.iter (fun name time ->
      Printf.fprintf chn "%-30s %7.3f\n" name time) extra
  end ;
  Printf.fprintf chn "%s" msg ;
  List.iter (fun (l,t) ->
    Printf.fprintf chn "%-30s %7.3f\n" l t) l ;
  Printf.fprintf chn "%-30s %7.3f\n" "TOTAL" !total

(* XXXYYZ
 *
 * [ { s = 0 ; a = 0; } ]
 * [ { s = 0 ; a = 0; } ; { s = 3 ; a = 0; } ]
 * [ { s = 0 ; a = 0; } ; { s = 3 ; a = 0; } ; { s = 5 ; a = 0 } ]
 * [ { s = 0 ; a = 0; } ; { s = 3 ; a = 1; } ] ( Z = 1 )
 * [ { s = 0 ; a = 3; } ; ] ( Z = 1 ; Y = 2 )
 * ( X = 3 ; Y = 2 ; Z = 1 )
 *)
