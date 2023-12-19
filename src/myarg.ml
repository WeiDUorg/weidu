(* This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
   starting from 18 December 2012 and WeiDU 231.06. *)

(* Note added due to LGPL terms.

   This file was edited by Valerio Bigiani, AKA The Bigg, starting from
   18 March 2006. All changes for this file are listed in
   diffs/src.arg.ml.diff file, as the output of a diff -Bw -c -N command.

   It was originally taken from Westley Weimer's WeiDU 185. *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

open BatteriesInit
open Hashtblinit
(* $Id: arg.ml,v 1.16 2001/12/07 13:40:48 xleroy Exp $ *)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | TwoStrings of (string -> string -> unit)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Tuple of spec list
  | Rest of spec               (* Stop interpreting keywords and call the
                                  function with each remaining argument *)
  | List of spec               (* As with Rest, but stop if a parameter is found which
                                  starts with - IE --foo-list a b c --bar bla *)

exception Bad of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3)::t when y1 = x -> y2
  | _::t -> assoc3 x t
;;

let good_terminal_p =
  let term = try Sys.getenv "TERM" with _ -> "" in
  let list = [ "xterm" ; "st" ; "konsole" ; "rxvt" ; "kitty" ; "gnome" ;
               "alacritty" ; "screen" ] in
  (try
    let _ = Str.search_forward (Str.regexp (String.concat "\\|" list)) term 0 in
    true
  with Not_found -> false)

let usage speclist errmsg =
  eprintf "%s\n" errmsg;
  let counter = ref 0 in
  List.iter (function (key, _, doc) ->
    incr counter ;
    if !counter > 10 && not good_terminal_p then begin
      eprintf "\n\t\tPress Enter For More Options\n" ;
      flush_all () ; 
      let _ = read_line () in 
      counter := 0 ;
    end ; 
    eprintf "  %s %s\n" key doc) 
    speclist;
  try ignore (assoc3 "-help" speclist)
  with Not_found -> eprintf "  -help\t\tdisplay this list of options\n";
    try ignore (assoc3 "--help" speclist)
    with Not_found -> eprintf "  --help\tdisplay this list of options\n";
;;

let current = ref 0;;

let parse speclist anonfun errmsg =
  let is_null s = match s with
    Unit _
  | Set _
  | Clear _ -> true
  | _ -> false
  in
  let is_recursive s = match s with
    Rest _
  | List _ -> true
  | _ -> false
  in
  let is_tuple s = match s with
    Tuple _ -> true
  | _ -> false
  in
  List.iter (fun x ->
    let a,spec,b = x in
    match spec with
    | Rest s
    | List s -> if is_recursive s || is_null s then failwith "Internal error: cannot nest recursive specs";
    | Tuple sl -> List.iter (fun s -> if is_recursive s || is_tuple s || is_null s then
        failwith "Internal error: cannot nest tuple specs"
      ) sl
    | _ -> ()
  ) speclist;
  let initpos = !current in
  let stop error =
    let progname =
      if initpos < Array.length Sys.argv then Sys.argv.(initpos) else "(?)" in
    begin match error with
    | Unknown "-help" -> ()
    | Unknown "--help" -> ()
    | Unknown s ->
        eprintf "%s: unknown option `%s'.\n" progname s
    | Missing s ->
        eprintf "%s: option `%s' needs an argument.\n" progname s
    | Wrong (opt, arg, expected) ->
        eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
          progname arg opt expected
    | Message s ->
        eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then exit 0
    else exit 2
  in
  let l = Array.length Sys.argv in
  incr current;
  while !current < l do
    let s = Sys.argv.(!current) in
    if String.length s >= 1 && String.get s 0 = '-' then begin
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        let rec treat_action action =
          match action with
          | Unit f -> f ();
          | Set r -> r := true;
          | Clear r -> r := false;
          | String f when !current + 1 < l ->
              let arg = Sys.argv.(!current+1) in
              f arg;
              incr current;
          | TwoStrings f when !current + 2 < l ->
              let arg1 = Sys.argv.(!current+1) in
              incr current;
              let arg2 = Sys.argv.(!current+1) in
              f arg1 arg2;
              incr current;
          | Int f when !current + 1 < l ->
              let arg = Sys.argv.(!current+1) in
              begin try f (int_of_string arg)
              with Failure "int_of_string" -> stop (Wrong (s, arg, "an integer"))
              end;
              incr current;
          | Float f when !current + 1 < l ->
              let arg = Sys.argv.(!current+1) in
              f (float_of_string arg);
              incr current;
          | Rest f ->
              while !current < l-1 do
                treat_action f;
              done;
          | List f ->
              let found_minus = ref false in
              while (!current < l-1) && (not !found_minus) do
                if Sys.argv.(!current+1).[0] = '-' then begin
                  found_minus := true ;
                end else begin
                  treat_action f;
                end
              done;
          | Tuple specs ->
              List.iter treat_action specs;
          | _ -> stop (Missing s)
        in treat_action action
      with Bad m -> stop (Message m);
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;
