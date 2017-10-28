(* Usage: diff orig_buff new_buff 

   Returns the unified diff between the string "orig_buff" and
   "new_buff". 
 *)

external diff: string -> string -> int -> string = "xdiff_diff"

(* Usage: new_buff, rej_buff = patch orig_buff patch_buff 

   Applies the unified diff in string "patch_buff" to the string
   "orig_buff". Returns the patched file ("new_buff") and any rejected
   chunks that couldn't be patched ("rej_buff").
 *)
external patch: string -> string -> string * string = "xdiff_patch"

(* Usage: orig_buff, rej_buff = revpatch new_buff patch_buff  

   Same as patch above, but the patch is applied in reverse so that
   "new_buff" is restored to "orig_buff" with any rejected chunks
   left in "rej_buff"
 *)
external revpatch: string -> string -> string * string = "xdiff_revpatch"
