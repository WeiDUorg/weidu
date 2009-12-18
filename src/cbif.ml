open Util
(* Decompression routines for compressed bif files *)

exception Error of string

let _ = Callback.register_exception "mlgz_exn" (Error "")

external cbf2bif : string -> string -> int
    = "mlgz_cbf2bif"

external zuncompress : string -> pos:int -> clen:int -> ulen: int -> string
    = "mlgz_uncompress"


external zcompress : int -> string -> int -> int -> string
(* level, in_buffer, position, lenght *)
    = "mlgz_compress"

let compress lvl str pos len =
  if !debug_ocaml then log_and_print "Begin compress\n";
  zcompress lvl str pos len
;;

let uncompress str pos clen ulen =
  if !debug_ocaml then log_and_print "Begin uncompress\n";
  zuncompress str pos clen ulen
;;
