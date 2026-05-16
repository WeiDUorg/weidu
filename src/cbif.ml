open BatteriesInit
open Hashtblinit
open Util
(* Decompression routines for compressed bif files *)

exception Error of string

let _ = Callback.register_exception "mlgz_exn" (Error "")

external cbf2bif_int : string -> string -> int
    = "mlgz_cbf2bif"
	
let cbf2bif file_in file_out =
    if Dryrun.active () then begin
        log_or_print "[DRY-RUN] would decompress CBF [%s] to [%s]\n"
          file_in file_out ;
        Dryrun.record_write () ;
        0
    end else begin
        (try recursive_mkdir "cache" 511 with Unix.Unix_error(Unix.EEXIST,_,_) -> ());
	    cbf2bif_int file_in file_out
    end

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
