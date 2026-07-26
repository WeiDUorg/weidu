(*
 * Streaming SHA-256 implementation used by FILE_SHA256.
 *
 * This module is self-contained so WeiDU does not acquire a runtime dependency
 * on platform-specific checksum executables or external OCaml packages.
 *)

let round_constants = [|
  0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
  0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
  0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
  0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
  0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
  0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
  0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
  0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
  0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
  0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
  0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
  0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
  0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
  0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
  0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
  0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
|]

type context = {
  state : int32 array;
  schedule : int32 array;
  block : Bytes.t;
  mutable block_length : int;
  mutable total_length : int64;
}

let create () = {
  state = [|
    0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
    0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
  |];
  schedule = Array.make 64 0l;
  block = Bytes.make 64 '\000';
  block_length = 0;
  total_length = 0L;
}

let rotate_right value amount =
  Int32.logor
    (Int32.shift_right_logical value amount)
    (Int32.shift_left value (32 - amount))

let xor3 a b c = Int32.logxor a (Int32.logxor b c)

let add4 a b c d =
  Int32.add (Int32.add a b) (Int32.add c d)

let byte block offset =
  Int32.of_int (Char.code (Bytes.get block offset))

let load_word block offset =
  Int32.logor
    (Int32.shift_left (byte block offset) 24)
    (Int32.logor
       (Int32.shift_left (byte block (offset + 1)) 16)
       (Int32.logor
          (Int32.shift_left (byte block (offset + 2)) 8)
          (byte block (offset + 3))))

let process_block context block offset =
  let w = context.schedule in
  for index = 0 to 15 do
    w.(index) <- load_word block (offset + (index * 4))
  done;
  for index = 16 to 63 do
    let x = w.(index - 15) in
    let y = w.(index - 2) in
    let sigma0 = xor3 (rotate_right x 7) (rotate_right x 18)
        (Int32.shift_right_logical x 3) in
    let sigma1 = xor3 (rotate_right y 17) (rotate_right y 19)
        (Int32.shift_right_logical y 10) in
    w.(index) <- add4 w.(index - 16) sigma0 w.(index - 7) sigma1
  done;

  let a = ref context.state.(0) in
  let b = ref context.state.(1) in
  let c = ref context.state.(2) in
  let d = ref context.state.(3) in
  let e = ref context.state.(4) in
  let f = ref context.state.(5) in
  let g = ref context.state.(6) in
  let h = ref context.state.(7) in

  for index = 0 to 63 do
    let sum1 = xor3 (rotate_right !e 6) (rotate_right !e 11)
        (rotate_right !e 25) in
    let choose = Int32.logxor
        (Int32.logand !e !f)
        (Int32.logand (Int32.lognot !e) !g) in
    let temporary1 = add4 !h sum1 choose
        (Int32.add round_constants.(index) w.(index)) in
    let sum0 = xor3 (rotate_right !a 2) (rotate_right !a 13)
        (rotate_right !a 22) in
    let majority = xor3
        (Int32.logand !a !b)
        (Int32.logand !a !c)
        (Int32.logand !b !c) in
    let temporary2 = Int32.add sum0 majority in

    h := !g;
    g := !f;
    f := !e;
    e := Int32.add !d temporary1;
    d := !c;
    c := !b;
    b := !a;
    a := Int32.add temporary1 temporary2
  done;

  context.state.(0) <- Int32.add context.state.(0) !a;
  context.state.(1) <- Int32.add context.state.(1) !b;
  context.state.(2) <- Int32.add context.state.(2) !c;
  context.state.(3) <- Int32.add context.state.(3) !d;
  context.state.(4) <- Int32.add context.state.(4) !e;
  context.state.(5) <- Int32.add context.state.(5) !f;
  context.state.(6) <- Int32.add context.state.(6) !g;
  context.state.(7) <- Int32.add context.state.(7) !h

let update context input offset length =
  if offset < 0 || length < 0 || offset > Bytes.length input - length then
    invalid_arg "Sha256.update";
  context.total_length <- Int64.add context.total_length (Int64.of_int length);
  let rec copy input_offset remaining =
    if remaining > 0 then begin
      let available = 64 - context.block_length in
      let copied = min available remaining in
      Bytes.blit input input_offset context.block context.block_length copied;
      context.block_length <- context.block_length + copied;
      if context.block_length = 64 then begin
        process_block context context.block 0;
        context.block_length <- 0
      end;
      copy (input_offset + copied) (remaining - copied)
    end
  in
  copy offset length

let finish context =
  let padded_length = if context.block_length < 56 then 64 else 128 in
  let padded = Bytes.make padded_length '\000' in
  Bytes.blit context.block 0 padded 0 context.block_length;
  Bytes.set padded context.block_length (Char.chr 0x80);

  let bit_length = Int64.mul context.total_length 8L in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let value = Int64.to_int
        (Int64.logand (Int64.shift_right_logical bit_length shift) 0xffL) in
    Bytes.set padded (padded_length - 8 + index) (Char.chr value)
  done;

  for index = 0 to (padded_length / 64) - 1 do
    process_block context padded (index * 64)
  done;

  Array.fold_left
    (fun result value -> result ^ Printf.sprintf "%08lx" value)
    "" context.state

let file filename =
  let channel = open_in_bin filename in
  let context = create () in
  let buffer = Bytes.create 65536 in
  try
    let rec read () =
      let length = input channel buffer 0 (Bytes.length buffer) in
      if length <> 0 then begin
        update context buffer 0 length;
        read ()
      end
    in
    read ();
    close_in channel;
    finish context
  with error ->
    close_in_noerr channel;
    raise error
