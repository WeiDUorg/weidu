open Util

type pixel_type =
| Trans
| Pixel of int*int*int*int

let size_of_str buff =
	let isCompressed = match String.sub buff 0 4 with
		| "MOS " -> false
		| "MOSC" -> true
		| _ -> failwith (Printf.sprintf "Not a MOS(C) file: header is %s" (String.sub buff 0 4))
	in
	let buff = if isCompressed then (
			let c_l = String.length buff - 12 in
			let u_l = Int32.to_int (int32_of_str_off buff 8) in
			Cbif.uncompress buff 12 c_l u_l
		) else buff
	in
	let xSizeP = short_of_str_off buff 0x08 in
	let ySizeP = short_of_str_off buff 0x0a in
	(buff,xSizeP,ySizeP)

let mos_of_str buff =
	let isCompressed = match String.sub buff 0 4 with
		| "MOS " -> false
		| "MOSC" -> true
		| _ -> failwith (Printf.sprintf "Not a MOS(C) file: header is %s" (String.sub buff 0 4))
	in
	let buff = if isCompressed then (
			let c_l = String.length buff - 12 in
			let u_l = Int32.to_int (int32_of_str_off buff 8) in
			Cbif.uncompress buff 12 c_l u_l
		) else buff
	in
	let xSizeP = short_of_str_off buff 0x08 in
	let ySizeP = short_of_str_off buff 0x0a in
	let xSizeB = short_of_str_off buff 0x0c in
	let ySizeB = short_of_str_off buff 0x0e in
	let bSize  =   int_of_str_off buff 0x10 in
	let poff   =   int_of_str_off buff 0x14 in
	let mos = Array.init xSizeP (fun i -> (Array.make ySizeP (Pixel(0,0,0,0)))) in
	let palette = Array.make 256 (Pixel(0,0,0,0)) in
	let pal_off = ref 24 in
	let off_off = ref (24 + xSizeB * ySizeB * 1024) in
	let img_off = ref (24 + xSizeB * ySizeB * 1028) in
	for i = 0 to ySizeB - 1 do
		for j = 0 to xSizeB - 1 do
			for k = 0 to 255 do
				let r = byte_of_str_off buff (!pal_off) in
				incr pal_off;
				let g = byte_of_str_off buff (!pal_off) in
				incr pal_off;
				let b = byte_of_str_off buff (!pal_off) in
				incr pal_off;
				let q = byte_of_str_off buff (!pal_off) in
				incr pal_off;
				palette.(k) <- if (k,r,g,b,q) = (0,0,255,0,0) then Trans else Pixel(r,g,b,q);
			done;
			off_off := !off_off + 4;
			let xMax = ref bSize in
			let yMax = ref bSize in
			xMax := min !xMax (xSizeP - j * 64);
			yMax := min !yMax (ySizeP - i * 64);
			for l = 0 to !yMax - 1 do
				for m = 0 to !xMax - 1 do
					mos.(j * bSize + m).(i * bSize + l) <- palette.(byte_of_str_off buff !img_off);
					incr img_off
				done;
			done;
		done;
	done;
	mos
;;

let reduce_palette i j mos palette xMax yMax =
	let curLen = (Hashtbl.length palette) in
	let cpalette = ref [] in
	Hashtbl.iter (fun a b -> cpalette := (a,b) :: !cpalette) palette;
	let cpalette = ref (List.sort (fun (a,b) (a1,b1) -> compare b b1) !cpalette) in
	let cnt = ref curLen in
	while !cnt > 254 do
		let x,c1 = List.hd !cpalette in
		cpalette := List.tl !cpalette;
		let cmp x x1 =
			match (x,x1) with
				| Pixel(a,b,c,d),Pixel(a1,b1,c1,d1) ->
					let e q = q * q in
					e(a-a1)+e(b-b1)+e(c-c1)+e(d-d1)
				| _ -> max_int
		in
		let diff = ref max_int in
		let curr = ref (x) in
		List.iter (fun (a,b) ->
			if cmp a x < !diff then begin
				diff := cmp a x;
				curr := a
			end
		) !cpalette;
		if (x <> !curr) then begin
			let tmp_p = ref [] in
			List.iter (fun (x1,cnt) ->
				tmp_p := (if x1 = !curr then (x1,cnt+c1) else (x1,cnt)) :: !tmp_p
			) !cpalette;
			cpalette := (List.sort (fun (a,b) (a1,b1) -> compare b b1) !tmp_p);
			for k = 0 to !yMax - 1 do
				for l = 0 to !xMax - 1 do
					if mos.(j * 64 + l).(i * 64 + k) = x then mos.(j * 64 + l).(i * 64 + k) <- !curr;
				done
			done;
			decr cnt
		end else begin
			cpalette := List.rev((x,c1) :: List.rev !cpalette)
		end
	done;
	Hashtbl.clear palette;
	List.iter (fun (a,b) -> Hashtbl.add palette a b) !cpalette;
;;

let str_of_mos mos =
	let xSizeP = Array.length mos in
	let ySizeP = Array.length mos.(0) in
	let xSizeB = (xSizeP + 63) / 64 in
	let ySizeB = (ySizeP + 63) / 64 in
	let result = String.make (24 + 1028 * xSizeB * ySizeB + xSizeP*ySizeP) '\000' in
	String.blit "MOS V1  "            0 result 0x0 8;
	String.blit (str_of_short xSizeP) 0 result 0x08 2;
	String.blit (str_of_short ySizeP) 0 result 0x0a 2;
	String.blit (str_of_short xSizeB) 0 result 0x0c 2;
	String.blit (str_of_short ySizeB) 0 result 0x0e 2;
	String.blit (str_of_int 64)       0 result 0x10 4;
	String.blit (str_of_int 24)       0 result 0x14 4;
	let pal_off = ref 24 in
	let off_off = ref (24 + xSizeB * ySizeB * 1024) in
	let img_off = ref (24 + xSizeB * ySizeB * 1028) in
	for i = 0 to ySizeB - 1 do
		for j = 0 to xSizeB - 1 do
			let xMax = ref 64 in
			let yMax = ref 64 in
			xMax := min !xMax (xSizeP - j * 64);
			yMax := min !yMax (ySizeP - i * 64);
			let palette = Hashtbl.create (!xMax * !yMax) in
			for k = 0 to !yMax - 1 do
				for l = 0 to !xMax - 1 do
					let cur = mos.(j * 64 + l).(i * 64 + k) in
					let cnt = try Hashtbl.find palette cur with _ -> 0 in
					Hashtbl.replace palette cur (cnt + 1)
				done
			done;
			let curLen = (Hashtbl.length palette) in
			String.blit (str_of_int (!img_off - (24 + xSizeB * ySizeB * 1028))) 0 result !off_off 4;
			off_off := !off_off + 4;
			reduce_palette i j mos palette xMax yMax;
			let m = ref 0 in
			let table = Hashtbl.create 256 in
			let old_pal_off = !pal_off in
			let hasTrans = ref false in
			Hashtbl.iter (fun x _ ->
				match x with
				| Trans ->
					hasTrans := true
				| Pixel(a,b,c,d) ->
					()
			) palette;
			if !hasTrans then (incr m; pal_off := !pal_off + 4);
			Hashtbl.iter (fun x _ ->
				match x with
				| Trans ->
					Hashtbl.add table x 0;
					String.blit (str_of_byte 0) 0 result (0+old_pal_off) 1;
					String.blit (str_of_byte 255) 0 result (1+old_pal_off) 1;
					String.blit (str_of_byte 0) 0 result (2+old_pal_off) 1;
					String.blit (str_of_byte 0) 0 result (3+old_pal_off) 1;
				| Pixel(a,b,c,d) ->
					Hashtbl.add table x !m;
					String.blit (str_of_byte a) 0 result !pal_off 1;
					incr pal_off;
					String.blit (str_of_byte b) 0 result !pal_off 1;
					incr pal_off;
					String.blit (str_of_byte c) 0 result !pal_off 1;
					incr pal_off;
					String.blit (str_of_byte d) 0 result !pal_off 1;
					incr pal_off;
					incr m;
			) palette;
			if !m >= 256 then failwith "Over 256 individual colors";
			while !m < 256 do
					String.blit (str_of_int 0) 0 result !pal_off 4;
					pal_off := !pal_off + 4;
					incr m;
			done;
			for k = 0 to !yMax - 1 do
				for l = 0 to !xMax - 1 do
					String.blit (str_of_byte (Hashtbl.find table mos.(j * 64 + l).(i * 64 + k))) 0 result !img_off 1;
					incr img_off;
				done
			done;
		done;
	done;
	result

