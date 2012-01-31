(* WeiMorph configuration information *)
open BatteriesInit

type variant = BG1
  | BG2
  | IWD2

type configuration = {
    mutable source : Load.game ;
    mutable target : Load.game ; 
    mutable source_variant : variant ;
    mutable target_variant : variant ; 
    mutable premade_npc_directory : string ;
    mutable random_spawn_cre : string ;  (* used for IWG2, actual binary data *) 
    mutable bringer_cre : string ;  (* used for IWG2, actual binary data *) 
    mutable blank_script_bcs : string ;      (* path to a blank script *) 
    mutable tra_directory : string ; 
    mutable baldur_bcs_prepend : string ;
  }

(* current global configuration information *) 
let config = { 
  source = Load.load_null_game () ;
  target = Load.load_null_game () ;
  source_variant = BG2 ;
  target_variant = IWD2 ; 
  premade_npc_directory = "" ; 
  bringer_cre = "" ; 
  random_spawn_cre = "" ; 
  blank_script_bcs = "" ; 
  tra_directory = "" ; 
  baldur_bcs_prepend = "" ; 
} 

let variant_of_string s = match (String.uppercase s) with
| "BG1" -> BG1
| "BG2" -> BG2
| "IWD2" -> IWD2
| _ -> failwith "unsupported infinity engine variant"

let iterdir s f = 
  let s_d_h = try Case_ins.unix_opendir s 
  with _ -> failwith ("cannot open directory " ^ s) in
  begin try
    while true do 
      let s' = Unix.readdir s_d_h in
      let filename = (s ^ "/" ^ s') in 
      try f filename with _ -> () 
    done 
  with e -> (Unix.closedir s_d_h ) end 
