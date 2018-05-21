open BatteriesInit
open Hashtblinit
open Util

type ids = {
    i_num : Int32.t ;
    i_name : string ;
    i_args : i_arg list ;
  } 
and i_arg = { 
    arg_kind : arg_kind ;
    arg_comment : string ;
    arg_file : string ; 
  } 
and arg_kind = Arg_Object 
  | Arg_Action 
  | Arg_Point 
  | Arg_String 
  | Arg_Integer

let print_arg_kind a = match a with
| Arg_Object -> "object" 
| Arg_Action -> "action" 
| Arg_Point  -> "point" 
| Arg_String -> "string" 
| Arg_Integer -> "integer"
