(* Static validation and runtime signals for TP2 loop-control statements. *)

open Tp

type flow =
  | Break
  | Continue

exception Action_flow of flow
exception Patch_flow of flow * string

let validation_error location format =
  Printf.ksprintf
    (fun message ->
      let filename =
        if location.control_filename = "" then "<input>"
        else location.control_filename
      in
      failwith
        (Printf.sprintf "%s:%d:%d: control statement error: %s"
           filename location.control_line location.control_column message))
    format

let validate_action_statement statement location loop_depth =
  if loop_depth = 0 then
    validation_error location "%s is not inside an action loop" statement

let validate_patch_statement statement location loop_depth =
  if loop_depth = 0 then
    validation_error location "%s is not inside a patch loop" statement

let rec validate_action_block loop_depth actions =
  List.iter (validate_action loop_depth) actions

and validate_action loop_depth = function
  | TP_ActionContinue location ->
      validate_action_statement "CONTINUE" location loop_depth
  | TP_ActionBreak location ->
      validate_action_statement "BREAK" location loop_depth
  | TP_ActionBashFor (_, body)
  | TP_ActionPHPEach (_, _, _, body)
  | TP_Action_For_Each (_, _, body)
  | TP_Outer_While (_, body) ->
      validate_action_block (loop_depth + 1) body
  | TP_Outer_For (initializers, _, increments, body) ->
      validate_patch_block 0 initializers ;
      validate_patch_block 0 increments ;
      validate_action_block (loop_depth + 1) body
  | TP_If (_, if_true, if_false) ->
      validate_action_block loop_depth if_true ;
      validate_action_block loop_depth if_false
  | TP_ActionTry (body, handlers) ->
      validate_action_block loop_depth body ;
      List.iter
        (fun (_, _, handler) -> validate_action_block loop_depth handler)
        handlers
  | TP_ActionMatch (_, handlers) ->
      List.iter
        (fun (_, _, handler) -> validate_action_block loop_depth handler)
        handlers
  | TP_WithTra (_, body)
  | TP_WithVarScope body
  | TP_ActionTime (_, body) ->
      validate_action_block loop_depth body
  | TP_Define_Action_Macro (_, _, body)
  | TP_Define_Action_Function (_, _, _, _, _, body)
  | TP_Define_Dimorphic_Function (_, _, _, _, _, body) ->
      validate_action_block 0 body
  | TP_Define_Patch_Macro (_, _, body)
  | TP_Define_Patch_Function (_, _, _, _, _, body) ->
      validate_patch_block 0 body
  | TP_CopyAllGamFiles (patches, _)
  | TP_CopyRandom (_, patches, _)
  | TP_Compile (_, _, patches, _)
  | TP_Outer_Inner_Buff (_, patches)
  | TP_Outer_Inner_Buff_Save (_, _, patches)
  | TP_Extend_Top (_, _, _, patches, _, _)
  | TP_Extend_Bottom (_, _, _, patches, _, _)
  | TP_Alter_TLK patches
  | TP_Alter_TLK_Range (_, _, patches)
  | TP_Alter_TLK_List (_, patches)
  | TP_Create (_, _, _, patches) ->
      validate_patch_block 0 patches
  | TP_Copy args ->
      validate_patch_block 0 args.copy_patch_list
  | TP_Add_Spell (_, _, _, _, patches, if_existing, on_disable) ->
      validate_patch_block 0 patches ;
      (match if_existing with
       | Some body -> validate_patch_block 0 body
       | None -> ()) ;
      (match on_disable with
       | Some body -> validate_patch_block 0 body
       | None -> ())
  | _ -> ()

and validate_patch_block loop_depth patches =
  List.iter (validate_patch loop_depth) patches

and validate_patch loop_depth = function
  | TP_PatchContinue location ->
      validate_patch_statement "CONTINUE" location loop_depth
  | TP_PatchBreak location ->
      validate_patch_statement "BREAK" location loop_depth
  | TP_PatchBashFor (_, body)
  | TP_PatchPHPEach (_, _, _, body)
  | TP_PatchForEach (_, _, body)
  | TP_PatchWhile (_, body) ->
      validate_patch_block (loop_depth + 1) body
  | TP_PatchFor (initializers, _, increments, body) ->
      validate_patch_block 0 initializers ;
      validate_patch_block 0 increments ;
      validate_patch_block (loop_depth + 1) body
  | TP_PatchIf (_, if_true, if_false) ->
      validate_patch_block loop_depth if_true ;
      validate_patch_block loop_depth if_false
  | TP_PatchTry (body, handlers) ->
      validate_patch_block loop_depth body ;
      List.iter
        (fun (_, _, handler) -> validate_patch_block loop_depth handler)
        handlers
  | TP_PatchMatch (_, handlers) ->
      List.iter
        (fun (_, _, handler) -> validate_patch_block loop_depth handler)
        handlers
  | TP_PatchReplaceBCSBlock (_, _, Some body, _, _)
  | TP_PatchReplaceBCSBlockRE (_, _, Some body)
  | TP_PatchWithTra (_, body)
  | TP_PatchWithVarScope body
  | TP_PatchTime (_, body) ->
      validate_patch_block loop_depth body
  | TP_PatchStringEvaluate (_, _, body, _)
  | TP_PatchInnerBuff (_, body)
  | TP_PatchInnerBuffFile (_, body)
  | TP_PatchInnerBuffSave (_, _, body)
  | TP_PatchSavFile (_, _, _, body)
  | TP_DecompileAndPatch body ->
      validate_patch_block 0 body
  | TP_PatchInnerAction actions ->
      validate_action_block 0 actions
  | _ -> ()

let validate_action_root actions =
  validate_action_block 0 actions

let validate_patch_root patches =
  validate_patch_block 0 patches

let validate_tp_file tp =
  List.iter
    (function
      | Always actions
      | Define_Action_Macro (_, _, actions) -> validate_action_root actions
      | Define_Patch_Macro (_, _, patches) -> validate_patch_root patches
      | _ -> ())
    tp.flags ;
  List.iter (fun component -> validate_action_root component.mod_parts)
    tp.module_list
