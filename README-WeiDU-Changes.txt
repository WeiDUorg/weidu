Version 240:
  * GAME_INCLUDES supports SoD.
  * Allow EDIT_SAV_FILE to add wholly new files to SAV files.
  * New NOMOVE keyword for ADD_CRE_ITEM.
  * New TP2 when clause: IF_EXISTS
  * Fix regression in --change-log which produced only the last backup
    file instead of all of them.
  * GAME_IS et al. do not fail on unrecognised games.
  * Use the correct key value for WBM.
  * Add support for MENU, LUA, TTF, PNG, TOT, TOH key values.
  * Add insert_point to ADD_CRE_EFFECT.
  * region_key and region_script for ALTER_AREA_REGION.
  * Add WITH_TRA and PATCH_WITH_TRA.
  * Fix misspelled parameter in ADD_SPELL_EFFECT.
  * ADD_STORE_ITEM evaluates variables for the flag parameter.
  * If engine.lua exists on EE-type games, engine_name will be used for
    USER_DIRECTORY.
  * The array construct automatically evaluates variables in the
    array name ($"%array_name%"())
  * ADD_CRE_EFFECT correctly sets the special parameter.
  * sc#addWmpAre writes the string reference -1 to the corresponding
    fields if the variables strName or strDesc are the empty string.
  * Add NEXT_STRREF value.
  * STRING_SET_EVALUATE can add new strings to the TLK with no deduplication.
  * ADD_JOURNAL supports writing to BGEE.LUA.
  * Update G3 links in the readme.
  * --make-tlk can emit dialogf.tlk
  * Fix bug in fj_are_structure resulting in incorrect deletion of items.
  * Fix documentation error pertaining to fj_are_structure and the variable
    fj_debug.

Version 239:
  * Remove DESCRIBE_ITEM and --list-eff.
  * Fix a problem with CREATEd spells and items of format version V1.
  * COPY_KIT fails better if oldKit does not exist.
  * --transitive does not get caught in infinite loops.
  * WeiDU uses native-looking separator characters (mostly cosmetic) in a few
    places it did not use to.
  * WeiDU is able to resolve the IDS symbol ANYONE.
  * WeiDU can resolve IDS symbols starting with a number (presently known cases).
  * WeiDU can parse MISSILE.IDS (BG2-flavour).
  * COPY_LARGE sets SOURCE_* and DEST_* variables.
  * Add the functions (DIRECTORY|FILE|RES|EXT)_OF_FILESPEC.
  * Add SIZE_OF_FILE.
  * Add DIRECTORY_EXISTS.
  * WeiDU does not verify trigger or action lists during --traify.
  * DELETE can recursively delete directories and everything in them.
  * RES_NAME_OF_SPELL_NUM, RES_NUM_OF_SPELL_NAME and NAME_NUM_OF_SPELL_RES are
    also available as patch functions and macros.
  * NAME_NUM_OF_SPELL_RES matches resource references case-insensitively.
  * Fix regression in OR() indentation.
  * Fix documentation of fj_are_structure: the spawn-point variable
    fj_base_num is unused and its functionality is implemented through
    the variable fj_difficulty.
  * Add fj_(pitch|volume)_variance to fj_are_structure's ambient variables.
  * To save time, WeiDU will not ask what should be done about components
    which cannot be installed.
  * CLONE_EFFECT fails louder on files with invalid signatures.
  * --force-uninstall-rest works as described.
  * As per readme, --safe-exit cannot be used together with --uninstall and
    friends.
  * Add EE_LANGUAGE variable.
  * Fix compatibility issue that had WeiDU resort to dumb-terminal behaviour on
    modern terminal emulators.
  * The functions (DELETE|CLONE)_EFFECT do not misindex files under certain conditions.
  * The ADD_SPELL_EFFECT, ADD_ITEM_EFFECT, ADD_ITEM_EQEFFECT and ADD_CRE_EFFECT
    functions support setting the special parameter. The corresponding macros
    do NOT support this.
  * The ALTER_(ITEM|SPELL)_EFFECT functions support altering the special
    parameter.
  * The functionality of the ALTER_AREA_* functions has been expanded.
  * Document EDIT_SAV_FILE.
  * TP2 precedence order is now MyMod/MyMod.tp2, MyMod/Setup-MyMod.tp2,
    MyMod.tp2, Setup-MyMod.tp2.
  * CREATE does not write length to 'first effect index' in ITM V1.1
  * Add silent option to CLONE_EFFECT and ALTER_EFFECT.
  * Document ADD_SPELL_CFEFFECT and add the corresponding function.
  * Update documentation of ADD_CRE_ITEM.
  * Update documentation of ADD_STORE_ITEM.

Version 238:
  * Add output_path option to HANDLE_AUDIO and HANDLE_TILESETS.
  * Add more keywords to GET_OFFSET_ARRAY and GET_OFFSET_ARRAY2.
  * Fix a compatibility problem between fj_are_structure and the AREA V9.1 format.
  * Fix a bug in the bounds checking when unbiffing tilesets.
  * Document [GET|READ]_STRREF_[F|S|FS] and the corresponding actions.
  * Fix unset-variable regression in ADD_AREA_REGION.
  * HANDLE_CHARSETS does not recurse over the wrong directory.
  * HANDLE_CHARSETS does not convert files present in noconvert_array.
  * Fix regression that broke tolower's recursion.
  * Fix a bug where ENGINE_IS could be true for both bgee and bg2ee.
  * Structural change to how DLGs are emitted, to silence false positives.
  * Add PVRZ functions. Thanks, Argent77.
  * default_language for HANDLE_CHARSETS.

Version 237:
  * The argument to --use-lang is lower-cased.
  * Add --version option.
  * setup-foo-bar(.exe) does not fail to find its TP2 file.
  * Support the EEs' file type 0x405 (GLSL).
  * Fix misnamed variables in ALTER_SPELL_EFFECT.
  * sc#addWmpAre initialises inclSv to a default value.
  * Fix a bug that incapacitated ALTER_ITEM_HEADER's damage_type variable.
  * Add more keywords to GET_OFFSET_ARRAY and GET_OFFSET_ARRAY2.
  * TRB files are no longer supported.
  * Cut WeiMorph.
  * Document SAFE.
  * Correct documentation of LOAD_TRA.
  * Add CREATE TP2 action.
  * ADD_AREA_ITEM and ADD_AREA_REGION are implemented through fj_are_structure.
  * --traify can handle the syntax of male/female string pairs.
  * Fix case-mismatch bug in SET_WEIGHT.
  * Add GAME_INCLUDES TP2 value.
  * Add STR_EQ as an alias for STRING_EQUAL_CASE.
  * REPLACE_EVALUATE defines up to and including MATCH200.
  * Fix variable-evaluation errors in ADD_ITEM_EFFECT.
  * If HANDLE_TILESETS is unable to install the tilesets, the
    installation fails.
  * Add HANDLE_CHARSETS.
  * Actions missing from action.ids are printed as decimal numbers
    rather than hexadecimal ones.
  * Fix bug that prevented WeiDU from finding files in the EEs' lang/
    directory if the main game directory was something other than
    the directory WeiDU was running in.
  * Tile size is not hardcoded to 5120 while biffing tile sets.
  * Biff path separator is (back)slash on x86 and x86_64 OS X and
    colon on PPC OS X.
  * Add (CLONE|DELETE|ALTER)_EFFECT functions. Thanks, CamDawg.
  * Fix bug where the wrong TLKs were loaded if --game pointed to an
    EE-type game.
  * Make BG(2): EE checks more general.
  * Preliminary/speculative compatibility with IWD: EE.
  * Documentation of GAME_IS does not state TOLM is a synonym for TOTLM.
  * WeiDU prints the subcomponent group name in addition to the
    component name where applicable.
  * In ambiguous engine checks, WeiDU prefers BGII: EE over BG: EE.
  * Add EET to GAME_IS.
  * Support SetGlobalTimerRandom().
  * Document WeiDU's script-handling abilities.
  * Check validity of EE TLKs against the right directory.
  * Translatable strings are provided for the BGEE languages Russian
    and Ukrainian.
  * Document MOVE's behaviour when toFile already exists.
  * If WeiDU fails to restore a MOVEd file during uninstallation, a
    warning is printed and the uninstallation continues.
  * Fix a bug in fj_are_structure that could lead to problems, including
    installation failures.

Version 236:
  * HANDLE_AUDIO and HANDLE_TILESETS delete the decompressed files
    upon uninstallation.
  * Fix regressions in fj_are_structure and FJ_CRE_REINDEX affecting
    BGII: EE.
  * WeiDU does not fail on uninstalling files copied from paths
    containing a colon (e.g., C:\something\or\other).
  * sc#addWmpAre does not make backups of save files (because save
    files are not safely uninstallable).
  * Support SetGlobalRandom().

Version 235:
  * Fix an elusive, semi-random bug during biffing. Thanks to tuxr for
    tracking this problem down.
  * Translatable strings are provided for the BGEE languages Japanese,
    Korean and Simplified Chinese.
  * Fix a regression concerning --nogame.
  * Improve ADD_KIT documentation.
  * SAVE_DIRECTORY and MPSAVE_DIRECTORY variables work properly on BG2EE.
  * BGII: EE does not detect as ToB, for purposes of (GAME|ENGINE)_IS.
  * Add the variable %USER_DIRECTORY%.

Version 234:
  * New strings are correctly added during the first run on BGEE.
  * More detailed error message for a particular class of errors.

Version 233:
  * HANDLE_AUDIO and HANDLE_TILESETS (mostly) handle special characters.
  * ADD_MUSIC does not set the wrong variable if the MUS already exists.
  * Document ADD_MUSIC.
  * Optimisations to DECOMPRESS_BIFF.
  * EXTEND_MOS uses the offset table.

Version 232:
  * Fixed blah blah blah for the benefit of non-English speaking people.
  * Fixed a stack overflow when using COPY_EXISTING_REGEXP and you have more
    than ~60000 files between your override and key.
  * Lowercase the file name of binaries on Linux.
  * AUTO_EVAL_STRINGS adds an implicit EVALUATE_BUFFER in front of every patch
    string (notably, when dealing with arrays or functions).
  * In LAUNCH_*_FUNCTION, RET num is shorthand for RET num = num.
  * Fixes to this and that in the macros.
  * REPLACE_ACTION_TEXT evaluates variables.
  * REPLACE_TRIGGER_TEXT evaluates IF and UNLESS also in state triggers, not
    only in trans triggers.
  * Can properly backup COPY actions on files that contains spaces in their
    names (or paths).
  * MOVEs are uninstalled in LIFO and not FIFO order.
  * DELETE, DELETE + added.
  * NAME_NUM_OF_SPELL_RES returns correct results.
  * If a directory named debugs exists, put debug files there, not in ./.
  * Do not fail if reading a bif file > 16 MB that puts the file entries after
    the file contents (instead of before). Thanks Blaze for coding this.
  * Wisp starts meddling.
  * GAME_IS and ENGINE_IS recognise BGEE and BG2EE.
  * Handle BGEE's new file types.
  * Updated the standard library of macros and functions to account for BGEE
    and BG2EE.
  * While loading game resources, create an override directory if none exists.
  * Correct inaccuracies in the documentation of the ADD_AREA_ITEM macro. The
    ADD_AREA_ITEM function defaults to container_to_add_to = 1 instead of 0.
  * Use BG2's TLK format and scripting style for BGEE and BG2EE.
  * We no longer distribute WeiGUI.
  * The variable "%SAVE_DIRECTORY%" evaluates to the location of the current
    game's saves. Also "%MPSAVE_DIRECTORY%" for multiplayer saves. Compatible
    with BGEE. Update sc#addWmpAre and COPY_ALL_GAM_FILES accordingly.
  * Update some of the TP2 tutorials to be slightly more contemporary.
  * Mention MOVE + in the documentation.
  * MOVE falls back onto COPY_LARGE instead of COPY when the destination exists.
  * Add the new functions ALTER_AREA_(ENTRANCE|REGION|ACTOR|CONTAINER|DOOR),
    ALTER_(ITEM|SPELL)_(EFFECT|HEADER), DELETE_(ITEM|SPELL)_HEADER, by CamDawg,
    and the new functions SUBSTRING and ADD_CRE_SCRIPT.
  * fj_are_structure writes ASCII safely for all the user-provided strings.
  * Fail with a better error when doing out-of-bounds READ_2DA_ENTRY_FORMERs.
  * If ADD_PROJECTILE is used to add an already existing projectile, the
    returned IDS value will not be off by 1.
  * ADD_JOURNAL added.
  * The variable MOD_FOLDER is set to the root of the backup directory.
  * Add the function HANDLE_AUDIO, which can install .ogg audio on any supported
    platform and game (including BG:EE).
  * Add the function HANDLE_TILESETS, which can install TISpack-compressed
    tilesets on any supported platform and game (that supports TIS V1).
  * Fix a bug in MOVE that could cause mod rollback to fail.
  * Traify distinguishes between sounded and unsounded strings. Traify also
    doesn't destroy your (D|TP*|BAF) or TRA files on syntax errors or the like.
    --traify-old-tra can be used on the output TRA of --traify to correctly
    merge new strings into an old traification.
  * REPLACE_BCS_BLOCK takes EVAL and optcase.
  * WHEN conditions in *_MATCH and *_TRY work correctly.
  * DECOMPRESS_BIFF is documented but works differently from before (old way
    should still work but is deprecated). It also won't destroy BIFFV1 files.
  * Fix a bug in PRETTY_PRINT_2DA.
  * New underlying system for handling TLKs.
    * BGEE compatibility.
    * --tlkout is no longer a necessary or implied argument.
    * New command line option: --use-lang; use for setting which lang dir
      to use on BGEE games.
    * For complicated reasons, WeiDU can no longer uninstall STRING_SETs from
      TLKs other than the standard ones (so don't use --tlkout on strange TLKs).
    * For similarly complicated reasons, WeiDU can no longer uninstall
      STRING_SETs from dialogf.tlk that does not form a pair with dialog.tlk
      (so don't use --ftlkout on strange TLKs).
  * On BGEE, biffs in lang/ are loaded.
  * Add the functions DELETE_WORLDMAP_LINKS and ADD_WORLDMAP_LINKS.
  * Under some circumstances, strings would not be added in the BG2
    format, despite --script-style, if the game was autodetected as
    something other than BG2.
  * ADD_MUSIC works with more than 100 musics on BGEE/BG2EE.
  * Fix a malformed music-already-exists check in ADD_MUSIC and some instances
    where variables were not being evaluated.
  * Fix malformed already-exists checks in ADD_KIT and ADD_PROJECTILE.
  * WEIDU_ARCH can also be "amd64", on both OS X and GNU/Linux.

Version 231:
  * Fixed a bug that caused DISABLE_FROM_KEY to delete random files instead
    of the ones it was told to.
  * SET_2DA_ENTRY now works correctly if the file begins with a newline and/or
    carriage return (ex. IWD1/TotSC scrlev.ids).
  * Fixed a regression that caused a patch macro to overwrite an action macro
    by the same name (or functions, or vice versa).
  * APPEND_COL_OUTER added.
  * APPEND_COL can take optBackup.
  * ADD_MUSIC, ADD_KIT also check for tob_hacks and/or GemRB when doing bounds
    check.
  * MOVE supports the directory-file-regexp construct in its from section.
  * You can MOVE + (but not MOVE) a file to a file, even if the path contains
    spaces. Don't use this feature without a real reason.
  * Automatically set ~%WEIDU_EXECUTABLE%~ to the name of the currently running
    WeiDU executable (E.G. C:\games\bg2\setup-mymod.exe or /usr/bin/WeiDU).
  * AT_NOW ~VIEW foo~ uses xdg-open instead of firefox on Unix.
  * MAKE_BIFF only biffs files that match against the search regexp.

Version 230:
  * GAME_IS support for IWD-in-BG2.
  * Fixed a case of missing @ reference killing the whole of WeiDU and not
    saving WeiDU.log.
  * MODDER OVERWRITING_FILE prints a message if you are writing to a file that
    exists physically and/or in biffs.
  * MODDER messages are also visible in SILENT mode.
  * *_BASH_FOR lists files in ASCIIbetical order, rather than inverse order.
  * *_BASH_FOR sets BASH_FOR_EXT.
  * APPEND, APPEND_OUTER - works like COPY -.
  * DISABLE_FROM_KEY prints to the log file the name of the files it removes.
  * ACTION_ variants for GET_STRREF_*, TO_LOWER and TO_UPPER.
  * GET_UNIQUE_FILE_NAME uses one IDS file per extension (so it can create
    both __ABCD.CRE and __ABCD.ITM).
  * Fixed MODDER being uninitialised in old-parser mode.
  * Fixed another bug with tolower.
  * Spell(Myself,0) will print a warning in MODDER MISSING_RESREF mode.
  * --change-log can figure out about MOVE, DISABLE_FROM_KEY or MAKE_BIFF.
  * LANGUAGE ~path/with/%WEIDU_OS%variable.tra~ works.
  * --rcmp-from partially implemented. Undocumented because its output is
    unreliable.
  * Synchronize the sample prompts.tra with the real string list.

Version 229:
  * Fixed some errors decompiling DLG files.
  * Some errors were printed but not logged.
  * Fixed an error that was preventing '[S]kip all installed components' from
    working.
  * --print-backtrace prints the stack trace when an error is found.
  * A number of changes made to the compile algorithm.
  * DECOMPILE_AND_PATCH added; it works like DECOMPILE_*_TO_*, except that it
    prevents you from forgetting the matching COMPILE_*_TO_*.
  * REFACTOR_TRIGGER acts as REFACTOR_BCS_TRIGGER or REFACTOR_D_TRIGGER
    depending on file type.
  * Changed something I do not care about in ADD_CRE_EFFECT.
  * FJ_CRE_VALIDITY doesn't break files when converting EFFV1 to/from EFFV2.
  * MODDER MISSING_EVAL warns you if it thinks you forgot to use EVAL in some
    places.
  * Can set MODDER options via --modder command line.
  * Fixed an inconsistency with backslashes on Linux.
  * Added more configurability to FJ_CRE_VALIDITY and subfunctions.
  * More fixes with sc#addWmpAre.

Version 228:
  * If you have MODDER SETUP_TRA NONE, no warnings will be printed if a .tra is
    missing.
  * sc#addWmpAre can set area flags.
  * [ SOUNDNAME ] will now strip the extraneous spaces.
  * New macros and functions: RES_NUM_OF_SPELL_NAME, RES_NAME_OF_SPELL_NUM,
    NAME_NUM_OF_SPELL_RES, GET_UNIQUE_FILE_NAME.
  * ADD_AREA_TYPE tp2 action added.
  * Failures inside REPLACE_EVALUATE now are propagated outside of it.
  * WRITE_* in REPLACE_EVALUATE is really ignored.
  * Tolower now works again on Windows.
  * ADD_STORE_ITEM doesn't break the file when the store sells no items.
  * DECOMPILE_DLG_TO_D uses SOURCE_RES and not SOURCE_FILE as a file name
    (prints BEGIN ~FILE~ and not BEGIN ~FILE.DLG~, avoids breaking the
    resulting file).
  * --quick-menu can install one of the quick selections from the command line.
    Can be combined with --force-(un)install, but only if the latter components
    are defined in ALWAYS_ASK.
  * Added some more code dumps to the docs.
  * COPY_KIT works if there is a case mismatch between two of the kit-related
    2das.
  * WeiDU now translates between TriggerOverride() and NextObjectTrigger()
    syntaxes (for use with ToBEx).
  * REGISTRY_{BG1,BG2,PST,IWD1,IWD2}_PATH variables set (if available) to the
    relevant game path as read from the registry, or to the empty string if
    the game path is not available in the registry.
  * KEEP_CRLF doesn't crash the installer if the file is zero or one byte
    long.
  * INDEX, RINDEX, INDEX_BUFFER, RINDEX_BUFFER added.
  * sc#addWmpAre patches save games under mpsave as well as under save.
  * Visibly fail if ADD_KIT or ADD_MUSIC are going out-of-bounds.

Version 227:
  * REPLACE_BCS_BLOCK(_REGEXP) now parses variable names.
  * REFACTOR_*_TRIGGER correctly clears the "Unfinished OR()" error message.
  * QUICK_MENU added for allowing the modder to specify one or more 'default'
    sets of components that can be installed in a single step.
  * INSTALL_BY_DEFAULT is now stricter.
  * ADD_ITEM_EFFECT will correctly skip magical headers.
  * Can now [R]einstall Widescreen on BG1 and PST (regression).
  * Can compile BAF files with numerical object.ids references (in particular,
    can install Widescreen over Auril's Bane).
  * APPEND(_OUTER) +, MOVE + don't create backups or are uninstalled.

Version 226:
  * Add a space in the "Installing [XYZ]" message.
  * LAUNCH_{PATCH,ACTION}_FUNCTION evaluates variables in the function name.
  * ADD_AREA_REGION_TRIGGER now works again.

Version 225:
  * Remove ARGS.*.TEXT and READLN.*.TEXT when doing temp_to_perm_uninstall.
  * Fixed ADD_STORE_ITEM and REMOVE_STORE_ITEM invalidating STO files under
    BG1.
  * Can now specify header ID and header type when using ITEM_EFFECT_TO_SPELL.
  * FJ_CRE_VALIDITY also enforces proper CRE order and EFFV2 effects.
  * Can ADD_KIT if the kit is already present.
  * STATE_WHICH_SAYS doesn't unload the mod's TRA.
  * WARN tp2 action prints a message and sets the status to 'INSTALLED WITH
    WARNINGS'.
  * PATCH_FAIL, PATCH_WARN do the obvious thing.
  * {PATCH,ACTION}_{MATCH,TRY,RERAISE} added.
  * ADD_MEMORIZED_SPELL (and other CRE-altering commands) doesn't break files
    that were broken by DLTCEP.
  * EXTEND_{TOP,BOTTOM} fails if the .baf file is invalid.
  * COMPILE_BAF_TO_BCS and DECOMPILE_BCS_TO_BAF fails on invalid files (except
    on known-invalid files).
  * Fixed sc#AddWmpAre corrupting links on AR3000 on worldm25.wmp.
  * A failure in COMPILE_BAF_TO_BCS doesn't break the parser.
  * Build-in macros now compare against the current file size, not the size
    reported when loading the file (notably, you can launch DELETE_CRE_EFFECT
    twice on the same CRE file).
  * 'Press ENTER to exit.' is always displayed if a component failed installing
    (unless you explicitly use --no-exit-pause).
  * Close biffs' file descriptors when re-loading the key.
  * Write version info in the debug.

Version 224:
  * Fixed a regression in ADD_PROJECTILE and friends.
  * DECOMPRESS_BIFF added.
  * sc#addWmpAre can patch worldm25.wmp or other worldmap areas.
  * Variables set in the patching code in ADD_SPELL are now visible outside the
    ADD_SPELL itself.
  * LOG, PATCH_LOG added.
  * When creating mymod/backup/*/{UNSETSTR,ARGS,READLN}.*, a textual
    representation of the same file is created in <old file>.TEXT.
  * Answers to *_READLN are logged to setup-mymod.debug.
  * Failing to remove mymod/backup/0 isn't cause to stop installation.
  * MOD_IS_INSTALLED works properly when restoring temporarily uninstalled
    components.

Version 223:
  * Fixed a bug that was breaking REPLACE_TRIGGER_TEXT_REGEXP in some
    circumstances.

Version 222:
  * Silence the irrelevant 'unable to unlink override/add_spell.ids' message.
  * Fixed something or something else with multiple combinations of GROUPS.
  * Fixed a regression that was corrupting CRE files.
  * Patch expressions in INT_VAR are evaluated on the current buffer, not the
    empty one.

Version 221:
  * The Readme explains how to achieve a given ordering on GROUPs.
  * REFACTOR_D_TRIGGER doesn't fail on a malformed input file.
  * Changed the compile environment to use OVerbld. Let me know if WeiDU
    suddenly doesn't work on a standard Win32 machine.
  * ADD_CRE_EFFECT doesn't break files using EFFv1.
  * IS_INSTALLED_AFTER added.
  * ADD_SPELL warns if it's ignoring the constraint list (only in MODDER mode).
  * ADD_SPELL checks spell.ids to avoid duplicating spells (see readme), and
    provides a way to disable the older spell (if it's of different level) or
    patch (rather than overwrite) the older spell (if it's of the same level).
  * Added ADD_SCHOOL and ADD_SECTYPE to create a guaranteed-unique school /
    secondary type.
  * COPY_EXISTING_REGEXP GLOB should be faster at getting the list of files to
    copy (I.E. before printing 'Copying and Patching XYZ files').
  * COPY_EXISTING_REGEXP won't copy and patch the same file twice if it matches
    against two given search regexps.
  * ADD_STORE_ITEM accepts multiple lines in the BEFORE/AFTER clause.
  * TP2_BASE_NAME variable added. If your tp2 file is mymod/setup-mymod.tp2,
    TP2_BASE_NAME is set to mymod.
  * Make TP2_FILE_NAME, TP2_BASE_NAME and COMPONENT_NUMBER available during
    uninstall.
  * If a mod being reinstalled fails its requirements, it will run its
    AT_INTERACTIVE_UNINSTALL commands.
  * Document FILE_IS_IN_COMPRESSED_BIFF.
  * FILE_IS_IN_COMPRESSED_BIFF returns 0 (rather than failing) if the file is
    not in any biff.
  * Added Miloch's fj_are_structure PATCH FUNCTION.
  * RESOLVE_STR_REF patch expression added.
  * BIFF_IS_COMPRESSED patch expression added.
  * SOURCE_BIFF patch added.
  * COPY_TRANS (and the variants of ICT) will print a warning message if the
    state you're *CT-ing into has unsafe actions for that variant of ICT
    (for example, StartCutScene() in ICT2 or DestroySelf() with CT/ICT1 and no
    throwback).
  * ADD_ITEM_EFFECT can accept the type of header to which the effect should be
    added to.
  * LABEL/ID_OF_LABEL added.
  * Create the cache directory if needed while decompiling .cbf files.
  * --biff-get whatever.tis correctly creates the header.
  * AT_(INTERACTIVE_)UNINSTALL_EXIT added. The actions are queued until the end
    of the WeiDU run (similarly to how AT_EXIT works). Don't use without a real
    reason.
  * New translatable strings: "Would you like to display the readme? [Y]es [N]o"
    @-1034, "Using Language" @-1035.
  * Always reset eval_pe_warn to true (and thus not forgetting to print the
    variable name if it is not found).
  * GROUP accepts a constraint.
  * IF/UNLESS added to various D actions.
  * BIT0..31, WNL/LNL/MNL/TAB variables added.
  * LAUNCH_*_FUNCTION INT_VAR a = a now works.
  * LAUNCH_*_FUNCTION INT_VAR a STR_VAR b is a synonym for INT_VAR a = a
    STR_VAR b = "%b%".

Version 220:
  * Fix REFACTOR_*_TRIGGER so that it doesn't loop infinitely if the 'template'
    string contains newlines.

Version 219:
  * IS_SILENT patch expression added.
  * WeInstall mymod/ will ignore the spurious slash.
  * (AT var) is equivalent to @<value of var>.
  * TRA_ENTRY_EXISTS added.
  * REFACTOR_D_TRIGGER, REFACTOR_BAF_TRIGGER added.
  * ADD_STORE_ITEM BEFORE ~itm1 itm2~ doesn't break the file if the store does
    not contain itm1.

Version 218:
  * Even more fixed with ADD_CRE_ITEM and PST/IWD2.
  * Area files are loaded only with MODDER when compiling a script.
  * Add a bunch of documentation.
  * Remove/reword/substantiate 'do not use this' in the documentation.
  * CLEAR_ARRAYS/CODES/INLINED/EVERYTHING to supplement CLEAR_MEMORY.
  * Fixed a case with SUBCOMPONENT and DESIGNATED where [Q]uit did not work.
  * If MODDER AREA_VARIABLES is active, check that the main variable string is
    <= 32 characters.
  * Swap CLASS and SUBRACE in IWD2 scripts.
  * Don't touch colons in IWD2 scripts.
  * Don't break SpellCastEffect in IWD2 scripts.

Version 217:
  * Really fix EDIT_SAV_FILE now.
  * ADD_CRE_ITEM works in PST and IWD2.

Version 216:
  * Trim the useless null termination in EDIT_SAV_FILE.
  * Do not reinstall FORCED_SUBCOMPONENTs if they're already installed and
    the user is using [S]kip all.
  * MOVE changes:
  * If the destination file exists, MOVE reverts to COPY behavior to ensure
    proper uninstallation. Either way, do not touch the same file with MOVE
    and other commands in the same component if you can help it.
  * can MOVE someDir someOtherDir (if someDir contains a subdirectory, a
    warning is printed and the subdirectory is skipped).
  * INTERJECT* IF_FILE_EXISTS target blah blah blah is now possible.
  * DISABLE_FROM_KEY file.ext... added to "remove" one or more files from the
    chitin.key.

Version 215:
   * Fix embarrassing MOVE regression.

Version 214:
  * Default values in FUNCTIONs override the current value in the caller
    environment (unless they're explicitly set via INT_VAR).
  * ++var and --var added.
  * The TOC was absent from the readme because of a stupid error.
  * Removed the macros added in the last version (apparently they were broken).
  * SET_2DA_ENTRY didn't properly fail if the 'row' parameter is == the number
    of long-enough rows. No, I don't care if this breaks your mod - fix it.
  * SPRINT foo #123 will not put the ~~s in the string.
  * Renaming WeInstall to  FastInstall adds automatically --safe-exit
    --quick-log --no-at-view to the options.
  * WRITE_FIELD offset blah blah will implicitly READ_FIELD offset THIS and
    STHIS.
  * README now only works if no component is installed. It used to be the
    reverse  :(
  * SUBCOMPONENT group requisites are now enforced even when using --yes.
  * WRITE_ASCII_LIST added.
  * Fix a couple of backup-related files not being closed.
  * Remove MOVE.* and MAPPINGS.* on uninstall. Remove ARGS.* and READLN.* on
    interactive uninstall. Remove the component-specific backup directory if it
    is empty. Remove the mod-wide backup directory if it is empty.
  * --process-script implies --quick-log --skip-at-view.
  * UNINSTALL_ORDER to customize the order of uninstallation. Miloch should
    provide documentation.
  * Can now Uninstall/Reinstall a component in --safe-exit mode, provided it's
    the last component in the current WeiDU.log.
  * tolower can now merge FoLdEr/SUBFolder and FOLder/subFOLDer. Shoot your
    modder if you need this.
  * Better ADD/REPLACE_CRE_ITEM for IWD2 and PST.
  * --no-exit-pause doesn't give you "Press ENTER to exit.".
  * CBif support under Linux.
  * Tolower under non-BGII.
  * MOVE somefile somedir now works (previously you needed to specify the full
    filename).
  * Print VERSION info during --change-log.

Version 213:
  * --save-components-name added. It will reprint WeiDU.log, while outputting
    the component names (useful if you're used to using --quick-log).
  * --safe-exit added, will save weidu.log after it begins the installation of
    every component. This means that quitting mid-installation will be
    recoverable by the standard uninstall procedure, IF you don't use strange
    stuff (assume that all mods that don't contain biffing and don't backup the
    override will work correctly). Also, you can't uninstall (or reinstall)
    anything while under --safe-exit.
  * MOVE tp2 action added. Don't use.
  * Fixed an embarrassing bug detecting BG1 (&TotSC) via GAME_IS.
  * ADD_CRE_ITEM works with IWD2 (again). I hate whoever coded that game.
  * Skip README and LANGUAGE queries if any component is already installed.
  * Added ADD_AREA_FLOOR_TRIGGER, ADD_AREA_INFO_TRIGGER,
    ADD_AREA_TRANSITION_TRIGGER and ADD_AREA_CONTAINER functions.

Version 212:
  * Various fixes to ADD_AREA_REGION_TRIGGER.
  * Make %COMPONENT_NUMBER% available at uninstall time.
  * Don't link to porn sites from the readme.
  * --traify-comment doesn't break if you use ~~s to wrap your DO actions in
    a .D file.
  * Fix STRING_SET_EVALUATE when the given pe is an unescaped string.
  * Fix REPLACE_EVALUATE to avoid loops.
  * SAY offset #-1 does work. Don't you love 31 bit integers?
  * Can STRING_SET recently added strings.
  * EVALUATE_BUFFER -> EVAL.
  * Compiling D files is now faster (fixed regression).
  * GAME_IS: added CA, check are and not mve files.
  * Will --change-log *.exe and *.key from ./, rather than the override.
  * Fix zlib decompression issues.
  * WEAPON = WEAPON1...4 in ADD_CRE_ITEM.

Version 211:
  * In macro-world, ADD_SPELL_CFEFFECT added.
  * Ton of readme fixes (Taimon, vit-mg, Mike1072).
  * {READ,GET}_STRREF_{F,S,FS} for getting the female/sound string references.
  * REPLACE_ACTION_TEXT, REPLACE_TRANS_{ACTION,TRIGGER} are case insensitive.
  * --log-extern for logging the output of AT_* calls. Coded by Taimon.
  * Detailed error message if LAUNCH_*_FUNCTION specifies a not-defined return
    value.
  * MOD_IS_INSTALLED (and probably something else) is now case-insensitive.
  * AT_* EXACT doesn't alter the case.
  * README can accept any number of file names. All are tried in order, and the
    first one will be opened.
  * EXTEND_MOS respects transparency tiles for compatibility with TutuGUI.
  * ADD_STORE_ITEM blah AT 5 added for devSin.
  * Correctly evaluate variables in strings like ~50% %var%~.
  * EXTEND_TOP/BOTTOM ~scripts/myfile.bs~ ~myfile.baf~ now works.
  * Added ACTION_CLEAR_ARRAY.
  * OUTER_SET var += -= *= etc. added.
  * --dcmp-from --dcmp-to 'works' if from has less states than to.
  * --game-by-type for setting the game path using the game directories in the
    register. Doesn't work on Mac or Linux.
  * If there is no chitin.key in ./ (and --game and friends is not specified),
    try looking for a game installation in ../, ../../, etc (up to four levels
    of depth).
  * Qwinn's ADD_AREA_REGION_TRIGGER macro added.
  * STRING_SET_EVALUATE also accepts strings as first argument
  * WRITE_EVALUATED_ASCII now also allows arrays as second argument

Version 210:
  * LAUNCH_*_FUNCTION can define variables local to the function.
  * Various fixed to {GAME,ENGINE}_IS.
  * LAUNCH_*_FUNCTION on an undefined function will print a clearer error
    message.
  * IS_AN_INT, VARIABLE_IS_SET don't have side effects with $() expressions.
  * I_C_T4 adds the fromaction to all transitions.
  * No "file not found" warning for FILE_MD5.

Version 209:
  * Various tweaks to compiling process. I hate Make; thankfully Taimon doesn't.
  * A lot of readme fixes and tweaks. I hate Tex; thankfully Taimon doesn't.
  * Refactored the tp2 handler for easiness of tweaking.
  * Components under GROUP will always be uninstalled if their predicate fails.
  * Apply the IF_EVAL bug even if there is an (implicit or explicit)
    NO_IF_EVAL_BUG and the current action uses IF_EVAL.
  * Many more variable evaluations.
  * FUNCTIONs added. Pending documentation.
  * REPLACE_TRIGGER_TEXT* is case insensitive in the pattern.
  * D REPLACE_*_TEXT_REGEXP and tp2 EXTEND_*_REGEXP also work on override files.
  * GLOB is always assumed under Elkhound for C_E_R, unless you specify NOGLOB.
  * Can parse STATE_NORMAL in state.ids.
  * Spurious "Warning at src/dc.ml.203" killed unless in --debug-ocaml mode.
  * ADD_PROJECTILE appends to missile.ids as well. Kudos to Galactycon.
  * Fixes to the macros. Thanks to Taimon.
  * Changes under-the-hood to the MACRO/FUNCTION library to facilitate write
    access to multiple authors.
  * ADD_{PROJECTILE,MUSIC,KIT} doesn't add twice if you're a bifftard.
  * In BAF-type sources, you can write """"""(some,really)w!acky s*ym[]bol""""""
    (five double quotes) to get it parsed as a single symbol. Useful for stuff
    like animate.ids in PST. WeiDU will automatically output this when
    decompiling BCS-type sources.
  * Linux WeiDU calls on OCaml 3.10.2.
  * Under the hood tweaks to some CRE functions. Thanks to Taimon for coding
    them.
  * Point parameters in IWD1 BCS files are preserved.
  * --tlk-cmp-to/from now outputs a working tp2 snippet.
  * --automate works correctly for all games. TBD.
  * {{,S}{BYTE,SHORT},LONG}_AT offset, for reading a value from the buffer in
    value-land (without requiring the READ_* bit).
  * --bcmp-to/from decompiles the BCS sources for optimal readability of the
    resulting diff.
  * --textcmp-to/from for textual diffs (2da, ids files).
  * --dcmp-to/from prints APPENDs for the states that are in the new dialogue
    file but not the old one.
  * If --*cmp-from is omitted, grab the file from the biffs and/or override;
    also, the patch is printed as a COPY_EXISTING rather than a COPY.
  * Running APPLY_BCS_PATCH or whatever will print the text and the patch for
    easiness of debugging.
  * GAME_IS BGT added.
  * Tweaks to the TP parser (MOD_IS_INSTALLED).
  * GAME_IS TOB will identify DLTC.
  * --game path/with/trailing/slash/ now works.

Version 208:
  * Thanks to Taimon's code, research and nagging, much faster tp parsing
    (2.5 to 0.6 seconds on 85 KLOC of tp2 over 33 files for me):
  * Removed O(n^3) time in some cases (researched and coded by Taimon).
  * The tp grammar is now left-associative rather than right-associative
    (again, based on Taimon's research).
  * Removed all Reduce/Reduce conflicts, severely lowered number of
    Shift/Reduce conflicts in the tp2 grammar (again, based on Taimon's
    research). (207 had 19 s/r and 66 r/r, 208 has 4 s/r and 0 r/r).
  * Mods that require the new parser have an automatic NO_IF_EVAL_BUG applied.
    Thus, you'll have to simply remember to use VERSION rather than remembering
    to use NO_IF_EVAL_BUG! Isn't that convenient! :)
  * Reverted the Proper TLK Flags change (it breaks Linux).
  * Added Gort's macroes to the library. Thanks, dude!
  * LOAD atrocity removed for library macros (if you really need to compute a
    factorial and/or overwrite iplot*.itm).
  * REPLACE_SAY works properly when the file name is in caps.
  *  *_BASH_FOR sets a BASH_FOR_SIZE variable.
  *  *_BASH_FOR accepts variables.
  * Fix in MAKE_BIFF and variables.
  * AT_* ~command~ EXACT for when you want to preserve the / and the \ as they
    are.
  * Fixed ALTER_TRANS using setup.tra references if you're using AUTO_TRA but
    not USING.
  * ALTER_TLK{,_RANGE,_LIST} to perform arbitrary patches on TLK strings. DNUT.
  * STRING_SET and friends evaluate variables.
  * Fix when compiling point objects in PST.
  * Fixed an ALLOW_MISSING regression.
  * setup-blahblah.exe acknowledges --nogame.
  * (INSERT,DELETE)_BYTES does bounds checking.

Version 207:
  * Can --traify a BAF file containing @xxx references (in particular, also
    with --traify-old-tra).
  * ALTER_TRANS (REPLY,  *JOURNAL, ACTION, TRIGGER) with an empty string will
    remove the feature rather than setting it to the empty string.
  * REPLACE_TRANS_TRIGGER works like REPLACE_TRANS_ACTION, but not like
    REPLACE_STATE_TRIGGER. Do I sense PHP naming conventions coming up?
  * Proper TLK flags are applied, rather than defaulting to 7. devSin and Qwinn
    will have to test this.
  * --change-log shouldn't out-of-memory on stupidly large installations (?).
  * APPEND{_OUTER} blah blah blah KEEP_CRLF added.
  * tolower doesn't crash with Too Many Files.
  * Can COMPILE_BAF_TO_BCS with ~string entries~ or @references.
  *  *_PHP_EACH fixes (coded by Taimon):
  * key_x variables are cleared at each pass (meaning that if you have arrays
    of different length they won't carry over).
  * key is a synonym of key_0.
  * EVALUATE_BUFFER_SPECIAL added for using a character other than %% for
    escaping variable names. DNUT.
  * You can write arbitrary patch expressions after a COMPILE. They'll be
    evaluated before compiling takes place. Like with EXTEND_*, these go between
    the list of to-be-compiled files and the list TRA files. This makes obsolete
    the special EVALUATE_BUFFER that was previously available to COMPILE.

Version 206:
  * Faster-on-older-input doesn't break newer input (this is more serious than
    it sounds).

Version 205:
  * --change-log-{rest,list} were swapped around in the source. This is fixed.
    Thanks to Taimon for finding the bug.
  * ACTION_DEFINE_ASSOCIATIVE_ARRAY added.
  * No more INSTALLED WITH WARNINGS if you're missing an EXTERN and there is no
    MODDER.
  * Faster tp2 parsing in some borderline cases, but only if the file did
    already parse under WeiDU 201. Mods that don't parse under WeiDU 201 take
    an extra 0.2 seconds for the initial parse. Send flames to devSin who
    requested this.
  * REMOVE_CRE_ITEMS, REMOVE_CRE_EFFECTS, REMOVE_KNOWN_SPELLS,
    REMOVE_MEMORIZED_SPELLS, REMOVE_2DA_ROW added for devSin.
  * ADD_PRO, ADD_MUSIC doesn't lowercase the variable name under Linux.
  * FILE_EXISTS_IN_GAME some_large_file doesn't print errors. Apparently, the
    compiler isn't smart enough to figure out that I accidentally missed a
    not  :(
  * Idem for MODDER MISSING_RESREF and movie files.
  * DESIGNATED 0 doesn't break [Q]uit.
  * --traify (et al) tpp/tpa/tph.
  * Fixed parse errors referencing the wrong line for non-indented BAF files
    (and possibly other similar cases). This is not an authorization for not
    indenting files, it's just to limit the damages caused by uncivilised
    people.
  * COPY* sets SOURCE_EXT and DEST_EXT from the source and destination file name
    extensions.
  * Restoring a backed-up chitin.key forces WeiDU to re-load it. This means that
    MAKE_BIFF is correctly restored in Widescreen Mod.
  * Fixes to the d-level IF_FILE_EXISTS option documentation. Many thanks to
    Taimon for writing these.
  * I_C_T{2,4} warning fixes. Actions that differ for a newline/whitespace are
    ignored, and we try to signal if a warning is caused by a third mod ICT2'ing
    here. Obviously, I cannot give full guarantee of avoiding false negatives
    and/or false positives. Use ICT(1) + throwback if you don't like warnings.
  * MODDER ICT2_ACTIONS switch added.
  * @-1012=~Re-installing %s Using Language...~ honored.
  * APPEND(et al) {IF|UNLESS} ~%somevar%~.
  * --clear-memory added for devSin. Calls CLEAR_MEMORY after each tp2 action.
  * SET_2DA_ENTRIES_NOW will not add trailing whitespace. Thanks to Taimon for
    sending the diff.

Version 204:
  * When auto-updating from very old versions (E.G. 154), we don't install the
    mod with the newer WeiDU.
  * While I was there, Windows now lets me re-run setup-olderweidu.exe for you.
  * Fixed false positives with MODDER and DisplayStringHeadOwner or StartMovie.
  * ZLIB compression and decompression routines available in TP2. Search for
    COMPRESS_* and DECOMPRESS_* in the readme. Some Half-orc Half-drow guy
    should provide sample usage.
  * SET_BG2_PROFICIENCY now sets the 'stats' field, whatever that is. It can
    also accept an expression instead of the name from stats.ids.
  * READ_* can use EVALUATE_BUFFER (and whatnot) in the variable name.
  * Fixed autoupdate on Mac not working and blocking mod installation. Shift
    blame on a certain somebody who forgot to test the feature under the
    overpriced OS.
  * AT_*_UNINSTALL works again on Mac.
  * --list-languages, --list-components added for extracting info from a tp2.

Version 203:
  * Warnings by default on parser merges (I.E. possible ambiguities). It was
    made silent to give some time to update TutuFix.
  * In the same vein, B_O_I_I_C END removed (this time, to cover BG1NPC's ass).
  * PRETTY_PRINT_2DA no longer depends on using SET_2DA_ENTRIES_NOW.
  * --make-biff, MAKE_BIFF works in PST.
  * READ_SLONG, REMOVE_CRE_ITEM, REMOVE_STORE_ITEM were defined but not coded.
    This is fixed.
  * Can COPY_KIT morituri.
  * --make-biff, MAKE_BIFF doesn't turn file names to lowercase.
  * SET_BG2_PROFICIENCY added.
  * Honor MODDER when installing, not only when reinstalling.
  * It is possible to select whether to use some debugging options in MODDER,
    and at what level. See the readme.
  * --debug-presence removed and dumped in MODDER options.
  * Fixed a problem with push*/pop_trans balancing.
  * STRING_LENGTH, BUFFER_LENGTH added.
  * --change-log added. Generates a log of which mods altered a given resource,
    as well as dumping all versions of that resource in the directory specified
    by --out. Also has -list and -rest versions.
  * Auto-update on Linux and Mac  :D
  * ALLOW_MISSING ar1010.wed in BG1 works correctly.
  * MAKE_BIFF accepts variables in all arguments.
  * REMOVE_KNOWN_SPELL isn't case sensitive anymore.
  * Added PlainAB's tutorial for READLN.

Version 202:
  * xan/xan.tp2 can uninstall/REQUIRE_COMPONENT/... against setup-xan.tp2.
  * ADD_(KNOWN|MEMORIZED)_SPELL works on Jastey's creature file.
  * AT_INTERACTIVE_NOW works correctly.
  * Moving from ocamlyacc to Elkhound. As an added bonus, if you want to write
    unreadable code you can say 1 TIMES LPAREN 4 PLUS 1 RPAREN as an alias for
    1*(4+1)  :)
  * More GET_OFFSET_ARRAY shorthand.
  * DEFINE_ASSOCIATIVE_ARRAY added. SConrad owes documentation, yada yada.
  * GET_DIRECTORY_ARRAY outArray path, GET_FILE_ARRAY outArray path added.
  * EDIT_SAV_FILE added for SConrad, who owes documentation.
  * No more "INSTALLED WITH WARNINGS" when doing a stack uninstall.
  * Added `ABS value'. It calculates the square root of (`value<<1 - 5).
  * EXTEND_MOS now works in game, not only in Near Infinity. Blame a bug in the
    IESDP  :(
  * Also, additional EXTEND_MOS modes.
  * Better reporting of line number in case of PARSE/LEXER error after an
    inlined file.
  * ADD_PRO/KIT/MUSIC won't fail on Mac/Linux if we have \ in the file name.
  * Tp2 EXTEND_* will acknowledge AUTO_TRA.
  * ACTION_IF foo BEGIN bar END ELSE ACTION_IF foo... allowed.
  * FILE_EXISTS_IN_GAME foo doesn't fail if foo is > 16 MB.
  * In MODDER mode, COMPILE/EXTEND_* will unload the current .tra files (I.E.
    setup.tra). If no .tra file is provided (either via USING or AUTO_TRA) a
    warning is printed (it could likely be a false positive).
  * --biff-get, COPY_EXISTING on tis files fixed.
  * Added CamDawg's tutorial for ALTER_TRANS.

Version 201:
  * Moved versioning system from a simple count to a version.patchlevel format.
    Releases with patchlevel 0 are considered "official", while version X.Y is
    a pre-release version of X+1.0. This will help upgrading for people who are
    using a beta version. "Unstable" versions will print a warning on startup.
  * Don't put tons of meaningless "Unable to Unlink" lines in the log file.
  * COMPILE ~some%var%~ USING ~other%vars%~ works.
  * No more WeiDU.lock. Redirect your "TLK is broken" posts to Kulyok :)
  * README ~something~ will ask you if you want the readme displayed. It will
    be completely ignored during a stack operation. During command-line
    operation it will be automatically displayed, unless you use --skip-at-view.
  * ALTER_TRANS file BEGIN state_list END BEGIN indices_list END BEGIN
    changes_list END added to .d commands. Expect CamDawg to provide some
    documentation.
  * PHP_EACH doesn't fail on an empty array.
  * Fixed a bug with COPY_TRANS filename_in_lower_case not working. Amazing
    how long some bugs can survive.
  * Print "NOT INSTALLED DUE TO ERRORS mod name" if there is a stopping error
    (E.G. a missing file).
  * Print "INSTALLED WITH WARNINGS mod name" if there is a non stopping error
    (E.G. a wrong trigger in a .d file).
  * SUCCESFULLY INSTALLED modname etc. are printed in the correct order.
  * Fixed PRETTY_PRINT_2DA indenting more than the required line.
  * WeInstall reports the correct error code now.
  * EXTEND_MOS "MODE" pixelCount added. Will extend the current file (either
    MOS or MOSC) so that one of its dimensions are preserved, whereas the other
    is increased (*not* decreased) to pixelCount. MODE describes which
    dimension to enlarge and how to compose the image. MODE is one of LEFT
    RIGHT HCENT (enlarging the X axis) or TOP BOTTOM VCENT (enlarging the Y
    axis). NB: LEFT RIGHT TOP BOTTOM say where the black will go, not where the
    image will go.

Version 200:
  * ADD_MEMORIZED_SPELL works even if you have 0 memorized spells and spell
    slots in the given level (unpatched HAER15.CRE). Thanks, devSin.
  * --cmp-from --cmp-to emits patches even if the file size differ. Thanks,
    CamDawg + Miloch.
  * --forceify, --dcmp, --list-biffs, --list-files, --biff, --biff-type,
    --test-trans should all now work again with --out somefile.
  * --dcmp now compares even state 0.
  * PRETTY_PRINT_2DA now accepts the line number to indent (default 2 if not
    provided, counting from 0). This may  break some mods that were using that
    command, sorry for the inconvenience (although I have found no such mod in
    my inspection of installed mods).
  * REPLACE_EVALUATE ~some%var%~ blah_blah now works.
  * COPY_KIT oldKit newKit (list_of_differences) added.
  * Added a lock file to setup-xxx.exe, to forbid users from installing two mods
    at the same time and ruin their install with broken dialog.tlk.
  * ADD_TRANS_ACTION doesn't cause bugs if the file name was in lowercase in the
    .D file.
  * You can say ADD_MEMORIZED_SPELL blah blah (spell count).
  * README ~path/to/readme.html~ tp2 flag added. It'll be launched after
    choosing the language, but before asking for installation. --skip-at-view
    still works.
  * REPLACE_TRANS_ACTION added. Note that this is regexp-style substitution, not
    full action substitution (like REPLACE_STATE_TRIGGER does). WeiDU is a
    pulsating lair of madness and I am a steaming pile of shit, I know.
  * --make-biff and MAKE_BIFF enabled again on Linux. The OCaml developers were
    able to track down and fix the problem that caused the segmentation fault.
  * Won't print the warning when using --make-biff and the folder contains a
    non-IE-specific extension.
  * Can keep a backup of two files with the same name but different directories.
    This will break if you have two files whose path is the same, if you replace
    / and \ with .; that is, if you have readme.debian/something.txt and
    readme/debian.something/txt. Hopefully, it won't happen in any sanely
    organized install.
  * {,PATCH_,ACTION_}PHP_EACH, {,PATCH_,ACTION_}DEFINE_ARRAY, GET_OFFSET_ARRAY,
    GET_OFFSET_ARRAY2, CLEAR_ARRAY added.
    SConrad owes complete documentation. I won't even try to write down what
    some of those does.
  * No more warning on using COPY on Dialog.tlk unless you use MODDER.
  * `READ_ASCII where what (32) NULL' added. It will stop at the first 0x00
    character, whereas `READ_ASCII where (32)' would read exactly 32 bytes.
  * ADD_MEMORIZED_SPELL blah blah #count added for DS.
  * ADD_MEMORIZED_SPELL will also ADD_KNOWN_SPELL.
  * ADD_KNOWN_SPELL does nothing if the spell is already known.
  * --traify-old-tra for merging new strings into an already traified file.
    Works with --traify-comment'ed files, doesn't require --traify# but is
    compatible, and doesn't put twice the same string in the .tra.
  * Updated the --traify tutorial to account for --traify-comment and
    --traify-old-tra.
  * No more CTD (& broken game) if a component name is a non-existing @xxx
    reference.

TODO:
  * Heavy refactoring under the hood. There should be no visible change, but
    debugging and feature addition is much easier.

Version 199:
  * COUND_2DA_COLS, PRETTY_PRINT_2DA tp2 patches added.
  * PATCH_(RE)INCLUDE ~something.tpp~ added. It's like INCLUDE something.tph,
    but it includes patches rather than actions.
  * SPACES var string will create a string %var%, of the same length of string,
    but containing only spaces (0x20). SPACES var ~12~ will crate %var% = "  ".
  * QUOTE var string will create a string %var%, which is a regexp string
    matching exactly string. QUOTE var "some^thing\" will create %var% =
    "some\^thing\\".
  * Will warn for typoes in EXTERNs with numeric exit state
    ("EXTERN missing-file 0" or variations).
  * Removed reference to SET_WEIGTH in tlexer.mll and tparser.mly. It's just for
    theoretical correctness, it doesn't cause actual problems.
  * weidu_fast_loading -related crap removed (see v192 log).
  * You can do COMPILE EVALUATE_BUFFER somefile (previously it was parsed, but
    not actually executed).
  * GAME_IS now actually works for multiple games (doesn't default to the last
    one).
  * MODDER tp2 flag added. If you're installing a tp2  *and* the MODDER flag
    is absent, warnings like "This expression has a typo" are disabled. If
    MODDER is there, or you're not installing a tp2 (E.G. you're compiling
    something via WeiDU), warnings are displayed as before.
  * --tcmp-from something.tra --tcmp-to somethingelse.tra improvement: it now
    outputs a valid tra file containing the strings to be translated (as opposed
    to a flat list of numbers).
  * If MODDER is enabled, DO ~something()~ DO ~somethingElse()~ reports an
    error; even if MODDER is not enabled, both actions are compiled as if it
    were DO ~Something() SomethingElse()~.
  * --make-biff and MAKE_BIFF disabled under Linux, because that action causes
    it to segfault randomly.
  * COUNT_REGEXP_INSTANCES bugfix. It wasn't counting correctly in case any of
    the following were matched: "^myregexp", "myregexpmyregexp", "myregexp$".
  * Absence of MODDER shuts up also the following lines from the .DEBUG file:
  * Not copying...
  * Defined inlined...
  * Appending file columnwise
  * XXX parsed.
  * Warning: Anyfile is a zero-byte file
  * ADD_MEMORIZED_SPELL spellname level spell_type added. Is a carbon copy of
    ADD_KNOWN_SPELL in syntax.
  * If there was an error and we are on Windows/Mac, we display the
    "Press enter to exit" message, to allow to copy logs even for
    double-clicking users.
  * AT_*EXIT is run before the "Press enter to exit" message, to allow to copy
    error messages in spawned processes even for double-clicking users.
  * When decompiling a BCS in IWD1, we don't strip the colon in  *BitGlobal
    actions and triggers.
  * Updated GROUP documentation: a mod can belong to multiple groups, and will
    be shown according to the OR of them. However, a GROUP must be the first in
    the list of at least one component (even a dummy, DEPRECATED one) in order
    to be asked about. Fixing this is harder than having you workaround it  :)
  * STATE_WHICH_SAYS foo in ~somefile~ doesn't require .dlg in somefile.

Version 198:
  * tolower will not lowercase the chitin and create the symlinks anymore, because
    there's a diff to turn all file calls in Near Infinity to lowercase as well.
    While it isn't merged in the main Near Infinity tree, you can get the diff and
    recompile Near Infinity from the first post at the following URL:
    http://forums.pocketplane.net/index.php/topic,23303.html
  * Improvements to BAF <-> BCS in IWD2: will now consider object rectangles, and will
    use correctly AVCLASS and CLASSMASK. There are still bugs, especially when dealing
    with new and creative ways of coding fixed strings.
  * weidu --out foo won't open (and overwrite) foo, unless I have something to actually
    write there.
  * --biff-get foo.res will whine if it didn't find any file.
  * LAUNCH_*_MACRO ~some_%var%~ works.
  * Fixed link to #offset in the readme.
  * Can --automate EFF files.

Version 197:
  * Un*x will launch firefox (rather than 'less') for AT_* "VIEW foo". Thus, you
    need firefox installed to view readmes. Raise your hand if you don't have it.
  * Forgot some shorthands from tlexer.
  * Tolower will lowercase the contents of chitin.key and create a symlink
    Override -> override to help NI on case-sensitive file systems.
  * Chitin.key handling slightly changed. Please report bugs (of the 'it claims it cannot
    find a file' type).
  * [N]o change, when the component is temporarily uninstalled, will reinstall said
    component now rather than at the end of the tp2 sequence. Should avoid griefs
    with REQUIRE_FILE and whatnot.
  * Small typo in the tb#fix_file_size macro. It'll allow to install Refinements on
    Linux, but it's a very minor point anyhow.

Version 196:
  * Mac version doesn't require the TK and X libraries (I hope, at least).
  * $array(index list) added. Read the tutorial on this feature, use it in a
    actually useful way, and send me the portion of code, so that I can craft a better
    example. Search the docs for 'array construct' to read the horrible example given.
  * ADD_KIT should install on SoA only.
  * ADD_KIT will add properly the ToB entries.
  * No more warnings on NO_LOG_RECORD.
  * --traify-comment typo solved.
  * Tp2 positions in which you could use only #int (E.G. ADD_MAP_NOTE) you can now use
    either the old #int or use any tp2 expression, PROVIDED IT IS BETWEEN PARENS.
      Example:
        ADD_MAP_NOTE #300 (200 + x) ~GRAY~ @1 is correct, while
        ADD_MAP_NOTE #300 200 + x ~GRAY~ @1 is not.
  * INCLUDE and REINCLUDE documentation added.
  * I split the D and TP2 lexer and parsers (since the joining of the two was too big
    for OcamlYacc to use). Report all instances of parse errors arising in already
    existing code.
  * ADD_CRE_ITEM should work with IWD2 (CRE V2.2). PLZ test.
  * BitGlobal fixes in IWD1 and IWD2 (split the string, remove the extraneous ':').
  * Fixed point parameter in PST scripts (no more failures).
  * MoveToSavedLocation(S:*,S:*) in BG now handles correctly the two strings.
  * The Linux version of WeiDU actually loads your CDx directories.
  * WeiGUI and WeInstall now also work on non-Windows.

Version 195:
  * Fixed IDS parsings (led to some warnings).
  * Fixed false positives in  *Global* typoes.
  * WRITE_ASCII_TERMINATE added.
  * WRITE_ASCIIE is a synonym of WRITE_EVALUATED_ASCII.
  * WRITE_ASCIIT is a synonym of WRITE_ASCII_TERMINATE.

Version 193/194:
  * More ADD_STORE_ITEM bugfixes if you use AFTER and the position item doesn't exist.
  * No more \t characters in 2da files.
  * --process-script added. Just an hack to prepare for the GUI installer.
  * The main () function is timed. Hopefully timings would be somewhat more correct.
  * Additional programs are shipped with the zip.
    - weinstall.exe: if you copy this and weidu.exe in your PATH directory (E.G.
      C:\windows\system32), you can type "weinstall mymod someoptions" as a shortcut
      for "weidu --tlkout dialog.tlk --ftlkout dialogf.tlk --log debugs/mymod.debug mymod.tp2 setup-mymod.tp2" etc.
      It can also handle --force-install 1 2 3 options (in non-weinstall WeiDU, you'd have
      to weidu other_options --force-install 1 --force-install 2 --force-install 3.
      It requires you to "mkdir debugs" in the bg2 directory beforehand though.
    - weigui.exe: copy weidu.exe in your PATH (it doesn't currently work if you put it in
      your bg2 directory) and weigui in the bg2 directory, along with tk83.dll. Weigui is
      a first beta of the infamous weidu installer gui. It doesn't currently feature the
      online compatibility database and it doesn't handle SUBCOMPONENTS or REQUIRE xxx
      predicates, but it's still a nicer frontend to WeiDU, especially if you have to
      install a specific mod in a bottom-of-the-stack position. Mods that use biffing (or
      other unusual modding techniques, such as backing up the tlk) via a script should put
      "// uses biffing" at the beginning of the tp2. Decompressing TIZ or OGG works fine
      without the warning, as does tp2-level MAKE_BIFF.
    - tk83.dll : required by weigui.exe.
  * APPEND_FILE_EVALUATE now evaluates both the filename and the contents.
  * Added a patch to decompress .cbf files for IWD (special thanks to Fred S Richardson for
    coding this one).
  * Better handling of --game on Macs.
  * ADD_KIT will pretty-print weapprof.2da and 25stweap.2da. If this slows down your
    mod installation, complain with Grim Squeaker.
  * Fixed --traify typo in the readme.
  * Will warn if you typed Global("foo","LOCAL",3) or somesuch.
  * Can work correctly with the first entry in stock STATE.IDS, SLOTS.IDS and others. Before,
    WeiDU used to skip the first line. Let me know if this generates warnings in your mod.
  * ADD_SPELL for adding spells to the general spellbook (the SPxxyztt namespace).
  * --untraify-d, --untraify-tra added. Note 1: you can use --untraify-d also for tp2 and baf.
    Note 2: it may happen that the untraified file no longer works. Life is hard.
  * ASK_EVERY_COMPONENT + GROUP behaviour fix.

Version 192:
  * You can now --make-biff (or tp2-level BIFF) a .tis file without the need of stripping it of
    the header beforehand, WeiDU takes care of this by itself.
  * Added BRANCH to the list of chain possibilities.
  * STATE_WHICH_SAYS lse FROM string, STATE_WHICH_SAYS patchexp IN string FROM string.
  * Can now use inlined tra/trb files.
  * If the global environment weidu_fast_loading is set to 1, loading times are faster by one
    second  *if the tlk or the key don't change*. Won't affect you, unless you spend too much
    time on repeating the same install over and over. Use at your own risk. (Hint: I don't use
    this).
  * Saving the log without --quick-log is faster by 2 seconds.
  * Problem (?) with file descriptors remaining opened should (?) be solved.
  * GROUP added. CamDawg owes docs.
  * ACTION_READLN and PATCH_READLN added. SimDing0 owes docs.
  * IS_AN_INT added.
  * FORCED_SUBCOMPONENT.
  * If all the parts of a SUBCOMPONENT fail their requirement, we don't ask about them now.
  * ADD_STORE_ITEM may work if the store file is misordered.
  * You can ADD_STORE_ITEM BEFORE|AFTER ~list_of_items~; they are tried in the order in which
    they are listed in list_of_items.
  * REPLACE_CRE_ITEM added. Since I don't care about this type of higher-level functions,
    it's likely that there are some bugs left in (which I will fix to the best of my ability).
  * More hacks in the baf lexer. Notably, X#Coran.baf from BG1 NPC project will work, and
    the action/parser lexer in D files are now - tentatively - switched to the new lexer as well.

Version 191:
  * You can use IF_FILE_EXISTS in a bunch of places in D files, causing the current transition
    or block to be skipped if the file is missing, without halting the compilation or whatever.
    Missing labels are still critical.
  * I_C_T3 will copy all new states into the starting transitions, not only the first one..
    bah, Ding0 owes docs as well  :)
  * I_C_T4 adds both the I_C_T2 actions exception and the I_C_T3 multiple copies.
  * Bunch of tp2 changes (IE more variable handling, that kind of stuff).
  * CLEAR_IDS_MAP tp2 action added.
  * ADD_PROJECTILE and ADD_MUSIC check for already existing entries before adding.
  * APPEND ~something.ids~ will clear_ids_map.
  * On the Unix version, all file system calls are mapped to a lowercased string.  *nix users
    should now be able to run WeiDU if they compile it. No change on MacOSX-PPC or Win32.
  * As a bonus, \ -> / always occurs on Mac/Linux, except a couple of places where
    I may have forgotten it in inlined files (where it won't matter anyway).
  *  *-rest command line parameters now have a  *-list variant, that basically allows
    weidu --biff-get-list sw1h01\.itm sw1h02\.itm --out foo to work like you'd expect.
  * The tp2 action SILENCE will block all printing on the stdout; VERBOSE will enable
    again all printing on the stdout. PRINT, PATCH_PRINT and end of component
    will automatically call VERBOSE, so remember to re-set SILENT after those.
    Also, PATCH_SILENCE and PATCH_VERBOSE.
  * ACTION_FOR_EACH variable IN string list BEGIN action list END and
    PATCH_FOR_EACH variable IN string list BEGIN patch list END added.

Version 190:
  * Removed some left-over extra debug info.
  * Added some debug info for MKDIR to help find out Horred the Plague's problem.
  * Failing mod flags will lead to uninstallation, except for the tricky 'can become false'
    ones, such as FORBID_FILE, FORBID_COMPONENT and REQUIRE_PREDICATE.
  * ACTION_IF ~%bar%~ THEN BEGIN AT_*UNINSTALL ~do_something~ END works. Other types of
    loops (EG INNER_ACTION, OUTER_FOR) do not.
    Keep in mind that variables need to have been declared (EG, stick to WEIDU_OS &C.).
  * UNINSTALL ~%myvar%.tp2~ 3 + 5 - "%somevar%" works.
  * APPEND_FILE_EVALUATE will evaluate %variables% in the file to be appended.
  * --traify-comment will leave /* @1 = ~xyz~  */ comments in the source file.
  * --args, --args-rest foo bar will set %argv0% to foo and %argv1% to bar. See readme.
  * Can now uninstall STRING_SET even if the UNSETSTR.xxx file is > Sys.max_string_lenght.
  * Can now uninstall the mod even if UNINSTALL.xxx is > Sys.max_string_lenght.
  * VARIABLE_IS_SET will allow to detect at install-time if a certain variable is set or not
    (currently there's no way to tell if it can be used as an integer or not).
  * MAKE_BIFF name_of_biff BEGIN list_of_files END added. It features automatic safe backup and
    reloading of the key file. list_of_files is in a non-trivial format, see the readme.
    (incidentally, it's the same format as the non-documented ACTION_BASH_FOR / PATCH_BASH_FOR).
  * Added D:\Program Files\Black Isle\BGII - SoA to the list of directories that are searched for
    the chitin.key file.
  * STRING_SET_EVALUATE patch_exp new_string added.
  * IDS_OF_SYMBOL (~ids_file~ ~SYMBOL~) patch expression added. Returns -1 if SYMBOL is not in ids_file,
    fails the installation if ids_file is missing altogether.
  * INCLUDE ~list_of_tph_files~ added. A tph file is a simple list of tp2 actions. Once this action
    is found, all actions in each tph file will be executed.
  * ALWAYS AT_*UNINSTALL ~foo~ works.
  * AT_*UNINSTALL in INCLUDEd files works.
  * For now, AT_*UNINSTALL does NOT work in macroes (either in main tp2, in tph, or from SML)

Version 189:
  * ADD_STORE_ITEM allows NONE as a valid flag.
  * ADD_STORE_ITEM BEFORE|AFTER ~%somevar%~ works.
  * READ_2DA_ENTRY_FORMER will now whine and halt if you're performing a R_2_E_N
    with a currently not initialized string, or doing a read out of bounds.
  * ADD_STORE_ITEM bugfixes; you can now AFTER the last item (or BEFORE the first);
    added LAST|FIRST as position identifiers.
  * ~!Triggers()~ in .d files no longer cause warnings in the DEBUG file.
  * READ_2DA_ENTRIES_NOW/FORMER and SET_2DA_ENTRY_LATER/NOW accept variables for
    that-string-whose-purpose-is-OMG-so-obscure.
  * Now the macro tb#fix_file_size works (assuming it was ever broken, which isn't
    necessarily true).
  * APPEND_OUTER works like APPEND, except that the file is loaded as an actual file,
    rather than as a biff or override resource (think like COPY vs. COPY_EXISTING).
  * SCRIPT_STYLE ~IWD~ works correctly.
  * ADD_CRE_ITEM ~QUIVER~, ~QITEM~, ~INV~, ~RING~ are shortcuts for ~QUIVER1 QUIVER2
    QUIVER3~, ~QITEM1 QITEM2 QITEM3~ etc.
  * No longer wrong warnings in .d file when decompiling abela.dlg.
  * BACKUP "/an/absolute/directory" works again. Please note that "/an/absolute/directory"
    DOESN'T work on Windows (you'll have to forget platform interoperability and use
    "\an\absolute\directory").
  * INSERT_2DA_ROW bugfixes (uses \r\n, no longer inserts \n at the beginning, adds a
    \r\n at the end).
  * PRINT, PATCH_PRINT no longer lose their newline.
  * TO_UPPER ~somevar~ will turn to uppercase the contents of somevar.
  * TO_LOWER ~somevar~ will display a random porn picture.
  * REMOVE_MEMORIZED_SPELL spell_list added. Please stress-test the function and check
    thoroughly the memorization info. This hasn't been tested at all on CRE V9.0.
  * Another REQUIRE_FILE ~data\25spells.bif~ + MIT bugfix. (MIT=Multi Install Tool).
  * COPY will revert to COPY_LARGE if there are no patches or constraints; COPY_LARGE
    will revert to COPY (with a dummy IF_EVAL 1 constraint to avoid looping) if the
    src file is inlined.
  * Tutorials which get updated (or added) will be duly noted from now on in the readme.
    Notabily, I changed all references to IF_EVAL to PATCH_IF + B_O_I_I_C changes, and
    mentioned SOURCE_SIZE sanity checks as well. Check if I've missed a spot.
  * Various tentatives of improving --traify and --trbify with --out.
  * TEXT_SPRINT, LOCAL_TEXT_SPRINT and OUTER_TEXT_SPRINT can be used like SPRINT, but will not
    be traified.
  * Compiling triggers and actions in .d files now reverts to v185 behaviour - notably,
    Kit(Myself,A!Kit) will not work anymore. This brings in too many unexpected problems.
  * ADD_CRE_ITEM bugfix.
  * COPY_LARGE with smaller file bugfix.
  * Helpful debug info if the modder has bad habits (EG \ in file names, AT_* ~NOTEPAD this~...)
  * --debug-change will print out if the patches don't alter the buffer.

Version 188:
  * All files are chmod'ded to a+wrx (hopefully).
  * You can save files as inlined via the `-' optional flag. See readme.
  * --debug-ocaml will enable more debugging info in random places. (Note: this
    is Bigg's way to replicate ASSERTs in OcaML, so you'll find them only where
    I had problems debugging a particular addition).
  * ADD_STORE_ITEM can define the position of the item to add.
  * Some standard macroes are shipped with weidu.exe. See readme for description
    and usage instructions. They must be initialized with LOAD.
  * tb#factorial
  * tb#fix_file_size
  * Macroes can define local variables: LOCAL_SET, LOCAL_SPRINT (see readme).
  * LOAD allows to load macroes from the standard library.
  * Uninstalling a mod with biffed files now works. My fault.
  * INNER_PATCH_SAVE et similia may now work.
  * REPLACE_TEXTUALLY from to (length) for patching binary files. This will
    not be evaluated correctly if the from string contains regexp constructs
    different from '.'; it'll work with variables.
  * You can specify whether the matching in REPLACE_TEXTUALLY, REPLACE_EVALUATE,
    REPLACE and COUNT_REGEXP_INSTANCES is case-sensitive or not. The standard is
    not changed, but you're advised to specify that manually anyway.
  * You can specify whether the matching in REPLACE_TEXTUALLY, REPLACE and
    COUNT_REGEXP_INSTANCES is exact or uses regexp patterns. The standard is
    not changed, but you're advised to specify that manually anyway.
  * DevSin diffs:
  * ADD_CRE_ITEM EQUIP bugfix
  * ADD_CRE_ITEM slot_list is now case-insensitive.
  * ` rather than ~ means BNOT.
  * USING accepts variables.
  * ^^= means XOR_EQUALS.
  * Some fixes to the readme.
  * Tricky uninstalling/reinstalling patterns now work (again, my fault).
  * COPY_ALL_GAM_FILES added for igi. THIS FEATURE DOESN'T ALLOW BACKUP.
  * The log now also contains the name of the subcomponent - EG
    ~TB#TWEAKS/TB#TWEAKS.TP2~ #0 #2550 // Faster Romances: -> Medium. (v2)
  * COPY_EXISTING_REGEXP ~%some_var%~ may work as you hoped it would.
  * APPEND ~%somevar%~ ~a_string~ works.
  * Tutorial about macroes (defining, launching, pseudo-scope, standard macroes).
  * Kudos to Ascension64 for revising the tutorials!
  * --debug-boiic will print out the name of the files that  *did* change in a
    COPY_* command that uses BUT_ONLY_IF_IT_CHANGES.
  * FORBID_COMPONENT may now work.
  * "Bash-style" FOR loops are available (ACTION/PATCH_BASH_FOR variable
    IN (directory regexp) list BEGIN action/patch list END)
  * REPLACE ~%var1%~ ~%var2%~ works.
  * Bugfixes when parsing IWD1/IWD2 IDS files (thanks FSR).
  * LOAD_TRA string_list action added.
  * GAME_IS ~list_of_games~ added as patch_exp.
  * GET_STRREF patch_exp variable added.
  * Tp2 and .D syntax is now case-insensitive (EG bEgIn @1 now works).
    Bonus points if you write all of your tp2 in alternating capitals.
  * MOD_IS_INSTALLED patch_exp added.
  * BUT_ONLY is shorthand for BUT_ONLY_IF_IT_CHANGES.
  * INSERT_2DA_ROW patch_exp patch_exp string.
  * COMPILE ~somedir~ works again. Sorry.

Version 187:
  * Is mostly the same as 186. 186 was used by Bigg for two months as a beta
    release. The version increase is to help upgrading for the various people
    who have used the 186 one for testing.
  * Bugfix when [U]ninstall All and the mod contains SUBCOMPONENTS.
  * Rules for identifying IDS strings inside BAFS are improved: notably,
    Kit(Player1,K!A_KIT) can get compiled.
  * Answering [U]ninstall when the component is not installed no longer gives
    an ugly "Uninstallation Failed" error (with instructions to send the debug
    file to xyz).
  * When asking about what to do with all installed and all non-installed
    components, 'Q' is a synonym of [S]kip (IE you have to give it twice to
    exit completely). That one always gets me :)
  * COUNT_REGEXP_INSTANCES regexp var counts how many `regexp` occurs inside
    the file, and puts the value inside var.
  * \ -> / conversion for Mac OS X users, I hope.
  * CLEAR_MEMORY added for Idobek. Don't. Use. This. Feature.
  * Some randomness with some evil SUBCOMPONENT grouping solved; if we have
    BEGIN ~a~ SUBCOMPONENT ~1~ FILE_EXIST ~somefile~
    BEGIN ~b~ SUBCOMPONENT ~1~ NOT FILE_EXIST ~somefile~
    and we have installed the first, then the file disappears, the first
    component is uninstalled, allowing us to install the second (in the slim
    chance that the file disappeared).
  * For Mac Users, .DS_Store is ignored by COMPILE.
  * --debug-presence tries to look if files referenced by BAF files are really
    there or are instead missing.

Version 186:
  * Bigg starts meddling (note: in the version string, 'Hybris' is a Greek
    term meaning something like 'excessive pride'.
  * STRING_EQUAL and STRING_EQUAL_CASE are similar to STRING_COMPARE[_CASE],
    however the return values are more obvious (true if equal, false if
    different).
  * APPEND_COL now takes variables (and evaluates them before counting the
    spaces). It can now prepend any amount of empty items to the string -
    see the readme.
  * INNER_PATCH_SAVE savevar buff added (see the readme).
  * TP2_FILE_NAME variable added.
  * When the mod has 4 or more components, and no --ask-all option is
    chosen, WeiDU will print out the component name, EG
    "The setup-npcflirt.tp2 mod has 16 distinct optional components."
  * When choosing whether to install a mod component or not, you can now give
    [I]nstall as a synonym of the still valid [Y]es. This way, you can make
    better prompts.tra files.
  * NO_IF_EVAL_BUG tp2 flag added. Don't use IF_EVAL with it (it'll usually cause
    crashes or other random behaviours).
  * --language X --force-install X, --force-uninstall X, --force-install-rest X Y...
    and --force-uninstall-rest X Y... command-line parameters added, see manual
    for description.
  * %INTERACTIVE% tp2 variable added; if installation is interactive it's set to 1,
    otherwise it's set to 0.
  * REQUIRE_FILE, FORBID_FILE and FILE_EXIST, when fed a 25.*\.bif file, look if it is
    referenced in the key, rather than looking for the actual file (to workaround
    the problem with multiple installs).
  * ADD_KIT ~swashtf~ will work if the kit sswashtf (or swashfta) is already present;
    it will set the %swashtf% even if the swashtf kit is already present.
  * COPY ~%a_var%~ ~override~ works even if %a_var% is a directory.
  * If you choose [I]nstall All or [R]einstall All options, you still get asked
    explicitly about subcomponents. This does not happen with CLI options such as
    --yes and similar. Hint: take the chance to learn how to use the cli :)
  * MKDIR ~%somevar%~ now works. MKDIR ~a/b/c~ will work even if the a/b directory
    doesn't exist. More formally, it'll perform like MDIR ~a~ ~a/b~ ~a/b/c~.
  * The BACKUP directory is created if missing.
  * Since most modders out there are dumbasses and use AT_INTERACTIVE_VIEW
    ~NOTEPAD this~ and~EXPLORER this~, now these two constructs are equivalent to
    the correct ~VIEW this~.
  * --skip-at-view blocks AT_* ~VIEW~ actions (and synonyms as the above entry), while
    the rest of actions (EG batch/shell files) are run as normal.
  * --quick-log doesn't print the name of installed components in the log (saves around
    4 seconds per mod for people with large collections).
  * Long-time pending tutorial for SET_2DA_ENTRY_LATER/NOW added.
  * READ_2DA_ENTRIES_NOW/READ_2DA_ENTRY_FORMER added, they work somewhat like
    SET_2DA_ENTRY_LATER/SET_2DA_ENTRIES_NOW and are supposed to be faster. See the
    (rather skinny) tutorial.
  * Biffs are created as a+wrx at the request of Loriel.
  * COPY_LARGE allows to copy (without patching) files that exceed the standard
     size limit.
  * Backup works even if the destination file is too big for the standard size limit.
  * AT_NOW and AT_INTERACTIVE_NOW are run when found, rather than at the end of the
    installation. DO NOT USE THEM WITH WEIDU --make-biff.
  * DEFINE/LAUNCH_ACTION_MACRO (with MACRO_ACTION as synonym) added, they are similar
    to the so-called functions in bash. On the same line, DEFINE/LAUNCH_PATCH_MACRO
    (with MACRO_PATCH as synonym). Tutorials pending.

Version 185:
  * TP2_AUTHOR variable for bigg.
  * COUNT_2DA_ROWS added for Diamond.
  * Bunch of devSin changes:
  * ADD_KIT bugfix.
  * Un-SET_STRING message removed.
  * !, &, |, &&, ||, << and >> do just what they do in C.
  * >>=, <<=, ~, ^^ added.
  * == works as an equality test for integers.
  * PATCH_RANDOM_SEED added.
  * RANDOM_SEED -- if you pass a non-integer it now does a self-init

Version 184:
  * --traify is back to the old behavior for ~~ empty strings. Don't use
    those!
  * STRING_MATCHES_REGEXP (= STRING_COMPARE_REGEXP) and
    STRING_CONTAINS_REGEXP added.
  * STRING_SET (_RANGE) tra files now take %variables%.
  * WeiDU TP2 actions like "COMPILE myDir" should no longer lock that
    directory "forever".
  * Exponentiation added: a  ** b     == a^b
                          a  ** (b c) == a^(b/c)

Version 183:
  * Fixed a bug with --reinstall and mods with many components.
  * WRITE_BYTE offset negativeNumber should now work correctly.
  * --noautoupdatemsg may now actually work.
  * COMPILE_D_TO_DLG will no longer leave FOO.DLG.DLG lying around.
  * Minor --traify bugfix involving empty strings with sounds.
  * ADD_TRANS_ACTION now handles @123 and whatnot.
  * STRING_SET_RANGE added.

Version 182:
  * EXTEND_*_REGEXP bug fixed. Thanks, bigg.
  * <<<<<<<< now substitutes variables in the filename.
  * STRING_SET now takes a list of arguments, which may make things faster
    if you have a big TRA file and you're using USING.
  * --ask-every makes WeiDU behave as if ASK_EVERY_COMPONENT were present.
  * EVALUATE_BUFFER made generic as a patch action and a modifier for
    strings in patch expressions or assignments. Thus
      SPRINT indirect = "%y%"
      SET EVALUATE_BUFFER "%indirect%" = 55
    sets y to 55, and so on. This removes the need for EVALUATE_BUFFER as a
    one-off modifier to EXTEND_TOP, etc. See manual.
  * optional SET allowed before +=, etc. That SET is required if you want
    to use EVALUATE_BUFFER on the destination variable name.

Version 181:
  * SET_2DA_ENTRY_LATER won't issue misleading warnings with non-int
    values. Also respects the required-column-count correctly.
  * Rastor's ICT2 tutorial added.
  * You may now put EVALUATE_BUFFER after TP2 EXTEND_* and TP2 variables in
    the source BAF will be evaluated.
  * "var += value" is now a valid TP2 patch statement (_not_ an
    "expression" or "value"). Also -=  *= /= |= &=.
  * --tlk-cmp now compares sound resource names as well as text strings.
  * --trbify and .TRB files added. Do not use. devSin owes docs.

Version 180:
  * SNPRINT bugfix?
  * SET_2DA_ENTRY_LATER and SET_2DA_ENTRIES_NOW added. Some subset of
    { bigg, CamDawg, devSin } will provide documentation later.
  * SUBCOMPONENT + DESIGNATED bug fixed. Thanks, CamDawg.

Version 179:
  * SNPRINT (like snprintf()) added for string truncation.
  * INSERT_FILE, WRITE_FILE take variables.
  * WeiDU should now work when compiled on 64-bit machines. Thanks, fca.
  * Variable handling bug fixed. Thanks, bigg.
  * More performance improvements. Thanks, bigg, devSin, Cam, etc. A random
    "complicated" mod goes from 16 seconds to 10 seconds.
  * WeiDU return values are now documented. Loosely, 0 = success,
    non-zero = failure.
  * Initial support for extracting data paths from Mac baldur.ini files.
    More help needed.

Version 178:
  * ADD_CRE_ITEM bugfix. Thanks, devSin.
  * TP2 --reinstall added. Works like --install but only for
    already-installed components. Auto-selects last-used language if
    possible.
  * Some performance enhancements. Turns out that WeiDU was spending quite
    a bit of time setting and evaluating variables. For example, a mod that
    crawls over every effect in  *.SPL now takes 8.7 seconds instead of 91
    seconds. You will probably only see a difference on mods that have
    non-trivial patch processing (lots of READ_*, WRITE_*, PATCH_IF, etc.).
    For example, Ascension (all comps, English) shows no improvement, going
    from 2.58s (warm cache) to 2.92s (warm cache) -- loosely in the noise.
    Ease-Of-Use (all components, warm cache) improves from 18.3s to 6.12s.
    For some reason, I was sure that BUT_ONLY_IF_IT_CHANGES was the
    culprit. In fact, as best as I can measure, BUT_ONLY is free. Anyway,
    let me know if your mod starts behaving strangely or suddenly takes
    forever to install (please use wall-clock or OS time to measure this,
    since WeiDU's "*.DEBUG" reporting changed between this version and the
    last version so the self-reported numbers can't be compared).

Version 177:
  * LOOKUP_IDS_SYMBOL_OF_INT documentation change.
  * ADD_CRE_ITEM multi-slot-equip fixed.
  * READ_SSHORT added.
  * --toplevel added for emitting DLG top-level states only.

Version 176:
  * D Action APPEND_EARLY added. Works just like APPEND, but the states
    are added early and can be the target for I_C_T (or whatnot). Please
    test this and let me know if there are problems.
  * The TP2 Action APPEND now evaluates variables in the string to be
    appended.
  * TP2 EXTEND_TOP and EXTEND_BOTTOM evaluate variables in their filename
    arguments.
  * REPLACE_TEXTUALLY now does variable substitution for both arguments.
  * LOOKUP_IDS_SYMBOL_OF_INT added.
  * COPY_RANDOM syntax documented.

Version 175:
  * SET_2DA_ENTRY now evaluates WeiDU variables in strings.
  * ADD_STORE_ITEM now takes variables for the item and
    "LIMITED"/"UNLIMITED" flags. Thanks, devSin.
  * INNER_PATCH_FILE added.
  * READ_STRREF added.
  * SPRINT added.
  * COPY ~%variable%~ ~dest~ now works. If you really have a file named
    %a%, make sure that you either catch it with globbing/regexps or that
    your TP2 file does not use a variable named "a".
  * --out, --textout, --textapp, --dout unified to --out (and --append)
  * (A != B) added for (NOT (A = B))
  * Precedence and associativity for values fixed up a bit. Notably,
      3*4+5 < 100 + 1  AND  myvar = 22
    Actually does what you expect:
      (((3  * 4) + 5) < (100 + 1)) AND (myvar = 22)
    As many of you noticed, I had the comparisons (<, =, ...) binding too
    tightly and a bunch of other things (e.g., bitwise ops) all had the
    same precedence. We now mimic C/C++. Of course, you can still put in
    extra parens if you're paranoid.
  * Values and Predicates, which used to be separate, are now one big group
    that contains everything that used to be in either one. Thus
      myvar = 3 + (FILE_CONTAINS "ROGER.CRE" "CRE") = 4
    Results in 1 being assigned to myvar.
  * The WeiDU grammar is now down to 8 shift/reduce conflicts (from more
    than one hundred), all of which spring from using "X = 55" as shorthand
    for "SET X = 55". Loosely, these two situations are ambiguous with only
    one token of lookahead:
      COPY ~a~ ~b~
     ~c~ = 5
    and:
      COPY ~a~ ~b~
     ~c~ ~d~
  * WEIDU_ARCH and WEIDU_OS built-in variables added.
  * WeiDU almost includes free beer for CamDawg, but he was a few minutes
    too late.

Version 174:
  * APPEND_FILE added.
  * Segfault involving out-of-order DESIGNATEDs fixed. Thanks,
    devSin.
  * "Foo component X not found" message elided.
  * Globbing now re-enabled by default on OSX (don't know why it was
    disabled ...).
  * Added --logapp.
  * ADD_CRE_ITEM can now take multiple possible item slots.
  * Added FILE_CONTAINS_EVALUATED, PATCH_PRINT.
  * Supported: "PATCH_IF foo BEGIN yada END ELSE PATCH_IF bar ..."
  * SAY_EVALUATED added.
  * WRITE_BYTE, SAY, etc., now also do bounds-checks.
  * READ_ASCII takes an optional size argument.
  * INNER_ACTION and INNER_PATCH added.
  * You can now specify state numbers for DLG->D on the command line.
  * INSTALL_BY_DEFAULT component flag added.

Version 173:
  * Added READ_BYTE (etc.) ELSE clause for error handling.
  * Added --noselfupdatemsg
  * Massive documenting binge: all command line options are now
    documented. I would appreciate it if people would check over
    these.
  * ADD_KIT tutorial URL changed.
  * WRITE_*ASCII now takes an option "required size" argument
    (pads with NULs if you are too small).
  * Added GLOB explanation.
  * Added --tlkcmp-strings for al17.

Version 172:
  * If you pass --yes, it will not ask you to press enter at the end.
  * You may now use ~~~~~ (instead of ~, " or %) to delimit a string.
    This should hopefully help with traifying TLK files with strings
    that contain all three of my previous faves. Special thanks to
    kenteam for his help in finding this bug.

Version 171:
  * SOURCE_SIZE variable now set automatically as well.

Version 170:
  * NO_LOG_RECORD module component flag added.
  * ";" parse problem with FOR loops fixed.
  * --debug-value switch added. Good luck, devSin.
  * WeiDU now does explicit bounds-checks on
    READ_BYTE/SHORT/LONG/ASCII. Thanks, devSin.
  * PORTRAIT_SMALL, PORTRAIT_LARGE constants added.
  * If you pass --uninstall, it will not ask you to enter at the end
    of the processing.
  * Empty Action Warning no longer displayed since we don't save
    empty actions.

Version 169:
  * 0-byte files now handled correctly when uninstalling.
  * x=5 can be used instead of SET x=5, but DO NOT do "x=y=4"
  * patch_expression: FOR (x=0; x<5; x=x+1) BEGIN blah blah END
  * Errors while COMPILEing will now actually abort a tp2 component
    installation.
  * ADD_CRE_ITEM tutorial moved to right place.
  * ALLOW_MISSING / FILE_EXISTS_IN_GAME fixup.
  * Added Rastor's customization tutorial.
  * DO Actions that do not contain a letter or a number are removed.
  * --uninstall now automatically picks the Language based on
    WeiDU.log (if it exists and is in range, etc.)
  * You can now inline files with the TP2 Action <<<<<<<<

Version 168:
  * IF and UNLESS now really take regexps, as the docs claim.
  * REPLACE_TEXTUALLY, REPLACE as well.
  * WeiDU will copy 0-byte files again.
  * STRING_SET takes USING.
  * RANDOM(lb ub) and RANDOM_SEED added.
  * STRING_COMPARE_CASE added.
  * DESIGNATED module flag added.

Version 167:
  * Added string concatenation.
  * Added DECOMPILE_DLG_TO_D and COMPILE_D_TO_DLG.
  * Added SOURCE_RES and DEST_RES variables.

Version 166:
  * Removed auto () message. Whoops!
  * "Many Components" + "Subcomponents" = "Skipping Too Much" bug
    fixed, I hope.
  * PATCH_IF now works. Thanks: CamDawg, bigg.
  * Smoketest: --transitive //from bugfix
  * Smoketest: --full-from does a two-pass printout to catch backward
    references and get correct //from comments. It is turned on
    automatically by --transitive.
  * SirLancelot: --traify-tlk can be combined with --traify#
  * Added SUB_COMPONENT tutorial by Cam.
  * I moved to a new laptop, so the "default" WeiDU is now built with
    cygwin+mingw instead of cygwin+msvc. This shouldn't introduce any
    problems, but let me know if it does (e.g., with viewing, or
    slashes and backslashes, or globbing).

Version 165:
  * "ERROR_LOCATING_RESOURCE" error messages will now explain what
    WeiDU thought it was doing when it was loading that resource.
  * If WeiDU runs into an error processing a D action it will now
    tell you which of your D files it was working on.
  * The "missing THEN causes WeiDU to die" bug should be fixed. It
    was, in fact, a bug in ocaml's parser. Please try out removing
    all of your "then"s (or whatever) and let me know if you can
    encounter the problem now. If you can, send me a test file.
  * With the help of Idobek and Jason Compton, I believe that I
    have fixed the FORBID_FILE bug, aka the "Keto Double
    Interjection" bug.
  * Also fixed a minor-buglet with the module name printing in
    WeiDU.log -- if there was an error during the install it would
    sometimes fail to get the module names and yell about it
    (although everything worked fine). Now it gets the names and
    does not yell.
  * Added igi's auto-update-all.
  * Idobek/Grim subcomponents added.

Version 164:
  * REQUIRE_PREDICATE evil warning messages suppressed. Thanks, igi
    and Reaper99.
  * "Error clearing  *.IDS" message, which served no purpose,
    has been removed.
  * ADD_TRANS_ACTION added for Darios, Meria, etc.
  * PATCH_IF added for bigg.
  * You can use FOO instead of %FOO% to get the value of the variable
    FOO, so long as it is unambiguous. This work in "patch
    expressions" only -- in other places (e.g., in
    WRITE_EVALUATED_ASCII) you still need the %%s.

Version 163:
  * ALLOW_MISSING now actually works for copying files (like it
    should have!). Thanks, CamDawg.
  * SET_2DA_ENTRY bug with setting the last entry on a line fixed.
    Thanks, igi.
  * Japh's ADD_CRE_ITEM and REMOVE_KNOWN_SPELL patches added. Thanks,
    Japh.

Version 162:
  * REQUIRE_PREDICATE component flag added.
  * --autotp command documentation removed since command does not
    exist.
  * encrypted IDS and 2DA files now work correctly with
    COPY_EXISTING. Thanks, igi.
  * --continue, which continues despite TP2 action errors, was added
    for Vlad.
  * --traify now works on %strings%. Thanks, SConrad.
  * SET_2DA_ENTRY is now dramatically faster (work that used to take
    7 seconds now takes 1.4). The patch command also no longer
    "pretty-prints" the resulting 2da file.
  * Added Japh's ADD_MAP_NOTE patch command.
  * Added Japh's ADD_KNOWN_SPELL patch command. You rule, Japh!
  * Added INTERJECT_COPY_TRANS2. Someone else should document it.

Version 161:
  * AT_INTERACTIVE_EXIT will now work successfully on components that
    were "temporarily uninstalled" and then "permanently uninstalled"
    at the user's request.
  * WeiDU.log now includes component names as comments. Blah.

Version 160:
  * Added COMPILE_BAF_TO_BCS and DECOMPILE_BCS_TO_BAF as "Patch"
    actions.
  * Added REPLACE_EVALUATE because Sim wanted it.

Version 159:
  * Special thanks to Avenger for sending me a bunch of PST files so
    that I could fix more of this stuff.
  * We can now decompile PST BCS files with point-triggers, like:
      NearLocation(Myself,[1086.1803],20)
  * We can now compile PST BAF files with point-triggers.
  * We can now decompile PST actions like:
      GlobalSetGlobal("Previous_Area","GLOBAL","Current_Area","GLOBAL")
    But note that you need "--script-style PST" (or a PST Chitin.key)
    when decompiling to get this right.
  * @-100 = "Foo" now actually works. Thanks, Rastor.

Version 158:
  * --script-style and SCRIPT_STYLE added: BG, IWD2, PST. Controls
    how BAF files are read in and how BAF and BCS files are written
    out. Generally autodetected. I need someone to send me all of
    PST's IDS files before I can improve or correct PST support.
    I would also like an example of a PST _trigger_ that uses a
    non-zero numbers pair (e.g., [55,66]).
  * Added --remove-biff. See example above. Don't use it.
  * %LANGUAGE% variable is now set automatically to the language
    directory name (e.g., "american" not "American English"). See
    AT_EXIT above for language-specific README viewing.
  * Some of WeiDU's "hard-coded" strings (e.g., "[Y]es or [N]o or
    [Q]uit?") can now be replaced by TRA files. See
    examples\prompts.tra . Could someone writeup some docs for this?

Version 157:
  * ("foo" STRING_COMPARE "%bar%") evaluates to ZERO if and only if
    its two arguments are EQUAL (and have the same length) after
    variable substitution. If they are not equal it returns 1 or -1
    just like C's strcmp() function. The two arguments must be
    strings. You may use STR_CMP for STRING_COMPARE.  Icelus wanted
    this.
  * Updated the documentation on division. 11 / 6 = 1 in WeiDU, not
    1.8 or 2 or somesuch.
  * REPLACE_TRIGGER_TEXT_REGEXP, REPLACE_ACTION_TEXT_REGEXP,
    REPLACE_ACTION_TEXT_PROCESS_REGEXP.
  * INSERT_FILE, WRITE_FILE added.
  * --noautoupdate may actually work now.

Version 156:
  * BUT_ONLY_IF_IT_CHANGES bugfix?

Version 155:
  * Added Gwen's "--transitive" for viewing banters.
  * Added FORBID_COMPONENT.
  * REQUIRE_FILE and FORBID_FILE are now handled in a "preprocessing"
    step.

Version 154:
  * Added BUT_ONLY_IF_IT_CHANGES.
  * SET_2DA_ENTRY now takes a patchexp.
  * Fixed a bug where READ patch actions were evaluated out-of-order
    in some cases.
  * Note: There is a KNOWN BUG involving --traify crashing on
    certain CHAIN constructors. Sorry. We're working on it.

Version 153:
  * Bugfix involving ALLOW_MISSING in mod A not being honored when
    mod B tries to reinstall A's component.
  * STRING_SET and "forced string references" now uninstall
    automatically with the standard WeiDU rollback. How many times do
    I have to tell you guys not to use this feature? Since no one
    is listening to me in that regard, you can all thank JRM for
    convincing me to code this up.
  * If you pass AT_INTERACTIVE_UNINSTALL (and friends) a single
    argument that is a .TP2 file, WeiDU will enqueue that TP2 file
    for you and run it when it is done with the current one
    (and any others you have previously enqueued). This prevents you
    from having to guess the executable to invoke or whatnot,
    and is apparently handy for people with voodoo uninstall
    procedures. Note that you can make it so that A.TP2 calls B.TP2
    and B.TP2 calls A.TP2 and the user loops forever. Don't. JRM
    again.

Version 152:
  * EXTEND_TOP / _BOTTOM now automatically handle target scripts that
    are not there.

Version 151:
  * CHITIN.KEY.FOO will no longer be found automatically.
  * Strings can now be %abc% as well as "abc" and ~abc~. This will
    apparently help with Big5 (Chinese) translations.

Version 150:
  * Biffs made by WeiDU now write out the correct (hopefully)
    file resource location and tis resource location values. Thanks
    Avenger and Horrid.
  * APPLY_BCS_PATCH_OR_COPY added to make Ease easier.
  * More accurate output on "compiling X files" statements.

Version 149:
  * Auto-update only on win32 ...
  * Read in triggers with 3 integer parameters from BAF files
    correctly now. Thanks, Avenger.
  * EXTEND_TOP, EXTEND_BOTTOM now handle missing arguments more
    gracefully (assume empty script).

Version 148:
  * Unknown transition strings are now written out as -1, not 0.
  * --make-biff should no longer make read-only biffs. Thanks, C
    Bisson.
  * Added Idobek's WHILE loop and SET_2DA_ENTRY tutorials. Thanks!
  * Added CamDawg's bitwise operator tutorial. Thanks!
  * Some Mac niceties by Loriel.
  * REPLAC_BCS_BLOCK can take BAF file arguments. Idobek is lazy.
  * TP2 Flag parsing was messed up -- thanks, Idobek.

Version 147:
  * Fred S. Richardson patches: MinGW compiler support, HD0: .INI
    path support (apparently handy for people with multiple BG2
    installs).
  * WeiDU now supports dialog.tlk of basically arbitrary size.
    TLK files above 16 megs load more slowly (surprise).

Version 146:
  * ADD_STATE_TRIGGER now takes an optional transition list. CBisson
    wanted it.
  * ADD_STORE_ITEM and ADD_GAME_NPC docs added by Japh.
  * BAND, BOR, BNOT, BXOR, BLSL, BASR, BLSR -- Smoketest's expression
    bitwise operators added.

Version 145:
  * Better error messages in --tcmp.
  * First attempt at handling both CHITIN.KEY and chitin.key (unix).

Version 144:
  * Added Fred S. Richardson's ADD_PROJECTILE. Thanks, Fred!
  * Fixed a bug that was causing "SAY NAME1 ~Foo~" to fail
    because NAME1 would not be evaluated.

Version 143:
  * WeiDU now requires OCaml 3.07.
  * WeiDU now reloads KIT.IDS (and all other IDS files) after
    an ADD_KIT.

Version 142:
  * ADD_KIT now adds to KIT.IDS.
  * Wow, I somehow left a "failwith foo" in the middle of the
    trigger list parsing code. Sigh! Thanks, Today's Newbie.

Version 141:
  * --make-biff OSX fix broke --make-biff on all other platforms.
    Whoops. Fixed. Thanks, C Bisson.
  * COPY_EXISTING_REGEXP can now take GLOB as a modifier.
    Apparently everyone wanted this feature.
  * SOURCE_DIRECTORY, SOURCE_FILESPEC, SOURCE_FILE,
    DEST_DIRECTORY, DEST_FILESPEC, DEST_FILE are now all set
    on every single file that is copied. Darious wanted this
    feature.

Version 140:
  * REQUIRE_COMPONENT bugfix. Thanks, Fred!

Version 139:
  * INTERJECT may now use chain-style epilogues.
  * --make-biff may well work on OSX.

Version 138:
  * WRITE_EVALUATED_ASCII added.

Version 137:
  * Too many open files bug fixed.
  * Added some #defines to make xdiff compile more often.
  * Added Japh's ADD_STORE_ITEM and ADD_GAME_NPC commands.
  * Auto-Update process creation code now hand-tweaked on win32
    platforms for bonus win98 compatibility. Special thanks to Kish
    for testing.

Version 136:
  * Auto-Update now uses WeiDU versions. Let's try this once more.

Version 135:
  * Fixed (hopefully!) an auto-update bug that was preventing
    Win98/WinME from using WeiDU. Auto-update has been revamped a
    bit: after an auto-update WeiDU will quit and you will have to
    click on Setup-Foo again (there will be a big message telling you
    to do this). Windows prevents me from doing it for you, sorry.

Version 134:
  * WeiDU auto-updating now works even if the running WeiDU is not
    the newest WeiDU. However, all WeiDU mods should upgrade to at
    least 134. Get the word out.
  * WeiDU.log format changes undone for now ... but they (or the next
    syntax change) will eventually come back. Compton begged for a
    reprieve, and he shall have it, but it's hardly permanent.

Version 133:
  * Fixed a problem with --traify FOO.BAF. Thanks, Kismet.
  * Fixed a problem in --traify-tlk with ~s in strings.
  * Fixed a problem with --make-tlk and a stack overflow.
  * Include Japh's STO automating code.
  * "setup-foo.exe this.tp2" runs "this.tp2" and not "setup-foo.tp2".
  * Added "component flags" in TP2 files.

Version 132:
  * Added Fred's Cygwin/GCC patch.
  * Added Fred's BCS-Diff magic. Presumably Fred will continue to bat
    1.000 by sending us some documentation. Look for a better
    multi-romance patch in the future.

Version 131:
  * Fixed a problem with decompiling some IWD2 scripts. Thanks,
    Avenger.
  * "THEN" keywords inside CHAINs are now truly optional.
  * Fixed a problem with --traify and partially tra'd files. Jason
    here, Jason there.

Version 130:
  * Weimorph changes.
  * WeiDU.log now includes the component name rather than just the
    number (when we have that info). This may help the people who
    seem to be posting that file to boards and whatnot. Notably, it
    helps me!
  * Fixed a CHAIN bug involving duplicate labels. Thanks, Grim
    Squeaker.

Version 129:
  * weimorph fixes: duplicate effects (again), poison, regeneration,
    biffing, cre-in-name, more "not in dialogue" game crashes, brown
    bears with crossbows, etc.
  * Fred Richardson has coded up the REPLACE_BCS_BLOCK_REGEXP tp2
    command. Docs to follow.
  * Fred Richardson's bugfix for REPLACE with WEIGHTs is included.
  * Massive weimorph changes.

Version 128:
  * We now give an error on multiple JOURNALs per DLG transition.
  * IWG1 updates for Japh.

Version 127:
  * WeiMorph changes for Japh and the boys. No real WeiDU changes.

Version 126:
  * You may now --traify BAF files.
  * IDS files are now only loaded once each.
  * Better handling of actions that take two "concatenation string"
    arguments, like Torment's
      IncrementGlobalOnce("Morte_Zombie_1","GLOBAL","Law","GLOBAL",-1)
    Thanks, Luke Carroll. WeiDU can now compile DMORTE.D from PST.

Version 125:
  * Added the "FILE_MD5 filename md5sum" predicate for Fred
    Richardson.
  * Documented the UNINSTALL tp2 action. Don't use it.

Version 124:
  * Fixed a globbing definition problem in the OSX arch stuff.
    Thanks, Devon.
  * Added Unix globbing support. Thanks again to Devon Smith. Note
    that care should be taken to make sure your globbing works on
    multiple platforms -- try to use lower-case letters, for example.
  * Fixed a --traify problem with ~male~ ~female~ strings. Thanks,
    Jason and Bhasbuto.
  * --traify-tlk will no longer print the female text if it is the
    same as the male text.
  * Refactor WeiMorph a bit to make Japh's BG1->BG2 quest a bit
    easier.

Version 123:
  * Fixed a bug in SAY (actually, offset in general) handling that
    was causing UNIDENTIFIED_DESC to have the DESC part replaced by
    its value instead of causing the entire thing to be replaced by
    its value. Thanks: Timothy Hoffman, Kish and Jason.

Version 123:
  * Added three Japheth tutorials (values, read_byte, make-biff).
    Thanks!
  * Fixed a bug where variables were not being substituted properly
    in COPY destinations.
  * NOTE! You cannot uninstall correctly from something like:
      COPY GLOB ~save\**\worldmap.wmp~ ~%SOURCE_DIRECTORY%~
    Only the last one will correctly uninstall. This is because WeiDU
    was not designed with the notion that their might be multiple
    files with the same name copied to different locations. Coding
    uninstall support for this would be boring, annoying and
    error-prone. If you want it, consider coding it up yourself or
    somehow getting you and your five friends to convince me that the
    fate of the world hangs in the balance.

Version 122:
  * Added --extract-kits. Use it like this:
      mkdir kitmod
      weidu --extract-kits 1 --textout Setup-Kitmod.tp2 --out kitmod
    It will extract all of the kits (and their weapon and proficiency
    information and their special abilities ...) with ID >= 1 from
    KITLIST.2DA. Search the resulting TP2 file and look for "FIX ME"
    -- if it occurs, WeiDU was not able to find some information (or
    it was not in the original game) and you should think about
    providing it. You can run the TP2 file through --traify later if
    you want. All required resources (ability files, etc.) will be
    put in the 'kitmod' directory.
  * Fixed a bug in COPY GLOB that was corrupting the OCAML runtime
    and causing WeiDU to hang.

Version 121:
  * COPY sets the %SOURCE_DIRECTORY% variable.
  * --debug-assign flag allows you to watch the values of TP2
    variables change.
  * Fixed a bug where WHILE loops were not processing READ
    commands. Thanks, Japh.
  * Added "SET_2DA_ENTRY row col req_col value" as a patch. Someone
    wanted this.

Version 120:
  * Added "exp ? exp : exp" -- the expression-valued if.
  * Added "SET variable = exp".
  * Added "WHILE exp BEGIN patch list END".
  * Case-insensitivity for actionoverride.
  * Added GLOB as an option to COPY for Japh. Do not use this
    non-portable feature.

Version 119:
  * Empty TLK files now get a default entry 0 <NO_TEXT>. Thanks,
    Talen.
  * --tlkmerge (and general file loading) bug fixed. Thanks, Dyara.
  * WeiDU will now notice if dialog.tlk and dialogf.tlk have
    different lengths and it will do something about it. Thanks,
    Dyara.

Version 118:
  * Jason's brief transition syntax is now accepted:
      + optional_trigger + reply_text do_journal_list next
  * + is now accepted as a synonym for GOTO in a 'next' block.
  * CHAIN/CHAIN3 blocks can now end with
      END transition_list
  * SAY can now take "expressions" for offsets.
  * You may now put a + after COPY (existing/regexp) to mean "do not
    make a backup of this file and don't uninstall it later". Do not
    use this feature.
  * Noted in the docs that the THEN BEGIN keywords in a state decl
  * are optional and that the THEN part of a transition is optional.
  * Massive documentation update to cover new syntax that has been
    creeping in.
  * PRINT ~Variable foo is %foo%.~ does what you would expect.

Version 117:
  * --automate NAME1 fix. Thanks, Thorfinn and others.
  * READ_*, WRITE_*, INSERT_BYTES, DELETE_BYTES, etc., may now all
    take "expressions" for their offsets and their values (where that
    makes sense).
  * BAF files may now use hex values (like 0x34).

Version 116:
  * Various portability fixes by Ras.
  * Fixed a bug with INTERJECT_COPY_TRANS. Thanks, Elanor.
  * R, I tried to send you some email and it bounced. Send me some
    more email with a working address.

Version 115:
  * Better error handling for out-of-range INTERJECT_COPY_TRANS.
    Thanks, Sim!
  * Yet another non-Setup-Foo.exe bugfix. This one should allow
    uninstallation. Thanks, GB.
  * CHAIN3 conditional fixing. Thanks, Jason.
  * Added REPLACE_ACTION_TEXT_PROCESS. Good luck, Ras.

Version 114:
  * Fixed a problem with --biff-get and encrypted files.
  * --trans now looks inside DLG actions (like EraseJournalEntry())
    and gives translation strings for them as well.
  * --biff-name bug with invalid strrefs fixed. Thanks, Sqweek.
  * Fixed a hideous bug in v113 that was causing WeiDU to overwrite
    TP2 files with itself.

Version 113:
  * We warn if you add strings but don't specify --tlkout. Thanks,
    AvengerTeamBG.
  * Better support for GB's multiple-directory TP2 scheme. However,
    the only truly supported method is to have all of your TP2 files
    in the main game directory and run Setup-Foo.
  * Updated up the CHAIN epilogue in the documented grammar.

Version 112:
  * Allow ! in tokens (but not as the lead character).
  * --nocomm disables comments in produced BAF files. Thanks,
    AvengerTeamBG.

Version 111:
  * Added ALWAYS blocks for TP2 files.

Version 110:
  * D-file actions and triggers are now replaced with their converted
    form. So you can say DO ~EraseJournalEntry(@1000)~ and have it
    work in the end. Thanks, Triangle.
  * TP2 logging is now case insensitive.
  * Added --make-tlk and --traify-tlk command-line arguments.
  * Extracting from BIFFs will no longer cause WeiDU to segfault if
    your KEY and BIFF files do not match up.
  * Fixed a problem with extracting biff resources that was causing
    COPY_EXISTING "this.cre" to fail. Thanks, Edsel Sabulao.

Version 109:
  * Fixed on BCS decompilation probs. Thanks, Dyara.
  * --biff-get and BCS decompiling no longer stop after the first
    error.
  * --biff-get can now get arbitrarily large files (e.g., AR1000.TIS).
    I officially  *do not care* for at least a few weeks if there is
    some random file hiding somewhere that WeiDU still cannot
    extract. Use WinBiff.

Version 108:
  * Allow things like LastSeenBy("foo") in scripts.
  * Added support for biff creation. Use the --make-biff option and
    pass it a directory argument. All files in that directory will be
    made into a buff, CHITIN.KEY will be updated.

Version 107:
  * Fixed a bug where the temp file for a big compressed biff would
    not be removed if weidu choked because the biff was way too big
    (or any other reason).
  * Actually, the entire handling of compressed biffs has been changed.
    Now we only do on-demand decompression. For example, the time to
    extract AR0602.WED drops from 9.6 seconds to 0.3 on my machine.
    No more temporary files at all.
  * Now faster when something adds a large number of new strings to
    dialog.tlk.
  * Fixed our handling of WEIGHTs (again!). Thanks, Dyara. Everyone,
    let me know if you find any flaws in the new handling.

Version 106:
  * IDENTIFIED_DESC now works as an offset.
  * Added SOLVED_JOURNAL and UNSOLVED_JOURNAL keywords. Thanks,
    Avenger.

Version 105:
  * Fix a WEIGHT problem. Thanks, GB.
  * Added --min and --max which work with --tlkcmp and --string.
    Fixed a minor bug in --tlkcmp. Thanks Harden Coonor.

Version 104:
  * One last sanity check for auto-update: ensure that when we copy A
    over B we set B's timestamp to that of A.
  * You may say "COMPILE ~mymod/big-folder-of-scripts~" and every D
    or BAF file in that folder will get compiled. This works just
    like giving COPY a directory.

Version 103:
  * Added ASK_EVERY_COMPONENT TP2 flag for Jason. Put up near
    AUTO_TRA.

Version 102:
  * Added READ_BYTE, etc. WRITE_BYTE and friends can now use compound
    expressions (possibly involving variables). See IWD2-Ease.
  * Added COPY_RANDOM.
  * Added the --biff-get-rest option, which basically applies
    --biff-get to all the rest of the command line arguments.

Version 101:
  * Unknown IDS entries (like ToB spells in SoA's SPELL.IDS) now
    default to 0. This was causing Sola installing problems.

Version 100:
  * Fixed a key path-separator problem. Thanks, Devon.
  * COMPILE ~foo.baf~ will now uninstall correctly. Thanks, Japh.

Version 99:
  * CHAIN "IF"s are now checked for validity.
  * Setup-Foo will now copy itself over all older Setup-Bar's in the
    same directory.
  * You can now COMPILE and EXTEND BAF files. See Setup-Solaufein.tp2.
  * BAF files can now take ~String Refs~ and @130 trans refs.
    Example:
      DisplayStringHead(Myself,~Flame Arrow~)
      DisplayString("Sola",@144)

Version 98:
  * Fixed an [ANYONE] problem in BCS->BAF world. Thanks, Dyara.
  * Fixed a problem where re-installing a mod with an error in the
    new version would give you "yes/no/uninstall" instead of
    "yes/no". Thanks, Quitch.

Version 97:
  * Added support for reading in some broken IDW DLG files (they come
    with out-of-range triggers/actions).
  * Added support for reading in some broken BCS files (they omit
    a non-optional action parameter).
  * ALLOW_MISSING is no longer case sensitive. Thanks, Carl Grenthe.
  * Fixed a lexer error involving unterminated // comments. Thanks,
    Moonfruit.
  * We now handle cd paths like "C:\CD2;E:\Other\CD2".
  * AUTO_TRA files are now associated only with their (single)
    associated D files and not with any other files. Thanks, Dyara
    and Quitch.
  * Fixed "See(LastSeenBy(Nearest([EVILCUTOFF])))" ->
          "See(LastSeenBy(Nearest)([EVILCUTOFF]))"
    problem. Thanks, Dyara.

Version 96:
  * Parse errors and warnings are now logged.
  * XOR-encrypted 2DA files are now decrypted. Previously only IDS
    files were decrypted.

Version 95:
  * We now verify D-file transition conditions. They were skipped before.
  * Invalid actions and trigger lists in D files are now only
    WARNINGS, as they should have been.
  * Location information for actions and trigger lists in D files
    is now much more accurate. Still not perfect, but at least it's
    on the right line.
  * BCS->BAF decompilation no longer throws up a bunch of warnings
    about "this file not found".
  * Added Japheth's R_C_T tutorial.
  * In BAF-land, typing things like "SeE(MySelF)" instead of
    "See(Myself)" is now only a warning.
  * If you say something like "LastSeenBy()" you are now given an
    explicit warning to use "LastSeenBy(Myself)" instead. The first
    one sometimes gets weird treatment by the game engine.
  * Fixed a case bug in APPENDI. Thanks, Quitch.
  * We are throwing a surprise 50th birthday party for my mother today.
    Wish us luck.

Version 94:
  * Fixed a problem with I_C_T reversing transition lists. Thanks for
    making it perfectly clear, Jason.

Version 93:
  * Fixed a problem with IDS files that have too much whitespace.

Version 92:
  * Fixed a decompiler bug where the wrong trigger (e.g.,
    HaveSpellRES instead of HaveSpell) might be printed.
  * WeiDU now checks actions and triggers in D files.
  * Documentation is no longer "too wide" for easy viewing. Thanks to
    Tapio Kivikkola for pointing this out.
  * Roland has taken up the "compile WeiDU for Mac users" mantle.
    Thanks!
  * Fixed a problem with CHAIN forgetting some actions. Thanks,
    Quitch.
  * CHAIN3 now supports weights for its initial condition.

Version 91:
  * WeiDU is now a BAF -> BCS compiler. Just pass BAF files as
    arguments. BCS files will be created in the current directory (or
    use --out). Again, let me know if we mis-compile something.
    Future work: allowing D-style string references in BAF files, etc.
  * WeiDU now reads all the .INI files in your game directory and
    looks for lines of the form CD1:=C:\My\Path.
  * New Uberchain tutorial by Jason.

Version 90:
  * Fixed a problem where SETUP-FOO didn't work.

Version 89:
  * Added Japeth's tutorials.
  * --tcmp no longer dies on an invalid file. Thanks, Falk!
  * WeiDU will now decrypt those annoying encrypted IDS files when
    extracting them from BIFFs.
  * WeiDU is now a BCS -> BAF decompiler. Just pass BCS files as
    arguments. BAF files will be created in the current directory (or
    you can use --out). I consider NI to be the "reference"
    decompiler. Let me know if you find a script where WeiDU and NI
    disagree on something that is not a comment, a MYAREA or an
    "ar1234".
  * You can now give WeiDU command-line arguments after seeing the
    options list.

Version 88:
  * EXTEND_TOP_REGEXP and EXTEND_BOTTOM_REGEXP TP2 actions added.
    Will the person who wanted this submit some docs?
  * --automate now handles AREs
  * You may now say !NUMBER ~Hello~ (or !NUMBER @44 or whatnot) in a
    D file instead of ~Hello~ to  *FORCE* the string ~Hello~ to
    overwrite whatever was already in strref NUMBER in dialog.tlk
  * --forceify option added. It behaves like --traify. So you might
    say: weidu --forceify my.d --dout new.d
    The created D file will be just like the old one, except that all
    of the strings in it will become forced strings with their
    current strref. No, I do not understand why you would want this
    either. But some people do.

Version 87:
  * Yada yada, more prep to get ready for the tactics mod.

Version 86:
  * Much nicer handling of mods with more than 4 components.
  * Added the UNINSTALL TP2 action to allow one mod component (say,
    the archer kit in a new tactics mod) to uninstall another (say,
    the old version of the archer kit in the sola mod).

Version 85:
  * Fixed a bug where --automate was missing name offsets at 0xC (the
    identified name for ITMs, etc). Rerun --automate if you use it to
    make sure you aren't missing anything.
  * Added some --automate docs based on Rene Heroux's suggestions.

Version 83-84:
  * Fixed a bug in "interject" that was the wrong number of
    parameters to be passed to "Global()" and was putting triggers
    (like InParty("Valen")) in the action slot. Sigh! Sigh!

Version 82:
  * Setup-Foo.exe now dies if both Setup-Foo.tp2 and Foo.tp2 are not
    present.
  * --automate dir option added. Throw it at a directory of
    ITM/SPL/CRE files (say, "mymod/itm"), it will spit out a TP2 file
    for you. Example use:
    C:\> weidu --automate foo/itm --textapp Setup-Foo.tp2
  * Special thanks to Victoria, who will surely be famous at some
    point. Let me know when you've updated your CHAIN tutorial and I
    will link to it.

Version 81:
  * CHAIN/CHAIN3 changes:
    + You may now specify an initial condition, a la:
      CHAIN IF ~Global("MyValygarBanter","GLOBAL",0)~ THEN BVALYGA foo
        "Valygar says Hello."
      END BVALYGA 50
    + You may end with "EXIT" instead of "END FILE LABEL".
    + You may end with "COPY_TRANS FILE LABEL" instead of "END FILE
      LABEL".
  * CHAIN/CHAIN3/INTERJECT/I_C_T changes:
    + You may include DO actions after spoken text:
      CHAIN BVALYGA foo
        "I shall smite thee!" DO ~SetGlobal("ValySmite","GLOBAL",1)~
      == BVICONI
        "But not me, rivvil! I am magic resistant."
      EXIT
    + If you are a true masochist, you may use DO and IF at the same
      time here:
      CHAIN BVALYGA foo
        "I shall smite thee!" DO ~SetGlobal("ValySmite","GLOBAL",1)~
      == BKELDOR
        IF ~IsValidForPartyDialogue("Keldorn")~ THEN
        "I shall prevent you from smiting anyone."
        DO ~SetGlobal("KeldornPreventsSmiting","GLOBAL",1)~
      EXIT
  * Special thanks to "Blue" for making the tutorial that suggested
    these changes. See examples/chain-banter.d for the tutorial
    example.

Version 80:
  * Handle empty BCS files in EXTEND_TOP/BOTTOM.

Version 79:
  * The current directory is no longer a search location. Compton
    wanted this, yell if it messes you up.

Version 78:
  * Fix a small typo noted by Jason Compton that caused all non-TIS
    files to be loaded incorrectly and all TIS files except the first
    to be loaded incorrectly.

Version 77:
  * Compressed BIFFs (sometimes called BIFCs) are now supported.

Version 76:
  * Also look in GAME/CDx/Data/ instead of just GAME/Data for BIFFs.

Version 75:
  * TRA files with errors are no longer held open.
  * TIS files can now be extracted correctly. Special thanks to
    Ghreyfain for providing enough data files to debug this problem.
    Turns out that BIFF files store special TIS tables that are used
    only for TIS resources, so they must be handled as a special
    case.

Version 74:
  * Pulled COPY_EXISTING_REGEXP out of COPY_EXISTING. Little
    backwards compat bug, sorry.

Version 73:
  * COPY_EXISTING source files can now take regular expressions.
  * Fixed a bug where a D file with a parse error would be held open
    by WeiDU. This bug reported by Quitch.
  * WeiDU works with Icewind Dale 2.

Version 71-72:
  * New slightly spiffier cross-referenced docs.
  * SET_STRING can now take @translations.

Version 70:
  * New traify docs by compton. Fixed pizza example.
  * Unify CHAIN/CHAIN3 docs.
  * --traify works on TP2 files. Use --dout and --traify# as before.

Version 69:
  * In a state declaration, IF ~StateTrig()~ THEN BEGIN label,
    the THEN and BEGIN tokens are optional.
  * In a transition, IF ~TransTrig()~ THEN ..., the
    THEN token is optional.
  * Abbreviated .D syntax: INTERJECT_COPY_TRANS can be I_C_T, etc.
    as per Compton's suggestions.
  * INTERJECT_COPY_TRANS now does the copy-trans bit on all of the
    interjections, not just the last one. The copy_trans bit comes
    above your interjections, so it should still work like it worked
    before in all cases where it worked before. Sigh.

Version 68:
  * Fixed a bug where saying --uninstall on a mod that had any
    uninstalled components would put you in an infinite loop.

Version 67:
  * Fixed a bug where temporarily uninstalled components would be
    handled poorly when you wanted to re-install them, blah, I can't
    even describe this bug. Anyway, it caused (at least) at lot of
    errors like "Unix.Stat(solarom/uninstall/1/uninstall.1)" but it
    didn't seem to actually hurt anything. Still, it marked a
    conceptual flaw in my understanding of what was going on.
  * The --tlkcmp command now produces TP2 STRING_SETs with
    @translation references and a TRA file that fills those in.

Version 66:
  * This time we're really sure about that Quitch bug. :-) See
    test\quitch\quitch.tp2 for a way of reproducing the failure. It
    used to die (as Quitch reported) but now it succeeds.
    If this fixes it, the problem was much worse than I thought: the
    D-file action list could get "duplicated" (or at least, not
    deleted) when an error happened.

Version 65:
  * Another attempt at fixing that bug Quitch reported. :-)

Version 64:
  * Fix a bug noticed by Quitch where the internal DLG state was not
    being cleared after an (un)successful install. This meant that
    if you had two D files with "BEGIN foo" either in two separate
    components or in the same component that you tried to re-install
    it would fail. This could also explain some errors people have
    been reporting about "multiple install"-like effects.
  * Complain EARLY if the output dialog.tlk file is read-only or a
    directory or something.

Version 63:
  * CHAIN replaced by CHAIN3 internally (but you can still use CHAIN).
  * --traify now emits the TRA things in the same order as they
    appear in your original D file. This makes --traify slower. Deal.
  * --traify# option allows you to specify a starting offset for
    the created translation strings.

Version 62:
  * tlkcmp docs by JC added.
  * Added "FILE_EXISTS_IN_GAME" predicate, which is true if the file
    is in EITHER the biffs OR the override directory.
  * --traify option added. weidu --traify my.d --dout new.d
    Will make new.d which is just like my.d except that all game
    strings now reference TRA strings in new.tra. Documentation of
    this feature will (we hope) be provided by our favorite
    mind-reading freelance journalist.

Version 61:
  * Added "--tlkcmp-to" and "--tlkcmp-from" for Jason (or something).
  * Added "ACTION_IF pred THEN BEGIN actions END" as a tp2 action
  * Added FILE_EXISTS, FILE_SIZE, FILE_CONTAINS, AND, OR, NOT as
    tp2 predicates.
  * Added FAIL and PRINT as tp2 actions.

Version 60:
  * Possible paths are no long displayed, since that was confusing
    users (no, really).
  * Added the "--biff-name X" option for "--biff-str" and
    "--biff-type" listings. A typical use would be something like
    weidu --biff-name 8 --biff-type CRE --biff-str SW1H
    to print the names of all creatures that have one-handed swords.

Version 59:
  * Registry thing really this time, but if this doesn't fix it I
    don't know what's going on.

Version 58:
  * Fixed a registry path bug.
  * Added --transref string to have --trans emit string refs.
  * Added Compton's new instructions on COPY_TRANS and INTERJECT.

Version 57:
  * Fixed a bug in the handling of COMPILE-USING TRA files when you
    do not have a LANGUAGE keyword in your TP2 file. Such TRA files
    are no longer ignored. I never noticed this myself because all of
    my mods have LANGUAGEs.

Version 56:
  * Say AT_EXIT "VIEW this" instead of AT_EXIT "notepad this". VIEW
    at the beginning of a shell command will be replaced by something
    appropriate for the user/architecture (notepad, mac osx viewer,
    whatever).
  * Added INTERJECT_COPY_TRANS, which does just what you would expect
    if you already understand the tersely-documented INTERJECT and
    COPY_TRANS actions. :-) Compton, some docs? :-)

Version 55:
  * Added INTERJECT action for even easier banter. Now we are truly
    meddling with forces we were not meant to know! Muhaha!
  * Replace whitespace handling in DLG files to avoid messing up
    CharName("Drizzt Do'Urden",Player1)
    in state 57 of c6drizz1.dlg.

Version 54:
  * AUTO_TRA bugfix. D files should now really get the matching TRA
    file.

Version 53:
  * Fix a bug with "--yes" and failed installations.
  * Added CHAIN3 directive to support branching in chained dialogues.
    This was recently used in the Improved Ilyich mod -- compare the
    new compact presentation to what was done for the Eclipse guys.

Version 52:
  * Allow constraints on COPY. You may now say
      COPY src dst    // multiple src-dst pairs OK
        patch_list
      constraint_list // "IF", "UNLESS", "IF_SIZE_IS"
  * IF_SIZE_IS added as a constraint.
  * See examples/copyif.tp2.

Version 51:
  * Make WeiDU unix friendly for a friend who would like to compile
    it.
  * Add a --nogame option for people who would like to test it out
    but do not have any Infinity Engine games.
  * Unify slash and backslash handling.

Version 50:
  * Reading from BIFF files of any size is now supported. Note that
    an individual file within a BIFF must still be <= 16777000 bytes.
  * More verbose error messages in a few places.

Version 49:
  * Pause the option list.
  * You can now say
    STRING_SET ~Hello~ ~Hola Boy~ ~Hola Girl~ [HOLA]
    to change every ~Hello~ string in DIALOG.TLK to that new one. The
    old STRING_SET 345 syntax still works. Have fun, Compton.
  * --biff-get can now take regular expressions
  * If we die in the middle of a TP2 installation then we uninstall
    all of the files copied so far, restoring things to the clean
    pre-attempted-install state.

Version 48:
  * Added --dcmp-to and --dcmp-from options to automatically spit
    out REPLACE-diffs between DLG files.

Version 47:
  * Added REPLACE_ACTION_TEXT and REPLACE_TRIGGER_TEXT as D
    actions. Go Jason!

Version 46:
  * Added REPLACE_BCS_BLOCK, INSERT_BYTES and DELETE_BYTES patch
    actions.
  * Added --cmp-to and --cmp-from for simple diffing.
  * Fixed a bug where copying FOO.EXT to folder1 and also folder2
    would create incorrect backup information: only folder1\FOO.EXT
    would get uninstalled.

Version 45:
  * Fixed a bug in REPLACE_TEXTUALLY (etc.) that was causing it to
    not match a whole bunch of strings.

Version 44:
  * Better handling of syntax changes in TP2 files (when one mod
    tries to uninstall another).
  * FORBID_FILE added.

Version 43:
  * ADD_KIT now takes some ToB parameters (High-Level ability
    abbreviation and starting equipment). The starting equipment was
    very tricky to get right, since you can't just append a column,
    you have to explicitly set column X (which may or may not be an
    append).
  * Compton's multisay and chain2 tutorial included!

Version 42:
  * Convert / to \ in AT_EXIT-style commands.
  * Fixed a bug in APPEND_COL that was causing it to never append
    anything. :-)

Version 41:
  * Added WRITE_ASCII (cf. WRITE_LONG) for writing in BCS script
    names and ITMs and whatnot.
  * Fixed MKDIR so that it actually makes directories.
  * Add AT_INTERACTIVE_UNINSTALL.
  * Fixed an "tetris uninstall" bug.
  * We can list all of the effects in a SPL or ITM.

Version 40:
  * WeiDU now keeps a log of all installed WeiDU mods and does the
    "tetris uninstall" automatically, then puts back all
    temporarily-uninstalled mods.
  * Added AT_INTERACTIVE_EXIT so that you can avoid spamming the user
    with your readme file whenever your mod happens to be randomly
    uninstalled.

Version 39:
  * Allow patches after EXTEND_TOP, EXTEND_BOTTOM for Michael.
  * Added MKDIR as a TP2 action.
  * Added REQUIRE_FILE as a TP2 action.
  * Added REPLACE_SAY as a D action.
  * Added REPLACE_STATE_TRIGGER as a D action.
  * Added SET_WEIGHT as a D action.
  * ADD_STATE_TRIGGER, ADD_TRANS_TRIGGER and REPLACE_STATE_TRIGGER
    may now operate on lists of states. Just put the extra states
    after the trigger.
  * EXTEND_TOP and EXTEND_BOTTOM may now operate on lists of states.
    Put the extra states before the transition list.
  * Perhaps some kind soul could write some docs about these new
    features?

Version 38:
  * Switch : to / in BIFF path names for Mac-VPC compat.
  * Added SET_STRING for Ghrey.

Version 36-7:
  * Added REPLACE_TEXTUALLY
  * Really fix Polish TLK handling this time. When the TLK string
    length is 0, sometimes the offset is a big negative number.

Version 35:
  * Added ADD_MUSIC
  * Added %VARIABLES% that are replaced by the kit number or music
    number from ADD_KIT or ADD_MUSIC.
  * More robust TLK handling (handle possible error with DIALOG.TLK
    from Polish version of Ascension?).

Version 34:
  * Fixed a bug in CHAIN where the filename after == would be
    reported as "not found" even if it was present. Thanks Michael!
  * Added APPEND_COL as a TP2 option.
  * Added COPY_EXISTING, WRITE_BYTE, WRITE_SHORT, WRITE_LONG.
  * Missing TRA files are no longer fatal errors. Having an undefined
    string reference (e.g., @55) is, however.
  * Missing files at uninstall-time are no longer fatal errors.
  * Michael Lyashenko tutorial included.

Version 33:
  * Minor profiling and algorithmic enhancements mean that WeiDU
    is now at least twice as fast as it was before when compiling D
    files. The "please be patient" notice is pretty much unnecessary.
  * Added a cheap menu option because I was getting tired of saying
    "No" to all three thousand of the options in the Sola mod.
    Unfortunately, DOS doesn't seem to like color.
  * Added ADD_KIT as a TP2 option. See mymod.tp2 for details.

Version 32:
  * Added an optional position to EXTEND_{TOP,BOTTOM} in .D files.

Version 31:
  * Fixed a bug where Multi-say states would forget their WEIGHT

Version 30:
  * Really process COPY_TRANS, this time for sure. :-)

Version 28:
  * Really process COPY_TRANS. Sigh.

Version 29:
  * Really stay silent about saving DLGs.

Version 27:
  * Sort transitions triggers in trans-trig-table order when loading.
    (under the assumption that transitions run from the bottom up
    in trans-trig-table order, so we should write them out that way
    in order to preserve the semantics)
  * COPY_TRANS processed before all other actions.

Version 26:
  * Handle state trigger weighting. Special thanks to Jason Compton
    and Ghreyfain for doing the research.
  * Log OR print "[FOO.DLG] created" but don't do both.
  * Added AT_UNINSTALL

Version 25:
  * Now delay "successfully installed" until dialog.tlk has been
    saved.

Version 23-24:
  * Incorporate Jason's README suggestions. Thanks a bundle!
  * Add support for IWD.
  * Convert multiple newlines to the right format when compiling and
    decompiling.

Version 22:
  * Replace [\r\n]+ in raw-text (triggers, actions) with "\r\n".
    This should help people who were getting doubled new-lines.

Version 21:
  * Search the Registry for the game path
  * Handle overwriting read-only files

Version 14-20:
  * --biff-get now ignores override/
  * After a TP error we reload the translation file, so no more
    weird strings during install
  * SETUP-FOO.EXE implies a search for FOO.TP2
  * Added COPY_TRANS (thanks, JC)
  * Added AUTO_TRA (thanks, Quitch)

Version 13:
  * Change TP to TP2 after a reported conflict with the Sola mod.

Version 12:
  * The dawn of recorded history.
  * Fixed an idempotence problem with the Un-Ininstall algorithm.
  * Fixed a translation problem with multiple packages in a TP file.
  * Allow multiple TRA files for one LANGUAGE in a TP file.
  * Add the AT_EXIT command.
