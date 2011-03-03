(* parsetables.ml *)
(* representation of parsing tables *)
(* based on elkhound/parsetables.h *)


(* The C++ implementation is designed so the sizes of the various
 * table entries can be adjusted.  I'm reflecting that design here,
 * but I am just using 'int' as the size choice everywhere, since
 * (I think) OCaml arrays don't get smaller if you use (e.g.) char.
 *
 * The way to make array of char efficiently is using strings, but
 * that's a TODO at best, for now.
 *)


(* for action entries; some places may still be called int *)
type tActionEntry = int

(* identifier for a state in the finite automaton *)
type tStateId = int
let cSTATE_INVALID = -1

(* entry in the goto table *)
type tGotoEntry = int

(* index for a terminal *)
type tTermIndex = int

(* index for a nonterminal *)
type tNtIndex = int

(* index for a production *)
type tProdIndex = int

(* ErrorBitsEntry goes here *)


(* encode a symbol in the 'stateSymbol' map *)
(*   N+1:  terminal N *)
(*   0:    no symbol *)
(*   -N-1: nonterminal N *)
type tSymbolId = int
let symIsTerm (id: tSymbolId) : bool =        id > 0
let symAsTerm (id: tSymbolId) : int =         id-1      (* why not TermIndex? don't know *)
let symIsNonterm (id: tSymbolId) : bool =     id < 0
let symAsNonterm (id: tSymbolId) : tNtIndex = -id-1


(* collection of data needed for the online parsing algorithm *)
type tParseTables = {
  (* grammar counts *)
  numTerms: int;
  numNonterms: int;
  numProds: int;

  (* # of states in LR automaton *)
  numStates: int;

  (* action table, indexed by (state*actionCols + lookahead) *)
  actionCols: int;
  actionTable_val: int array;
  actionTable_cnt: int array;
  mutable actionTable_use: int array;

  (* goto table, indexed by (state*gotoCols + nontermId) *)
  gotoCols: int;
  gotoTable_val: int array;
  gotoTable_cnt: int array;
  mutable gotoTable_use: int array;

  (* production info, indexed by production id *)
  prodInfo_rhsLen: int array;         (* this is 'unsigned char' array in C++ *)
  prodInfo_lhsIndex: tNtIndex array;

  (* map state to symbol shifted to arrive at that state *)
  stateSymbol: tSymbolId array;

  (* ambiguous actions: one big list, for allocation purposes; then
   * the actions encode indices into this table; the first indexed
   * entry gives the # of actions, and is followed by that many
   * actions, each interpreted the same way ordinary 'actionTable'
   * entries are *)
  ambigTableSize: int;                (* redudant in OCaml... *)
  ambigTable: tActionEntry array;

  (* total order on nonterminals; see elkhound/parsetables.h *)
  nontermOrder: tNtIndex array;

  (* TODO: implement some of the table compression options? *)

  (* start state id (always 0) *)
  startState: tStateId;
  
  (* index of last production to reduce *)
  finalProductionIndex: int;
}

let build_table vals cnts =
  let res = Array.make (Array.fold_left (+) 0 cnts) 0 in
  if Array.length vals <> Array.length cnts then failwith "build_table internal 1";
  let pointer = ref 0 in
  let nonnull = ref 0 in
  for i = 0 to Array.length vals - 1 do
    let this_val = vals.(i) in
    let this_cnt = ref cnts.(i) in
    while !this_cnt > 0 do
      res.(!pointer) <- this_val;
      incr pointer;
      decr this_cnt;
      if this_val <> 0 && this_val <> 65535 then
        incr nonnull
    done;
  done;
  if !pointer <> Array.length res then failwith "build_table internal 2";
  res

let build_actionTable tables =
  tables.actionTable_use <- build_table tables.actionTable_val tables.actionTable_cnt
  
let build_gotoTable tables =
  tables.gotoTable_use <- build_table tables.gotoTable_val tables.gotoTable_cnt


(* -------------- ParseTables client access interface -------------- *)
let getActionEntry (tables: tParseTables) (state: int) (tok: int) : tActionEntry =
begin
  if Array.length tables.actionTable_use = 0 then
    build_actionTable tables;
  tables.actionTable_use.(state * tables.actionCols + tok)
end

let getActionEntry_noError (tables: tParseTables) (state: int) (tok: int) : tActionEntry =
begin
  (getActionEntry tables state tok)
end


let isShiftAction (tables: tParseTables) (code: tActionEntry) : bool =
begin
  code > 0 && code <= tables.numStates
end

(* needs tables for compression *)
let decodeShift (code: tActionEntry) (shiftedTerminal: int) : tStateId =
begin
  code-1
end

let isReduceAction (code: tActionEntry) : bool =
begin
  code < 0
end

(* needs tables for compression *)
let decodeReduce (code: tActionEntry) (inState: tStateId) : int =
begin
  -(code+1)
end

let isErrorAction (*tables*) (code: tActionEntry) : bool =
begin
  code = 0
end

                       
(* this returns an index into the ambigTable *)
(* needs tables for compression *)
let decodeAmbigAction (tables: tParseTables) (code: tActionEntry) 
                      (inState: tStateId) : int =
begin
  code - 1 - tables.numStates
end


let getGotoEntry (tables: tParseTables) (stateId: tStateId)
                 (nontermId: int) : tGotoEntry =
begin
  if Array.length tables.gotoTable_use = 0 then
    build_gotoTable tables;
  tables.gotoTable_use.(stateId * tables.gotoCols + nontermId)
end

(* needs tables for compression *)
let decodeGoto (code: tGotoEntry) (shiftNonterminal: int) : tStateId =
begin
  code
end


let getProdInfo_rhsLen (tables: tParseTables) (rule: int) : int =
begin
  tables.prodInfo_rhsLen.(rule)
end

let getProdInfo_lhsIndex (tables: tParseTables) (rule: int) : int =
begin
  tables.prodInfo_lhsIndex.(rule)
end


let getNontermOrdinal (tables: tParseTables) (idx: int) : int =
begin
  tables.nontermOrder.(idx)
end


(* EOF *)
