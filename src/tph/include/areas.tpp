// Weidu Area Patching Tool.
//
// Q_AREInitVars, Q_AREAdd_InitVars and Q_AREAdd_Process macros belong to Qwinn and are designed to add any desirable number
// of objects to an area file. They do not, however, cover the writing procedure, leaving all the newly added space empty.
//
// It was concluded that manual writing is time consuming and unappealing, especially for novice modders, so there are
// ADD_AREA_FLOOR_TRIGGER, ADD_AREA_INFO_TRIGGER, ADD_AREA_TRANSITION_TRIGGER and ADD_AREA_CONTAINER functions designed by
// plainab/Sasha Al-Therin and Ardanis/GeN1e to cover the most common tasks of area editing.
//
// wapt_inserter, wapt_routine and wapt_array macros are designed to reduce the overall amount of code. They are not
// supposed to be directly called by a modder. Don't do it, unless you understand everything about of them and really
// know what you're doing. And honestly, I somehow doubt anyone would be interested...
//
// Sasha insisted that the tool should allow for adding multiple objects at once, to realize the full potential of Qwinn's
// work. However, due to certain troubles with assigning new text strings to containers and infotriggers, I had to reduce the
// maximum quantity allowed to 10. Hopefully it's still more than enough.
//
// Also, functions were changed from ACTION, as they initially were, to PATCH. First, due to the abovementioned problems with
// new text strings. Second, in my opinion it's easier to follow.


//////////////////////////////////////

// This macro does the following:
//   sets 'bbox' to 0, which is the starting value for all ADD_AREA_ functions
//   calculates the total amount of new objects
//   calculates the total amount of new vertices
//   inserts new objects and vertices using Qwinn's algorithm
//   WRITEs new vertices
//   sets 'vert_entry' - the starting index of newly added vertices 

DEFINE_PATCH_MACRO wapt_inserter BEGIN
  bbox=0 // vertex index for bounding box calculation

  wapt_object=0
  FOR (i=0;i<10;i+=1) BEGIN // calculating the amount of new objects, up to 10
    PATCH_IF (VARIABLE_IS_SET $wapt_object_name("%i%")) BEGIN
      wapt_object+=1
    END
  END

  vert=0                               //calculating the number of new
  FOR (i=0;i<wapt_object;i+=1) BEGIN   //vertices, to save modder the
    vert+= $wapt_vertices("%i%")       //trouble of doing it manually
  END

  LAUNCH_PATCH_MACRO ~wapt_Q_ARE_InitVars~    //launch variable initiation macro
  LAUNCH_PATCH_MACRO ~wapt_Q_AREAdd_InitVars~ //launch variable initiation macro for adding new sections
  Q_New_Vertx = vert
  PATCH_FOR_EACH ob_type IN ~Conta~ ~Entra~ ~Trigg~ BEGIN         //which type of object we're adding
    PATCH_IF ~%wapt_ob_type%~ STRING_EQUAL_CASE ~%ob_type%~ BEGIN //wapt_ob_type is preset by STR_VAR
      SET $Q_New("%ob_type%") = wapt_object
    END
  END
  LAUNCH_PATCH_MACRO ~wapt_Q_AREAdd_Process~  //launch macro that adds new space for above listed entries

  vert_entry = Q_Num_Vertx - Q_New_Vertx      //starting index for new vertices, used by the main FUNCTION
  FOR (i=0;i<vert;i+=1) BEGIN                             //
    WRITE_SHORT (Q_NewOffset_Vertx+i*4)   $wapt_vx("%i%") //writing new vertices
    WRITE_SHORT (Q_NewOffset_Vertx+i*4+2) $wapt_vy("%i%") //
  END
END

/////////////////////////////////////

// This macro does the following:
//   sets 'new_loc' - the starting offset for writing down the new object
//   writes the object's name, which is 32 char long 
//   calculates and writes the bounding box for the object

DEFINE_PATCH_MACRO wapt_routine BEGIN // vars used for bbox math: a b c c1 c2 bbox_off
  new_loc = $Q_NewOffset("%wapt_ob_type%") + i*$Q_Siz("%wapt_ob_type%") // from where we'll do writing
  WRITE_ASCIIE new_loc $wapt_object_name("%i%")                         // object's name

  bbox_off=new_loc+wapt_bbox                                 //offset for this object's bbox, wapt_bbox is preset by INT_VAR
  FOR (a=0;a<4;a+=1) BEGIN                                   //0=X_low - left, 1=Y_low - down, 2=X_high - right, 3=Y_high - top
    PATCH_IF a=0 OR a=2 BEGIN SPRINT xy ~wapt_vx_~ END ELSE BEGIN SPRINT xy ~wapt_vy_~ END //X or Y coordinate of the vertex
    c=bbox                                                   //we assume that the extreme vertex is the 1st one of the object
    c1=EVALUATE_BUFFER ~%%xy%%c%%~                           //coord_value_1=(X/Y)_%vert_index%, coord of object's the 1st vertex 
    FOR (b=bbox+1;b<$wapt_vertices("%i%")+bbox;b+=1) BEGIN   //start with 2nd vertex of the object, finish with the last
      c2=EVALUATE_BUFFER ~%%xy%%b%%~                         //coord_value_2=(X/Y)_%counter%, other vertices of the object
      PATCH_IF (a<2&c2<c1)|(a>1&c2>c1) BEGIN //compare the 1st vertex's value with that of the others, if some exceeds then...
        c=b                                  //...mark it as the new extreme and...
        c1=c2                                //...use it's value to check against the remaining
      END
      WRITE_SHORT (bbox_off+a*2) EVALUATE_BUFFER ~%%xy%%c%%~ //precisely the box's values for the object, writing that down
    END
  END
  bbox+=$wapt_vertices("%i%") // update the bbox index so it matches the 1st vertex of the next object
END // should ADD_DOOR (it has 3 polygons instead of 1) be introduced, this macro will have to be revamped

///////////////////////////////////

// This macro does the following:
//   loads the array block from the main function, 'sol' stands for 'SHORT or LONG (or ASCII)'
//   checks if the appropriate variable was set by modder
//   if it was, then WRITEs it at the appropriate offset

DEFINE_PATCH_MACRO wapt_array BEGIN // vars used: ia
  FOR (ia=0;ia<array;ia+=1) BEGIN                                       // 'array' is set within the main function
    PATCH_IF (VARIABLE_IS_SET $wapt($$nam("%ia%")("%i%")) ) BEGIN
      PATCH_IF ($sol("%ia%")=2) BEGIN WRITE_SHORT  (new_loc+$off("%ia%")) $wapt($$nam("%ia%")("%i%")) END
      PATCH_IF ($sol("%ia%")=4) BEGIN WRITE_LONG   (new_loc+$off("%ia%")) $wapt($$nam("%ia%")("%i%")) END
      PATCH_IF ($sol("%ia%")=8) BEGIN WRITE_ASCIIE (new_loc+$off("%ia%")) $wapt($$nam("%ia%")("%i%")) END
    END
  END
END

//////////////////////////////////////////////

// Note by Ardanis:
// As you can tell by the prefix, it originally was a Qwinn's macro. I compressed it
// into a more accessible format. A fair example of how to make a good use of arrays.

// Bigg, Weidu's readme says it needs a real example of array usage. Does this pass for one?

DEFINE_PATCH_MACRO ~wapt_Q_ARE_InitVars~ BEGIN

PATCH_IF (GAME_IS ~pst~)                     BEGIN Q_Game=1 END
PATCH_IF (GAME_IS ~bg2 tob tutu tutu_totsc~) BEGIN Q_Game=2 END
PATCH_IF (GAME_IS ~bg1 totsc iwd how~)       BEGIN Q_Game=3 END

DEFINE_ARRAY object BEGIN Actor Trigg Spawn Entra Conta Items Ambie Varia Doors Tiled Vertx Explo Anima Songs RestS MapNo ProTr END
DEFINE_ARRAY Siz    BEGIN 0x110 0xc4  0xc8  0x68  0xc0  0x14  0xd4  0x54  0xc8  0x6c  0x4   0x0   0x4c  0x90  0xe4  0x34  0x1A  END
DEFINE_ARRAY OoN    BEGIN 0x58  0x5a  0x64  0x6c  0x74  0x76  0x82  0x8c  0xa4  0xb4  0x80  0x0   0xac  0x0   0x0   0xc8  0xd0  END
DEFINE_ARRAY OoO    BEGIN 0x54  0x5c  0x60  0x68  0x70  0x78  0x84  0x88  0xa8  0xb8  0x7c  0xa0  0xb0  0xbc  0xc0  0xc4  0xcc  END
DEFINE_ARRAY SoL    BEGIN 2     2     4     4     2     2     2     4     4     4     2     0     4     0     0     4     4     END

FOR (i=0;i<17;i+=1) BEGIN
  SET $Q_Siz($object("%i%"))=$Siz("%i%")     // size of object's section
  SET $Q_OoN($object("%i%"))=$OoN("%i%")     // offset of number of objects
  SET $Q_OoO($object("%i%"))=$OoO("%i%")     // offset of offset of objects
  SET $Q_SoL($object("%i%"))=$SoL("%i%")     // SHORT or LONG offset of number of objects
  PATCH_IF i=15 & Q_Game=1 BEGIN                                 // PST uses different values
    Q_OoN_MapNo=0xcc Q_OoO_MapNo=0xc8 END
  PATCH_IF $SoL("%i%")=2 BEGIN               // number of objects
    READ_SHORT $Q_OoN($object("%i%")) $Q_Num($object("%i%")) END // if SoL = 2 = SHORT, then READ_SHORT
  PATCH_IF $SoL("%i%")=4 & ( i!=16 | Q_Game=2) BEGIN             // non-BG2 games can't have projectiles
    READ_LONG  $Q_OoN($object("%i%")) $Q_Num($object("%i%")) END // if SoL = 4 = LONG, then READ_LONG
  PATCH_IF $SoL("%i%")=0 BEGIN                                   // if SoL = 0 = not needed, don't READ, instead
    SET $Q_Num($object("%i%"))=1 END                             //   SET it to 1, as this can't be any other
  PATCH_IF i!=16 | Q_Game=2 BEGIN            // offset of objects
    READ_LONG  $Q_OoO($object("%i%")) $Q_Off($object("%i%")) END // offsets' READs always LONG
END

PATCH_IF Q_Game!=2 BEGIN                                         // if it's non-BG2 then set everything
  Q_OoN_ProTr=0 Q_OoO_ProTr=0 Q_Num_ProTr=0 Q_Off_ProTr=0 END    // related to projectiles to zero

END

/////////////////////////////////////////////////

// Note by Ardanis:
// Again, Qwinn's stuff that was revamped.

DEFINE_PATCH_MACRO ~wapt_Q_AREAdd_InitVars~ BEGIN
FOR (i=0;i<17;i+=1) BEGIN
  SET $Q_New($object("%i%"))=0        // number of new objects
  SET $Q_NewOffset($object("%i%"))=0  // writing offset for new objects
END
Q_ManualInsert=0  // unnecessary, but may be required for RESHAPE_AREA_POLYGON, should it be added later (unlikely)
END

//////////////////////////////////////////

// Note by Ardanis:
// Unlike the other two Qwinn's macros, this one is almost untouched, except of adding the 'wapt_' prefix to
// it's name and correcting the name of the macro LAUNCHed at the very end, which now has that prefix as well

DEFINE_PATCH_MACRO ~wapt_Q_AREAdd_Process~
BEGIN

// DO NOT use this macro without first running Q_AREAdd_InitVars.
// Documentation for the use of this macro is contained within that macro definition.

PATCH_FOR_EACH "S1" IN
  ~Actor~ ~Trigg~ ~Spawn~ ~Entra~ ~Conta~ ~Items~ ~Ambie~ ~Varia~ ~Doors~
  ~Tiled~ ~Vertx~ ~Anima~ ~MapNo~ ~ProTr~
BEGIN
  SET "Q_NewSect" = $Q_New("%S1%") // How many new sections user has asked for
  PATCH_IF !("Q_NewSect" = 0) THEN
  BEGIN
	// WRITE_ASCII 0x33c ~%S1%~ #32 // DEBUG
	SET "Q_OoNSect" = $Q_OoN("%S1%") // Offset where count of each section is stored
	SET "Q_NumSect" = $Q_Num("%S1%") // Original count for that section
	SET "Q_SoLSect" = $Q_SoL("%S1%")  // Whether original count is stored as long or short
	SET "Q_OoOSect1" = $Q_OoO("%S1%") // Offset of offset for the section
	SET "Q_Offset1" = $Q_Off("%S1%") // Offset of the section being added to
	SET "Q_SizSect" = $Q_Siz("%S1%") // The size of one new section
	PATCH_FOR_EACH "S2" IN
	  ~Actor~ ~Trigg~ ~Spawn~ ~Entra~ ~Conta~ ~Items~ ~Ambie~ ~Varia~ ~Doors~
	  ~Tiled~ ~Vertx~ ~Explo~ ~Anima~ ~Songs~ ~RestS~ ~MapNo~ ~ProTr~
	BEGIN
	  // WRITE_ASCII 0x33c ~%S1% %S2%~ #32 // DEBUG
	  SET "Q_Offset2" = $Q_Off("%S2%") // Offset of each other section
	  SET "Q_OoOSect2" = $Q_OoO("%S2%") // Offset of that offset
	  SET "Q_OldInsert" = $Q_NewOffset("%S2%") // Previous insert offsets need to be updated too

	  PATCH_IF ("Q_Offset2" >= "Q_Offset1") AND NOT ("%S1%" STRING_EQUAL "%S2%") THEN
	  BEGIN
		WRITE_LONG "Q_OoOSect2" ("Q_Offset2" + ("Q_NewSect" * "Q_SizSect"))
	  END
	  PATCH_IF ("Q_OldInsert" >= "Q_Offset1") AND NOT ("%S1%" STRING_EQUAL "%S2%") THEN
	  BEGIN
		SET $Q_NewOffset("%S2%") = $Q_NewOffset("%S2%") + ("Q_NewSect" * "Q_SizSect")
	  END
	END
	SET $Q_NewOffset("%S1%") = "Q_Offset1" + ("Q_NumSect" * "Q_SizSect")
	SET "Q_InsertOffset" = $Q_NewOffset("%S1%")
	PATCH_IF "Q_ManualInsert" = 0 THEN
	BEGIN
	  INSERT_BYTES "Q_InsertOffset" ("Q_NewSect" * "Q_SizSect")
	END
        PATCH_IF "Q_SoLSect" = 2 THEN BEGIN WRITE_SHORT "Q_OoNSect" ("Q_NumSect" + "Q_NewSect") END
                                 ELSE BEGIN WRITE_LONG  "Q_OoNSect" ("Q_NumSect" + "Q_NewSect") END
	LAUNCH_PATCH_MACRO ~wapt_Q_ARE_InitVars~  // Reset all our variables to their new values
  END
END

END

/////////////////////////////////////////////////

DEFINE_PATCH_FUNCTION add_area_transition_trigger
  INT_VAR
    wapt_bbox=0x22
  STR_VAR
    wapt_ob_type= ~Trigg~
  BEGIN
///////
LAUNCH_PATCH_MACRO wapt_inserter

FOR (i=0;i<wapt_object;i+=1) BEGIN
  LAUNCH_PATCH_MACRO wapt_routine
  WRITE_SHORT (new_loc + 0x20)  2                      // transition trigger 

  DEFINE_ARRAY nam BEGIN vertices cursor exit_are flags END
  DEFINE_ARRAY off BEGIN 0x2a     0x34   0x38     0x60  END
  DEFINE_ARRAY sol BEGIN 2        4      8        2     END
  array=4  // how long the array block is
  LAUNCH_PATCH_MACRO wapt_array

  WRITE_ASCIIE (new_loc+0x40) $wapt_exit_name("%i%")

  WRITE_LONG  (new_loc + 0x2c)  vert_entry             // vertex index
  vert_entry += $wapt_vertices("%i%")                  // update vertex index for the next object
END

FOR (j=0;j<wapt_object;j+=1) BEGIN           // okay, this is a temporary solution, as it couldn't fit into initial framework
  SPRINT are_name $wapt_exit_are("%j%")
  INNER_ACTION BEGIN
    COPY_EXISTING ~%are_name%.are~ ~override~
      LAUNCH_PATCH_MACRO ~wapt_Q_ARE_InitVars~
      LAUNCH_PATCH_MACRO ~wapt_Q_AREAdd_InitVars~
      Q_New_Entra = 1
      LAUNCH_PATCH_MACRO ~wapt_Q_AREAdd_Process~

      new_loc = Q_NewOffset_Entra
      WRITE_ASCIIE new_loc $wapt_exit_name("%j%")
      WRITE_SHORT new_loc+0x20 $wapt_exit_x("%j%")
      WRITE_SHORT new_loc+0x22 $wapt_exit_y("%j%")
      PATCH_IF (VARIABLE_IS_SET $wapt_orient("%j%")) BEGIN
        WRITE_SHORT new_loc+0x24 $wapt_orient("%j%")
      END
    BUT_ONLY
  END
END
///
END

////////////////////////////////////////////

DEFINE_PATCH_FUNCTION add_area_floor_trigger
  INT_VAR
    wapt_bbox=0x22
  STR_VAR
    wapt_ob_type= ~Trigg~
  BEGIN
///////
LAUNCH_PATCH_MACRO wapt_inserter

FOR (i=0;i<wapt_object;i+=1) BEGIN
  LAUNCH_PATCH_MACRO wapt_routine
  WRITE_SHORT (new_loc+0x20)  0                        // floor trap

  WRITE_SHORT (new_loc+0x6c) 1                         // trigger is active by default

  DEFINE_ARRAY nam BEGIN vertices flags detect_diff disarm_diff trapped detected launch_x launch_y script dialog END
  DEFINE_ARRAY off BEGIN 0x2a     0x60  0x68        0x6a        0x6c    0x6e     0x70     0x72     0x7c   0xbc   END
  DEFINE_ARRAY sol BEGIN 2        4     2           2           2       2        2        2        8      8      END
  array=10  // how long the array block is
  LAUNCH_PATCH_MACRO wapt_array

  WRITE_LONG  (new_loc + 0x2c)  vert_entry             // vertex index
  vert_entry += $wapt_vertices("%i%")                  // update vertex index for the next object
END
///
END

///////////////////////////////////////////

DEFINE_PATCH_FUNCTION add_area_info_trigger
  INT_VAR
    wapt_bbox=0x22
  STR_VAR
    wapt_ob_type= ~Trigg~
  RET
    wapt_string_0 wapt_string_1 wapt_string_2 wapt_string_3 wapt_string_4
    wapt_string_5 wapt_string_6 wapt_string_7 wapt_string_8 wapt_string_9
  BEGIN
///////
LAUNCH_PATCH_MACRO wapt_inserter

FOR (i=0;i<wapt_object;i+=1) BEGIN
  LAUNCH_PATCH_MACRO wapt_routine
  WRITE_SHORT (new_loc+0x20) 1                          // info point

  WRITE_LONG (new_loc+0x34) 22                          // cursor type is 'info point' by default

  DEFINE_ARRAY nam BEGIN vertices cursor script dialog END
  DEFINE_ARRAY off BEGIN 0x2a     0x34   0x7c   0xbc   END
  DEFINE_ARRAY sol BEGIN 2        4      8      8      END
  array=4  // how long the array block is
  LAUNCH_PATCH_MACRO wapt_array

  PATCH_IF (VARIABLE_IS_SET $wapt_string("%i%")) BEGIN
    SET $wapt_string("%i%") = (new_loc+0x64) 
  END

  WRITE_LONG  (new_loc + 0x2c)  vert_entry              // vertex index
  vert_entry += $wapt_vertices("%i%")                   // update vertex index for the next object
END
///
END

////////////////////////////////////////

DEFINE_PATCH_FUNCTION add_area_container
  INT_VAR
    wapt_bbox=0x38
  STR_VAR
    wapt_ob_type= ~Conta~
  RET
    wapt_string_0 wapt_string_1 wapt_string_2 wapt_string_3 wapt_string_4
    wapt_string_5 wapt_string_6 wapt_string_7 wapt_string_8 wapt_string_9
  BEGIN
///////
LAUNCH_PATCH_MACRO wapt_inserter

FOR (i=0;i<wapt_object;i+=1) BEGIN
  LAUNCH_PATCH_MACRO wapt_routine

  DEFINE_ARRAY nam BEGIN use_x use_y type lock_diff flags detect_diff disarm_diff trapped detected launch_x launch_y script vertices key_item END
  DEFINE_ARRAY off BEGIN 0x20  0x22  0x24 0x26      0x28  0x2c        0x2e        0x30    0x32     0x34     0x36     0x48   0x54     0x78     END
  DEFINE_ARRAY sol BEGIN 2     2     2    2         4     2           2           2       2        2        2        8      4        8        END
  array=14  // how long the array block is
  LAUNCH_PATCH_MACRO wapt_array

  PATCH_IF (VARIABLE_IS_SET $wapt_string("%i%")) BEGIN
    SET $wapt_string("%i%") = (new_loc+0x84)
  END

  WRITE_LONG (new_loc+0x40) Q_Num_Items
  WRITE_LONG (new_loc+0x50)  vert_entry                    // vertex index
  vert_entry += $wapt_vertices("%i%")                      // update vertex index for the next object
END
///
END
