(* Effects *)
open BatteriesInit
open Util

let op_str = "Bonus to AC_Attacks per 2 round bonus_Awaken_Berserk_Calm_Charm creature_Charisma bonus_Set item color_Set colorglow solid_Set colorglow pulse_Constitution bonus_Cure poison_Damage_Death_Defrost_Dexterity bonus_Haste_Current HP bonus_Max HP bonus_Intelligence bonus_Invisibility_Lore bonus_Luck bonus_Remove fear_Panic_Poison_Remove curse_Acid resistance bonus_Cold resistance bonus_Electricity resistance bonus_Fire resistance bonus_Magic damage resistance bonus_Raise dead_Save vs death bonus_Save vs wands bonus_Save vs polymorph bonus_Save vs breath bonus_Save vs spell bonus_Silence_Sleep_Slow_Sparkle_Bonus wizard spells_Stone to flesh_Strength bonus_Stun_Unstun_Visible_Vocalize_Wisdom bonus_Character color 1_Character color 2_Character color 3_Animation change_THAC0 bonus_Slay_Reverse alignment_Change alignment_Dispel effects_Stealth bonus_Casting failure_Unknown (3D)_Bonus priest spells_Infravision_Remove infravision_Blur_Translucent_Summon creature_Unsummon creature_Non-detection_Remove non-detection_Change sex_Set reaction_Attack damage bonus_Blindness_Cure blindness_Feeblemindedness_Cure feeblemindedness_Disease_Cure disease_Deafness_Cure deafness_Set AI script_Immunity to projectile_Magical fire resistance bonus_Magical cold resistance bonus_Slashing resistance bonus_Crushing resistance bonus_Piercing resistance bonus_Missile resistance bonus_Lockpicking bonus_Find traps bonus_Pick pockets bonus_Fatigue bonus_Intoxification bonus_Tracking bonus_Change level_Exceptional strength bonus_Regeneration_Modify duration_Protection from creature type_Immunity to effect_Immunity to spell level_Change name_XP bonus_Gold bonus_Break morale_Change portrait_Reputation_Hold creature_Retreat from_Create item_Remove item_Equip weapon_Disallow spellcasting_Detect alignment_Detect invisible_Clairvoyance_Show creatures_Mirror image_Immunity to weapons_Visual animation effect_Create inventory item_Remove inventory item_Teleport_Unlock_Movementrate bonus_Summon monsters_Confusion_Aid (non-cumulative)_Bless (non-cumulative)_Chant (non-cumulative)_Draw upon holy might (non-cumulative)_Luck (non-cumulative)_Petrification_Polymorph_Force visible_Bad chant (non-cumulative)_Character animation change_Display string_Casting glow_Lighting effects_Display portrait icon_Create item in slot_Disable button_Disable spellcasting_Cast spell_Learn spell_Cast spell (scroll)_Identify_Find traps_Summon hostile creature_Play movie_Sanctuary_Entangle overlay_Minor globe overlay_Protection from normal missiles overlay_Web effect overlay_Grease effect overlay_Mirror image effect_Unknown (A0)_Remove fear 2_Remove paralysis_Free action_Slow poison_Burning hands overlay_Magic resistance bonus_Missile THAC0 bonus_Remove creature_Remove portrait icon_Play damage animation_Give innate ability_Remove spell_Reduced damage from poison_Play sound_Hold creature type_Unknown (B0)_Use EFF file_THAC0 vs. type bonus_Damage vs. type bonus_Disallow item_Unknown (B5)_Unknown (B6)_Unknown (B7)_No collision detection_Hold creature 2_Increase critical hits_Set local variable_Increase spells cast pr. round_Increase casting speed factor_Increase attack speed factor_Unknown (BF)_Find familiar_Invisible detection by script_Unknown (C2)_Unknown (C3)_Unknown (C4)_Physical mirror_Unknown (C6)_Reflect spells_Spell turning_Spell deflection_Unknown (CA)_Damage turning_Protection from spell school_Spell type deflection_Protection from spell_Reflect specified spell_Minimum HP_Power word: Kill_Power word: Stun_Imprisonment_Freedom_Maze_Select Spell_Play 3D Effect_Level drain_Power word: Sleep_Stoneskin effect_Attack roll penalty_True sight_Remove spell protections_Teleport field_Random attack sequence_Restoration_Unknown (E1)_Spell shield_Unknown (E3)_Unknown (E4)_Unknown (E5)_Remove magical protections_Time stop_Cast spell on condition_Modify proficiencies_Contingency_Wing buffet_Project image_Unknown (ED)_Disintegrate_Farsight_Remove physical protections_Control creature_Cure confusion_Drain item charges_Drain wizard spells_Unknown (F5)_Unknown (F6)_Attack nearest creature_Give item effect_Use EFF file 2_Maximum damage each hit_Change bard song effect_Set trap_Set map marker 1_Set map marker 2_Create item 2_Unknown (100)_Create spell sequencer_Activate spell sequencer_Spell trap_Unknown (104)_Restore lost spell_Unknown (106)_Backstab bonus_Drop weapons_Set global variable_Unknown (10A)_Disable display string_Clear fog of war_Shake window_Unpause caster_Remove animation_Use EFF File on condition_Zone of sweet air_Remove zone of sweet air_Hide in shadow bonus_Detect illusions bonus_Set traps bonus_THAC0 bonus_Reenable button_Wild magic_Restore normal spellcasting_Modify script state_Use EFF file 3_THACO bonus_Melee weapon damage bonus_Missile weapon damage bonus_Remove feet circle_Fist THAC0 bonus_Fist damage bonus_Change title_Disable visual effect_Immunity to backstab_Unknown (125)_Unknown (126)_Play animation sequence_Immunity to specific animation_Creature uninterruptable_Use 2DA file_Unknown (12B)_Unknown (12C)_Die roll number_Can use any item_Backstab every hit_Mass raise dead_Off-hand THACO bonus_Unknown (132)_Tracking bonus_Unknown (134)_Set local variable_Immune to timestop_Unknown (137)_Unknown (138)_Unknown (139)_Stoneskin protection_Unknown (13B)_Rest (Wish)_Double number of attacks_Unknown (13E)_Unknown (13F)" 

let op_list = ref []  

let name_of_opcode i = 
  (if !op_list = [] then op_list := Str.split (Str.regexp "_") op_str) ;
  List.nth !op_list i 

open Printf

let describe_itm buff =
  let out = Buffer.create 255 in 
  let flags = int_of_str_off buff 0x18 in
  if (flags land 0b1000000) <> 0 then bprintf out "Magical. " 
  else bprintf out "Non-Magical. " ; 
  let ench = int_of_str_off buff 0x60 in
  if ench <> 0 then bprintf out "%+d. " ench ; 
  let cost = int_of_str_off buff 0x34 in
  bprintf out "Cost: %d.\n" cost ; 
  let num_eff = short_of_str_off buff 0x70 in
  let eff_off = int_of_str_off buff 0x6a in
  let num_ab = short_of_str_off buff 0x68 in
  let ab_off = int_of_str_off buff 0x64 in 
  if num_eff > 0 then bprintf out "Equipped:\n" ; 
  
  let do_eff off = begin
    let opcode = short_of_str_off buff off in
    if opcode >= 7 && opcode <= 9 then
      ()
    else begin 
      let targ = byte_of_str_off buff (off + 2) in
      let p1 = byte_of_str_off buff (off + 0xbc - 0xaa) in 
      let p2 = byte_of_str_off buff (off + 0xbd - 0xaa) in 
      let save = int_of_str_off buff (off + 0xce - 0xaa) in 
      let savebonus = int_of_str_off buff (off + 0xd2 - 0xaa) in 
      let dur = byte_of_str_off buff (off + 0x7e - 0x72) in 
      let disres = byte_of_str_off buff (off + 0x7f - 0x72) in 
      let name = name_of_opcode opcode in 
      if p1 = 0 && p2 = 0 then
	()
      else begin 
	(if (p1 - p2) <> 100 then 
	  bprintf out "  %d%%: %s" (p1 - p2) name 
	else 
	  bprintf out "  %s" name) ; 
	if (opcode = 101) then begin 
	  let what = int_of_str_off buff (off + 0xaa - 0xa2) in 
	  bprintf out ": %s" (name_of_opcode what) 
	end else if (opcode = 12) then begin
	  let fixed = int_of_str_off buff (off + 0x6e - 0x6a) in 
	  let num_d = int_of_str_off buff (off + 0x86 - 0x6a) in 
	  let d_size = int_of_str_off buff (off + 0x8a - 0x6a) in 
	  (if num_d > 0 && d_size > 0 then bprintf out " %dd%d" num_d d_size) ;
	  (if fixed > 0 then bprintf out " %+d" fixed) 
	end else begin 
	  let i1 = int_of_str_off buff (off + 0xae - 0xaa) in
	  let i2 = int_of_str_off buff (off + 0xb2 - 0xaa) in
	  let st = get_string_of_size buff (off + 0xbe - 0xaa) 8 in 
	  bprintf out " %d %d %s" i1 i2 st ; 
	end ; 
	if save <> 0 then bprintf out " (save at %+d)" savebonus ;
	if targ = 1 then bprintf out " (self)" ; 
	(if dur = 1 then bprintf out " (perm)"
	else if dur = 9 then bprintf out " (permafter)" 
	else if dur = 4 then bprintf out " (delayperm)" ) ; 
	bprintf out "\n" ; 
      end 
    end
  end in 

  for i = 0 to pred num_eff do
    let off = eff_off + (i * (0xa2 - 0x72)) in
    do_eff off 
  done ;
  for i = 0 to pred num_ab do
    let off = ab_off + (i * (0xaa - 0x72)) in 
    let range = short_of_str_off buff (off + 0x80 - 0x72) in 
    let speed = short_of_str_off buff (off + 0x84 - 0x72) in 
    let tohit = short_of_str_off buff (off + 0x86 - 0x72) in 
    let dicesize = short_of_str_off buff (off + 0x88 - 0x72) in 
    let numdice = short_of_str_off buff (off + 0x8a - 0x72) in 
    let dambonus = short_of_str_off buff (off + 0x8c - 0x72) in 
    let allow = byte_of_str_off buff (off + 0x98 - 0x72) in 
    bprintf out "Ability:\n  Range: %d\n  Speed: %d\n  To Hit: %d\n  Damage: %dd%d%+d %b\n" range speed tohit numdice dicesize dambonus (allow <> 0);
    let numeff = short_of_str_off buff (off + 0x90 - 0x72) in
    let effidx = short_of_str_off buff (off + 0x92 - 0x72) in 
    for j = 0 to pred numeff do
      let off = eff_off + ((effidx + j) * (0xa2 - 0x72)) in
      do_eff off
    done 
  done ;
  Buffer.contents out

