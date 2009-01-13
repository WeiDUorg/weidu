// WARNING: this is an example of proper syntax only. Do not expect it
// to do anything useful if you compile it and run the DLGs. 

REPLACE PPUMB01

IF ~True()~ THEN BEGIN 0 // from:
  SAY #47559
  IF ~(IsValidForPartyDialogue("J#Kelsey")
Global("KelseyPPUMB01","LOCALS",0)~ THEN EXTERN J#KLSYJ PPUMB01_1
  IF ~OR(2)
!IsValidForPartyDialogue("J#Kelsey")
!Global("KelseyPPUMB01","LOCALS",0)~ THEN REPLY #47560 DO 
~StartStore("ppumb01",LastTalkedToBy())~ EXIT
  IF ~OR(2)
!IsValidForPartyDialogue("J#Kelsey")
!Global("KelseyPPUMB01","LOCALS",0)~ THEN REPLY #47561 GOTO 1
END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN PPUMB01_1
    SAY ~Heh. I really enjoy when clerics with bad attitudes try to make 
me feel all warm and welcome. They're so bad at it!~
    IF ~~ THEN EXTERN PPUMB01 PPUMB01_2
  END
END

APPEND PPUMB01
IF ~~ THEN BEGIN PPUMB01_2
    SAY ~Silence! Do you have need of Umberlee's services or not?~
  IF ~~ THEN REPLY ~I do. What services have you available?~ 
    DO ~StartStore("ppumb01",LastTalkedToBy())~ EXIT
  IF ~~ THEN REPLY ~I do not have need of your services.~ EXTERN PPUMB01 1
END
  END


/*
After fixing that, I'm now getting "Cannot resolve label KELSEYSHOP06_3 
for file [SHOP06]" from this code:
*/





REPLACE SHOP06

IF ~NumTimesTalkedToGT(0)
OR(2)
!IsValidForPartyDialog("J#Kelsey")
!Global("J#KelseySHOP06","LOCALS",0)~
 THEN BEGIN 9 // from:
  SAY #16072
  IF ~~ THEN REPLY #16073 GOTO 5
  IF ~~ THEN REPLY #16076 GOTO 10
  IF ~~ THEN REPLY #16077 GOTO 1
END
END

APPEND SHOP06
IF ~NumTimesTalkedToGT(0)
IsValidForPartyDialog("J#Kelsey")
Global("J#KelseySHOP06","LOCALS",0)~
THEN BEGIN KELSEYSHOP06_1
  SAY ~What can I do for you this day?~
  IF ~~ THEN DO ~SetGlobal("J#KelseySHOP06","LOCALS",1)~
  EXTERN J#KLSYJ KELSEYSHOP06_2
END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_2
    SAY ~Why, it's Hes, isn't it?~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_3
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_3
    SAY ~Yes! It is Hes! And it is Kelsey! How are times finding you?~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP06_4
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_4
    SAY ~Things go well, Hes...business obligations are light, but I am 
travelling with <CHARNAME> now.~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_5
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_5
    SAY ~Indeed, and the travel seems to suit you. Since you have so much 
to do, you will be needing to equip well, yes?~
    IF ~~ THEN EXTERN J#KLSYJ KELSEYSHOP06_6
  END
END

APPEND J#KLSYJ

  IF ~~ THEN BEGIN KELSEYSHOP06_6
    SAY ~Ah, well, it's <CHARNAME>'s loot to spend, Hes, but rest assured, 
I will be sure to mention your name when <PRO_HESHE> needs supplies.~
    IF ~~ THEN EXTERN SHOP06 KELSEYSHOP06_7
  END
END

APPEND SHOP06

  IF ~~ THEN BEGIN KELSEYSHOP06_7
    SAY ~Then this has been a good day for both of us!~
    IF ~~ THEN EXIT
  END
END


/*

For the record, we are not getting errors with these REPLACEs.
*/


REPLACE CORNEIL 
IF ~~ THEN BEGIN 8 // from: 6.1 7.0
  SAY #59583
  IF ~ReputationGT(LastTalkedToBy,3)
!IsValidForPartyDialog("J#Kelsey")~ THEN REPLY #59584 GOTO 9
  IF ~ReputationGT(LastTalkedToBy,3)
IsValidForPartyDialog("J#Kelsey")~ THEN REPLY ~And just how much is this 
'monetary sacrifice', exactly?~ /* #59584 */ EXTERN J#KLSYJ CORNEIL_1
  IF ~ReputationLT(LastTalkedToBy,4)~ THEN REPLY #59589 GOTO 10
  IF ~~ THEN REPLY #59585 GOTO 5
END
END

APPEND J#KLSYJ
  IF ~~ THEN BEGIN CORNEIL_1
	SAY ~This isn't going to be pretty, <CHARNAME>...the Cowled 
Wizards are basically a monopoly on magic, and monopolies keep prices high 
and quantities low...~
  IF ~~ THEN EXTERN CORNEIL 9
END
END


REPLACE CORNEIL
IF ~~ THEN BEGIN 11 // from: 9.0
  SAY #59594
  IF ~!IsValidForPartyDialog("J#Kelsey")~ THEN DO 
~SetGlobal("BribedCowled","GLOBAL",1)~ EXIT
  IF ~IsValidForPartyDialog("J#Kelsey")~ THEN DO 
~SetGlobal("BribedCowled","GLOBAL",1)~ EXTERN J#KLSYJ CORNEIL_2
END
END

APPEND J#KlsyJ
  IF ~~ THEN BEGIN CORNEIL_2
    SAY ~Yow! That was worse than I thought. But if it means I can cast 
freely while we're in the city, perhaps it was worthwhile. I doubt we're 
totally free of their scrutiny, however.~
    IF ~~ THEN EXIT
  END
END

