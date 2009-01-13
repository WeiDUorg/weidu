APPEND J#KLSYJ
IF ~~ THEN BEGIN Bhaal22
  SAY ~Irenicus represents everything that is wrong with power, and
everything I should be using my own powers to fight. Working with you to
defeat him means making a difference, <CHARNAME>. I'm with you.~
[FWKLSYF3] /* #75406 */
COPY_TRANS PLAYER1 33
END
END

ADD_TRANS_TRIGGER PLAYER1 33 ~!IsValidForPartyDialog("J#Kelsey")~
  
EXTEND_BOTTOM PLAYER1 33
  IF ~IsValidForPartyDialog("J#Kelsey")
Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN GOTO 55
  IF ~IsValidForPartyDialog("J#Kelsey")
!Global("J#KelseyRomanceActive","GLOBAL",2)~ THEN GOTO 56
END
