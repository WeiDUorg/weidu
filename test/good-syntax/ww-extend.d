BEGIN foozle

IF ~~ THEN BEGIN start
  SAY ~Acid Arrows~
  IF ~Orig#1~ THEN EXIT
  IF ~Orig#2~ THEN EXIT
  IF ~Orig#3~ THEN EXIT
  IF ~Orig#4~ THEN EXIT
  IF ~Orig#5~ THEN EXIT
END

EXTEND_TOP foozle start #1
  IF ~New-one-from-top~ THEN EXIT
END

EXTEND_BOTTOM foozle start #1
  IF ~New-one-from-bottom~ THEN EXIT
END

/* SHOULD CREATE
IF ~~ THEN BEGIN 0 
  SAY ~Acid Arrows~
  IF ~Orig#1~ THEN EXIT
  IF ~New-one-from-top~ THEN EXIT
  IF ~Orig#2~ THEN EXIT
  IF ~Orig#3~ THEN EXIT
  IF ~Orig#4~ THEN EXIT
  IF ~New-one-from-bottom~ THEN EXIT
  IF ~Orig#5~ THEN EXIT
END
*/
