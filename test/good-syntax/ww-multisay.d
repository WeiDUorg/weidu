// Shows that multi-say works 
BEGIN foozle

IF ~Pre Multi Say~ THEN BEGIN l1
  SAY ~Stoneskin~
  IF ~Pre1 (to multi) ~ THEN GOTO l2
  IF ~Pre2~ THEN EXIT
END

IF ~SomeCondition~ THEN BEGIN l2
  SAY ~Acid Arrows~
    = ~Black Dragon~
    = ~Conster~
    = ~Domination~
    = ~Elminster~
  IF ~One~ THEN EXTERN ONE 1
  IF ~Two~ THEN EXTERN TWO 2
  IF ~Pre~ THEN GOTO l1
  IF ~Post~ THEN GOTO l3
END

IF ~Past Multi Say~ THEN BEGIN l3
  SAY ~Finger of Death~
  IF ~PastCond1 (to multi)~ THEN GOTO l2
  IF ~PastCond2~ THEN EXIT
END
